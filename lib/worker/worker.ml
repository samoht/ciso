(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
open Message
open Irmin_unix

let debug fmt = Gol.debug ~section:"worker" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Worker: " ^^ fmt)

module Body = Cohttp_lwt_body
module Code = Cohttp.Code
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

module Local = Irmin.Basic(Irmin_git.FS)(Irmin.Contents.String)

module System = struct

  (* from ocaml-git/lib/unix/git_unix.ml *)

  let openfile_pool = Lwt_pool.create 200 (fun () -> Lwt.return_unit)

  let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> Lwt.fail (Failure (Printexc.to_string e))
    | e -> Lwt.fail e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
    | e -> Lwt.fail e

  let protect f x = Lwt.catch (fun () -> f x) protect_unix_exn

  let safe f x = Lwt.catch (fun () -> f x) ignore_enoent

  let remove_file f = safe Lwt_unix.unlink f

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir && Sys.is_directory dir then Lwt.return_unit
      else (
        let clear =
          if Sys.file_exists dir then (
            Log.debug "%s already exists but is a file, removing." dir;
            remove_file dir;
          ) else
            Lwt.return_unit
        in
        clear >>= fun () ->
        aux (Filename.dirname dir) >>= fun () ->
        Log.debug "mkdir %s" dir;
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then Lwt.fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return_unit in
    match Cstruct.len b with
    | 0   -> Lwt.return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let with_write_file ?temp_dir file fn =
    begin match temp_dir with
      | None   -> Lwt.return_unit
      | Some d -> mkdir d
    end >>= fun () ->
    let dir = Filename.dirname file in
    mkdir dir >>= fun () ->
    let tmp = Filename.temp_file ?temp_dir (Filename.basename file) "write" in
    Lwt_pool.use openfile_pool (fun () ->
        Log.info "Writing %s (%s)" file tmp;
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644) >>= fun fd ->
        Lwt.finalize
          (fun () -> protect fn fd >>= fun () -> Lwt_unix.rename tmp file)
          (fun _  -> Lwt_unix.close fd)
      )

  let write_file file ?temp_dir b =
    with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)

  let read_file file =
    Unix.handle_unix_error (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            Log.info "Reading %s" file;
            let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
            let ba = Lwt_bytes.map_file ~fd ~shared:false () in
            Unix.close fd;
            Lwt.return (Cstruct.of_bigarray ba)
          ))
      ()

end

type store = string -> Local.t

let task name msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = "worker " ^ name in
  Irmin.Task.create ~date ~owner msg

type worker_status = Working of Job.id | Idle

type id = [`Worker] Id.t

type t = {
  id   : id;                                     (* id assigned by the master *)
  local: store;                                                (* local store *)
  store: Store.t;                                             (* global store *)
  mutable status : worker_status;                (* working on a task of idle *)
}

exception WrongResponse of string * string

let idle_sleep = 3.0
let working_sleep = 40.0
let master_timeout = 15.0
let origin_fs = ref []

let body_of_message m =
  Message.sexp_of_worker_msg m
  |> Sexplib.Sexp.to_string
  |> Body.of_string

let message_of_body body =
  Body.to_string body >>= fun b ->
  Sexplib.Sexp.of_string b
  |> Message.master_msg_of_sexp
  |> Lwt.return

let working_directory ?(test = true) () =
  let home = Sys.getenv "HOME" in
  let path = if test then Sys.getcwd () else Filename.concat home "ci-worker" in
  if not (Sys.file_exists path) then
    Lwt_unix.mkdir path 0o770 >|= fun () -> path
  else
    Lwt.return path

let path_of_obj id = ["object"; Id.to_string id]

let clean_up t =
  let print_path k =
    let path = String.concat "/" k in
    Lwt_io.printf "remove %s\n%!" path
  in
  let clean_dir = ["object"] in
  Lwt_list.iter_s (fun dir ->
    Local.list (t ("list files of " ^ dir)) [dir] >>= fun paths ->
    Lwt_list.iter_s (fun path ->
      print_path path >>= fun () ->
      Local.remove (t ("remove " ^ (String.concat "/" path))) path
      ) paths
    ) clean_dir

let local_store ?(fresh = false) dir =
  let config = Irmin_git.config ~root:dir ~bare:true () in
  Local.create config (task dir) >>= fun t ->
  (if fresh then clean_up t else Lwt.return_unit) >|= fun () ->
  t

let rec id_of_path = function
  | [id]    -> id
  | _ :: tl -> id_of_path tl
  | _ -> failwith "empty path"

let local_query t id =
  debug "query: %s" (Id.pretty id);
  let path = path_of_obj id in
  Local.mem (t ("query object " ^ Id.to_string id)) path

let local_publish t id obj =
  debug "publish: %s" (Id.pretty id);
  local_query t id >>= fun exist ->
  (if exist then Lwt.return ()
   else (
     debug "publish: %s doesn't exist!" (Id.pretty id);
     let path = path_of_obj id in
     let value = Object.to_string obj in
     let ln = String.length value in
     debug "publish: length(%s) = %d" (Id.pretty id) ln;
     Local.update (t ("publish object " ^ Id.to_string id)) path value))
  >|= fun () ->
  debug "publish: %s complete!" (Id.pretty id)

let local_retrieve t id =
  debug "retrieve: %s" (Id.pretty id);
  let path = path_of_obj id in
  Local.read_exn (t ("retrieve object" ^ Id.to_string id)) path >|= fun o ->
  debug  "retrieve: %s complete!" (Id.pretty id);
  Object.of_string o

let local_retrieve_all t =
  let retrieve_dir = ["object"] in
  Lwt_list.fold_left_s (fun acc dir ->
      Local.list (t "retrieve objects") [dir] >>= fun paths ->
      Lwt_list.fold_left_s (fun acc' path ->
          Local.read (t ("read from " ^ (String.concat "/" path))) path >|=
          function
          | None -> acc'
          | Some c ->
            let id = id_of_path path in
            let jid = Id.of_string `Job id in
            let oid  = Id.of_string `Object id in
            let obj = Object.of_string c in
            (jid, oid, obj) :: acc'
        ) acc paths
    ) [] retrieve_dir

let with_client_request tag request =
  let timeout_response () =
    Lwt_unix.sleep master_timeout >|= fun () ->
    let resp = Response.make ~status:`I_m_a_teapot () in
    let body = Body.empty in
    resp, body
  in
  let err_handler = function
    | Failure f -> err "[ERROR]: %s\n%!" f
    | exn       ->
      err "[ERROR]: connection to master failed:\n%s" (Printexc.to_string exn)
  in
  Lwt.catch (fun () ->
      Lwt.pick [timeout_response (); request] >>= fun ((resp, _) as r) ->
      (* FIXME: use correct status code *)
      if Response.status resp = `I_m_a_teapot
      then err "master timeout %s " tag
      else Lwt.return r
    ) err_handler

(* POST base/worker<id>/objects -> `Created *)
let worker_publish base t result jid =
  (* FIXME: obj is not used! *)
  debug "publish: object %s" (Id.pretty jid);
  let m = Message.Publish (result, jid) in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%s/objects" (Id.to_string t.id) in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  with_client_request "publish" (Client.post ~body uri) >>= fun (resp, _) ->
  let status = Response.status resp in
  if status = `Created then Lwt.return_unit
  else Lwt.fail (WrongResponse (Code.string_of_status status, "publish"))

(* POST base/worker/registration -> `Created *)
let worker_register store base build_store =
  let host = Host.detect () in
  let body = body_of_message (Message.Register host) in
  let uri_path = "worker/registration" in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  with_client_request "register" (Client.post ~body uri) >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Cohttp.Code.string_of_status status, "register") in
  if status = `Created then
    match m with
    | Ack_heartbeat | New_job _ -> Lwt.fail exn
    | Ack_register id ->
      debug "register: success: id:%s" (Id.pretty id);
      working_directory () >>= fun dir ->
      build_store dir >>= fun local ->
      let worker = { id; store; local; status = Idle} in
      local_retrieve_all local >>= fun tups ->
      if tups = [] then Lwt.return worker
      else
        Lwt_list.iter_s (fun (jid, id, o) ->
          Store.publish_object store id o >>= fun () ->
          Store.unlog_job store jid >>= fun () ->
          worker_publish base worker `Success jid
        ) tups >>= fun () ->
        Lwt.return worker
  else
    Lwt.fail exn

(* POST base/worker<id>/state -> `Ok *)
let worker_heartbeat base t =
  let m =
    match t.status with
    | Working jid ->
      debug "heartbeat: working";
      Heartbeat (Some jid)
    | Idle ->
      debug "heartbeat: idle";
      Heartbeat None in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%s/state" (Id.to_string t.id) in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  with_client_request "heartbeat" (Client.post ~body uri) >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "heartbeat") in
  if status = `OK then
    match m with
    | Ack_heartbeat -> Lwt.return None
    | New_job (id, jdesp) -> Lwt.return (Some (id, jdesp))
    | Ack_register _ -> Lwt.fail exn
  else
    Lwt.fail exn

let worker_spawn base t job_lst =
  let m_job_lst =
    List.rev_map (fun (id, job, deps) ->
        let desp = Job.to_string job in
        id, desp, deps
      ) job_lst
  in
  let m = Message.Spawn_jobs m_job_lst in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%s/newjobs" (Id.to_string t.id) in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  with_client_request "spawn" (Client.post ~body uri)
  >>= fun (resp, _body) ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "spawn") in
  if status = `Created then Lwt.return_unit
  else Lwt.fail exn

(* GET worker_ip:port/path -> `OK *)
let worker_request_object base worker oid =
  local_query worker.local oid >>= fun exist ->
  if exist then (
    debug "object: get object %s locally" (Id.pretty oid);
    local_retrieve worker.local oid
  ) else (
    debug "object: get object %s remotely" (Id.pretty oid);
    Store.retrieve_object worker.store oid >>= fun obj ->
    local_publish worker.local oid obj >>= fun () ->
    (* FIXME: should provide a better link between objects and jobs *)
    (* URGENT FIXME: do we need to update the job task here? *)
    let jid = Id.of_string `Job (Id.to_string oid) in
    worker_publish base worker `Success jid >>= fun () ->
    Lwt.return obj
  )

(* FIXME: use Bos? *)

let (/) = Filename.concat

let niet () = Lwt.return_unit

let exec ?on_failure ?(on_success=niet) cmd =
  let on_failure = match on_failure with
    | None   -> (fun () -> err "%S: failure" cmd)
    | Some f -> f
  in
  let open Lwt_unix in
  Lwt_unix.system cmd >>= function
  | WEXITED rc when rc = 0 -> on_success ()
  | WEXITED _ | WSIGNALED _ | WSTOPPED _ -> on_failure ()

let install_archive (name, content) =
  let tmp = Filename.concat "/tmp" name in
  if Sys.file_exists tmp then Sys.remove tmp;
  System.write_file tmp content >>= fun () ->
  Lwt.return tmp

let extract_archive tar =
  if not (Filename.check_suffix tar ".tar.gz") then
    raise (Invalid_argument tar);
  let cmd = Printf.sprintf "tar -xzf %s -C /" tar in
  let on_failure () = err "extract: cannot untar %s" tar in
  let on_success () = debug "extract: OK"; Lwt.return_unit in
  exec ~on_success ~on_failure cmd

let install_files ~src ~dst files =
  let cp src dst =
    let comm = Printf.sprintf "cp %s %s" src dst in
    let on_failure () = err "cp: cannot copy %s to %s" src dst in
    let on_success () = debug "%s installed" src; Lwt.return_unit in
    exec ~on_success ~on_failure comm
  in
  Lwt_list.iter_s (fun f ->
      let src_path = Filename.concat src f in
      let dst_path = Filename.concat dst f in
      System.mkdir (Filename.dirname (dst / f)) >>= fun () ->
      cp src_path dst_path
    ) files

let name_of_archive name =
  assert (Filename.check_suffix name ".tar.gz");
  Filename.chop_extension (Filename.chop_extension name)

let clean_tmp action name =
  let file = "/tmp" / name in
  let dir = "/tmp" / name_of_archive name in
  let comm = Printf.sprintf "rm -rf %s %s" file dir in
  let on_success () =
    debug "clean_tmp: %s done (%s %s)!" action file dir;
    Lwt.return_unit
  in
  let on_failure () = err "cannot remove %s and %s" file dir in
  exec ~on_failure ~on_success comm

let apply_archive prefix obj =
  let files = Object.files obj in
  let archive = Object.archive obj in
  install_archive archive >>= fun arch_path ->
  extract_archive arch_path >>= fun () ->
  let src = name_of_archive arch_path in
  install_files ~src ~dst:prefix files >>= fun () ->
  Lwt.return arch_path

let apply_object prefix obj =
  apply_archive prefix obj >>= fun arch_path ->
  let src = name_of_archive arch_path in
  let path = Filename.concat src "installed" in
  Opam.update_metadata ~install:true ~path >>= fun () ->
  clean_tmp "apply" (Filename.basename arch_path)

(*
(* FIXME: shoubd not be used *)
let patch_ocamlfind prefix =
  let bin_path = Filename.concat prefix "bin/ocamlfind" in
  if not (Sys.file_exists bin_path) then Lwt.return ""
  else (
    debug "patch: %s/bin/ocamlfind" prefix;
    Lwt_io.open_file ~mode:Lwt_io.input bin_path >>= fun ic ->
    Lwt_io.read ic >>= fun c ->
    Lwt_io.close ic >>= fun () ->
    let open Re in
    let re =
      compile (seq [str ".opam/";
                    group (rep1 (compl [char '/']));
                    str "/lib/findlib.conf"])
    in
    let subs = exec re c in
    let sb, se = get_ofs subs 1 in
    let switch = Filename.basename prefix in
    for i = 0 to (se - sb - 1) do
      let char = try String.get switch i
        with _ -> '_' in
      let pos = sb + i in
      Bytes.set c pos char;
    done;
    let pb, pe = get_ofs subs 0 in
    let conf_path = String.sub c pb (pe - pb) in
    debug "findlib.conf: path=%s" conf_path;
    Lwt_io.open_file ~mode:Lwt_io.output bin_path >>= fun oc ->
    Lwt_io.write oc c >>= fun () ->
    Lwt_io.close oc >|= fun () ->
    Filename.concat (Sys.getenv "HOME") conf_path
  )
*)

let hash str =
  let `Hex h =
    str
    |> (fun x -> Cstruct.of_string x)
    |> Nocrypto.Hash.SHA1.digest
    |> (fun x -> Hex.of_cstruct x) in
  h

let fs_snapshots ?white_list dir =
  let checksum_of_file file =
    let checksum_of_ic ic =
      Lwt_io.read ic >>= fun content ->
      Lwt.return (hash (file ^ content)) in
    Lwt_io.with_file ~mode:Lwt_io.input file checksum_of_ic
  in
  let rec loop checksums = function
    | [] -> Lwt.return checksums
    | path :: tl ->
      (* soft link to absent file *)
      if not (Sys.file_exists path) then loop checksums tl
      else if not (Sys.is_directory path) then
        checksum_of_file path >>= fun c ->
        loop ((path, c) :: checksums) tl
      else
        let files =
          Sys.readdir path
          |> Array.to_list
          |> List.rev_map (fun f -> Filename.concat path f) in
        loop checksums (List.rev_append files tl)
  in
  let sub_dirs =
    Sys.readdir dir |> Array.to_list
    |> (fun lst ->
        match white_list with
        | Some wl -> List.filter (fun n -> List.mem n wl) lst
        | None   -> lst)
    |> List.rev_map (fun n -> Filename.concat dir n)
  in
  loop [] sub_dirs

(* FIXME: use opam-lib *)
let read_installed dir =
  let path = dir / "installed" in
  assert (Sys.file_exists path);
  let rec collect_lines acc ic =
    Lwt_io.read_line_opt ic >>= function
    | None -> Lwt.return acc
    | Some l -> collect_lines (l :: acc) ic
  in
  Lwt_io.with_file ~mode:Lwt_io.input path (collect_lines [])

let collect_output prefix p v = function
  | `Delegate _ -> assert false
  | `Success -> Lwt.return []
  | `Fail _ ->
     let relative_path = ["build"; p ^ "." ^ v] |> String.concat "/" in
     let path = Filename.concat prefix relative_path in
     if Sys.file_exists path then
       let files = Sys.readdir path |> Array.to_list in
       List.filter (fun f ->
           List.exists (fun suffix -> Filename.check_suffix f suffix)
             [".info"; ".err"; ".out"; ".env"]) files
       |> List.rev_map (fun f -> Filename.concat relative_path f)
       |> Lwt.return
     else
       Lwt.return []

let collect_installed prefix ~before ~after =
  let module CsMap = Map.Make(String) in
  let cmap =
    List.fold_left
      (fun acc (f, checksum) -> CsMap.add f checksum acc)
      CsMap.empty before
  in
  (* TODO: collect deleted files *)
  let installed =
    List.fold_left (fun acc (f, checksum) ->
        if not (CsMap.mem f cmap) then f :: acc else
          let cs = CsMap.find f cmap in
          if cs <> checksum then f :: acc
          else acc
      ) [] after
  in
  (* 1 is for the delimiter *)
  let len = 1 + String.length prefix in
  let chop_prefix f =
    try String.sub f len (String.length f - len)
    with e -> print_endline f; raise e
  in
  Lwt.return (List.rev_map chop_prefix installed)

let create_archive prefix id output files old nw =
  let dir = "/tmp" / Id.to_string id in
  (if Sys.file_exists dir then exec ("rm -rf " ^ dir) else Lwt.return ())
  >>= fun () ->
  System.mkdir dir >>= fun () ->
  let files = List.rev_append output files in
  install_files ~src:prefix ~dst:dir files >>= fun () ->
  let pkg_file = dir / "installed" in
  let write_pkgs old nw oc =
    (* FIXME: use opam-lib ? *)
    let installed_pkgs = List.filter (fun p -> not (List.mem p old)) nw in
    let content = String.concat "\n" installed_pkgs in
    Lwt_io.write oc content
  in
  Lwt_io.with_file ~mode:Lwt_io.output pkg_file (write_pkgs old nw) >>= fun () ->
  let name = Id.to_string id ^ ".tar.gz" in
  let path = "/tmp" / name in
  let cmd = Printf.sprintf "tar -zcf %s %s" path dir in
  exec cmd >>= fun () ->
  System.read_file path >|= fun content ->
  name, content

let pkg_build prefix jid name version =
  (* patch_ocamlfind prefix >>= fun write_path ->
  (if write_path = "" then Lwt.return_unit
     else Opam.findlib_conf ~prefix ~write_path) >>= fun () -> *)
  let white_list = ["lib"; "bin"; "sbin"; "doc"; "share"; "etc"; "man"] in
  debug "pkg_build: snapshot %s BEFORE" prefix;
  fs_snapshots ~white_list prefix >>= fun before ->
  read_installed prefix >>= fun old_pkgs ->
  debug "pkg_build: FOR REAL %s.%s" name version;
  Opam.install ~name ~version >>= fun result ->
  (match result with
   | `Delegate _ -> assert false
   | `Success -> debug "pkg_build: %s SUCCESS" name
   | `Fail f  -> debug "pkg_build: %s FAIL: %s" name f);
  debug "pkg_build: snapshot %s AFTER" prefix;
  fs_snapshots ~white_list prefix >>= fun after ->
  read_installed prefix >>= fun new_pkgs ->
  collect_output prefix name version result >>= fun output ->
  collect_installed prefix ~before ~after >>= fun installed ->
  create_archive prefix jid output installed old_pkgs new_pkgs
  >>= fun archive ->
  clean_tmp "execute" (fst archive) >>= fun () ->
  Opam.uninstall ~name ~version >|= fun () ->
  result, output, installed, archive

let compiler_fs_origin root switch ?snapshots () =
  (match snapshots with
   | Some s -> Lwt.return s
   | None -> fs_snapshots ~white_list:[switch] root)
  >|= fun ss ->
  debug "snapshots: %s origin fs state" switch;
  origin_fs := List.rev_map fst ss

let switch_clean_up  root compiler =
  let fs = !origin_fs in
  if fs = [] then ()
  else (
    let module FileSet = Set.Make(String) in
    let fset =
      List.fold_left (fun acc f -> FileSet.add f acc) FileSet.empty fs
    in
    let read_dir dir =
      Sys.readdir dir
      |> Array.to_list
      |> List.rev_map (Filename.concat dir)
    in
    (* post-order iterate the fs,
       remove non-origin files and empty direcoties *)
    let q = Queue.create () in
    let s = Stack.create () in
    Queue.add (Filename.concat root compiler) q;
    while not (Queue.is_empty q) do
      let elem = Queue.pop q in
      if Sys.is_directory elem then
        read_dir elem
        |> List.iter (fun sub -> Queue.add sub q);
      Stack.push elem s;
    done;
    while not (Stack.is_empty s) do
      let elem = Stack.pop s in
      if Sys.is_directory elem then
        if read_dir elem = [] then Unix.rmdir elem
        else ()
      else if not (FileSet.mem elem fset) then
        Sys.remove elem
      else ()
    done
  )

let build_compiler_object cid root compiler =
  debug "snapshot: %s BEFORE" root;
  fs_snapshots ~white_list:[compiler] root >>= fun before ->
  Opam.install_switch compiler >>= fun () ->
  debug "snapshot: %s AFTER" root;
  fs_snapshots ~white_list:[compiler] root >>= fun after ->
  compiler_fs_origin root compiler ~snapshots:after () >>= fun () ->
  let prefix = Filename.concat root compiler in
  collect_installed prefix ~before ~after >>= fun files ->
  read_installed prefix >>= fun new_pkgs ->
  create_archive prefix cid [] files [] new_pkgs >|= fun archive ->
  Object.create ~id:cid ~outputs:[] ~files ~archive

let install_compiler worker c host =
  let root = OpamFilename.Dir.to_string OpamStateConfig.(!r.root_dir) in
  (* FIXME: the hash should not be computed here *)
  let cid = Id.of_string `Object (hash (Host.to_string host ^ root ^ c)) in
  let apply_compiler_obj obj =
    debug "apply: %s start" c;
    (* prefix directory has to be existe for apply_archive to work *)
    let prefix = Filename.concat root c in
    (if not (Sys.file_exists prefix && Sys.is_directory prefix)
     then Lwt_unix.mkdir prefix 0o770
     else Lwt.return ()) >>= fun () ->
    apply_archive prefix obj >>= fun _ ->
    (* TODO: clean tmp archive file *)
    Opam.switch_to c >|= fun () ->
    debug "apply: %s end" c
  in
  local_query worker.local cid >>= fun local_exist ->
  if local_exist then
    (debug "install: %s retrieve locally" c;
     local_retrieve worker.local cid) >>=
    apply_compiler_obj >>= fun () ->
    compiler_fs_origin root c ()
  else
    Store.query_compiler worker.store cid >>= fun remote_exist ->
    if remote_exist then
      (debug "install: %s retrieve remotely" c;
       Store.retrieve_compiler worker.store cid) >>= fun cobj ->
      local_publish worker.local cid cobj >>= fun () ->
      apply_compiler_obj cobj >>= fun () ->
      compiler_fs_origin root c ()
    else
      build_compiler_object cid root c >>= fun cobj ->
      Store.publish_compiler worker.store cid cobj >>= fun () ->
      local_publish worker.local cid cobj

let pkg_job_execute base worker jid job deps =
  let root = OpamFilename.Dir.to_string OpamStateConfig.(!r.root_dir) in
  let repos = Job.repos job in
  let pins = Job.pins job in
  Lwt.catch (fun () ->
      (match repos with
       | [] -> Opam.clean_repos (); Lwt.return_unit
       | _  -> Opam.add_repos repos)
      >>= fun () ->
      Opam.update () >>= fun () ->
      let c_curr = Opam.compiler () in
      let c = Job.compiler job in
      (if c = c_curr then Lwt.return_unit
       else
         Opam.export_switch c >>= fun () ->
         install_compiler worker c (Job.host job) >|= fun () ->
         debug "execute: compiler %s installed" c
      ) >>= fun () ->
      (match pins with
       | [] -> Lwt.return_unit
       | _  ->
         let build =
           let dir = Filename.concat root c in
           Filename.concat dir "build"
         in
         Unix.mkdir build 0o775;
         Opam.add_pins pins
      ) >>= fun () ->
      let prefix = Opam.get_var "prefix" in
      debug "execute: opam load state";
      Opam.show_repo_pin () >>= fun () ->
      debug "execute: %d dependencies" (List.length deps);
      Lwt_list.iter_s (fun dep ->
          worker_request_object base worker dep >>= fun obj ->
          apply_object prefix obj
        ) deps
      >>= fun () ->
      (* FIXME: we should not have access to task here *)
      let pkgs = Task.packages (Job.task job) in
      let graph = Opam.resolve_packages pkgs in
      match Opam.is_simple graph with
      | None ->
        debug "execute: simple=false";
        let job_lst = Opam.jobs ~repos ~pins graph in
        worker_spawn base worker job_lst >|= fun () ->
        let delegate_id =
          List.fold_left (fun acc (id, job, _) ->
              let p = List.hd (Task.packages (Job.task job)) in
              if List.mem p pkgs then id :: acc else acc
            ) [] job_lst
          |> (fun id_lst -> assert (1 = List.length id_lst); List.hd id_lst)
        in
        let result = `Delegate delegate_id in
        (* FIXME: this is weird *)
        let archive = "", Cstruct.of_string "" in
        let id = Job.output job in
        let obj = Object.create ~id ~outputs:[] ~files:[] ~archive in
        switch_clean_up root c;
        result, obj
      | Some pkg ->
        debug "execute: simple=%s" (Package.to_string pkg);
        let v =
          match Package.version pkg with Some v -> v | None -> assert false
        in
        pkg_build prefix jid (Package.name pkg) v
        >|= fun (result, outputs, files, archive) ->
        (* FIXME: update job result *)
        let id = Job.output job in
        let obj = Object.create ~id ~outputs ~files ~archive in
        switch_clean_up root c;
        result, obj
    ) (fun exn ->
      let result = match exn with
        | Failure f -> `Fail f
        | _ -> `Fail "unknow execution failure" in
      (* FIXME: this is weird *)
      let archive = "", Cstruct.of_string "" in
      let id = Job.output job in
      (* FIXME: update job result *)
      let obj = Object.create ~id ~outputs:[] ~files:[] ~archive in
      switch_clean_up root (Job.compiler job);
      Lwt.return (result, obj))

let install_compiler job =
  let jid = Job.id job in
  let comp = Job.compiler job in
  debug "execute: build compiler: %s" comp;
  try
    Opam.switch_to comp >>= fun () ->
    let switch = comp in
    let path = Opam.get_var "prefix" / switch in
    debug "execute: snapshot %s AFTER" path;
    read_installed path >>= fun new_pkgs ->
    fs_snapshots path >>= fun after_build ->
    collect_installed path ~before:[] ~after:after_build >>= fun files ->
    create_archive path jid [] files [] new_pkgs >>= fun archive ->
    let result = `Success in
    debug "execute: create object";
    let id = Job.output job in
    (* FIXME: update job result *)
    let obj = Object.create ~id ~outputs:[] ~files ~archive in
    clean_tmp "compiler" (fst archive) >>= fun () ->
    Opam.remove_switch switch >|= fun () ->
    result, obj
  with _ ->
    (* FIXME: catch-all is bad *)
    let result = `Fail (comp ^ " build fail") in
    (* FIXME: this is weird *)
    let archive = "", Cstruct.of_string "" in
    let id = Job.output job in
    (* FIXME: update job result *)
    let obj = Object.create ~id ~outputs:[] ~files:[] ~archive in
    Lwt.return (result, obj)

let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (jid, deps) ->
  local_query worker.local jid >>= fun completed ->
  if completed then execution_loop base worker cond
  else (
    worker.status <- Working jid;
    (* URGENT FIXME: we don't want to get the task here *)
    (match Task.to_compiler task with
     | None   -> pkg_job_execute base worker jid job deps
     | Some _ -> compiler_job_execute jid job)
    >>= fun (result, obj) ->
    let id = Job.output job in
    (match result with
     | `Delegate _ ->
       Store.publish_object worker.store id obj >>= fun () ->
       Store.unlog_job worker.store jid >>= fun () ->
       worker_publish base worker result jid
     | `Success ->
       Store.publish_object worker.store id obj >>= fun () ->
       Store.unlog_job worker.store jid >>= fun () ->
       Lwt.join [worker_publish base worker result jid;
                 local_publish worker.local id obj]
     | `Fail _ ->
       Store.publish_object worker.store id obj >>= fun () ->
       worker_publish base worker result jid
    ) >>= fun () ->
    worker.status <- Idle;
    execution_loop base worker cond
  )

let rec heartbeat_loop base worker cond =
  match worker.status with
  | Idle -> begin worker_heartbeat base worker >>= function
    | None ->
      Lwt_unix.sleep idle_sleep
      >>= fun () -> heartbeat_loop base worker cond
    | Some (id, desp) ->
      Lwt_condition.signal cond (id, desp);
      Lwt_unix.sleep idle_sleep
      >>= fun () -> heartbeat_loop base worker cond end
  | Working _ ->
    worker_heartbeat base worker >>= fun _ ->
    Lwt_unix.sleep working_sleep
    >>= fun () -> heartbeat_loop base worker cond

let run ~base ~uri ~fresh =
  Store.create ~uri () >>= fun store ->
  let base = Uri.of_string base in
  let build_store = local_store ~fresh in
  worker_register store base build_store >>= fun worker ->
  let cond = Lwt_condition.create () in
  Lwt.pick [
    heartbeat_loop base worker cond;
    execution_loop base worker cond;
  ]
