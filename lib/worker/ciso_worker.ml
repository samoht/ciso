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

let debug fmt = Gol.debug ~section:"worker" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Worker: " ^^ fmt)
let failwith fmt = Printf.ksprintf failwith ("Ciso.Worker: " ^^ fmt)

let (/) = Filename.concat

module Body = Cohttp_lwt_body
module Code = Cohttp.Code
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

module IdSet = struct
  include Set.Make(struct
      type t = [`Object] Id.t
      let compare = Id.compare
    end)
  let of_list = List.fold_left (fun s e -> add e s) empty
end

module System = struct

  (* FIXME: use Bos? *)

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

  (* end of ocaml-git *)

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
    let tmp = "/tmp/ciso-" / name in
    if Sys.file_exists tmp then Sys.remove tmp;
    write_file tmp content >>= fun () ->
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
        let src_path = src / f in
        let dst_path = dst / f in
        mkdir (Filename.dirname (dst / f)) >>= fun () ->
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

end

type t = { w: Worker.t; local : Store.t; global: Store.t }

let idle_sleep = 3.0
let working_sleep = 40.0
let master_timeout = 15.0
let origin_fs = ref []

let working_directory w =
  let path = Sys.getenv "HOME"/ "worker-" ^ Id.to_string (Worker.id w) in
  if not (Sys.file_exists path) then
    Lwt_unix.mkdir path 0o770 >|= fun () -> path
  else
    Lwt.return path

let local_store w =
  working_directory w >>= fun root ->
  Store.local ~root ()

let create w uri =
  local_store w        >>= fun local ->
  Store.remote ~uri () >|= fun global ->
  { w; local; global }

let fs_snapshots ?white_list dir =
  let rec loop checksums = function
    | [] -> checksums
    | path :: tl ->
      (* soft link to absent file *)
      if not (Sys.file_exists path) then loop checksums tl
      else if not (Sys.is_directory path) then
        loop ((path, Digest.file path) :: checksums) tl
      else
        let files =
          Sys.readdir path
          |> Array.to_list
          |> List.rev_map (fun f -> path / f)
        in
        loop checksums (List.rev_append files tl)
  in
  let sub_dirs =
    Sys.readdir dir
    |> Array.to_list
    |> (fun lst -> match white_list with
        | Some wl -> List.filter (fun n -> List.mem n wl) lst
        | None    -> lst)
    |> List.rev_map (fun n -> dir / n)
  in
  loop [] sub_dirs

let collect_outputs prefix pkg = function
  | `Success -> Lwt.return []
  | `Fail _  ->
     let relative_path = "build" / Package.to_string pkg in
     let path = prefix / relative_path in
     if Sys.file_exists path then
       let files = Sys.readdir path |> Array.to_list in
       List.filter (fun f ->
           List.exists
             (fun suffix -> Filename.check_suffix f suffix)
             [".info"; ".err"; ".out"; ".env"]
         ) files
       |> List.rev_map (fun f -> relative_path / f)
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

let compiler_fs_origin root switch ?snapshots () =
  let ss = match snapshots with
    | Some s -> s
    | None -> fs_snapshots ~white_list:[switch] root
  in
  debug "snapshots: %s origin fs state" switch;
  origin_fs := List.rev_map fst ss

let switch_clean_up root compiler =
  let fs = !origin_fs in
  if fs = [] then ()
  else (
    let module FileSet = Set.Make(String) in
    let fset =
      List.fold_left (fun acc f -> FileSet.add f acc) FileSet.empty fs
    in
    let read_dir dir =
      Sys.readdir dir |> Array.to_list |> List.rev_map (fun f -> dir / f)
    in
    (* post-order iterate the fs, remove non-origin files and empty direcoties *)
    let q = Queue.create () in
    let s = Stack.create () in
    Queue.add (root / compiler) q;
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


(* FIXME: outputs should not be in the archive *)
let create_object prefix id outputs files ~old_pkgs ~new_pkgs =
  let dir = "/tmp" / Id.to_string id in
  (if Sys.file_exists dir then System.exec ("rm -rf " ^ dir) else Lwt.return ())
  >>= fun () ->
  System.mkdir dir >>= fun () ->
  let files = List.rev_append outputs files in
  System.install_files ~src:prefix ~dst:dir files >>= fun () ->
  let installed = List.filter (fun p -> not (List.mem p old_pkgs)) new_pkgs in
  Opam.write_installed installed;
  let name = Id.to_string id ^ ".tar.gz" in
  let path = "/tmp" / name in
  let cmd = Printf.sprintf "tar -zcf %s %s" path dir in
  System.exec cmd >>= fun () ->
  System.read_file path >|= fun content ->
  Object.create name, content

let extract_object prefix obj =
  let files = Object.files obj in
  let archive = Object.archive obj in
  System.install_archive archive >>= fun arch_path ->
  System.extract_archive arch_path >>= fun () ->
  let src = System.name_of_archive arch_path in
  System.install_files ~src ~dst:prefix files >>= fun () ->
  System.clean_tmp "extract_object" (Filename.basename arch_path) >>= fun () ->
  Lwt.return arch_path

let pkg_build prefix jid pkg =
  let white_list = ["lib"; "bin"; "sbin"; "doc"; "share"; "etc"; "man"] in
  debug "pkg_build: snapshot %s BEFORE" prefix;
  let before = fs_snapshots ~white_list prefix in
  let old_pkgs = Opam.installed () in
  debug "pkg_build: %s" (Package.to_string pkg);
  Opam.install pkg >>= fun result ->
  (match result with
   | `Success -> debug "pkg_build: %s SUCCESS" (Package.to_string pkg)
   | `Fail f  -> debug "pkg_build: %s FAIL: %s" (Package.to_string pkg) f);
  debug "pkg_build: snapshot %s AFTER" prefix;
  let after = fs_snapshots ~white_list prefix in
  let new_pkgs = Opam.installed () in
  collect_outputs prefix pkg result >>= fun output ->
  collect_installed prefix ~before ~after >>= fun installed ->
  create_archive prefix jid output installed ~old_pkgs ~new_pkgs
  >>= fun archive ->
  System.clean_tmp "pkg_build" (fst archive) >>= fun () ->
  Opam.remove pkg >|= fun () ->
  result, output, installed, archive

let build_compiler_object cid root compiler =
  debug "snapshot: %s BEFORE" root;
  let before = fs_snapshots ~white_list:[compiler] root in
  Opam.install_switch compiler >>= fun () ->
  debug "snapshot: %s AFTER" root;
  let after = fs_snapshots ~white_list:[compiler] root in
  compiler_fs_origin root compiler ~snapshots:after ();
  let prefix = root / compiler in
  collect_installed prefix ~before ~after >>= fun files ->
  let new_pkgs = Opam.installed () in
  create_archive prefix cid [] files ~old_pkgs:[] ~new_pkgs >|= fun archive ->
  Object.create ~id:cid ~outputs:[] ~files ~archive

let install_compiler worker c host =
  let root = OpamFilename.Dir.to_string OpamStateConfig.(!r.root_dir) in
  let apply_compiler_obj obj =
    debug "apply: %s start" c;
    (* prefix directory has to be existe for apply_archive to work *)
    let prefix = root / c in
    (if not (Sys.file_exists prefix && Sys.is_directory prefix)
     then Lwt_unix.mkdir prefix 0o770
     else Lwt.return ()) >>= fun () ->
    install_object prefix obj >>= fun _ ->
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
         let build = root / Compiler.to_string c / "build" in
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

let run host uri =
  let worker = Worker.create host in
  Store.remote ~uri () >>= fun global ->
  local_store worker   >>= fun local  ->
  worker_register store base build_store >>= fun worker ->
  let cond = Lwt_condition.create () in
  Lwt.pick [
    heartbeat_loop base worker cond;
    execution_loop base worker cond;
  ]
