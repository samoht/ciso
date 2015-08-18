open Lwt.Infix
open Message

let debug fmt = Gol.debug ~section:"worker" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Worker: " ^^ fmt)

module Body = Cohttp_lwt_body
module Code = Cohttp.Code
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

type t = {
  id : int;                           (* worker id assigned by master *)
  token : Common_types.worker_token;  (* used to publish in the store *)
  store : store;
  mutable status : worker_status;  (* working on a task of idle *)
}
and worker_status =
  | Working of Common_types.id
  | Idle
and store = (string -> ([`BC], Irmin.Contents.String.Path.t,
                               Irmin.Contents.String.t) Irmin.t)

exception WrongResponse of string * string

let idle_sleep = 3.0
let working_sleep = 40.0
let master_timeout = 15.0

let sub len str = String.sub str 0 len
let sub_abbr = sub 5

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

let path_of_obj id = ["object"; id]
let path_of_com id = ["compiler"; id]

let clean_up t =
  let print_path k =
    let path = String.concat "/" k in
    Lwt_io.printf "remove %s\n%!" path
  in
  let clean_dir = ["object"] in
  Lwt_list.iter_s (fun dir ->
    Irmin.list (t ("list files of " ^ dir)) [dir] >>= fun paths ->
    Lwt_list.iter_s (fun path ->
      print_path path >>= fun () ->
      Irmin.remove (t ("remove " ^ (String.concat "/" path))) path
      ) paths
    ) clean_dir

let local_store ?(fresh = false) dir =
  let basic =
    Irmin.basic (module Irmin_unix.Irmin_git.FS) (module Irmin.Contents.String)
  in
  let config = Irmin_git.config ~root:dir ~bare:true () in
  Irmin.create basic config Irmin_unix.task >>= fun t ->
  (if fresh then clean_up t else Lwt.return_unit) >|= fun () ->
  t

let rec id_of_path = function
  | [id] -> id
  | _ :: tl -> id_of_path tl
  | _ -> failwith "empty path"

let comp_s = function true -> "compiler" | false -> "object"

let local_query t ?(compiler = false) id =
  debug "query: %s %s" (comp_s compiler) (sub_abbr id);
  let path = (if compiler then path_of_com else path_of_obj) id in
  Irmin.mem (t ("query object " ^ id)) path

let local_publish t ?(compiler = false) id obj =
  debug "publish: %s %s" (comp_s compiler) (sub_abbr id);
  local_query t ~compiler id >>= fun exist ->
  (if exist then Lwt.return ()
   else (
     debug "publish: %s doesn't exist!" (sub_abbr id);
     let path = (if compiler then path_of_com else path_of_obj) id in
     let value = Object.string_of_t obj in
     let ln = String.length value in
     debug "publish: length(%s) = %d" (sub_abbr id) ln;
     Irmin.update (t ("publish object " ^ id)) path value))
  >|= fun () ->
  debug "publish: %s %s complete!" (comp_s compiler) (sub_abbr id)

let local_retrieve t ?(compiler = false) id =
  debug "retrieve: %s %s" (comp_s compiler) (sub_abbr id);
  let path = (if compiler then path_of_com else path_of_obj) id in
  Irmin.read_exn (t ("retrieve object" ^ id)) path >|= fun o ->
  debug  "retrieve: %s %s complete!" (comp_s compiler) (sub_abbr id);
  Object.t_of_string o

let local_retrieve_all t =
  let retrieve_dir = ["object"] in
  Lwt_list.fold_left_s (fun acc dir ->
      Irmin.list (t "retrieve objects") [dir] >>= fun paths ->
      Lwt_list.fold_left_s (fun acc' path ->
          Irmin.read (t ("read from " ^ (String.concat "/" path))) path >>= function
          | None -> Lwt.return acc'
          | Some c ->
            let id = id_of_path path in
            let obj = Object.t_of_string c in
            Lwt.return ((id, obj) :: acc')
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
let worker_publish base t result oid _obj =
  (* FIXME: obj is not used! *)
  debug "publish: object %s" (sub_abbr oid);
  let headers = Cohttp.Header.of_list ["worker", t.token] in
  let m = Message.Publish (result, oid) in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%d/objects" t.id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  with_client_request "publish" (Client.post ~headers ~body uri)
  >>= fun (resp, _) ->
  let status = Response.status resp in
  if status = `Created then Lwt.return_unit
  else Lwt.fail (WrongResponse (Code.string_of_status status, "publish"))

(* POST base/worker/registration -> `Created *)
let worker_register base build_store =
  let host = Host.detect () |> Host.to_string in
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
    | Ack_register (id, token) ->
      debug "register: success: %d token: %s" id (sub_abbr token);
      working_directory () >>= fun dir ->
      build_store dir >>= fun store ->
      let worker = { id; token; store; status = Idle} in
      local_retrieve_all store >>= fun tups ->
      if tups = [] then Lwt.return worker
      else Lwt_list.iter_s (fun (id, o) ->
          Store.publish_object token id o >>= fun () ->
          Store.unlog_job id >>= fun () ->
          worker_publish base worker `Success id o) tups >>= fun () ->
        Lwt.return worker
  else
    Lwt.fail exn

(* POST base/worker<id>/state -> `Ok *)
let worker_heartbeat base t =
  let headers = Cohttp.Header.of_list ["worker", t.token] in
  let m =
    match t.status with
    | Working jid ->
      debug "heartbeat: working";
      Heartbeat (Some jid)
    | Idle ->
      debug "heartbeat: idle";
      Heartbeat None in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%d/state" t.id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  with_client_request "heartbeat" (Client.post ~headers ~body uri)
  >>= fun (resp, body) ->
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
  let headers = Cohttp.Header.of_list ["worker", t.token] in
  let m_job_lst =
    List.rev_map (fun (id, job, deps) ->
        let desp = Task.sexp_of_job job |> Sexplib.Sexp.to_string in
        id, desp, deps
      ) job_lst
  in
  let m = Message.Spawn_jobs m_job_lst in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%d/newjobs" t.id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  with_client_request "spawn" (Client.post ~headers ~body uri)
  >>= fun (resp, _body) ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "spawn") in
  if status = `Created then Lwt.return_unit
  else Lwt.fail exn

(* GET worker_ip:port/path -> `OK *)
let worker_request_object base worker oid =
  let store = worker.store in
  local_query store oid >>= fun exist ->
  if exist then (
    debug "object: get object %s locally" (sub_abbr oid);
    local_retrieve store oid
  ) else (
    debug "object: get object %s remotely" (sub_abbr oid);
    Store.retrieve_object oid >>= fun obj ->
    local_publish store oid obj >>= fun () ->
    worker_publish base worker `Success oid obj >>= fun () ->
    Lwt.return obj
  )

let with_lwt_comm ?fail ?success comm =
  let fail = match fail with
    | None -> fun () -> Lwt.return_unit
    | Some f -> f in
  let success = match success with
    | None -> fun () -> Lwt.return_unit
    | Some f -> f in
  Lwt_unix.system comm >>= function
  | Lwt_unix.WEXITED rc when rc = 0 -> success ()
  | Lwt_unix.WEXITED _ | Lwt_unix.WSIGNALED _ | Lwt_unix.WSTOPPED _ -> fail ()

let install_archive (name, content) =
  let tmp = Filename.concat "/tmp" name in
  if Sys.file_exists tmp then Sys.remove tmp;
  let write_content oc =
    Lwt_io.fprint oc content in
  Lwt_io.with_file ~mode:Lwt_io.output tmp write_content
  >>= fun () ->
  (* log "apply" "archive" ~info:tmp; *)
  Lwt.return tmp

let extract_archive tar =
  if not (Filename.check_suffix tar ".tar.gz") then
    raise (Invalid_argument tar);
  let comm = Printf.sprintf "tar -xzf %s -C /" tar in
  let fail () = err "extract: cannot untar %s" tar in
  (* let success () = return (log "extract" tar ~info:"OK") in *)
  with_lwt_comm ~fail comm

let install_files ~src ~dst files =
  let prepare_cp dst file =
    let file_lt = Re_str.split (Re_str.regexp_string "/") file in
    let rec mkdir parent = function
      | [] | [_] -> Lwt.return ()
      | dir :: path ->
        let dir = Filename.concat parent dir in
        (if not (Sys.file_exists dir && Sys.is_directory dir)
         then Lwt_unix.mkdir dir 0o770
         else Lwt.return ())
        >>= fun () ->
        mkdir dir path
    in
    mkdir dst file_lt in
  let cp src dst =
    let comm = Printf.sprintf "cp %s %s" src dst in
    let fail () = err "cp: cannot copy %s to %s" src dst in
    with_lwt_comm ~fail comm
  in
  Lwt_list.iter_s (fun f ->
      let src_path = Filename.concat src f in
      let dst_path = Filename.concat dst f in
      prepare_cp dst f >>= fun () ->
      cp src_path dst_path
    ) files

let clean_tmp action name =
  let file = Filename.concat "/tmp" name in
  let dir =
    name
    |> Filename.chop_extension |> Filename.chop_extension
    |> Filename.concat "/tmp"
  in
  let comm = Printf.sprintf "rm -r %s %s" file dir in
  let success () =
    debug "clean_tmp: %s done (%s %s)!" action file dir;
    Lwt.return_unit
  in
  with_lwt_comm ~success comm

let apply_archive prefix obj =
  let installed, archive = Object.apply_info obj in
  install_archive archive >>= fun arch_path ->
  extract_archive arch_path >>= fun () ->
  (* name.tar.gz *)
  let src = arch_path |> Filename.chop_extension |> Filename.chop_extension in
  install_files ~src ~dst:prefix installed >>= fun () ->
  Lwt.return arch_path

let apply_object prefix obj =
  apply_archive prefix obj >>= fun arch_path ->
  (* name.tar.gz *)
  let src = arch_path |> Filename.chop_extension |> Filename.chop_extension in
  let path = Filename.concat src "installed" in
  Ci_opam.update_metadata ~install:true ~path >>= fun () ->
  clean_tmp "apply" (Filename.basename arch_path)

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
        | None -> lst)
    |> List.rev_map (fun n -> Filename.concat dir n)
  in
  loop [] sub_dirs

let read_installed dir =
  let path = Filename.concat dir "installed" in
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

let create_archive prefix id output installed old nw =
  let dir = Filename.concat "/tmp" (sub_abbr id) in
  (if Sys.file_exists dir then with_lwt_comm ("rm -r " ^ dir)
   else Lwt.return ()) >>= fun () ->
  Lwt_unix.mkdir dir 0o770 >>= fun () ->
  (List.rev_append output installed
   |> install_files ~src:prefix ~dst:dir) >>= fun () ->
  let pkg_file = Filename.concat dir "installed" in
  let write_pkgs old nw oc =
    let installed_pkgs = List.filter (fun p -> not (List.mem p old)) nw in
    let content = String.concat "\n" installed_pkgs in
    Lwt_io.write oc content
  in
  Lwt_io.with_file ~mode:Lwt_io.output pkg_file (write_pkgs old nw) >>= fun () ->
  let name = (sub_abbr id) ^ ".tar.gz" in
  let path = Filename.concat "/tmp" name in
  let comm = Printf.sprintf "tar -zcf %s %s" path dir in
  with_lwt_comm comm >>= fun () ->
  Lwt_io.with_file ~mode:Lwt_io.input path (fun x -> Lwt_io.read x)
  >|= fun content ->
  name, content

let pkg_build prefix jid name version =
  (* TODO: disable ocamlfind patch *)
  patch_ocamlfind prefix >>= fun write_path ->
  (if write_path = "" then Lwt.return_unit
   else Ci_opam.findlib_conf ~prefix ~write_path) >>= fun () ->
  let white_list = ["lib"; "bin"; "sbin"; "doc"; "share"; "etc"; "man"] in
  debug "pkg_build: snapshot %s BEFORE" prefix;
  fs_snapshots ~white_list prefix >>= fun before ->
  read_installed prefix >>= fun old_pkgs ->
  debug "pkg_build: FOR REAL %s.%s" name version;
  Ci_opam.install ~name ~version >>= fun result ->
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
  Ci_opam.uninstall ~name ~version >|= fun () ->
  result, output, installed, archive

let compiler_fs_origin root compiler ?snapshots () =
  (match snapshots with
   | Some s -> Lwt.return s
   | None -> fs_snapshots ~white_list:[compiler] root)
  >|= fun ss ->
  debug "snapshots: %s origin fs state" compiler;
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
  Ci_opam.install_switch compiler >>= fun () ->
  debug "snapshot: %s AFTER" root;
  fs_snapshots ~white_list:[compiler] root >>= fun after ->
  compiler_fs_origin root compiler ~snapshots:after () >>= fun () ->
  let prefix = Filename.concat root compiler in
  collect_installed prefix ~before ~after >>= fun installed ->
  read_installed prefix >>= fun new_pkgs ->
  create_archive prefix cid [] installed [] new_pkgs >|= fun archive ->
  Object.make_obj cid `Success ~output:[] ~installed archive

let install_compiler worker (c, host) =
  let root = OpamFilename.Dir.to_string OpamStateConfig.(!r.root_dir) in
  let cid = hash (host ^ root ^ c) in
  let apply_compiler_obj obj =
    debug "apply: %s start" c;
    (* prefix directory has to be existe for apply_archive to work *)
    let prefix = Filename.concat root c in
    (if not (Sys.file_exists prefix && Sys.is_directory prefix)
     then Lwt_unix.mkdir prefix 0o770
     else Lwt.return ()) >>= fun () ->
    apply_archive prefix obj >>= fun _ ->
    (* TODO: clean tmp archive file *)
    Ci_opam.switch c >|= fun () ->
    debug "apply: %s end" c
  in
  let compiler = true in
  local_query ~compiler worker.store cid >>= fun local_exist ->
  if local_exist then
    (debug "install: %s retrieve locally" c;
     local_retrieve ~compiler worker.store cid) >>=
    apply_compiler_obj >>= fun () ->
    compiler_fs_origin root c ()
  else
    Store.query_compiler cid >>= fun remote_exist ->
    if remote_exist then
      (debug "install: %s retrieve remotely" c;
       Store.retrieve_compiler cid) >>= fun cobj ->
      local_publish ~compiler worker.store cid cobj >>= fun () ->
      apply_compiler_obj cobj >>= fun () ->
      compiler_fs_origin root c ()
    else
      build_compiler_object cid root c >>= fun cobj ->
      Store.publish_compiler worker.token cid cobj >>= fun () ->
      local_publish ~compiler worker.store cid cobj

let pkg_job_execute base worker jid job deps =
  let root = OpamFilename.Dir.to_string OpamStateConfig.(!r.root_dir) in
  let (c, h) = Task.env_of_job job in
  let repo = Task.repo_of_job job in
  let pin = Task.pin_of_job job in
  Lwt.catch (fun () ->
      (match repo with
       | None -> Ci_opam.clean_repositories (); Lwt.return_unit
       | Some repo ->
         Ci_opam.add_repositories repo)
      >>= fun () ->
      Ci_opam.update () >>= fun () ->
      let c_curr = Ci_opam.compiler () in
      (if c = c_curr then Lwt.return_unit
       else
         Ci_opam.export_switch c >>= fun () ->
         install_compiler worker (c, h) >|= fun () ->
         debug "execute: compiler %s installed" c) >>= fun () ->
      (match pin with
       | None -> Lwt.return_unit
       | Some pin ->
         let build =
           let dir = Filename.concat root c in
           Filename.concat dir "build"
         in
         Unix.mkdir build 0o775;
         Ci_opam.add_pins pin) >>= fun () ->
      let name, version, depopts = Task.info_of_pkg_task (Task.task_of_job job) in
      let prefix = Ci_opam.get_var "prefix" in
      debug "execute: opam load state";
      Ci_opam.show_repo_pin () >>= fun () ->
      debug "execute: %d dependencies" (List.length deps);
      Lwt_list.iter_s (fun dep ->
          worker_request_object base worker dep >>= fun obj ->
          match Object.result_of_t obj with
          | `Success -> apply_object prefix obj
          | `Fail _ ->
            let dep_id = Object.id_of_t obj in
            Lwt.fail_with ("dependency broken: " ^ dep_id)
          | `Delegate _ -> Lwt.fail_with "delegate in deps"
        ) deps
      >>= fun () ->
      let is_resolvable, graph =
        Ci_opam.resolvable ~name ?version ?depopts ()
      in
      if is_resolvable then (
        debug "execute: resolvable=true";
        let job_lst = Ci_opam.jobs_of_graph ?repository:repo ?pin graph in
        worker_spawn base worker job_lst >>= fun () ->
        let delegate_id =
          List.fold_left (fun acc (id, job, _) ->
              let name', _ = Task.info_of_task (Task.task_of_job job) in
              if name' = name then id :: acc
              else acc
            ) [] job_lst
          |> (fun id_lst -> assert (1 = List.length id_lst); List.hd id_lst)
        in
        let result = `Delegate delegate_id in
        let obj = Object.make_obj jid result ~output:[] ~installed:[] ("", "") in
        switch_clean_up root c;
        Lwt.return (result, obj)
      ) else (
        debug "execute: resolvable=false";
        let v = match version with Some v -> v | None -> assert false in
        pkg_build prefix jid name v
        >>= fun (result, output, installed, archive) ->
        let obj = Object.make_obj jid result ~output ~installed archive in
        switch_clean_up root c;
        Lwt.return (result, obj))
    ) (fun exn ->
      let result = match exn with
        | Failure f -> `Fail f
        | _ -> `Fail "unknow execution failure" in
      let obj = Object.make_obj jid result ~output:[] ~installed:[] ("", "") in
      switch_clean_up root c;
      Lwt.return (result, obj))

let compiler_job_execute jid job =
  let task = Task.task_of_job job in
  let comp = Task.(match task with
      | Compiler c -> c
      | Package _ | Github _ -> failwith "not compiler build task")
  in
  debug "execute: build compiler: %s" comp;
  try
    Ci_opam.switch comp >>= fun () ->
    let switch = comp in
    let prefix = Ci_opam.get_var "prefix" in
    let path = Filename.concat (Filename.dirname prefix) switch in
    debug "execute: snapshot %s AFTER" path;
    read_installed path >>= fun new_pkgs ->
    fs_snapshots path >>= fun after_build ->
    collect_installed path ~before:[] ~after:after_build >>= fun installed ->
    create_archive path jid [] installed [] new_pkgs >>= fun archive ->
    let result = `Success in
    debug "execute: create object";
    let obj = Object.make_obj jid result ~output:[] ~installed archive in
    clean_tmp "compiler" (fst archive) >>= fun () ->
    Ci_opam.remove_switch switch >>= fun () ->
    Lwt.return (result, obj)
  with _ ->
    (* FIXME: catch-all is bad *)
    let result = `Fail (comp ^ " build fail") in
    let obj = Object.make_obj jid result ~output:[] ~installed:[] ("", "") in
    Lwt.return (result, obj)

let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (id, desp) ->
  local_query worker.store id >>= fun completed ->
  if completed then execution_loop base worker cond
  else (
    worker.status <- Working id;
    (try
       Sexplib.Sexp.of_string desp
       |> Task.job_entry_of_sexp
       |> Task.unwrap_entry
       |> Lwt.return
     with _ ->
       err "execute: sexp %s" desp
    ) >>= fun (job, deps) ->
    let task = Task.task_of_job job in
    let open Task in
    (match task with
     | Package _  -> pkg_job_execute base worker id job deps
     | Compiler _ -> compiler_job_execute id job
     | Github _   -> Lwt.fail_with "to be implemented")
    >>= fun (result, obj) ->
    (match result with
     | `Delegate _ ->
       Store.publish_object worker.token id obj >>= fun () ->
       Store.unlog_job id >>= fun () ->
       worker_publish base worker result id obj
     | `Success ->
       Store.publish_object worker.token id obj >>= fun () ->
       Store.unlog_job id >>= fun () ->
       Lwt.join [worker_publish base worker result id obj;
                 local_publish worker.store id obj]
     | `Fail _ ->
       Store.publish_object worker.token id obj >>= fun () ->
       worker_publish base worker result id obj) >>= fun () ->
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

let worker base_str store_str fresh =
  Store.initial_store ~uri:store_str () >>= (fun () ->
      let base = Uri.of_string base_str in
      let build_store = local_store ~fresh in
      worker_register base build_store >>= fun worker ->
      let cond = Lwt_condition.create () in
      Lwt.pick [
        heartbeat_loop base worker cond;
        execution_loop base worker cond;
      ])
  |> Lwt_main.run

let base = Cmdliner.Arg.(
  required & pos 0 (some string) None & info []
    ~docv:"HOST" ~doc:"the uri string of master node")

let store = Cmdliner.Arg.(
  required & pos 1 (some string) None & info []
    ~docv:"STORE" ~doc:"the uri string of data store")

let fresh = Cmdliner.Arg.(
  value & flag & info ["fresh"; "f"]
    ~doc:"start with a fresh new local store")

let () = Cmdliner.Term.(
  let worker_cmd =
    pure worker $ base $store $ fresh,
    info ~doc:"start a worker" "worker" in
  match eval worker_cmd with `Error _ -> exit 1 | _ -> exit 0)
