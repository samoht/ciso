open Lwt
open Message

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
let working_sleep = 15.0
let master_timeout = 15.0

let sub len str = String.sub str 0 len
let sub_abbr = sub 5

let origin_fs = ref []

let log action func ~info =
  let title = Printf.sprintf "%s@%s" action func in
  if info = "" then Printf.eprintf "[%s]\n%!" title
  else Printf.eprintf "[%s]: %s\n%!" title info

let time () = Unix.(
  let tm = localtime (time ()) in
  Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec)

let body_of_message m =
  Message.sexp_of_worker_msg m
  |> Sexplib.Sexp.to_string
  |> Body.of_string

let message_of_body body =
  Body.to_string body >>= fun b ->
  Sexplib.Sexp.of_string b
  |> Message.master_msg_of_sexp
  |> return

let working_directory ?(test = true) id () =
  let home = Sys.getenv "HOME" in
  let path = if test then Sys.getcwd ()
             else Filename.concat home "ci-worker" in
  if not (Sys.file_exists path) then
    Lwt_unix.mkdir path 0o770 >>= fun () -> return path
  else return path


let path_of_obj id = ["object"; id]
let path_of_com id = ["compiler"; id]


let clean_up t =
  let print_path k =
    let path = String.concat "/" k in
    Lwt_io.printf "remove %s\n%!" path  in
  let clean_dir = ["object"] in
  Lwt_list.iter_s (fun dir ->
    Irmin.list (t ("list files of " ^ dir)) [dir] >>= fun paths ->
    Lwt_list.iter_s (fun path ->
      print_path path >>= fun () ->
      Irmin.remove (t ("remove " ^ (String.concat "/" path))) path) paths)
    clean_dir


let local_store ?(fresh = false) dir =
  let basic = Irmin.basic (module Irmin_unix.Irmin_git.FS)
                          (module Irmin.Contents.String) in
  let config = Irmin_git.config ~root:dir ~bare:true () in
  Irmin.create basic config Irmin_unix.task >>= fun t ->
  (if fresh then clean_up t else return ()) >>= fun () ->
  return t

let rec id_of_path = function
  | [id] -> id
  | _ :: tl -> id_of_path tl
  | _ -> failwith "empty path"


let local_query t ?(compiler = false) id =
  let info = Printf.sprintf "%s %s %s"
    (if compiler then "compiler" else "object") (sub_abbr id) (time ()) in
  log "query" "local" ~info;
  let path = (if compiler then path_of_com else path_of_obj) id in
  Irmin.mem (t ("query object " ^ id)) path


let local_publish t ?(compiler = false) id obj =
  let info = Printf.sprintf "%s %s %s"
    (if compiler then "compiler" else "object") (sub_abbr id) (time ()) in
  log "publish" "local" ~info;
  local_query t ~compiler id >>= fun exist ->
  (if exist then return () else
    let () = log "publish" "local" ~info:("doesn't exist " ^ (time ())) in
    let path = (if compiler then path_of_com else path_of_obj) id in
    let value = Object.string_of_t obj in
    let ln = String.length value in
    let info = Printf.sprintf "length of value %d %s" ln (time ()) in
    log "publish" "local" ~info;
    Irmin.update (t ("publish object " ^ id)) path value)
  >>= fun () ->
  log "publish" "local" ~info:("completed " ^ (time ()));
  return_unit


let local_retrieve t ?(compiler = false) id =
  let info = Printf.sprintf "%s %s %s"
    (if compiler then "compiler" else "object") (sub_abbr id) (time ()) in
  log "retrieve" "local" ~info;
  let path = (if compiler then path_of_com else path_of_obj) id in
  Irmin.read (t ("retrieve object" ^ id)) path >>= function
  | None -> fail (raise (Invalid_argument ("no object for " ^ id)))
  | Some o ->
     log "retrieve" "local" ~info:("completed " ^ (time ()));
     return (Object.t_of_string o)


let local_retrieve_all t =
  let retrieve_dir = ["object"] in
  Lwt_list.fold_left_s (fun acc dir ->
    Irmin.list (t "retrieve objects") [dir] >>= fun paths ->
    Lwt_list.fold_left_s (fun acc' path ->
      Irmin.read (t ("read from " ^ (String.concat "/" path))) path >>= function
      | None -> return acc'
      | Some c ->
         let id = id_of_path path in
         let obj = Object.t_of_string c in
         return ((id, obj) :: acc')) acc paths) [] retrieve_dir


let with_client_request tag request =
  let timeout_response () =
    Lwt_unix.sleep master_timeout >>= fun () ->
    let resp = Response.make ~status:`I_m_a_teapot () in
    let body = Body.empty in
    return (resp, body) in

  let err_handler exn =
    (match exn with
     | Failure f -> Printf.eprintf "[ERROR]: %s\n%!" f
     | _  -> Printf.eprintf "[ERROR]: connection to master failed\n%!");
    return (exit 1) in
  catch (fun () ->
      pick [timeout_response (); request] >>= fun ((resp, _) as r) ->
      if Response.status resp = `I_m_a_teapot
      then fail_with ("master timeout " ^ tag)
      else return r) err_handler


(* POST base/worker<id>/objects -> `Created *)
let worker_publish base {id; token} result oid obj =
  log "send" "publish" ~info:("object " ^ (sub_abbr oid));
  let headers = Cohttp.Header.of_list ["worker", token] in
  let m = Message.Publish (result, oid) in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%d/objects" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request "publish" (Client.post ~headers ~body uri)
  >>= fun (resp, _) ->
  let status = Response.status resp in
  if status = `Created then return ()
  else fail (WrongResponse (Code.string_of_status status, "publish"))


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
    | Ack_heartbeat | New_job _ -> fail exn
    | Ack_register (id, token) ->
       let info = Printf.sprintf "success: %d token: %s" id (sub_abbr token) in
       log "send" "register" ~info;
       working_directory id () >>= fun dir ->
       build_store dir >>= fun store ->
       let worker = { id; token; store; status = Idle} in

       local_retrieve_all store >>= fun tups ->
       if tups = [] then return worker
       else Lwt_list.iter_s (fun (id, o) ->
           Store.publish_object token id o >>= fun () ->
           Store.unlog_job id >>= fun () ->
           worker_publish base worker `Success id o) tups >>= fun () ->
           return worker
  else fail exn


(* POST base/worker<id>/state -> `Ok *)
let worker_heartbeat base { id; token; status } =
  let headers = Cohttp.Header.of_list ["worker", token] in
  let m =
    match status with
    | Working jid ->
       log "send" "heartbeat" ~info:("working " ^ time ());
       Heartbeat (Some jid)
    | Idle ->
       log "send" "heartbeat" ~info:("idle " ^ time ());
       Heartbeat None in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%d/state" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request "heartbeat" (Client.post ~headers ~body uri)
  >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "heartbeat") in
  if status = `OK then
    match m with
    | Ack_heartbeat -> return None
    | New_job (id, jdesp) -> return (Some (id, jdesp))
    | Ack_register _ -> fail exn
  else fail exn


let worker_spawn base {id; token} job_lst =
  let headers = Cohttp.Header.of_list ["worker", token] in
  let m_job_lst = List.rev_map (fun (id, job, deps) ->
      let desp = Task.sexp_of_job job
                 |> Sexplib.Sexp.to_string in
      id, desp, deps)job_lst in
  let m = Message.Spawn_jobs m_job_lst in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "workers/%d/newjobs" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request "spawn" (Client.post ~headers ~body uri)
  >>= fun (resp, body) ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "spawn") in
  if status = `Created then return_unit
  else fail exn


(* GET worker_ip:port/path -> `OK *)
let worker_request_object base worker oid =
  let store = worker.store in
  local_query store oid >>= fun exist ->
  if exist then begin
      log "requst" "object"
          ~info:(Printf.sprintf "get object %s locally" (sub_abbr oid));
      local_retrieve store oid
    end
  else begin
      let info = Printf.sprintf "get object %s remotely" (sub_abbr oid) in
      log "request" "object" ~info;
      Store.retrieve_object oid >>= fun obj ->
      local_publish store oid obj >>= fun () ->
      worker_publish base worker `Success oid obj >>= fun () ->
      return obj
    end


let with_lwt_comm ?fail ?success comm =
  let fail = match fail with
    | None -> fun () -> return_unit
    | Some f -> f in
  let success = match success with
    | None -> fun () -> return_unit
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
  return tmp


let extract_archive tar =
  if not (Filename.check_suffix tar ".tar.gz") then
    raise (Invalid_argument tar);
  let comm = Printf.sprintf "tar -xzf %s -C /" tar in
  let fail () = return (log "extract" tar ~info:"failed") in
  (* let success () = return (log "extract" tar ~info:"OK") in *)
  with_lwt_comm ~fail comm


let install_files ~src ~dst files =
  let prepare_cp dst file =
    let file_lt = Re_str.split (Re_str.regexp_string "/") file in
    let rec mkdir parent = function
      | [] | [_] -> return ()
      | dir :: path ->
         let dir = Filename.concat parent dir in
         (if not (Sys.file_exists dir && Sys.is_directory dir)
          then Lwt_unix.mkdir dir 0o770
          else return ())
         >>= fun () -> mkdir dir path in
    mkdir dst file_lt in
  let cp src dst =
    let comm = Printf.sprintf "cp %s %s" src dst in
    let fail () = return (log "install" src ~info:"failed") in
    with_lwt_comm ~fail comm in
  Lwt_list.iter_s (fun f ->
    let src_path = Filename.concat src f in
    let dst_path = Filename.concat dst f in
    prepare_cp dst f >>= fun () ->
    cp src_path dst_path) files


let clean_tmp action name =
  let file = Filename.concat "/tmp" name in
  let dir = name
            |> Filename.chop_extension |> Filename.chop_extension
            |> Filename.concat "/tmp" in
  let comm = Printf.sprintf "rm -r %s %s" file dir in
  let success () =
    return (log action "clean" ~info:(file ^ " " ^ dir)) in
  with_lwt_comm ~success comm


let apply_archive prefix obj =
  let installed, archive = Object.apply_info obj in
  install_archive archive >>= fun arch_path ->
  extract_archive arch_path >>= fun () ->
  (* name.tar.gz *)
  let src = arch_path |> Filename.chop_extension |> Filename.chop_extension in
  install_files ~src ~dst:prefix installed >>= fun () ->
  return arch_path

let apply_object state prefix obj =
  apply_archive prefix obj >>= fun arch_path ->
  (* name.tar.gz *)
  let src = arch_path |> Filename.chop_extension |> Filename.chop_extension in
  let path = Filename.concat src "installed" in
  Ci_opam.update_metadata ~install:true state ~path
  >>= fun ns -> clean_tmp "apply" (Filename.basename arch_path)
  >>= fun () -> return ns


let patch_ocamlfind prefix =
  let bin_path = Filename.concat prefix "bin/ocamlfind" in
  if not (Sys.file_exists bin_path) then return ""
  else begin
      log "execute" "patch" ~info:(prefix ^ "/bin/ocamlfind");
      Lwt_io.open_file Lwt_io.input bin_path >>= fun ic ->
      Lwt_io.read ic >>= fun c ->
      Lwt_io.close ic >>= fun () ->

      let open Re in
      let re = compile (seq [str ".opam/";
                             group (rep1 (compl [char '/']));
                             str "/lib/findlib.conf"]) in
      let subs = exec re c in
      let sb, se = get_ofs subs 1 in
      let switch = Filename.basename prefix in
      for i = 0 to (se - sb - 1) do
        let char = try String.get switch i
                   with _ -> '_' in
        let pos = sb + i in
        String.set c pos char;
      done;

      let pb, pe = get_ofs subs 0 in
      let conf_path = String.sub c pb (pe - pb) in
      log "findlib.conf" "path" ~info:conf_path;

      Lwt_io.open_file Lwt_io.output bin_path >>= fun oc ->
      Lwt_io.write oc c >>= fun () ->
      Lwt_io.close oc >>= fun () ->

      return (Filename.concat (Sys.getenv "HOME") conf_path) end


let hash str =
  let `Hex h =
    str
    |> Cstruct.of_string
    |> Nocrypto.Hash.SHA1.digest
    |> Hex.of_cstruct in
  h


let fs_snapshots ?white_list dir =
  let checksum_of_file file =
    let checksum_of_ic ic =
      Lwt_io.read ic >>= fun content ->
      return (hash (file ^ content)) in
    Lwt_io.with_file Lwt_io.input file checksum_of_ic in
  let rec loop checksums = function
    | [] -> return checksums
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
         loop checksums (List.rev_append files tl) in

  let sub_dirs =
    Sys.readdir dir |> Array.to_list
    |> (fun lst ->
       match white_list with
       | Some wl -> List.filter (fun n -> List.mem n wl) lst
       | None -> lst)
    |> List.rev_map (fun n -> Filename.concat dir n) in
  loop [] sub_dirs


let read_installed dir =
  let path = Filename.concat dir "installed" in
  assert (Sys.file_exists path);
  let rec collect_lines acc ic =
    Lwt_io.read_line_opt ic >>= function
    | None -> return acc
    | Some l -> collect_lines (l :: acc) ic in
  Lwt_io.with_file Lwt_io.input path (collect_lines [])


let collect_output prefix p v = function
  | `Delegate _ -> assert false
  | `Success -> return []
  | `Fail _ ->
     let relative_path = ["build"; p ^ "." ^ v] |> String.concat "/" in
     let path = Filename.concat prefix relative_path in
     if Sys.file_exists path then
       let files = Sys.readdir path |> Array.to_list in
       List.filter (fun f ->
           List.exists (fun suffix -> Filename.check_suffix f suffix)
             [".info"; ".err"; ".out"; ".env"]) files
       |> List.rev_map (fun f -> Filename.concat relative_path f)
       |> return
     else return []


let collect_installed prefix ~before ~after =
  let module CsMap = Map.Make(String) in
  let cmap = List.fold_left (fun acc (f, checksum) ->
      CsMap.add f checksum acc) CsMap.empty before in
  (* TODO: collect deleted files *)
  let installed = List.fold_left (fun acc (f, checksum) ->
      if not (CsMap.mem f cmap) then f :: acc else
        let cs = CsMap.find f cmap in
        if cs <> checksum then f :: acc
        else acc) [] after in
  (* 1 is for the delimiter *)
  let len = 1 + String.length prefix in
  let chop_prefix f =
    try String.sub f len (String.length f - len)
    with e -> print_endline f; raise e in
  return (List.rev_map chop_prefix installed)


let create_archive prefix id output installed old nw =
  let dir = Filename.concat "/tmp" (sub_abbr id) in
  (if Sys.file_exists dir then with_lwt_comm ("rm -r " ^ dir)
   else return ()) >>= fun () ->
  Lwt_unix.mkdir dir 0o770 >>= fun () ->
  (List.rev_append output installed
   |> install_files ~src:prefix ~dst:dir) >>= fun () ->

  let pkg_file = Filename.concat dir "installed" in
  let write_pkgs old nw oc =
    let installed_pkgs = List.filter (fun p -> not (List.mem p old)) nw in
    let content = String.concat "\n" installed_pkgs in
    Lwt_io.write oc content in
  Lwt_io.with_file Lwt_io.output pkg_file (write_pkgs old nw) >>= fun () ->

  let name = (sub_abbr id) ^ ".tar.gz" in
  let path = Filename.concat "/tmp" name in
  let comm = Printf.sprintf "tar -zcf %s %s" path dir in
  with_lwt_comm comm >>= fun () ->
  Lwt_io.with_file Lwt_io.input path Lwt_io.read >>= fun content ->
  return (name, content)


let clean_object prefix obj =
  (* "installed" are all files, there are no directory *)
  let installed = Object.installed_of_t obj in
  let files = List.rev_map (fun f -> Filename.concat prefix f) installed in

  List.iter (fun f -> if Sys.file_exists f then Sys.remove f) files;
  let id = Object.id_of_t obj in
  let info = Printf.sprintf "clean object %s" (sub_abbr id) in
  log "execute" "clean" ~info


let pkg_build state prefix jid name version =
  (* TODO: disable ocamlfind patch *)
  patch_ocamlfind prefix >>= fun write_path ->
  (if write_path = "" then return_unit
   else Ci_opam.findlib_conf ~prefix ~write_path) >>= fun () ->

  let white_list = ["lib"; "bin"; "sbin"; "doc"; "share"; "etc"; "man"] in
  log "execute" "snapshot" ~info:(prefix ^ " BEFORE");
  fs_snapshots ~white_list prefix >>= fun before ->
  read_installed prefix >>= fun old_pkgs ->
  log "execute" "FOR REAL" ~info:(name ^ "." ^  version);

  Ci_opam.opam_install state ~name ~version >>= fun result ->
  (match result with
   | `Delegate _ -> assert false
   | `Success -> log "execute" name ~info:"SUCCESS"
   | `Fail f -> log "execute" name ~info:("FAIL: " ^ f));
  return_unit >>= fun () ->

  log "execute" "snapshot" ~info:(prefix ^ " AFTER");
  fs_snapshots ~white_list prefix >>= fun after ->
  read_installed prefix >>= fun new_pkgs ->
  collect_output prefix name version result >>= fun output ->
  collect_installed prefix ~before ~after >>= fun installed ->

  create_archive prefix jid output installed old_pkgs new_pkgs
  >>= fun archive ->

  clean_tmp "execute" (fst archive) >>= fun () ->
  Ci_opam.opam_uninstall ~name ~version >>= fun () ->
  return (result, output, installed, archive)


let compiler_fs_origin root compiler ?snapshots () =
  (match snapshots with
   | Some s -> return s
   | None -> fs_snapshots ~white_list:[compiler] root)
  >>= fun ss ->
  log "snapshots" compiler ~info:"origin fs state";
  origin_fs := List.rev_map fst ss;
  return_unit


let switch_clean_up  root compiler =
  let fs = !origin_fs in
  if fs = [] then ()
  else begin
    let module FileSet = Set.Make(String) in
    let fset = List.fold_left (fun acc f ->
      FileSet.add f acc) FileSet.empty fs in
    let read_dir dir =
      Sys.readdir dir
      |> Array.to_list
      |> List.rev_map (Filename.concat dir) in
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
    done end


let build_compiler_object cid root compiler =
  log "execute" "snapshot" ~info:(root ^ " BEFORE");
  fs_snapshots ~white_list:[compiler] root >>= fun before ->
  Ci_opam.opam_install_switch root compiler >>= fun () ->
  log "execute" "snapshot" ~info:(root ^ " AFTER");
  fs_snapshots ~white_list:[compiler] root >>= fun after ->
  compiler_fs_origin root compiler ~snapshots:after () >>= fun () ->

  let prefix = Filename.concat root compiler in
  collect_installed prefix ~before ~after >>= fun installed ->
  read_installed prefix >>= fun new_pkgs ->
  create_archive prefix cid [] installed [] new_pkgs >>= fun archive ->
  let obj = Object.make_obj cid `Success ~output:[] ~installed archive in
  return obj


let install_compiler base worker (c, host) =
  let root = Ci_opam.detect_root () in
  let cid = hash (host ^ root ^ c) in

  let apply_compiler_obj obj =
    log "apply" c ~info:("start " ^ (time ()));
    (* prefix directory has to be existe for apply_archive to work *)
    let prefix = Filename.concat root c in
    (if not (Sys.file_exists prefix && Sys.is_directory prefix)
     then Lwt_unix.mkdir prefix 0o770
     else return ()) >>= fun () ->
    apply_archive prefix obj >>= fun _ ->
    (* TODO: clean tmp archive file *)
    Ci_opam.opam_switch_switch root c >>= fun () ->
    log "apply" c ~info:("end " ^ (time ()));
    return_unit in

  let compiler = true in
  local_query ~compiler worker.store cid >>= fun local_exist ->
  if local_exist then
    (log "install" c ~info:"retrieve locally";
     local_retrieve ~compiler worker.store cid) >>= apply_compiler_obj
    >>= fun () -> compiler_fs_origin root c ()
  else
    Store.query_compiler cid >>= fun remote_exist ->
    if remote_exist then
       (log "install" c ~info:("retrieve remotely " ^ time ());
        Store.retrieve_compiler cid) >>= fun cobj ->
       local_publish ~compiler worker.store cid cobj >>= fun () ->
       apply_compiler_obj cobj >>= fun () ->
       compiler_fs_origin root c ()
     else
       build_compiler_object cid root c >>= fun cobj ->
       Store.publish_compiler worker.token cid cobj >>= fun () ->
       local_publish ~compiler worker.store cid cobj


let pkg_job_execute base worker jid job deps =
  let (c, h) = Task.env_of_job job in
  let root = Ci_opam.detect_root () in
  let repo = Task.repo_of_job job in
  let pin = Task.pin_of_job job in

  catch (fun () ->
    Ci_opam.set_root root;
    (match repo with
     | None -> Ci_opam.clean_repositories (); return_unit
     | Some repo ->
        Ci_opam.add_repositories repo) >>= fun () ->
    Ci_opam.opam_update ~repos_only:true () >>= fun () ->

    let c_curr = Ci_opam.detect_compiler () in
    (if c = c_curr then return_unit
     else
       Ci_opam.export_existed_switch root c >>= fun () ->
       install_compiler base worker (c, h) >>= fun () ->
       log "execute" "compiler" ~info:(c ^ " installed");
       return_unit) >>= fun () ->

    (match pin with
     | None -> return_unit
     | Some pin ->
        let build =
          let dir = Filename.concat root c in
          Filename.concat dir "build" in
        Unix.mkdir build 0o775;
        Ci_opam.add_pins pin) >>= fun () ->

    let name, version, depopts = Task.info_of_pkg_task (Task.task_of_job job) in
    let prefix = Ci_opam.get_opam_var "prefix" in

    log "execute" "opam" ~info:"load state";
    let state = Ci_opam.load_state () in
    Ci_opam.show_repo_pin state >>= fun () ->

    let info = Printf.sprintf "of total %d" (List.length deps) in
    log "execute" "dependency" ~info;
    Lwt_list.fold_left_s (fun s dep ->
        worker_request_object base worker dep >>= fun obj ->
        match Object.result_of_t obj with
        | `Success -> apply_object s prefix obj
        | `Fail _ ->
           let dep_id = Object.id_of_t obj in
           fail_with ("dependency broken: " ^ dep_id)
        | `Delegate _ -> fail_with "delegate in deps") state deps

    >>= fun s ->
    let is_resolvable, graph = Ci_opam.resolvable ~name ?version ?depopts s in
    if is_resolvable then
      let () = log "execute" "resolvable" ~info:"true" in
      let job_lst = Ci_opam.jobs_of_graph ?repository:repo ?pin graph in
      worker_spawn base worker job_lst >>= fun () ->

      let delegate_id =
        List.fold_left (fun acc (id, job, _) ->
            let name', _ = Task.info_of_task (Task.task_of_job job) in
            if name' = name then id :: acc
            else acc) [] job_lst
        |> (fun id_lst -> assert (1 = List.length id_lst); List.hd id_lst) in
      let result = `Delegate delegate_id in
      let obj = Object.make_obj jid result ~output:[] ~installed:[] ("", "") in

      switch_clean_up root c;
      return (result, obj)
    else
      let () = log "execute" "resolvable" ~info:"false" in
      let v = match version with Some v -> v | None -> assert false in
      pkg_build s prefix jid name v
      >>= fun (result, output, installed, archive) ->
      let obj = Object.make_obj jid result ~output ~installed archive in

      switch_clean_up root c;
      return (result, obj)) (fun exn ->
    let result = match exn with
      | Failure f -> `Fail f
      | _ -> `Fail "unknow execution failure" in
    let obj = Object.make_obj jid result ~output:[] ~installed:[] ("", "") in
    switch_clean_up root c;
    return (result, obj))


let compiler_job_execute base worker jid job =
  let root = Ci_opam.detect_root () in
  let task = Task.task_of_job job in
  let comp = Task.(match task with
    | Compiler c -> c
    | Package _ | Github _ -> failwith "not compiler build task") in
  log "execute" "compiler" ~info:("build compiler: " ^ comp);

  try
    Ci_opam.opam_install_switch root comp >>= fun () ->

    let switch = comp in
    let prefix = Ci_opam.get_opam_var "prefix" in
    let path = Filename.concat (Filename.dirname prefix) switch in
    log "execute" "snapshot" ~info:(path ^ " AFTER");
    read_installed path >>= fun new_pkgs ->
    fs_snapshots path >>= fun after_build ->

    collect_installed path ~before:[] ~after:after_build >>= fun installed ->
    create_archive path jid [] installed [] new_pkgs >>= fun archive ->
    let result = `Success in
    log "execute" "compiler" ~info:"create object";
    let obj = Object.make_obj jid result ~output:[] ~installed archive in

    clean_tmp "compiler" (fst archive) >>= fun () ->
    Ci_opam.opam_remove_switch root switch >>= fun () ->
    return (result, obj)
  with _ ->
       let result = `Fail (comp ^ " build fail") in
       let obj = Object.make_obj jid result ~output:[] ~installed:[] ("", "") in
       return (result, obj)


let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (id, desp) ->
  local_query worker.store id >>= fun completed ->
  if completed then execution_loop base worker cond
  else begin
      worker.status <- Working id;

      let job, deps = try Sexplib.Sexp.of_string desp
                          |> Task.job_entry_of_sexp
                          |> Task.unwrap_entry
                      with _ -> log "execute" "sexp" desp; exit 1 in
      let task = Task.task_of_job job in
      Task.(match task with
       | Package _ -> pkg_job_execute base worker id job deps
       | Compiler _ -> compiler_job_execute base worker id job
       | Github _ -> fail_with "to be implemented")

      >>= fun (result, obj) ->
      (match result with
       | `Delegate _ ->
          Store.publish_object worker.token id obj >>= fun () ->
          Store.unlog_job id >>= fun () ->
          worker_publish base worker result id obj
       | `Success ->
          Store.publish_object worker.token id obj >>= fun () ->
          Store.unlog_job id >>= fun () ->
          join [worker_publish base worker result id obj;
                local_publish worker.store id obj]
       | `Fail f ->
          Store.publish_object worker.token id obj >>= fun () ->
          worker_publish base worker result id obj) >>= fun () ->
      worker.status <- Idle;
      execution_loop base worker cond
    end


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
  pick [heartbeat_loop base worker cond;
        execution_loop base worker cond;])
  |> Lwt_main.run

let base = Cmdliner.Arg.(
  required & pos 0 (some string) None & info []
    ~docv:"HOST" ~doc:"the uri string of master node")

let store = Cmdliner.Arg.(
  required & pos 1 (some string) None & info []
    ~docv:"STORE" ~doc:"the uri string of data store")

let switch = Cmdliner.Arg.(
  required & pos 2 (some string) None & info []
    ~docv:"SWITCH" ~doc:"the opam switch name for the worker")

let fresh = Cmdliner.Arg.(
  value & flag & info ["fresh"; "f"]
    ~doc:"start with a fresh new local store")

let () = Cmdliner.Term.(
  let worker_cmd =
    pure worker $ base $store $ fresh,
    info ~doc:"start a worker" "worker" in
  match eval worker_cmd with `Error _ -> exit 1 | _ -> exit 0)
