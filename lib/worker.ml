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
let working_sleep = 5.0
let master_timeout = 15.0

let sub len str = String.sub str 0 len
let sub_abbr = sub 5

let log action func ~info =
  let title = Printf.sprintf "%s@%s" action func in
  if info = "" then Printf.eprintf "[%s]\n%!" title
  else Printf.eprintf "[%s]: %s\n%!" title info

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
  let home = if test then "." else Sys.getenv "HOME" in
  let path = Filename.concat home ("ci-worker" ^ (string_of_int id)) in
  if not (Sys.file_exists path) then
    Lwt_unix.mkdir path 0o770 >>= fun () -> return path
  else return path


let clean_up t =
  let print_kv k =
    let path = String.concat "/" k in
    Irmin.read (t "read value") k >>= (function
    | None -> return "none" | Some v -> return "value") >>= fun value ->
    Lwt_io.printf "%s -> %s\n%!" path value  in
  let remove_kv k =
    Irmin.read (t "read value") k >>= function
    | None -> return ()
    | Some _ -> print_kv k >>= fun () ->
                Irmin.remove (t "remove kv") k in
  Irmin.iter (t "iter kv") remove_kv


let local_store ?(fresh = false) dir =
  let basic = Irmin.basic (module Irmin_unix.Irmin_git.FS)
                          (module Irmin.Contents.String) in
  let config = Irmin_git.config ~root:dir ~bare:true () in
  Irmin.create basic config Irmin_unix.task >>= fun t ->
  (if fresh then clean_up t else return ()) >>= fun () ->
  return t


let path_of_id id =
  let sub_dir = String.sub id 0 2 in
  ["object"; sub_dir; id]

let rec id_of_path = function
  | [id] -> id
  | _ :: tl -> id_of_path tl
  | _ -> failwith "empty path"

let local_query t id =
  let path = path_of_id id in
  Irmin.read (t ("query object " ^ id)) path >>= function
  | None -> return false
  | Some _ -> return true


let local_publish t id obj =
  local_query t id >>= fun exist ->
  if exist then return () else
    let path = path_of_id id in
    let value = Object.string_of_t obj in
    Irmin.update (t ("publish object " ^ id)) path value


let local_retrieve t id =
  let path = path_of_id id in
  Irmin.read (t ("retrieve object" ^ id)) path >>= function
  | None -> fail (raise (Invalid_argument ("no object for " ^ id)))
  | Some o -> return (Object.t_of_string o)


let local_retrieve_all t =
  let tups = ref [] in
  let iter k =
    Irmin.read (t "read object") k >>= function
      | None -> return ()
      | Some v -> tups := (id_of_path k, Object.t_of_string v) :: !tups;
                  return () in
  Irmin.iter (t "retrieve all objects") iter >>= fun () ->
  return (!tups)


let with_client_request tag request =
  let timeout_response () =
    Lwt_unix.sleep master_timeout >>= fun () ->
    let resp = Response.make ~status:Code.(`I_m_a_teapot) () in
    let body = Body.empty in
    return (resp, body) in

  let err_handler exn =
    (match exn with
     | Failure f -> Printf.eprintf "[ERROR]: %s\n%!" f
     | _  -> Printf.eprintf "[ERROR]: connection to master failed\n%!");
    return (exit 1) in
  catch (fun () ->
      pick [timeout_response (); request] >>= fun ((resp, _) as r) ->
      if Response.status resp = Code.(`I_m_a_teapot)
      then fail_with ("master timeout " ^ tag)
      else return r) err_handler


(* POST base/worker<id>/objects -> `Created *)
let worker_publish base {id; token} result oid obj =
  log "send" "publish" ~info:("object " ^ (sub_abbr oid));
  let headers = Cohttp.Header.of_list ["worker", token] in
  let m = Message.Publish (result, oid) in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "worker%d/objects" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request "publish" (Client.post ~headers ~body uri)
  >>= fun (resp, _) ->
  let status = Response.status resp in
  if status = Code.(`Created) then return ()
  else fail (WrongResponse (Code.string_of_status status, "publish"))


(* POST base/worker/registration -> `Created *)
let worker_register base build_store =
  let body = body_of_message Message.Register in
  let uri_path = "worker/registration" in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request "register" (Client.post ~body uri) >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Cohttp.Code.string_of_status status, "register") in
  if status = Code.(`Created) then
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
       (* log "send" "heartbeat" ~info:("working " ^ (sub_abbr jid)); *)
       Heartbeat (Some jid)
    | Idle ->
       log "send" "heartbeat" ~info:"idle";
       Heartbeat None in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "worker%d/state" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request "heartbeat" (Client.post ~headers ~body uri)
  >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "heartbeat") in
  if status = Code.(`OK) then
    match m with
    | Ack_heartbeat -> return None
    | New_job (id, jdesp) -> return (Some (id, jdesp))
    | Ack_register _ -> fail exn
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
      let info = Printf.sprintf "get object%s from data store" (sub_abbr oid) in
      log "request" "object" ~info;
      Store.retrieve_object oid >>= fun obj ->
      choose [local_publish store oid obj;
              worker_publish base worker `Success oid obj] >>= fun () ->
      return obj
    end


let get_opam_var var =
  let comm = Lwt_process.shell ("opam config var " ^ var) in
  let null = Lwt_process.(`Dev_null) in
  let proc_in = Lwt_process.open_process_in ~stdin:null ~stderr:null comm in
  let ic = proc_in#stdout in
  Lwt_io.read_line ic


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


let apply_object state prefix obj =
  let installed, archive = Object.apply_info obj in
  install_archive archive >>= fun arch_path ->
  extract_archive arch_path >>= fun () ->

  (* name.tar.gz *)
  let src = arch_path |> Filename.chop_extension |> Filename.chop_extension in
   install_files ~src ~dst:prefix installed >>= fun () ->
   (* clean_tmp "apply" (fst archive) >>= fun () -> *)
   Ci_opam.update_metadata ~install:true state (Filename.concat src "installed")


let hash str =
  let hex_of_cs cs =
    let buf = Buffer.create 16 in
    Cstruct.hexdump_to_buffer buf cs;
    Buffer.contents buf in
  let stripe_nl_space s = Re.(
    let re = compile (alt [compl [notnl]; space]) in
    replace_string re ~by:"" s) in
  Cstruct.of_string str |> Nocrypto.Hash.SHA1.digest
  |> hex_of_cs |> stripe_nl_space


let fs_snapshots dir =
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
    let white_list = ["lib"; "bin"; "sbin"; "doc"; "share";
                      "etc"; "man"] in
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun n -> List.mem n white_list)
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


let collect_installed prefix before after =
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

let job_execute base worker jid job deps =
  (* let inputs = Task.inputs_of_job job in *)
  let info = Printf.sprintf "of total %d" (List.length deps) in
  log "execute" "dependency" ~info;

  get_opam_var "prefix" >>= fun prefix ->
  let state = Ci_opam.load_state () in
  Lwt_list.fold_left_s (fun s dep ->
    worker_request_object base worker dep >>= fun obj ->
    apply_object s prefix obj) state deps >>= fun state ->

  log "execute" "snapshot" ~info:(prefix ^ " BEFORE");
  fs_snapshots prefix >>= fun before_build ->
  read_installed prefix >>= fun old_pkgs ->
  let p, v = Task.info_of_task (Task.task_of_job job) in
  log "execute" "FOR REAL" ~info:(p ^ "." ^  v);

  Ci_opam.opam_install state p v >>= fun (n_state, result) ->
  (match result with
   | `Success -> log "execute" p ~info:"SUCCESS"
   | `Fail f -> log "execute" p ~info:("FAIL: " ^ f));
  return_unit >>= fun () ->

  log "execute" "snapshot" ~info:(prefix ^ " AFTER");
  fs_snapshots prefix >>= fun after_build ->
  read_installed prefix >>= fun new_pkgs ->
  collect_output prefix p v result >>= fun output ->
  collect_installed prefix before_build after_build >>= fun installed ->

  create_archive prefix jid output installed old_pkgs new_pkgs
  >>= fun archive ->
  clean_tmp "execute" (fst archive) >>= fun () ->

  Ci_opam.opam_uninstall n_state p v >>= fun () ->
  Lwt_list.fold_left_s (fun s dep ->
      local_retrieve worker.store dep >>= fun obj ->
      clean_object prefix obj;

      let _, (name, _) = Object.apply_info obj in
      let path = Filename.concat "/tmp" name
                 |> Filename.chop_extension |> Filename.chop_extension
                 |> (fun dir -> Filename.concat dir "installed") in
      Ci_opam.update_metadata ~install:false s path) n_state deps >>= fun _ ->
  return (result, (Object.create jid result output installed archive))


let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (id, desp) ->
  local_query worker.store id >>= fun completed ->
  if completed then execution_loop base worker cond
  else begin
      let job, deps = Sexplib.Sexp.of_string desp
                      |> Task.job_entry_of_sexp
                      |> Task.unwrap_entry in
      worker.status <- Working id;
      job_execute base worker id job deps >>= fun (result, obj) ->
      (match result with
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


let worker base_str store_str ip port fresh =
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

let ip = Cmdliner.Arg.(
  value & opt string "127.0.0.1" & info ["ip"]
    ~doc:"the ip address of this worker")

let port = Cmdliner.Arg.(
  value & opt int 8000 & info ["port"]
    ~doc:"the port number of this worker")

let test = Cmdliner.Arg.(
  value & flag & info ["test"; "t"]
    ~doc:"when set, data store will be installed in the current directory")

let fresh = Cmdliner.Arg.(
  value & flag & info ["fresh"; "f"]
    ~doc:"start with a fresh new local store")

let () = Cmdliner.Term.(
  let worker_cmd =
    pure worker $ base $store $ ip $ port $ fresh,
    info ~doc:"start a worker" "worker" in
  match eval worker_cmd with `Error _ -> exit 1 | _ -> exit 0)
