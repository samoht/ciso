open Lwt
open Message

module Body = Cohttp_lwt_body
module Code = Cohttp.Code
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

type t = {
  id : int;                        (* worker id assigned by master *)
  token : string;                  (* used to publish in the store *)
  store : store;
  mutable status : worker_status;  (* working on a task of idle *)
}
and worker_status =
  | Working of string
  | Idle
and store = (string -> ([`BC], Irmin.Contents.String.Path.t,
                               Irmin.Contents.String.t) Irmin.t)

exception WrongResponse of string * string

let idle_sleep = 3.0
let working_sleep = 5.0
let master_timeout = 3.0

let sub len str = String.sub str 0 len

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


let with_client_request request =
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
      then fail_with "master timeout"
      else return r) err_handler


(* POST base/worker<id>/objects -> `Created *)
let worker_publish base {id; token} oid obj =
  log "send" "publish" ~info:("object " ^ (sub 5 oid));
  let headers = Cohttp.Header.of_list ["worker", token] in
  let m = Message.Publish oid in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "worker%d/objects" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request (Client.post ~headers ~body uri)
  >>= fun (resp, _) ->
  let status = Response.status resp in
  if status = Code.(`Created) then return ()
  else fail (WrongResponse (Code.string_of_status status, "publish"))


(* POST base/worker/registration -> `Created *)
let worker_register base build_store =
  let body = body_of_message Message.Register in
  let uri_path = "worker/registration" in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request (Client.post ~body uri) >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Cohttp.Code.string_of_status status, "register") in
  if status = Code.(`Created) then
    match m with
    | Ack_heartbeat | New_task _ -> fail exn
    | Ack_register (id, token) ->
       let info = Printf.sprintf "success: %d token: %s" id (sub 10 token) in
       log "send" "register" ~info;
       working_directory id () >>= fun dir ->
       build_store dir >>= fun store ->
       let worker = { id; token; store; status = Idle} in

       local_retrieve_all store >>= fun tups ->
       if tups = [] then return worker
       else Lwt_list.iter_p (fun (id, o) ->
           join [Store.publish_object token id o >>= fun () ->
                 Store.unlog_task id;
                 worker_publish base worker id o;]) tups >>= fun () ->
           return worker
  else fail exn


(* POST base/worker<id>/state -> `Ok *)
let worker_heartbeat base { id; token; status } =
  let headers = Cohttp.Header.of_list ["worker", token] in
  let m =
    match status with
    | Working tid ->
       log "send" "heartbeat" ~info:("working " ^ (sub 5 tid));
       Heartbeat (Some tid)
    | Idle ->
       log "send" "heartbeat" ~info:"idle";
       Heartbeat None in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "worker%d/state" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  with_client_request (Client.post ~headers ~body uri) >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "heartbeat") in
  if status = Code.(`OK) then
    match m with
    | Ack_heartbeat -> return None
    | New_task (id, tdesp) -> return (Some (id, tdesp))
    | Ack_register _ -> fail exn
  else fail exn


(* GET worker_ip:port/path -> `OK *)
let worker_request_object base worker oid =
  let store = worker.store in
  local_query store oid >>= fun exist ->
  if exist then begin
      log "send" "object"
          ~info:(Printf.sprintf "get object %s locally" (sub 5 oid));
      local_retrieve store oid
    end
  else begin
      let info = Printf.sprintf "get object %s from data store" (sub 5 oid) in
      log "send" "object" ~info;
      Store.retrieve_object oid >>= fun obj ->
      choose [local_publish store oid obj;
              worker_publish base worker oid obj] >>= fun () ->
      return obj
    end


let apply_object obj = return ()


let task_execute base worker tid task =
  let inputs = Task.inputs_of_t task in
  let info = Printf.sprintf "of total %d" (List.length inputs) in
  log "execute" "inputs" ~info;
  Lwt_list.iter_p (fun input ->
    worker_request_object base worker input >>= fun obj ->
    apply_object obj) inputs >>= fun () ->
  let p, v = Task.info_of_t task in
  log "execute" "dummy" ~info:(p ^ "." ^  v);
  return (Object.create tid)


let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (tid, tdesp) ->
  local_query worker.store tid >>= fun completed ->
  if completed then execution_loop base worker cond
  else begin
      let task = Sexplib.Sexp.of_string tdesp |> Task.t_of_sexp in
      worker.status <- Working tid;
      task_execute base worker tid task >>= fun obj ->
      choose [Store.publish_object worker.token tid obj >>= fun () ->
              Store.unlog_task tid;
              worker_publish base worker tid obj;
              local_publish worker.store tid obj] >>= fun () ->
      worker.status <- Idle;
      execution_loop base worker cond
    end

let rec heartbeat_loop base worker cond =
  match worker.status with
  | Idle -> begin worker_heartbeat base worker >>= function
    | None ->
       Lwt_unix.sleep idle_sleep
       >>= fun () -> heartbeat_loop base worker cond
    | Some (id, tdesp) ->
       Lwt_condition.signal cond (id, tdesp);
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
