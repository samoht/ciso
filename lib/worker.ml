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


let working_directory id () =
  let home = Sys.getenv "HOME" in
  let path = Filename.concat home ("ci-worker" ^ (string_of_int id)) in
  if not (Sys.file_exists path) then
    Lwt_unix.mkdir path 0o770 >>= fun () -> return path
  else return path


let local_store dir () =
  let basic = Irmin.basic (module Irmin_unix.Irmin_git.FS)
                          (module Irmin.Contents.String) in
  let config = Irmin_git.config ~root:dir ~bare:true () in
  Irmin.create basic config Irmin_unix.task


let path_of_id id =
  let file = String.sub id 0 6 in
  ["object"; file]


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


(* POST base/worker/registration -> `Created *)
let worker_register base  =
  let body = body_of_message Message.Register in
  let uri_path = "worker/registration" in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  Client.post ~body uri >>= fun (resp, body) ->
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
       local_store dir () >>= fun store ->
       return { id; token; store; status = Idle}
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

  Client.post ~headers ~body uri >>= fun (resp, body) ->
  message_of_body body >>= fun m ->
  let status = Response.status resp in
  let exn = WrongResponse (Code.string_of_status status, "heartbeat") in
  if status = Code.(`OK) then
    match m with
    | Ack_heartbeat -> return None
    | New_task (id, tdesp) -> return (Some (id, tdesp))
    | Ack_register _ -> fail exn
  else fail exn


(* POST base/worker<id>/objects -> `Created *)
let worker_publish base {id; token} oid obj =
  Store.publish_object token oid obj >>= fun () ->

  log "send" "publish" ~info:("object " ^ (sub 5 oid));
  let headers = Cohttp.Header.of_list ["worker", token] in
  let m = Message.Publish oid in
  let body = body_of_message m in
  let uri_path = Printf.sprintf "worker%d/objects" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in

  Client.post ~headers ~body uri
  >>= fun (resp, _) ->
  let status = Response.status resp in
  if status = Code.(`Created) then return ()
  else fail (WrongResponse (Code.string_of_status status, "publish"))


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
      choose[local_publish store oid obj;
             worker_publish base worker oid obj] >>= fun () ->
      return obj
    end

(* dummy execution
let task_execute base worker tid task =
  let t_inputs = Task.inputs_of_t task in
  let inputs_str = String.concat " " (List.rev_map (sub 5) t_inputs) in
  let info = Printf.sprintf "task %s need %s" (sub 5 obj_id) inputs_str in
  log "execute" "task" ~info;
  Lwt_list.map_p (fun input_id ->
    worker_request_object base worker input_id
    >>= fun obj_str -> return (input_id, obj_str)) t_inputs
  >>= fun input_tups ->
    let task_str = Task.string_of_t task in
    let inputs_str = String.concat "\n" (List.rev_map snd input_tups) in
    let content = Printf.sprintf "%s\n  [inputs]:\n%s\n" task_str inputs_str in
    to_local_obj worker obj_id content *)

let apply_object obj = return ()


let task_execute base worker tid task =
  let inputs = Task.inputs_of_t task in
  Lwt_list.iter_p (fun input ->
    worker_request_object base worker input >>= fun obj ->
    apply_object obj) inputs >>= fun () ->
  log "execute" "dummy" ~info:(Task.info_of_t task);
  return (Object.create "id" ("ip",8000) "dir")


let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (tid, tdesp) ->
  local_query worker.store tid >>= fun completed ->
  if completed then execution_loop base worker cond
  else begin
      let task = Sexplib.Sexp.of_string tdesp |> Task.t_of_sexp in
      worker.status <- Working tid;
      task_execute base worker tid task >>= fun obj ->
      choose [worker_publish base worker tid obj;
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

let worker base_str store_str ip port =
  Store.initial_store ~uri:store_str () >>= (fun () ->

 let base = Uri.of_string base_str in
  worker_register base >>= fun worker ->

    let cond = Lwt_condition.create () in
    join [heartbeat_loop base worker cond;
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

let () = Cmdliner.Term.(
  let worker_cmd =
    pure worker $ base $store $ ip $ port,
    info ~doc:"start a worker" "worker" in
  match eval worker_cmd with `Error _ -> exit 1 | _ -> exit 0)
