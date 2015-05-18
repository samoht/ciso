open Lwt
open Message

module Body = Cohttp_lwt_body
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

type t = {
  id : int;
  addr : string * int;
  sha : string;
  mutable tasks : Task.t list;
  mutable objects : Object.t list;
  mutable status : worker_status;
}
and worker_status =
  | Working of Task.t
  | Idle

exception WrongResponse of string * string

let idle_sleep = 10.0
let working_sleep = 20.0
let body_of_sexp sexp = Body.of_string (Sexplib.Sexp.to_string sexp)
let sexp_of_body body = Sexplib.Sexp.of_string (Body.to_string body)

let get_working_dir worker =
  let root = Sys.getcwd () in
  let dir = Printf.sprintf "worker%d" worker.id in
  let path = Filename.concat root dir in
  if not (Sys.file_exists path) then Unix.mkdir path 0o775;
  path

(* POST base/workers -> `Created *)
let worker_register base addr =
  let msg = Message.sexp_of_worker_msg (Register addr) in
  let body = body_of_sexp msg in
  let uri = Uri.resolve "" base (Uri.of_string "workers") in
  Client.post ~body uri >>= fun (resp, body) ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status,
                             Body.to_string) in
    if status = Cohttp.Code.`Created then
      match Message.master_msg_of_sexp (sexp_of_body body) with
        | Ack_register (id, sha) ->
           return { id; addr; sha; tasks = []; objects = []; status = Idle}
        | _ -> fail exn
    else fail exn

(* POST base/worker<id> -> `Ok *)
let worker_heartbeat base ({ id; sha; status } as worker) =
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let msg =
    match status with
    | Working t -> Heartbeat (Some (Task.id_of_t t))
    | Idle -> Heartbeat None in
  let body = body_of_sexp (Message.sexp_of_worker_msg msg) in
  let uri_path = String.sprintf "worker%d" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  Client.post ~headers ~body uri >>= fun (resp, body) ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status,
                             Body.to_string body) in
    if status = Cohttp.Code.`OK then
      match Messgae.master_msg_of_sexp (sexp_of_body body) with
        | Ack_heartbeat -> return None
        | New_task (t_id, o_id) -> return Some (t_id, o_id)
        | _ -> fail exn
    else fail exn

(* GET base/worker<id>/newtask/<task_id> -> `OK  *)
let worker_request_task base {id; sha} task_id =
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let uri_path = Printf.sprintf "worker%d/newtask/%d" id task_id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  Client.get ~headers uri >>= fun (resp, body) ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status,
                             Body.to_string body) in
    if status = Cohttp.Code.`OK then return (Task.t_of_sexp (sexp_of_body body))
    else fail exn

(* GET worker_ip:port/path -> `OK *)
let worker_request_object obj =
  let base = Uri.of_string (Object.addr_of_t obj) in
  let path = Uri.of_string (Object.path_of_t obj) in
  let uri = Uri.resolve "" base path in
  Client.get uri >>= fun (resp, body) ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status,
                             Body.to_string body) in
    if status = Cohttp.Code.`OK then return (Body.to_string body)
    else fail exn

(* POST base/worker<id>/objects -> `Created *)
let worker_publish base {id; addr; sha} obj =
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let obj_info = Object.(id_of_t obj, path_of_t obj) in
  let body = body_of_sexp (sexp_of_worker_msg (Publish (addr, obj_info))) in
  let uri_path = Printf.sprintf "worker%d/objects" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  Client.post ~headers ~body uri >>= fun (resp, body) ->
    let status = Resp.status resp in
    if status = Cohttp.Code.`Created then return ()
    else fail (WrongResponse (Cohttp.Code.string_of_status status,
                              Body.to_string body))
(* dummy execution *)
let task_execute work_dir task obj_id addr =
  let t_id = Task.id_of_t task in
  let t_inputs = Task.inputs_of_t t in
  Lwt_list.map_p (fun input ->
    Object.id_of_t input, worker_request_object input) t_intpus
  >>= fun input_tups ->
    let file = Printf.sprintf "task%d.out" t_id in
    let path = Filename.concat work_dir file in
    Lwt_io.open_file
      ~flags:[Unix.O_WRONLY; Unix.O_TRUNC] ~mode:Lwt_io.output path
  >>= fun oc ->
    let str = Task.string_of_t task in
    let inputs = String.concat "\n" (List.map (fun (id, content) ->
      Printf.sprintf "%d ->\n\t%s\n" id content) input_tups) in
    Lwt_io.fprintf oc "%s\n  [inputs]:\n%s\n" str inputs
  >>= fun () -> Lwt_io.close oc
  >>= fun () ->
    let relative_path = Filename.concat (Filename.basename work_dir) file in
    return (Object.create obj_id addr relative_path)

let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (task_id, obj_id) ->
    let task_id_last = try Task.id_of_t (List.hd worker.tasks) with _ -> (-1) in
    if task_id_last = task_id then execution_task base worker cond
    else begin
        worker_request_task base worker task_id
        >>= fun task ->
          worker.status <- Working (Task.id_of_t task);
          task_execute (get_working_dir worker) task obj_id worker.addr
        >>= fun obj ->
          worker_publish base worker obj;
          worker.tasks <- task :: worker.tasks;
          worker.objects <- obj :: worker.objects;
        >>= fun () ->
          worker.status <- Idle;
          execution_task base worker cond
      end

let rec heartbeat_loop base worker cond =
  match worker.status with
  | Idle -> worker_heartbeat base worker >>= function
    | None ->
       Lwt_unix.sleep idle_sleep
       >>= fun () -> heartbeat_loop base worker cond
    | Some (t_id, o_id) ->
       Lwt_condition.signal cond (t_id, o_id);
       Lwt_unix.sleep idle_sleep
       >>= fun () -> heartbeat_loop base worker cond
  | Working _ ->
     worker_heartbeat base worker >>= fun _ ->
     Lwt_unix.sleep working_sleep
     >>= fun () -> heartbeat_loop base work_dir cond

let worker_server ip port =
  

let worker_client base ip port =
  worker_register base (ip, port)
  >>= fun worker ->
    let cond = Lwt_condition.create () in
    join [heartbeat_loop base worker cond; execution_loop base worker cond]
