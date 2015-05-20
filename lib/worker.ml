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

let get_working_dir worker =
  let root = Sys.getcwd () in
  let dir = Printf.sprintf "worker%d" worker.id in
  let path = Filename.concat root dir in
  if not (Sys.file_exists path) then Unix.mkdir path 0o775;
  path

let to_local_obj dir id addr content =
  let file = Printf.sprintf "%d.out" id in
  let path = Filename.concat dir file in
  Lwt_io.open_file
    ~flags:[Unix.O_WRONLY; Unix.O_TRUNC] ~mode:Lwt_io.output path
  >>= fun oc -> Lwt_io.fprint oc content
  >>= fun () -> Lwt_io.close oc
  >>= fun () ->
    let relative_path = Filename.concat (Filename.basename dir) file in
    return (Object.create id addr relative_path)

(* POST base/workers -> `Created *)
let worker_register base ((ip, port) as addr) =
  let msg = Message.sexp_of_worker_msg (Register (ip, port)) in
  let body = body_of_sexp msg in
  let uri = Uri.resolve "" base (Uri.of_string "workers") in
  Client.post ~body uri
  >>= fun (resp, body) -> Body.to_string body
  >>= fun body_str ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status, body_str) in
    if status = Cohttp.Code.(`Created) then
      match Message.master_msg_of_sexp (Sexplib.Sexp.of_string body_str) with
      | Ack_register (id, sha) ->
         return { id; addr; sha; tasks = []; objects = []; status = Idle}
      | _ -> fail exn
    else fail exn

(* POST base/worker<id> -> `Ok *)
let worker_heartbeat base { id; sha; status } =
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let msg =
    match status with
    | Working t -> Heartbeat (Some (Task.id_of_t t))
    | Idle -> Heartbeat None in
  let body = body_of_sexp (Message.sexp_of_worker_msg msg) in
  let uri_path = Printf.sprintf "worker%d" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  Client.post ~headers ~body uri
  >>= fun (resp, body) -> Body.to_string body
  >>= fun body_str ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status, body_str) in
    if status = Cohttp.Code.(`OK) then
      match Message.master_msg_of_sexp (Sexplib.Sexp.of_string body_str) with
      | Ack_heartbeat -> return None
      | New_task (t_id, o_id) -> return (Some (t_id, o_id))
      | _ -> fail exn
    else fail exn

(* GET base/worker<id>/newtask/<task_id> -> `OK  *)
let worker_request_task base {id; sha} task_id =
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let uri_path = Printf.sprintf "worker%d/newtask/%d" id task_id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  Client.get ~headers uri
  >>= fun (resp, body) -> Body.to_string body
  >>= fun body_str ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status, body_str) in
    if status = Cohttp.Code.(`OK) then
        return (Task.t_of_sexp (Sexplib.Sexp.of_string body_str))
    else fail exn

(* POST base/worker<id>/objects -> `Created *)
let worker_publish base {id; addr; sha} obj =
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let obj_info = Object.(id_of_t obj, path_of_t obj) in
  let body = body_of_sexp (sexp_of_worker_msg (Publish (addr, obj_info))) in
  let uri_path = Printf.sprintf "worker%d/objects" id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  Client.post ~headers ~body uri
  >>= fun (resp, body) -> Body.to_string body
  >>= fun body_str ->
    let status = Response.status resp in
    if status = Cohttp.Code.(`Created) then return ()
    else fail (WrongResponse (Cohttp.Code.string_of_status status, body_str))

(* GET base/object<obj_id> -> `OK *)
let worker_consult_object base (ip, _) obj_id =
  let uri_path = Printf.sprintf "object%d" obj_id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  let headers = Cohttp.Header.of_list ["ip", ip] in
  Client.get ~headers uri
  >>= fun (resp, body) -> Body.to_string body
  >>= fun body_str ->
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status, body_str) in
    if status = Cohttp.Code.(`OK) then
      match Message.master_msg_of_sexp (Sexplib.Sexp.of_string body_str) with
      | Best_object (i, p, path) -> return (i, p, path)
      | _ -> fail exn
    else fail exn

(* GET worker_ip:port/path -> `OK *)
let worker_request_object base worker obj_id =
  worker_consult_object base worker.addr obj_id
  >>= fun (ip, port, path) ->
    let headers =
      Cohttp.Header.of_list ["obj_id",string_of_int obj_id] in
    let base_str = Printf.sprintf "http://%s:%d" ip port in
    let base = Uri.of_string base_str in
    let path = Uri.of_string path in
    let uri = Uri.resolve "" base path in
    Client.get ~headers uri
  >>= fun (resp, body) -> Body.to_string body
  >>= fun body_str -> (* TODO: publish the just got object  *)
    let status = Response.status resp in
    let exn = WrongResponse (Cohttp.Code.string_of_status status, body_str) in
    if status = Cohttp.Code.(`OK) then
      choose
        [(to_local_obj (get_working_dir worker) obj_id worker.addr body_str
          >>= fun obj -> worker_publish base worker obj
          >>= fun () -> return "should not show");
         return body_str]
    else fail exn

(* dummy execution *)
let task_execute base worker task obj_id =
  let t_inputs = Task.inputs_of_t task in
  Lwt_list.map_p (fun input_id ->
    worker_request_object base worker input_id
    >>= fun obj_str -> return (input_id, obj_str)) t_inputs
  >>= fun input_tups ->
    let task_str = Task.string_of_t task in
    let inputs_str = String.concat "\n" (List.map (fun (id, content) ->
      Printf.sprintf "%d ->\n\t%s\n" id content) input_tups) in
    let content = Printf.sprintf "%s\n  [inputs]:\n%s\n" task_str inputs_str in
    to_local_obj (get_working_dir worker) obj_id worker.addr content

let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (task_id, obj_id) ->
    let task_id_last = try Task.id_of_t (List.hd worker.tasks) with _ -> (-1) in
    if task_id_last = task_id then execution_loop base worker cond
    else begin
        worker_request_task base worker task_id
        >>= fun task ->
          worker.status <- Working task;
          task_execute base worker task obj_id
        >>= fun obj -> worker_publish base worker obj
        >>= fun () ->
          worker.tasks <- task :: worker.tasks;
          worker.objects <- obj :: worker.objects;
          worker.status <- Idle;
          execution_loop base worker cond
      end

let rec heartbeat_loop base worker cond =
  match worker.status with
  | Idle -> begin worker_heartbeat base worker >>= function
    | None ->
       Lwt_unix.sleep idle_sleep
       >>= fun () -> heartbeat_loop base worker cond
    | Some (t_id, o_id) ->
       Lwt_condition.signal cond (t_id, o_id);
       Lwt_unix.sleep idle_sleep
       >>= fun () -> heartbeat_loop base worker cond end
  | Working _ ->
     worker_heartbeat base worker >>= fun _ ->
     Lwt_unix.sleep working_sleep
     >>= fun () -> heartbeat_loop base worker cond

let worker_file_server worker =
  let ip, port = worker.addr in
  Conduit_lwt_unix.init ~src:ip ()
  >>= fun ctx ->
    let ctx = Cohttp_lwt_unix_net.init ~ctx () in
    let mode = Conduit_lwt_unix.(`TCP (`Port port)) in
    let callback conn req body =
      let meth, headers, uri = Cohttp_lwt_unix.Request.(
        meth req, headers req, uri req) in
      let fname = Cohttp_lwt_unix.Server.resolve_file ~docroot:"." ~uri in
      let obj_id = match Cohttp.Header.get headers "obj_id" with
        | Some id -> id | None -> "-1" in
      let file = Printf.sprintf "worker%d/%s.out" worker.id obj_id in
      if Sys.file_exists fname && file = fname && meth = Cohttp.Code.(`GET) then
        Cohttp_lwt_unix.Server.respond_file ~fname ()
      else return (Response.make ~status:Cohttp.Code.(`No_content) (),
                   Body.empty)
    in
    Cohttp_lwt_unix.Server.create ~mode ~ctx
      (Cohttp_lwt_unix.Server.make ~callback ())

let worker base_str ip port =
  let base = Uri.of_string base_str in
  worker_register base (ip, port)
  >>= (fun worker ->
    let cond = Lwt_condition.create () in
    join [heartbeat_loop base worker cond; execution_loop base worker cond;
          worker_file_server worker])
  |> Lwt_main.run

let base = Cmdliner.Arg.(
  required & pos 0 (some string) None & info []
    ~docv:"HOST" ~doc:"the uri string of master node")

let ip = Cmdliner.Arg.(
  value & opt string "127.0.0.1" & info ["ip"]
    ~doc:"the ip address of this worker")

let port = Cmdliner.Arg.(
  value & opt int 8000 & info ["port"]
    ~doc:"the port number of this worker")

let () = Cmdliner.Term.(
  let worker_cmd =
    pure worker $ base $ ip $ port,
    info ~doc:"start a worker" "ci-worker" in
  match eval worker_cmd with `Error _ -> exit 1 | _ -> exit 0)
