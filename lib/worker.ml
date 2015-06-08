open Lwt
open Message

module Body = Cohttp_lwt_body
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response

type t = {
  id : int;                      (* worker id assigned by master *)
  addr : string * int;           (* ip * port number where worker is hosted *)
  sha : string;                  (* a checksum used for master authentication *)
  mutable tasks : Task.t list;     (* all completed tasks *)
  mutable objects : Object.t list; (* objects hosted on this machine *)
  mutable status : worker_status;  (* working on a task of idle *)
}
and worker_status =
  | Working of Task.t
  | Idle

exception WrongResponse of string * string

let idle_sleep = 3.0
let working_sleep = 5.0
let body_of_sexp sexp = Body.of_string (Sexplib.Sexp.to_string sexp)

let sub len str = String.sub str 0 len

let log action func ~info =
  let title = Printf.sprintf "%s@%s" action func in
  if info = "" then Printf.eprintf "[%s]\n%!" title
  else Printf.eprintf "[%s]: %s\n%!" title info

let get_working_dir worker =
  let root = Sys.getcwd () in
  let dir = Printf.sprintf "worker%d" worker.id in
  let path = Filename.concat root dir in
  if not (Sys.file_exists path) then Unix.mkdir path 0o775;
  path

let to_local_obj worker id content =
  let file = Printf.sprintf "%s.out" (sub 5 id) in
  let dir = get_working_dir worker in
  let path = Filename.concat dir file in
  return (open_out path)
  >>= fun oc -> return (output_string oc content)
  >>= fun () ->
    let () = close_out oc in
    let relative_path = Filename.concat (Filename.basename dir) file in
    let obj = Object.create id worker.addr relative_path in
    worker.objects <- obj :: worker.objects;
    return obj

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
         let info = Printf.sprintf "success: %d %s" id sha in
         log "send" "register" ~info;
         return { id; addr; sha; tasks = []; objects = []; status = Idle}
      | _ -> fail exn
    else fail exn

(* POST base/worker<id> -> `Ok *)
let worker_heartbeat base { id; sha; status } =
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let msg =
    match status with
    | Working t ->
       let t_id = Task.id_of_t t in
       log "send" "heartbeat" ~info:("working " ^ (sub 5 t_id));
       Heartbeat (Some (Task.id_of_t t))
    | Idle ->
       log "send" "heartbeat" ~info:"idle";
       Heartbeat None in
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
      | New_task (id, task) -> return (Some (id, task))
      | _ -> fail exn
    else fail exn

(* GET base/worker<id>/newtask/<task_id> -> `OK  *)
let worker_request_task base {id; sha} task_id =
  log "send" "task" ~info:("request task " ^ (sub 5 task_id));
  let headers = Cohttp.Header.of_list ["worker", sha] in
  let uri_path = Printf.sprintf "worker%d/newtask/%s" id task_id in
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
  log "send" "publish" ~info:("object " ^ (sub 5 (Object.id_of_t obj)));
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
let worker_consult_object base {addr} obj_id =
  let ip, port = addr in
  let info = "need positions of object " ^ (sub 5 obj_id) in
  log "send" "consult" ~info;
  let uri_path = Printf.sprintf "object%s" obj_id in
  let uri = Uri.resolve "" base (Uri.of_string uri_path) in
  let headers = Cohttp.Header.of_list ["ip", ip; "port", string_of_int port] in
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
  let obj = List.filter (fun obj ->
    Object.id_of_t obj = obj_id) worker.objects in
  if List.length obj <> 0 then begin
    log "send" "object"
        ~info:(Printf.sprintf "get object %s locally" (sub 5obj_id));
    let path = Object.path_of_t (List.hd obj) in
    assert (Sys.file_exists path);
    let ic = open_in path in
    let line = input_line ic in
    let str = Printf.sprintf "\tFrom %s: %s\n" (sub 5 obj_id)  line in
    return str
    (* Lwt_io.(open_file ~mode:input ~flags:[Unix.O_RDONLY] file
    >>= fun ic -> read ic*) end
  else begin
    worker_consult_object base worker obj_id
    >>= fun (ip, port, path) ->
    let headers =
      Cohttp.Header.of_list ["obj_id",sub 5 obj_id] in
    let req_base = Uri.of_string (Printf.sprintf "http://%s:%d" ip port) in
    let path = Uri.of_string path in
    let uri = Uri.resolve "" req_base path in
    let info = Printf.sprintf "get object %s from %s"
      (sub 5 obj_id) (Uri.to_string uri) in
    log "send" "object" ~info;
    Client.get ~headers uri
    >>= fun (resp, body) -> Body.to_string body
    >>= fun body_str ->
      let status = Response.status resp in
      let exn = WrongResponse (Cohttp.Code.string_of_status status, body_str) in
      if status = Cohttp.Code.(`OK) then
        choose
          [(to_local_obj worker obj_id body_str
            >>= fun obj -> worker_publish base worker obj
            >>= fun () -> return "should not show");
           return body_str]
      else fail exn end

(* dummy execution *)
let task_execute base worker task obj_id =
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
    to_local_obj worker obj_id content

let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (id, task) ->
    if List.mem id (List.rev_map Task.id_of_t worker.tasks) then
      execution_loop base worker cond
    else begin
        let task = Task.t_of_sexp (Sexplib.Sexp.of_string task) in
        worker.status <- Working task;
        task_execute base worker task id
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
    | Some (id, task) ->
       Lwt_condition.signal cond (id, task);
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
      let obj_id = match Cohttp.Header.get headers "obj_id" with
        | Some id -> id | None -> "-1" in
      let file = Printf.sprintf "worker%d/%s.out" worker.id obj_id in
      let fname = Filename.concat (Sys.getcwd ()) file in
      log "receive" "request" ~info:("object at " ^ fname);
      if Sys.file_exists file && meth = Cohttp.Code.(`GET) then
        let ic = open_in fname in
        let line = input_line ic in
        let str = Printf.sprintf "\tFrom %s: %s\n" obj_id  line in
        return (Response.make ~status:Cohttp.Code.(`OK) (), Body.of_string str)
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
    join [heartbeat_loop base worker cond;
          execution_loop base worker cond;
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
    info ~doc:"start a worker" "worker" in
  match eval worker_cmd with `Error _ -> exit 1 | _ -> exit 0)
