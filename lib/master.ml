open Lwt

module Response = Cohttp_lwt_unix.Response
module Body = Cohttp_lwt_body
module Code = Cohttp.Code

type task_tbl = (Task.t, int) Hashtbl.t
type obj_tbl = (int, Object.t list) Hashtbl.t
type worker_tbl = (string * int, int * string) Hashtbl.t (* ip, port->id, sha *)
type queue_tbl = (int * string, Task.t Queue.t) Hashtbl.t (* id, sha->queue  *)

exception WrongMessage of string

let t_tbl : task_tbl = Hashtbl.create 16
let o_tbl : obj_tbl  = Hashtbl.create 16
let w_tbl : worker_tbl = Hashtbl.create 16
let q_tbl : queue_tbl = Hashtbl.create 16

let get_sha str = str

let new_worker ip port =
  if Hashtbl.mem w_tbl (ip, port) then return (Hashtbl.find w_tbl (ip, port))
  else begin
      let mem = Hashtbl.length w_tbl in
      let id = succ mem in
      let sha = get_sha (ip ^ (string_of_int port) ^ (string_of_int id)) in
      Hashtbl.add w_tbl (ip, port) (id, sha);
      Hashtbl.add q_tbl (id, sha) (Queue.create ());
      return (id, sha)
      end

let register_handler subs headers body =
  Body.to_string body
  >>= (fun body_str ->
    let msg = Message.worker_msg_of_sexp (Sexplib.Sexp.of_string body_str) in
    let ip, port = match msg with
      | Message.Register (i, p) -> i, p
      | _ -> raise (WrongMessage body_str) in
    new_worker ip port)
  >>= fun (id, sha) ->
    let resp = Response.make ~status:Code.(`Created) () in
    let msg_sexp = Message.(sexp_of_master_msg (Ack_register (id, sha))) in
    let body = Body.of_string (Sexplib.Sexp.to_string msg_sexp) in
    return (resp, body)

(* heartbeat : POST base/worker<id> -> `Ok *)
let heartbeat_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let sha = match Cohttp.Header.get headers "worker" with
    | Some str -> str | None -> "" in
  let t_q = Hashtbl.find q_tbl (id, sha) in
  let t_id = if Queue.is_empty t_q then (-1)
             else Task.id_of_t (Queue.peek t_q) in
  Body.to_string body
  >>= fun body_str ->
    let msg = Message.worker_msg_of_sexp (Sexplib.Sexp.of_string body_str) in
    let resp_msg = Message.(match msg with
      | Heartbeat None ->
         if t_id = (-1) then Message.Ack_heartbeat
         else Message.New_task (t_id, Hashtbl.find t_tbl (Queue.peek t_q))
      | Heartbeat (Some execution_id) ->
         assert (t_id = execution_id); Message.Ack_heartbeat
      |_ -> raise (WrongMessage body_str)) in
    let resp = Response.make ~status:Code.(`OK) () in
    let msg_sexp = Message.sexp_of_master_msg resp_msg in
    let body = Body.of_string (Sexplib.Sexp.to_string msg_sexp) in
    return (resp, body)

(* request_task : GET base/worker<id>/newtask/<task_id> -> `OK  *)
let request_task_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let t_id = int_of_string (groups.(2)) in
  let sha = match Cohttp.Header.get headers "worker" with
    | Some str -> str | None -> "" in
  let t_q = Hashtbl.find q_tbl (id, sha) in
  let t = Queue.peek t_q in
  assert (t_id = Task.id_of_t t);
  let resp = Response.make ~status:Code.(`OK) () in
  let body = Body.of_string (Sexplib.Sexp.to_string (Task.sexp_of_t t)) in
  return (resp, body)

(* publish : POST base/worker<id>/objects -> `Created *)
let publish_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let sha = match Cohttp.Header.get headers "worker" with
    | Some str -> str | None -> "" in
  Body.to_string body
  >>= fun body_str ->
    let msg = Message.worker_msg_of_sexp (Sexplib.Sexp.of_string body_str) in
    let addr, obj_info = Message.(match msg with
      | Publish (a, o) -> a, o | _ -> raise (WrongMessage body_str)) in
    assert ((id, sha) = Hashtbl.find w_tbl addr);
    let (obj_id, obj_path) = obj_info in
    let obj = Object.create obj_id addr obj_path in
    Hashtbl.replace o_tbl obj_id (obj :: (Hashtbl.find o_tbl obj_id));
    return (Response.make ~status:Code.(`Created) (), Body.empty)

let handler_route_table = Re.(
  let post, get = Code.(`POST, `GET) in
  [(post, str "/workers"), register_handler;
   (post, seq [str "/worker"; group (rep1 digit); eos]), heartbeat_handler;
   (get,  seq [str "/worker"; group (rep1 digit);
               str "/newtask/"; group (rep1 digit)]), request_task_handler;
   (post, seq [str "/worker"; group (rep1 digit);
               str "/objects"]), publish_handler])

let route_handler meth path = Re.(
  List.fold_left (fun acc ((m, p), h) ->
    if meth <> m then acc
    else begin
        let re = compile p in
        try
          let indexes = get_all_ofs (exec re path) in
          let groups = Array.map (fun (b, e) ->
            if b = (-1) || e = (-1) then ""
            else String.sub path b (e - b)) indexes in
          (h groups) :: acc
        with Not_found -> acc
      end) [] handler_route_table)

let callback conn req body =
  let meth, headers, uri = Cohttp_lwt_unix.Request.(
    meth req, headers req, uri req) in
  let path = Uri.path uri in
  Printf.eprintf "[Request]: %s %s\n%!" (Code.string_of_method meth) path;
  let handler = route_handler meth path in
  if List.length handler <> 1 then
    return (Response.make ~status:Code.(`Not_found) (),
            Body.of_string "Service not found...")
  else begin
      let handler = List.hd handler in
      catch (fun () -> handler headers body) (fun exn ->
        (match exn with
         | WrongMessage str->
            Printf.eprintf "Received undecodable message: %s\n" str;
         | _ ->
            Printf.eprintf "Error while handling the request:\n%s %s"
                           (Cohttp.Code.string_of_method meth) path);
        return (Response.make ~status:Code.(`Not_found) (),
                Body.of_string "Service not found..."))
    end

let master ip port =
  Conduit_lwt_unix.init ~src:ip ()
  >>= (fun ctx ->
    let ctx = Cohttp_lwt_unix_net.init ~ctx () in
    let mode = Conduit_lwt_unix.(`TCP (`Port port)) in
    Cohttp_lwt_unix.Server.create ~mode ~ctx
      (Cohttp_lwt_unix.Server.make ~callback ()))
  |> Lwt_unix.run

let ip = Cmdliner.Arg.(
  value & opt string "127.0.0.1" & info ["ip"]
    ~doc:"the ip address of the master")

let port = Cmdliner.Arg.(
  value & opt int 8080 & info ["port"]
    ~doc:"the port number of the master")

let () = Cmdliner.Term.(
  let master_cmd =
    pure master $ ip $ port,
    info ~doc:"start the master" "ci-master" in
  match eval master_cmd with `Error _ -> exit 1 | _ -> exit 0)
