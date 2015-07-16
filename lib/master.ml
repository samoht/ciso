open Lwt
open Common_types

module Response = Cohttp_lwt_unix.Response
module Body = Cohttp_lwt_body

let log handler worker_id info =
  let title = Printf.sprintf "worker%d@%s" worker_id handler in
  Printf.eprintf "[%s]: %s\n%!" title info

let body_of_message m =
  Message.sexp_of_master_msg m
  |> Sexplib.Sexp.to_string
  |> Body.of_string

let message_of_body body =
  Body.to_string body >>= fun b ->
  Sexplib.Sexp.of_string b
  |> Message.worker_msg_of_sexp
  |> return

let empty_response ~status =
  let resp = Response.make ~status () in
  let body = Body.empty in
  return (resp, body)


let register_handler groups headers body =
  message_of_body body >>= fun m ->
  let compiler, host = Message.(match m with
      | Register (c, h) -> c, h
      | Heartbeat _ | Publish _ | Spawn_jobs _ ->
         failwith "Wrong message for register") in
  let id, token = Monitor.new_worker compiler host in
  Store.register_token token >>= fun () ->

  let m = Message.Ack_register (id, token) in
  let resp = Response.make ~status:`Created () in
  let body = body_of_message m in
  return (resp, body)


let heartbeat_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let token = match Cohttp.Header.get headers "worker" with
    | Some t -> t | None -> "" in
  Monitor.verify_worker id token;

  message_of_body body >>= fun m ->
  let resp_m = Message.(match m with
      | Heartbeat None ->
         log "heartbeat" id "idle";
         (match Scheduler.find_job token with
          | None -> Ack_heartbeat
          | Some (jid, tdesp) -> New_job (jid, tdesp))
      | Heartbeat (Some jid) ->
         Message.Ack_heartbeat
      | Register _ | Publish _ | Spawn_jobs _ ->
         failwith "wrong message for heartbeat") in

  let resp = Response.make ~status:`OK () in
  let body = body_of_message resp_m in
  return (resp, body)


let publish_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let token = match Cohttp.Header.get headers "worker" with
    | Some t -> t | None -> "" in
  Monitor.verify_worker id token;

  message_of_body body >>= fun m ->
  let result, jid = Message.(match m with
      | Publish (result, id) -> result, id
      | Register _ | Heartbeat _ | Spawn_jobs _ ->
         failwith "wrong message for publish") in
  let r = match result with
      | `Success -> "SUCCESS"
      | `Fail f -> "FAIL: " ^ f
      | `Delegate d -> "DELEGATE: " ^ Scheduler.task_info d in
  log "publish" id (Printf.sprintf "object %s %s" (Scheduler.task_info jid) r);
  if result = `Success then Monitor.publish_object jid token;
  Scheduler.publish_object token result jid >>= fun () ->

  empty_response `Created


let spawn_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let token = match Cohttp.Header.get headers "worker" with
    | Some t -> t | None -> "" in
  Monitor.verify_worker id token;

  message_of_body body >>= fun m ->
  let m_job_lst = Message.(match m with
      | Spawn_jobs job_lst -> job_lst
      | Publish _ | Register _ | Heartbeat _ ->
         failwith "wrong message for spawn") in
  let job_lst = List.rev_map (fun (jid, desp, deps) ->
      let job = Sexplib.Sexp.of_string desp
                |> Task.job_of_sexp in
      jid, job, deps) m_job_lst in
  Scheduler.update_tables job_lst >>= fun () ->

  empty_response `Created


let github_hook_handler groups headers body =
  let pr_num = int_of_string (groups.(1)) in
  Scheduler.github_hook pr_num
  >>= fun () ->
  empty_response `Accepted


let user_pkg_demand_handler groups headers body =
  let pkg = groups.(1) in
  let name, version = Ci_opam.parse_user_demand pkg in
  let task = Task.make_pkg_task ~name ?version () in

  let env_lst = Monitor.worker_environments () in
  let job_lst = List.rev_map (fun (c, h) ->
    let id = Task.hash_id task [] c h in
    let job = Task.make_job id [] c h task in
    id, job, []) env_lst in
  Scheduler.update_tables job_lst >>= fun () ->

  let resp = Response.make ~status:`Accepted () in
  let ids = List.rev_map (fun (id, _, _) -> id) job_lst in
  let body_str = Printf.sprintf "%s\n" (String.concat "\n" ids) in
  let body = Body.of_string body_str in
  return (resp, body)


let user_compiler_demand_handler groups headers body =
  let c = groups.(1) in
  let task = Task.make_compiler_task c in

  let env_lst = Monitor.worker_environments () in
  if env_lst = [] then empty_response `OK
  else
    let c, h = List.hd env_lst in
    let id = Task.hash_id task [] c h in
    let job = Task.make_job id [] c h task in
    Scheduler.update_tables [id, job, []] >>= fun () ->

    let resp = Response.make ~status:`Accepted () in
    let body = Body.of_string (Printf.sprintf "%s\n" id) in
    return (resp, body)


let user_query_handler groups headers body =
  let id = groups.(1) in
  Scheduler.query_state id >>= fun s_str ->
  let info = Scheduler.task_info id in
  let body_str = Printf.sprintf "%s: %s\n" info s_str in
  let body = Body.of_string body_str in
  let resp = Response.make ~status:`OK () in
  return (resp, body)


let handler_route_table = Re.(
  let post = `POST in
  [(post,
    str "/worker/registration"),
    register_handler;
   (post,
    seq [str "/worker"; group (rep1 digit); str "/state"]),
    heartbeat_handler;
   (post,
    seq [str "/worker"; group (rep1 digit); str "/objects"]),
    publish_handler;
   (post,
    seq [str "/worker"; group (rep1 digit); str "/newjobs"]),
    spawn_handler;
   (post,
    seq [str "/github/"; group (rep1 digit); eos]),
    github_hook_handler;
   (post,
    seq [str "/package/"; group (rep1 any); eos]),
    user_pkg_demand_handler;
   (post,
    seq [str "/compiler/"; group (rep1 any); eos]),
    user_compiler_demand_handler;
   (post,
    seq [str "/object/"; group (rep1 any); eos]),
    user_query_handler])


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
  let handler = route_handler meth path in

  if List.length handler <> 1 then empty_response `Not_found
  else
    let handler = List.hd handler in
    let err_handler exn =
      let meth = Cohttp.Code.string_of_method meth in
      let err_m = match exn with
        | Failure str -> Printf.sprintf "Error: %s %s -> %s \n%!" meth path str
        | _ -> Printf.sprintf "Error: %s %s -> unknown \n%!" meth path in
      prerr_endline err_m;
      empty_response `No_content in
    catch (fun () -> handler headers body) err_handler


let master fresh store ip port =
  Store.initial_store ~uri:store ~fresh () >>= (fun () ->
  Scheduler.bootstrap () >>= fun () ->
  Conduit_lwt_unix.init ~src:ip ()
  >>= fun ctx ->
    let ctx = Cohttp_lwt_unix_net.init ~ctx () in
    let mode = Conduit_lwt_unix.(`TCP (`Port port)) in
    let t_server = Cohttp_lwt_unix.Server.create ~mode ~ctx
      (Cohttp_lwt_unix.Server.make ~callback ()) in

    let rec t_monitor () =
      Monitor.worker_monitor () >>= fun workers ->
      List.iter (fun (_, t) -> Scheduler.invalidate_token t) workers;
      t_monitor () in
    join [t_server; t_monitor ()])
  |> Lwt_unix.run

let ip = Cmdliner.Arg.(
  value & opt string "127.0.0.1" & info ["ip"]
    ~doc:"the ip address of the master")

let port = Cmdliner.Arg.(
  value & opt int 8080 & info ["port"]
    ~doc:"the port number of the master")

let fresh = Cmdliner.Arg.(
  value & flag & info ["fresh"; "f"]
    ~doc:"start with a fresh new store")

let store = Cmdliner.Arg.(
  required & pos 0 (some string) None & info []
    ~doc:"the address to contact the data store" ~docv:"STORE")


let () = Cmdliner.Term.(
  let master_cmd =
    pure master $ fresh $ store $ ip $ port,
    info ~doc:"start the master" "master" in
  match eval master_cmd with `Error _ -> exit 1 | _ -> exit 0)
