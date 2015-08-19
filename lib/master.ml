open Lwt
open Common_types

module Response = Cohttp_lwt_unix.Response
module Body = Cohttp_lwt_body

let log handler worker_id info =
  let title = Printf.sprintf "worker%d@%s" worker_id handler in
  Printf.eprintf "[%s]: %s\n%!" title info

let time () = Unix.(
  let tm = localtime (time ()) in
  Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec)

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


let register_handler params headers body =
  message_of_body body >>= fun m ->
  let host = Message.(match m with
      | Register h -> h
      | Heartbeat _ | Publish _ | Spawn_jobs _ ->
         failwith "Wrong message for register") in
  let id, token = Monitor.new_worker host in
  Store.register_token token >>= fun () ->

  let m = Message.Ack_register (id, token) in
  let resp = Response.make ~status:`Created () in
  let body = body_of_message m in
  log "register" id "new worker registered";
  return (resp, body)


let heartbeat_handler params headers body =
  let id = List.assoc "id" params |> int_of_string in
  let token = match Cohttp.Header.get headers "worker" with
    | Some t -> t | None -> "" in
  Monitor.verify_worker id token;

  message_of_body body >>= fun m ->
  let resp_m = Message.(match m with
      | Heartbeat None ->
         log "heartbeat" id "idle";
         (match Scheduler.find_job token with
          | None -> Ack_heartbeat
          | Some (jid, c, desp) -> Monitor.new_job jid c token;
                                   New_job (jid, desp))
      | Heartbeat (Some jid) ->
         Message.Ack_heartbeat
      | Register _ | Publish _ | Spawn_jobs _ ->
         failwith "wrong message for heartbeat") in

  let resp = Response.make ~status:`OK () in
  let body = body_of_message resp_m in
  return (resp, body)


let publish_handler params headers body =
  let id = List.assoc "id" params |> int_of_string in
  let token = match Cohttp.Header.get headers "worker" with
    | Some t -> t | None -> "" in
  Monitor.verify_worker id token;

  message_of_body body >>= fun m ->
  let result, jid = Message.(match m with
      | Publish (result, id) -> result, id
      | Register _ | Heartbeat _ | Spawn_jobs _ ->
         failwith "wrong message for publish") in
  let r = match result with
      | `Success ->
         Monitor.publish_object jid token; "SUCCESS"
      | `Delegate d ->
         Monitor.job_completed jid token; "DELEGATE: " ^ Scheduler.task_info d
      | `Fail f ->
         Monitor.job_completed jid token; "FAIL: " ^ f in
  log "publish" id (Printf.sprintf "object %s %s" (Scheduler.task_info jid) r);
  Scheduler.publish_object token result jid >>= fun () ->

  empty_response `Created


let spawn_handler params headers body =
  let id = List.assoc "id" params |> int_of_string in
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


let github_hook_handler params headers body =
  let pr_num = List.assoc "pr_num" params |> int_of_string in
  Scheduler.github_hook pr_num
  >>= fun () ->
  empty_response `Accepted


let user_pkg_demand_handler params headers body =
  let c = try List.assoc "compiler" params with _ -> "" in
  let dep = try List.assoc "depopt" params with _ -> "" in

  let repo_name = try List.assoc "name" params with _ -> "" in
  let repo_addr = try List.assoc "address" params with _ -> "" in
  let repo_p = try List.assoc "priority" params with _ -> "" in
  let split s ~by = Re.(
    by |> char |> compile
    |> (fun re -> split re s)) in

  let pin_pkg = try List.assoc "pin" params with _ -> "" in
  let pin_target = try List.assoc "target" params with _ -> "" in

  let compilers = split c ~by:';' in
  let depopts = split dep ~by:';' in
  let repository =
    if repo_name = "" || repo_addr = "" then None
    else begin
      let names = split repo_name ~by:';' in
      let addrs = split repo_addr ~by:';' in
      assert (List.length names = List.length addrs);
      let tups = List.combine names addrs in
      if repo_p = "" then
        Some (List.map (fun (n, add) -> n, add, None) tups)
      else
        let p = split repo_p ~by:';' in
        assert (List.length names = List.length p);
        let priorities = List.map (fun p -> Some (int_of_string p)) p in
        let rec combine acc = function
          | [], [] -> acc
          | (h1, h2) :: tup_tl, h :: tl ->
             combine ((h1, h2, h) :: acc) (tup_tl, tl)
          | _ -> failwith "uneven list for combine" in
        Some (combine [] (tups, priorities)) end in
  let pin =
    if pin_pkg = "" || pin_target = "" then None
    else begin
      let pkgs = split pin_pkg ~by:';' in
      let targets = split pin_target ~by:';' in
      assert (List.length pkgs = List.length targets);
      Some (List.combine pkgs targets) end in

  let pkg = List.assoc "pkg" params in
  let name, version = Ci_opam.parse_user_demand pkg in
  let depopts = if depopts = [] then None
                else Some (List.rev_map Ci_opam.parse_user_demand depopts) in
  let ptask = Task.make_pkg_task ~name ?version ?depopts () in

  let worker_hosts = Monitor.worker_environments () in
  let envs = List.fold_left (fun acc h ->
    let compilers = if compilers = [] then Monitor.compilers ()
                    else compilers in
    (List.rev_map (fun c -> h, c) compilers) @ acc) [] worker_hosts in
  (List.rev_map (fun (h, c) ->
     let id = Task.hash_id ?repository ?pin ptask [] c h in
     let job = Task.make_job id [] c h ptask ?repository ?pin ()  in
     id, job, []) envs)
  |> return
  >>= fun job_lst ->
  Scheduler.update_tables job_lst >>= fun () ->

  let headers = Cohttp.Header.of_list
    ["Access-Control-Allow-Origin","*"] in
  let resp = Response.make ~status:`Accepted ~headers () in
  let ids = `A (List.rev_map (fun (id, _, _) -> `String id) job_lst) in
  let body = Body.of_string (Ezjsonm.to_string ids) in
  return (resp, body)


let user_job_query_handler params headers body =
  let jid = List.assoc "jid" params in
  Scheduler.progress_info jid >>= fun str ->
  let body = Body.of_string str in
  let headers = Cohttp.Header.of_list
    ["Access-Control-Allow-Origin","*"] in
  let resp = Response.make ~status:`OK ~headers () in
  return (resp, body)


let user_worker_query_handler param headers body =
  let statuses = Monitor.worker_statuses () in
  List.rev_map (fun (wid, token, status) ->
      let h, c_opt = Monitor.worker_env token in
      let wid = "worker_id", Ezjsonm.int wid in
      let token = "worker_token", Ezjsonm.string token in
      let status = "worker_status",
          Monitor.info_of_status status
          |> snd
          |> Ezjsonm.option Ezjsonm.string in
      let host = "worker_host", Ezjsonm.string h in
      let compiler = "worker_compiler", Ezjsonm.option Ezjsonm.string c_opt in
      [wid; token; status; host; compiler]) statuses
  |> List.rev
  |> Ezjsonm.list Ezjsonm.dict
  |> Ezjsonm.to_string
  |> fun str ->
     let resp = Response.make ~status:`OK () in
     let body = Body.of_string str in
     return (resp, body)


let user_object_query_handler params headers body =
  let jid = List.assoc "jid" params in
  catch (fun () ->
    Store.retrieve_job jid >>= fun (job, _) ->
    let task_info = Task.(task_of_job job |> info_of_task) in
    let inputs = Task.inputs_of_job job in
    Lwt_list.rev_map_s (fun i ->
      Store.retrieve_job i >>= fun (j, _) ->
      return (i, Task.(task_of_job j |> info_of_task))) inputs
    >>= fun inputs_info ->

    let c, h = Task.env_of_job job in
    catch (fun () ->
        Store.retrieve_object jid >>= fun obj ->
        let result = Object.result_of_t obj in
        let r, r_info = Object.info_of_result result in
        let r = "result", Ezjsonm.string r in
        let info = "info", Ezjsonm.option Ezjsonm.string r_info in
        return (Ezjsonm.value (Ezjsonm.dict [r; info])))
        (fun exn -> return (Ezjsonm.string "No result"))
    >>= fun result ->

    let task_info_str (p, v_opt) =
      match v_opt with
      | None -> p
      | Some v -> Printf.sprintf "%s %s" p v in
    let ass_lst = [
       "id", Ezjsonm.string jid;
       "task_info", Ezjsonm.string (task_info_str task_info);
       "inputs", List.rev_map (fun (id, info) ->
           ["input_id", Ezjsonm.string id;
            "input_info", Ezjsonm.string (task_info_str info)]) inputs_info
           |> Ezjsonm.list (fun lst -> Ezjsonm.value (Ezjsonm.dict lst));
       "host", Ezjsonm.string h;
       "compiler", Ezjsonm.string c;
       "result", result] in
    let str = Ezjsonm.to_string (Ezjsonm.dict ass_lst) in
    let headers = Cohttp.Header.of_list
    ["Access-Control-Allow-Origin","*"] in
    let resp = Response.make ~status:`OK ~headers () in
    let body = Body.of_string str in
    return (resp, body))
    (fun exn -> empty_response ~status:`Not_found)


open Opium.Std

let handler_wrapper handler keys req =
  let params = List.map (fun k -> k, param req k) keys in
  let headers = Request.headers req in
  let body = req.Request.body in
  handler params headers body >>= fun r ->
  Response.of_response_body r
  |> return

let register =
  post "/worker/registration"
       (handler_wrapper register_handler [])

let heartbeat =
  post "/workers/:id/state"
       (handler_wrapper heartbeat_handler ["id"])

let publish =
  post "/workers/:id/objects"
       (handler_wrapper publish_handler ["id"])

let spawn =
  post "/workers/:id/newjobs"
       (handler_wrapper spawn_handler ["id"])

let github_hook =
  post "/github/:pr_num"
       (handler_wrapper github_hook_handler ["pr_num"])

let package_demand =
  post "/package/:pkg" (fun req ->
    let uri = Request.uri req in
    let query' = Uri.query uri in
    let query = List.map (fun (k, vs) -> k, String.concat ";" vs) query' in
    let pkg = param req "pkg" in
    let params = ("pkg", pkg) :: query in

    let headers = Request.headers req in
    let body = req.Request.body in
    user_pkg_demand_handler params headers body >>= fun r ->
    Response.of_response_body r
    |> return)

let job_query =
  get "/object/:jid"
      (handler_wrapper user_job_query_handler ["jid"])

let worker_query =
  get "/workers/statuses"
      (handler_wrapper user_worker_query_handler [])

let object_info_query =
  get "/object/:jid/info"
      (handler_wrapper user_object_query_handler ["jid"])

let server =
  App.empty
  |> register |> heartbeat |> publish |> spawn
  |> package_demand |> github_hook
  |> job_query |> worker_query |> object_info_query


let master fresh store ip port =
  Store.initial_store ~uri:store ~fresh () >>= (fun () ->
  Scheduler.bootstrap () >>= fun () ->

    let rec t_monitor () =
      Monitor.worker_monitor () >>= fun workers ->
      log "monitor" (-1) ("some worker dies " ^ time ());
      List.iter (fun (_, t) -> Scheduler.invalidate_token t) workers;
      t_monitor () in

    join [App.start (server |> App.port port);
          t_monitor ()])

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
