(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

let debug fmt = Gol.debug ~section:"master" fmt

let heartbeat_handler t =
  let wid = parse_wid params in
  message_of_body body >|= fun m ->
  let resp_m =
    let open Message in
    match m with
    | Heartbeat None ->
      debug "heartbeat: %s idle" (Id.to_string wid);
      (match Scheduler.find_job wid with
       | None -> Ack_heartbeat
       | Some (job, deps) ->
         let jid = Job.id job in
         Monitor.start wid jid;
         New_job (jid, deps))
    | Heartbeat (Some _jid) ->
      Message.Ack_heartbeat
    | Register _ | Publish _ | Spawn_jobs _ ->
      failwith "wrong message for heartbeat"
  in
  let resp = Response.make ~status:`OK () in
  let body = body_of_message resp_m in
  resp, body

let publish_handler s params headers body =
  let wid = parse_wid params in
  message_of_body body >>= fun m ->
  let result, jid =
    let open Message in
    match m with
    | Publish (result, id) -> result, id
    | Register _ | Heartbeat _ | Spawn_jobs _ ->
      failwith "wrong message for publish"
  in
  (match result with
   | `Success ->
     Store.retrieve_job s jid >|= fun (job, _) ->
     let id = Job.output job in
     Monitor.publish wid id; "SUCCESS"
   | `Delegate d ->
     Monitor.complete wid;
     Lwt.return ("DELEGATE: " ^ Scheduler.job_info d)
   | `Fail f ->
     Monitor.complete wid;
     Lwt.return ("FAIL: " ^ f)
  ) >>= fun r ->
  debug "publish: %s object %s %s" (Id.to_string wid) (Scheduler.job_info jid) r;
  Scheduler.publish_object s result jid >>= fun () ->
  empty_response ~status:`Created

let spawn_handler s params headers body =
  message_of_body body >>= fun m ->
  let m_job_lst =
    let open Message in
    match m with
    | Spawn_jobs job_lst -> job_lst
    | Publish _ | Register _ | Heartbeat _ ->
      failwith "wrong message for spawn"
  in
  let job_lst =
    List.rev_map (fun (jid, desp, deps) ->
        let job = Job.of_string desp in
        jid, job, deps
      ) m_job_lst
  in
  Scheduler.update_tables s job_lst >>= fun () ->
  empty_response ~status:`Created

let github_hook_handler _s params _headers _body =
  let _pr_num = List.assoc "pr_num" params |> int_of_string in
  (* Scheduler.github_hook s pr_num >>= fun () ->
     empty_response ~status:`Accepted *)
  failwith "TODO"

let mk_pkg str =
  match Stringext.cut str ~on:"." with
  | None        -> Package.create str
  | Some (n, v) -> Package.create ~version:v n

let user_pkg_demand_handler s params _headers _body =
  let c = try List.assoc "compiler" params with _ -> "" in
  let dep = try List.assoc "depopt" params with _ -> "" in
  let repo_name = try List.assoc "name" params with _ -> "" in
  let repo_addr = try List.assoc "address" params with _ -> "" in
  let split s ~by = Re.(by |> char |> compile |> (fun re -> split re s)) in
  let pin_pkg = try List.assoc "pin" params with _ -> "" in
  let pin_target = try List.assoc "target" params with _ -> "" in
  let compilers = split c ~by:';' in
  let depopts = split dep ~by:';' in
  let repos =
    if repo_name = "" || repo_addr = "" then
      []
    else (
      let names = split repo_name ~by:';' in
      let addrs = split repo_addr ~by:';' in
      assert (List.length names = List.length addrs);
      List.combine names addrs
      |> List.map (fun (n, a) -> Task.Repository (n, a))
    )
  in
  let pins =
    if pin_pkg = "" || pin_target = "" then
      []
    else (
      let pkgs = split pin_pkg ~by:';' in
      let targets = split pin_target ~by:';' in
      assert (List.length pkgs = List.length targets);
      List.combine pkgs targets
      |> List.map (fun (p,t) -> Task.Pin (p, t))
    )
  in
  let pkg = mk_pkg (List.assoc "pkg" params) in
  let depopts = List.rev_map mk_pkg depopts in
  let ptask = Task.create ~depopts pkg in
  let hosts = Monitor.available_hosts () in
  let compilers =
    if compilers = [] then [ "3.12.1"; "4.00.1"; "4.01.0"; "4.02.3" ]
    else compilers
  in
  let envs =
    List.fold_left (fun acc h ->
        (List.rev_map (fun c -> h, c) compilers) @ acc
      ) [] hosts
  in
  let job_lst =
    List.rev_map (fun (host, compiler) ->
        (* URGENT FIXME: confusion between task and jobs. Here we
           should create a task, not a job. *)
        let inputs = [] in
        let id = Task.id ~repos ~pins ~inputs ~compiler ~host ptask `Job in
        let result = `Unknown in
        (* FIXME: ugly *)
        let output = Id.of_string `Object (Id.to_string id) in
        (* FIXME: should not repeat the arguments twice
           (ie. Job.create should do the hashing. *)
        let job =
          Job.create ~id ~output ~inputs ~result ~compiler ~host ~repos ~pins
            ptask
        in
        id, job, []
      ) envs
  in
  Scheduler.update_tables s job_lst >|= fun () ->
  let resp = Response.make ~status:`Accepted () in
  let ids = List.rev_map (fun (id, _, _) -> Id.to_string id) job_lst in
  let body_str = Printf.sprintf "%s\n" (String.concat "\n" ids) in
  let body = Body.of_string body_str in
  resp, body

let user_job_query_handler s params _headers _body =
  let jid = parse_jid params in
  Scheduler.progress_info s jid >|= fun str ->
  let body = Body.of_string str in
  let resp = Response.make ~status:`OK () in
  resp, body

let user_worker_query_handler _param _headers _body =
  let status = Monitor.status () in
  let info =
    List.rev_map (fun (wid, status) ->
        let status_str = match status with
          | Monitor.Idle       -> "Idle"
          | Monitor.Working id ->
            Printf.sprintf "Working %s" (Scheduler.job_info id)
        in
        let h = Monitor.host wid in
        Printf.sprintf "worker %s, %s, %s"
          (Id.to_string wid) (Host.to_string h) status_str
      ) status
  in
  let str =
    Printf.sprintf "%s\n"
      (if info <> [] then (String.concat "\n" info) else "No alive workers")
  in
  let resp = Response.make ~status:`OK () in
  let body = Body.of_string str in
  Lwt.return (resp, body)

let user_object_query_handler s params _headers _body =
  let jid = parse_jid params in
  Lwt.catch (fun () ->
      Store.retrieve_job s jid >>= fun (job, _) ->
      let oid = Job.output job in
      Store.retrieve_object s oid >|= fun obj ->
      let input_info = Job.inputs job |> List.map Id.to_string in
      let task_info = Job.task job |> Task.info_of_task in
      let result = Job.result job in
      let input_info = Printf.sprintf "\n%s" (String.concat "\n" input_info) in
      let ass_lst = [
        "Id"        , Id.pretty jid;
        "Task"     , task_info;
        "Inputs"   , input_info;
        "Compiler" , Job.compiler job;
        "Host"     , Host.to_string (Job.host job);
        "Result"   , Job.string_of_result result
      ] in
      let str =
        List.map (fun (n, v) -> Printf.sprintf "[%s]: %s" n v) ass_lst
        |> String.concat "\n"
        |> (fun info -> info ^ "\n")
      in
      let resp = Response.make ~status:`OK () in
      let body = Body.of_string str in
      resp, body)
    (fun _exn ->
       empty_response ~status:`Not_found)

open Opium.Std

let handler_wrapper handler keys req =
  let params = List.map (fun k -> k, param req k) keys in
  let headers = Request.headers req in
  let body = req.Request.body in
  handler params headers body >>= fun r ->
  Response.of_response_body r
  |> Lwt.return

let register s =
  post "/worker/registration"
       (handler_wrapper (register_handler s) [])

let heartbeat =
  post "/workers/:id/state"
       (handler_wrapper heartbeat_handler ["id"])

let publish s =
  post "/workers/:id/objects"
       (handler_wrapper (publish_handler s) ["id"])

let spawn s =
  post "/workers/:id/newjobs"
       (handler_wrapper (spawn_handler s) ["id"])

let github_hook s =
  post "/github/:pr_num"
       (handler_wrapper (github_hook_handler s) ["pr_num"])

let package_demand s =
  post "/package/:pkg" (fun req ->
    let uri = Request.uri req in
    let query' = Uri.query uri in
    let query = List.map (fun (k, vs) -> k, String.concat ";" vs) query' in
    let pkg = param req "pkg" in
    let params = ("pkg", pkg) :: query in
    let headers = Request.headers req in
    let body = req.Request.body in
    user_pkg_demand_handler s params headers body >>= fun r ->
    Response.of_response_body r
    |> Lwt.return)

let job_query s =
  get "/object/:jid"
      (handler_wrapper (user_job_query_handler s) ["jid"])

let worker_query =
  get "/workers/statuses"
      (handler_wrapper user_worker_query_handler [])

let object_info_query t =
  get "/object/:jid/info"
      (handler_wrapper (user_object_query_handler t) ["jid"])

let server s =
  App.empty
  |> register s
  |> heartbeat
  |> publish s
  |> spawn s
  |> package_demand s
  |> github_hook s
  |> job_query s
  |> worker_query
  |> object_info_query s

let run ~uri ~ip:_ip ~port =
  (* FIXME: ip is not used! *)
  Store.create ~uri () >>= fun s ->
  Scheduler.bootstrap s >>= fun () ->
  let rec t_monitor () =
    Monitor.worker_monitor s >>= fun workers ->
    debug "monitor: a worker died!";
    List.iter (fun (_, t) -> Scheduler.invalidate_token t) workers;
    t_monitor ()
  in
  Lwt.join [App.start (server s |> App.port port); t_monitor ()]
