open Lwt
open Common_types

module Response = Cohttp_lwt_unix.Response
module Body = Cohttp_lwt_body
module Code = Cohttp.Code

type worker_id = int
type worker_tbl = (worker_id, worker_token) Hashtbl.t  (* id -> token *)
type checkin_tbl = (worker_id, int) Hashtbl.t (* id -> check in times *)
exception WrongMessage of string

let w_tbl : worker_tbl = Hashtbl.create 16
let c_tbl : checkin_tbl = Hashtbl.create 16
let check_round = 7.0
let worker_cnt = ref 0

let log handler worker_id info =
  let title = Printf.sprintf "worker%d@%s" worker_id handler in
  Printf.eprintf "[%s]: %s\n%!" title info

let get_sha1 str =
  let hex_of_cs cs =
    let buf = Buffer.create 16 in
    Cstruct.hexdump_to_buffer buf cs;
    Buffer.contents buf in
  let stripe_nl_space s = Re.(
    let re = compile (alt [compl [notnl]; space]) in
    replace_string re ~by:"" s) in
  str |> Cstruct.of_string |>
  Nocrypto.Hash.SHA1.digest |>
  hex_of_cs |> stripe_nl_space

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

let new_token id =
  let id = string_of_int id in
  let time = string_of_float (Sys.time ()) in
  get_sha1 (id ^ time)


let worker_checkin id =
  let times = Hashtbl.find c_tbl id in
  Hashtbl.replace c_tbl id (succ times)

let eliminate_workers ids =
  if ids = [] then return () else
    let eliminate_one id =
      let token = Hashtbl.find w_tbl id in
      Hashtbl.remove w_tbl id;
      Hashtbl.remove c_tbl id;
      Scheduler.invalidate_token token;
      Store.invalidate_token token in
    Lwt_list.iter_p eliminate_one ids >>= fun () ->
    Hashtbl.fold (fun id _ acc -> id :: acc) c_tbl []
    |> List.rev_map (fun id -> "worker" ^ (string_of_int id))
    |> String.concat " "
    |> Lwt_io.printf "\t[Alive workers]: %s\n%!"

let worker_monitor () =
  let count () =
    Hashtbl.fold (fun id times acc -> (id, times) :: acc) c_tbl [] in
  let rec loop t =
    t >>= fun last ->
    let now = count () in
    let down =
      List.fold_left (fun acc (id, times) ->
          let last_times = try List.assoc id last
                           with Not_found -> pred times in
          if last_times = times then id :: acc else acc)
        [] now in
    eliminate_workers down >>= fun () ->
    Lwt_unix.sleep check_round >>= fun () ->
    loop (return now) in
  loop (return (count ()))

let register_handler groups headers body =
  let id = incr worker_cnt; !worker_cnt in
  let token = new_token id in
  Hashtbl.replace w_tbl id token;
  Hashtbl.replace c_tbl id (-1);
  worker_checkin id;
  Scheduler.register_token token;
  Store.register_token token >>= fun () ->

  let m = Message.Ack_register (id, token) in
  let resp = Response.make ~status:Code.(`Created) () in
  let body = body_of_message m in
  return (resp, body)


let heartbeat_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let token = match Cohttp.Header.get headers "worker" with
    | Some t -> t | None -> "" in
  if token <> Hashtbl.find w_tbl id then failwith "fake worker"
  else worker_checkin id;

  message_of_body body >>= fun m ->
  let resp_m = Message.(match m with
      | Heartbeat None ->
         log "heartbeat" id "idle";
         (match Scheduler.find_job token with
          | None -> Ack_heartbeat
          | Some (jid, tdesp) -> New_job (jid, tdesp))
      | Heartbeat (Some jid) ->
         let info = Scheduler.task_info jid in
         log "heartbeat" id ("working job " ^ info);
         Message.Ack_heartbeat
      | Register | Publish _ -> failwith "wrong message for heartbeat") in

  let resp = Response.make ~status:Code.(`OK) () in
  let body = body_of_message resp_m in
  return (resp, body)


let publish_handler groups headers body =
  let id = int_of_string (groups.(1)) in
  let token = match Cohttp.Header.get headers "worker" with
    | Some t -> t | None -> "" in
  if token <> Hashtbl.find w_tbl id then failwith "fake worker";

  message_of_body body >>= fun m ->
  let jid = Message.(match m with
      | Publish id -> id
      | Register | Heartbeat _ -> failwith "wrong message for publish") in
    log "publish" id ("object " ^ (Scheduler.task_info jid));
    Scheduler.publish_object token jid >>= fun () ->

    empty_response Code.(`Created)

let github_hook_handler groups headers body =
  let pr_num = int_of_string (groups.(1)) in
  Scheduler.github_hook pr_num
  >>= fun () ->
  empty_response Code.(`Accepted)


let user_demand_handler groups headers body =
  let pkg = groups.(1) in
  Scheduler.user_demand pkg
  >>= fun () ->
  empty_response Code.(`Accepted)


let handler_route_table = Re.(
  let post = Code.(`POST) in
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
    seq [str "/github/"; group (rep1 digit); eos]),
    github_hook_handler;
   (post,
    seq [str "/package/"; group (rep1 any); eos]),
    user_demand_handler])


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

  if List.length handler <> 1 then empty_response Code.(`Not_found)
  else
    let handler = List.hd handler in
    let err_handler exn =
      let meth = Cohttp.Code.string_of_method meth in
      let err_m = match exn with
        | Failure str -> Printf.sprintf "Error: %s %s -> %s \n%!" meth path str
        | _ -> Printf.sprintf "Error: %s %s -> unknown \n%!" meth path in
      prerr_endline err_m;
      empty_response Code.(`No_content) in
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
    join [t_server; worker_monitor ()])
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
