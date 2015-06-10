open Lwt

(* id -> the task who is supposed to produce the object *)
type task_tbl = (string, Task.t) Hashtbl.t

(* id -> object
   when the object isn't produced, there will be no binding in the table,
   an object may have multiple copies,
   so there might be multiple bindings for one id *)
type obj_tbl = (string, Object.t) Hashtbl.t

(* id -> id
   if task A has dependencies objects b, c, d,
   and task A is supposed to produce object a,
   then there will be bindins b -> a, c -> a, d -> a, *)
type hook_tbl = (string, string) Hashtbl.t

type state = [`Pending | `Dispatched | `Runnable | `Completed]
type state_tbl = (string, state) Hashtbl.t

let t_tbl : task_tbl = Hashtbl.create 16
let o_tbl : obj_tbl  = Hashtbl.create 16
let h_tbl : hook_tbl = Hashtbl.create 16
let s_tbl : state_tbl = Hashtbl.create 16

let user = "ocaml"
let repo = "opam-repository"
let token = ref None

let task_info id =
  let task = Hashtbl.find t_tbl id in
  (String.sub id 0 5) ^ ":" ^ Task.info_of_t task

(* return a deterministic id, based on pakcage name, version, and dependencies
   could add os and architecture later *)
let hash_id pkg v inputs =
  let str = pkg ^ v ^ (String.concat ";" inputs) in
  let hash str =
    let hex_of_cs cs =
      let buf = Buffer.create 16 in
      Cstruct.hexdump_to_buffer buf cs;
      Buffer.contents buf in
    let stripe_nl_space s = Re.(
      let re = compile (alt [compl [notnl]; space]) in
      replace_string re ~by:"" s) in
    Cstruct.of_string str |> Nocrypto.Hash.SHA1.digest
    |> hex_of_cs |> stripe_nl_space in
  hash str

let oid_of_task t =
  Hashtbl.fold (fun obj_id task acc ->
    if task = t then obj_id else acc) t_tbl ""

let task_of_oid obj_id =
  try Hashtbl.find t_tbl obj_id with _ -> failwith "task not found"

let get_objects obj_id =
  Hashtbl.find_all o_tbl obj_id

let distance_of_ips ipx ipy =
  let int_of_ip ip = Re.(
    let decimals = split (compile (str ".")) ip in
    let (_, sum) =
      List.fold_right (fun d (deg, sum) ->
        (deg * 1000, sum + deg * (int_of_string d))) decimals (1, 0) in
    sum) in
  abs (int_of_ip ipx - int_of_ip ipy)

let find_task ip port =
  let runnables = Hashtbl.fold (fun oid state acc ->
      if state = `Runnable then oid :: acc else acc) s_tbl [] in
  let oid_inputs = List.rev_map (fun oid ->
      let task = task_of_oid oid in
      let inputs = Task.inputs_of_t task in
      oid, inputs) runnables in
  let distance_of_input input =
    let distances = List.rev_map (fun obj ->
        let obj_ip = Object.ip_of_t obj in
        distance_of_ips obj_ip ip)
        (Hashtbl.find_all o_tbl input) in
    List.fold_left (fun acc dis ->
        if dis < acc then dis else acc)
      (List.hd distances) (List.tl distances) in
  let distance_of_inputs inputs =
    let distances = List.rev_map distance_of_input inputs in
    List.fold_left (fun acc dis -> acc + dis) 0 distances in
  if oid_inputs = [] then ""
  else begin
      let (oid, inputs) = List.hd oid_inputs in
      let oid, _ = List.fold_left (fun (acc_oid, acc_dis) (oid, inputs) ->
          let dis = distance_of_inputs inputs in
          if dis < acc_dis then oid, dis
          else acc_oid, acc_dis)
        (oid, distance_of_inputs inputs) (List.tl oid_inputs) in
      Hashtbl.replace s_tbl oid `Dispatched;
      Printf.eprintf "\t[scheduler@find_task]: [%s] -> %s\n%!"
        (String.concat " " (List.rev_map task_info runnables)) (task_info oid);
      oid
    end

let publish_object_hook obj_id =
  if not (Hashtbl.mem h_tbl obj_id) then return ()
  else begin
      let oids = Hashtbl.find_all h_tbl obj_id in
      let tups = List.rev_map (fun oid -> oid, task_of_oid oid) oids in
      Lwt_list.iter_p (fun (oid, task) ->
        let state = Hashtbl.find s_tbl oid in
        let inputs = Task.inputs_of_t task in
        if state <> `Pending then return ()
        else if List.for_all (fun input -> Hashtbl.mem o_tbl input) inputs then
          return (Hashtbl.replace s_tbl oid `Runnable)
        else return ()) tups
    end

let publish_object obj_id obj =
  let publish () =
    Hashtbl.add o_tbl obj_id obj;
    Hashtbl.replace s_tbl obj_id `Completed;
    return () in
  publish ()
  >>= fun () -> publish_object_hook obj_id
  >>= fun () ->
    let runnables = Hashtbl.fold (fun oid state acc ->
        if state = `Runnable then oid :: acc else acc) s_tbl [] in
    let str = String.concat " " (List.rev_map task_info runnables) in
    return (Printf.eprintf "\t[scheduler@publish]: %s -> [%s]\n%!"
        (task_info obj_id) str)

let init_gh_token name =
  Github_cookie_jar.init ()
  >>= fun jar -> Github_cookie_jar.get jar name
  >>= function
    | Some auth -> return (Github.Token.of_auth auth)
    | None -> fail (failwith "None auth")

(* /packages/<pkg>/<pkg.version>/{opam, url, descr, files/.., etc} *)
let packages_of_pull token num = Github.Monad.(
  Github.Pull.files ~token ~user ~repo ~num ()
  |> Github.Stream.to_list
  >>= fun files ->
    List.fold_left (fun acc file ->
        let parts = Array.of_list
          (Str.split (Str.regexp "/") file.Github_t.file_filename) in
        let pkg = try
            if parts.(0) = "packages" && parts.(3) <> "descr"
            then parts.(2) else ""
          with _ -> "" in
        if pkg <> "" && not (List.mem pkg acc) then pkg :: acc else acc)
      [] files
    |> return)

let pull_info token num = Github.Monad.(
  let open Github_t in
  Github.Pull.get ~token ~user ~repo ~num ()
  >>= fun pull_resp ->
    let pull = Github.Response.value pull_resp in
    let base = pull.pull_base and head = pull.pull_head in
    let base_repo =
      match base.branch_repo with
      | Some repo -> repo | None -> failwith "pr_info" in
    Task.make_pull
      num base_repo.repository_clone_url base.branch_sha head.branch_sha
    |> return)

let resolve_and_add ?pull pkg =
  let action_graph = Ci_opam.resolve pkg in
  let new_task ?pull pkg v inputs =
    let id = hash_id pkg v inputs in
    if Hashtbl.mem t_tbl id then id else
      let task = Task.make_task ?pull id pkg v inputs in
      Hashtbl.add t_tbl id task;
      List.iter (fun input -> Hashtbl.add h_tbl input id) inputs;
      Hashtbl.add s_tbl id
        (if inputs = [] then `Runnable else `Pending);
      id in
  Ci_opam.add_task ?pull new_task action_graph;
  Printf.eprintf "\t[scheduler@resolve]: %d tasks\n%!" (Hashtbl.length t_tbl)

let github_hook num =
  (match !token with
    | Some t -> return t
    | None -> begin
        init_gh_token "scry"
        >>= fun t ->
          token := Some t;
          return t
      end)
  >>= fun token -> Github.Monad.(
    (packages_of_pull token num
    >>= fun pkgs -> pull_info token num
    >>= fun pull ->
        List.iter (resolve_and_add ~pull) pkgs;
        return ())
    |> run)

let user_demand_handler pkg =
  resolve_and_add pkg;
  return ()
