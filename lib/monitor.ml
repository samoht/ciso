open Common_types
open Lwt

(* set of task/object ids*)
module IdSet = Set.Make(struct
  type t = id
  let compare = String.compare
end)

(* map from worker token to tasks completed by that worker as IdSet.t *)
module LogMap = Map.Make(struct
  type t = worker_token
  let compare = String.compare
end)

(* id -> (token, compiler, host) *)
type worker_tbl = (worker_id, worker_token * compiler * host) Hashtbl.t

(* id -> check in times *)
type checkin_tbl = (worker_id, int) Hashtbl.t

(* id -> worker status *)
type worker_status = Idle | Working of id
type status_tbl = (worker_token, worker_status) Hashtbl.t


let check_round = 25.0
let worker_cnt = ref 0


let w_tbl : worker_tbl = Hashtbl.create 16
let c_tbl : checkin_tbl = Hashtbl.create 16
let s_tbl : status_tbl = Hashtbl.create 16
let w_map = ref LogMap.empty


let hash_token str =
  let `Hex h =
    str
    |> Cstruct.of_string
    |> Nocrypto.Hash.SHA1.digest
    |> Hex.of_cstruct in
  h


let new_token info =
  let time = string_of_float (Sys.time ()) in
  hash_token (info ^ time)


let worker_checkin id =
  let times = Hashtbl.find c_tbl id in
  Hashtbl.replace c_tbl id (succ times)


let new_worker c h =
  let id = incr worker_cnt; !worker_cnt in
  let info = (string_of_int id) ^ c ^ h in
  let token = new_token info in
  w_map := LogMap.add token IdSet.empty !w_map;
  Hashtbl.replace s_tbl token Idle;
  Hashtbl.replace w_tbl id (token, c, h);
  Hashtbl.replace c_tbl id (-1);
  worker_checkin id;
  id, token


let verify_worker id token =
  let token_record, _, _ = try Hashtbl.find w_tbl id
                           with Not_found -> "", "", "" in
  if token = token_record then worker_checkin id
  else failwith "fake worker"


let new_job id token =
  let status = Hashtbl.find s_tbl token in
  assert (status = Idle);
  Hashtbl.replace s_tbl token (Working id)


let job_completed id token =
  let status = Hashtbl.find s_tbl token in
  if status = Working id then Hashtbl.replace s_tbl token Idle


let publish_object id token =
  let pkgs_set = LogMap.find token !w_map in
  let n_set = IdSet.add id pkgs_set in
  w_map := LogMap.add token n_set !w_map;
  job_completed id token


let worker_statuses () =
  Hashtbl.fold (fun id (token, _, _) acc ->
      let status = Hashtbl.find s_tbl token in
      (id, token, status) :: acc) w_tbl []


let info_of_status = function
  | Idle -> "Idle", None
  | Working id -> "Working", Some id


let job_rank token deps =
  let pkgs_set = LogMap.find token !w_map in
  let deps_set = List.fold_left (fun set input ->
      IdSet.add input set) IdSet.empty deps in
  IdSet.cardinal (IdSet.inter pkgs_set deps_set)


let worker_environments () =
  Hashtbl.fold (fun _ (_, c, h) acc ->
      if List.mem (c, h) acc then acc
      else (c, h) :: acc) w_tbl []


let worker_env token =
  Hashtbl.fold (fun _ (t, c, h) acc ->
      if t = token then (c, h) :: acc
      else acc) w_tbl []
  |> (fun lst -> assert (1 = List.length lst); List.hd lst)


let eliminate_workers workers =
  if workers = [] then return () else
    let eliminate_one (id, token) =
      Hashtbl.remove w_tbl id;
      Hashtbl.remove c_tbl id;
      w_map := LogMap.remove token !w_map;
      Hashtbl.remove s_tbl token;
      Store.invalidate_token token in
    Lwt_list.iter_p eliminate_one workers >>= fun () ->

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
    let down_workers =
      List.fold_left (fun acc (id, now_times) ->
          let last_times =
            try List.assoc id last
            with Not_found -> pred now_times in
          if last_times = now_times then id :: acc else acc)
        [] now in
    if down_workers = [] then
      Lwt_unix.sleep check_round >>= fun () ->
      loop (return now)
    else
      let workers = List.rev_map (fun id ->
          let t, _, _ = Hashtbl.find w_tbl id in id, t) down_workers in
      eliminate_workers workers >>= fun () ->
      return workers in
  loop (return (count ()))
