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

let check_round = 25.0

let w_tbl : worker_tbl = Hashtbl.create 16
let c_tbl : checkin_tbl = Hashtbl.create 16
let worker_cnt = ref 0
let w_map = ref LogMap.empty


let hash_sha1 str =
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


let new_token info =
  let time = string_of_float (Sys.time ()) in
  hash_sha1 (info ^ time)


let worker_checkin id =
  let times = Hashtbl.find c_tbl id in
  Hashtbl.replace c_tbl id (succ times)


let new_worker c h =
  let id = incr worker_cnt; !worker_cnt in
  let info = (string_of_int id) ^ c ^ h in
  let token = new_token info in
  w_map := LogMap.add token IdSet.empty !w_map;
  Hashtbl.replace w_tbl id (token, c, h);
  Hashtbl.replace c_tbl id (-1);
  worker_checkin id;
  id, token


let verify_worker id token =
  let token_record, _, _ = try Hashtbl.find w_tbl id
                           with Not_found -> "", "", "" in
  if token = token_record then worker_checkin id
  else failwith "fake worker"


let publish_object id token =
  let pkgs_set = LogMap.find token !w_map in
  let n_set = IdSet.add id pkgs_set in
  w_map := LogMap.add token n_set !w_map


let job_rank token deps =
  let pkgs_set = LogMap.find token !w_map in
  let deps_set = List.fold_left (fun set input ->
      IdSet.add input set) IdSet.empty deps in
  IdSet.cardinal (IdSet.inter pkgs_set deps_set)


let eliminate_workers workers =
  if workers = [] then return () else
    let eliminate_one (id, token) =
      Hashtbl.remove w_tbl id;
      Hashtbl.remove c_tbl id;
      w_map := LogMap.remove token !w_map;
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
