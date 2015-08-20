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

let debug fmt = Gol.debug ~section:"scheduler" fmt

type state = [`Pending | `Runnable | `Completed | `Dispatched of Monitor.t ]

type job_tbl = (Job.id, Job.t) Hashtbl.t
type state_tbl = (Job.id, state) Hashtbl.t
type fail_tbl = (Job.id, int) Hashtbl.t

(* [deps_tbl] contains the transitive closure of objects needed by all
   the jobs *)
type deps_tbl = (Job.id, Object.id list) Hashtbl.t

let j_tbl: job_tbl = Hashtbl.create 16
let d_tbl: deps_tbl = Hashtbl.create 16
let s_tbl: state_tbl = Hashtbl.create 16
let f_tbl: fail_tbl = Hashtbl.create 16

let fail_limit = 2

let job_info id =
  try
    let job = Hashtbl.find j_tbl id in
    let info = Job.task job |> Task.info_of_task in
    Id.to_string id ^ ":" ^ info
  with
  | Not_found ->
    let ids =
      Hashtbl.fold (fun id _ acc -> Id.to_string id :: acc) j_tbl []
      |> String.concat "\n"
    in
    Printf.sprintf "Object %s not in the ids: [\n%s]" (Id.pretty id) ids
  | e -> raise e

let get_runnables ?host () =
  let env_match h = match host with
    | None    -> true
    | Some h' -> h' = h
  in
  Hashtbl.fold (fun id j acc ->
      if `Runnable = Hashtbl.find s_tbl id && env_match (Job.host j)
      then id :: acc
      else acc
    ) j_tbl []

let count_runnables () =
  let r = get_runnables () in
  List.length r, Hashtbl.length j_tbl

let invalidate_token wtoken =
  Hashtbl.iter (fun id s ->
      if s = (`Dispatched wtoken)
      then Hashtbl.replace s_tbl id `Runnable
    ) s_tbl;
  let r, sum = count_runnables () in
  debug "invalidate %d/%d jobs" r sum

let find_job m =
  let host = Monitor.host m in
  let runnables = get_runnables ~host () in
  match runnables with
  | [] -> None
  | i :: runnables ->
    let rank i = Monitor.job_rank m (Hashtbl.find d_tbl i) in
    let id, _ = List.fold_left (fun (max_i, max_r) i ->
        let r = rank i in
        if r > max_r then (i, r) else (max_i, max_r)
      ) (i, rank i) runnables
    in
    Hashtbl.replace s_tbl id (`Dispatched wtoken);
    debug "find_job:  -> %s" (task_info id);
    let job = Hashtbl.find j_tbl id in
    let deps = Hashtbl.find d_tbl id |> List.map Job.object_id in
    Some (id, deps)

let err_not_found id = Printf.ksprintf failwith "%s: not found" (Id.pretty id)

let publish_object_hook s id =
  if not (Hashtbl.mem h_tbl id) then Lwt.return_unit
  else (
    let ids = Hashtbl.find_all h_tbl id in
    let tups =
      List.rev_map (fun i ->
          i,
          try Hashtbl.find j_tbl i with Not_found ->
            debug "publish_hook: Not_found J_TBL %s" (Id.pretty i);
            err_not_found i
        ) ids
    in
    Lwt_list.fold_left_s (fun cache (i, job) ->
        let state =
          try Hashtbl.find s_tbl i with Not_found ->
            debug "publish_hook: Not_found S_TBL %s" (Id.pretty i);
            err_not_found i
        in
        if state <> `Pending then Lwt.return cache
        else (
          let inputs = Job.inputs job in
          let cached, store =
            List.partition (fun input -> List.mem_assoc input cache) inputs
          in
          if List.exists (fun input -> false = List.assoc input cache) cached
          then Lwt.return cache else
            Lwt_list.rev_map_s (fun input -> Store.query_object s input) store
            >>= fun store_results ->
            let store_tups = List.combine store (List.rev store_results) in
            Lwt_list.fold_left_s (fun acc tup ->
                Lwt.return (tup :: acc)) cache store_tups
            >|= fun new_cache ->
            if List.for_all (fun re -> re = true) store_results then
              Hashtbl.replace s_tbl i `Runnable;
            new_cache
        )) [] tups
    >>= fun _ -> Lwt.return_unit
  )

let publish_object s _wtoken result id =
  (* FIXME: wtoken is not used!! *)
  (match result with
   | `Delegate _ | `Success ->
      Hashtbl.replace s_tbl id `Completed;
      publish_object_hook s id
   | `Fail _ ->
      let cnt = try Hashtbl.find f_tbl id with Not_found -> 0 in
      let n_cnt = succ cnt in
      if n_cnt >= fail_limit then
        (debug "publish: Fail -> FAIL";
         Hashtbl.replace s_tbl id `Completed)
      else
        (debug "publish: Fail -> RETRY";
         Hashtbl.replace f_tbl id n_cnt;
         Hashtbl.replace s_tbl id `Runnable);
      Lwt.return_unit)
  >|= fun () ->
  debug "publish: publish hook completed";
  let info_lst = List.rev_map task_info (get_runnables ()) in
  debug "publish: {%s}" (String.concat " ; " info_lst)

let update_tables s jobs =
  Lwt_list.filter_p (fun (jid, _, _) ->
      let id = Job.object_id jid in
      Store.query_object s id >|= fun in_store ->
      not (in_store || Hashtbl.mem j_tbl jid)
    ) jobs
  >>= fun new_jobs ->
  (* cache contains the results of Store.query_object, it can answer
     whether a previously queried dep is in the data store or not *)
  Lwt_list.fold_left_s (fun cache (jid, j, deps) ->
      Store.log_job s jid (j, deps) >>= fun () ->
      Hashtbl.replace j_tbl jid j;
      Hashtbl.replace d_tbl jid deps;
      let _cached, to_lookup =
        List.partition (fun d -> List.mem_assoc d cache) deps
      in
      Lwt_list.rev_map_s (fun d -> Store.query_object s d) to_lookup
      >>= fun lookup_results ->
      let new_cache = List.combine to_lookup (List.rev lookup_results) in
      let cache = List.rev_append new_cache cache in
      let in_store d = List.assoc d cache in
      if List.for_all (fun d -> in_store d) deps then
        Hashtbl.replace s_tbl id `Runnable
      else (
        let hooks = List.filter (fun i -> not (in_store i)) (Job.inputs j) in
        List.iter (fun h -> Hashtbl.add h_tbl h id) hooks;
         Hashtbl.replace s_tbl id `Pending;
      );
      Lwt.return cache
    ) [] new_jobs
  >|= fun _ ->
  let run, sum = count_runnables () in
  debug "update: %d/%d jobs" run sum

let bootstrap s =
  debug "bootstrap: read unfinished jobs";
  Store.retrieve_jobs s >>=
  update_tables s >|= fun () ->
  let r, sum = count_runnables () in
  debug "bootstrap: %d/%d jobs" r sum

let state_of_id s id =
  if Hashtbl.mem s_tbl id then
    Lwt.return (Hashtbl.find s_tbl id)
  else
    Store.query_object s id >>= fun in_store ->
    if in_store then Lwt.return `Completed
    else err_not_found id

let progress_of_id s id =
  (if Hashtbl.mem d_tbl id then Lwt.return (Hashtbl.find d_tbl id)
   else Store.retrieve_job s id >>= fun (_, deps) -> Lwt.return deps)
  >>= fun deps ->
  Lwt_list.rev_map_s (fun d -> state_of_id s d >|= fun s -> d, s) deps
  >>= fun dep_states ->
  state_of_id s id >|= fun s ->
  (id, s) :: dep_states

let get_progress s id =
  state_of_id s id >>= fun state ->
  if state <> `Completed then progress_of_id s id
  else
    Store.retrieve_object s id >>= fun obj ->
    match Object.result obj with
    | `Delegate del ->
      progress_of_id s del >|= fun delegates ->
      let delegate_state = List.assoc del delegates in
      let state =
        if delegate_state <> `Completed then `Pending else `Completed
      in
      (id, state) :: delegates
    | `Success | `Fail _ -> progress_of_id s id

let string_of_state = function
  | `Completed -> "Completed"
  | `Pending -> "Pending"
  | `Runnable -> "Runnalbe"
  | `Dispatched _ -> "Dispatched"

let progress_info s id =
  get_progress s id >|= fun progress ->
  List.rev_map (fun (_id, s) ->
      let format =
        if _id = id then Printf.sprintf " -> %s %s"
        else Printf.sprintf "    %s %s"
      in
      format (task_info ~abbr:false _id) (string_of_state s)
    ) progress
  |> List.rev
  |> fun str_lst ->
  Printf.sprintf "%s\n" (String.concat "\n" str_lst)
