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
open Common_types

let debug fmt = Gol.debug ~section:"scheduler" fmt

type job_tbl = (id, Job.t) Hashtbl.t
type deps_tbl = (id, id list) Hashtbl.t

type hook_tbl = (id, id) Hashtbl.t

type state = [`Pending | `Runnable | `Completed | `Dispatched of worker_token]
type state_tbl = (id, state) Hashtbl.t

type fail_tbl = (id, int) Hashtbl.t

let j_tbl : job_tbl = Hashtbl.create 16
let d_tbl : deps_tbl = Hashtbl.create 16
let h_tbl : hook_tbl = Hashtbl.create 16
let s_tbl : state_tbl = Hashtbl.create 16

let fail_limit = 2
let f_tbl : fail_tbl = Hashtbl.create 16

let sub_abbr str = String.sub str 0 5

let task_info ?(abbr = true) id =
  try
    let job = Hashtbl.find j_tbl id in
    let info = Job.task job |> Task.info_of_task in
    (if abbr then String.sub id 0 5 else id ) ^ ":" ^ info
  with
  | Not_found ->
    let ids =
      Hashtbl.fold (fun id _ acc -> id :: acc) j_tbl [] |> String.concat "\n"
    in
    Printf.sprintf "Object %s not in the ids: [\n%s]" id ids
  | e -> raise e

let rec get_runnables ?host ?compiler () =
  let env_match (c, h) = function
    | None, None -> true
    | Some h', None -> h' = h
    | None, Some c' -> c' = c
    | Some h', Some c' -> h' = h && c' = c
  in
  Hashtbl.fold (fun id j acc ->
      if `Runnable = Hashtbl.find s_tbl id
         && env_match (Job.env j) (host, compiler)
      then id :: acc
      else acc) j_tbl []
  |> fun lst ->
  if compiler <> None && lst = [] then get_runnables ?host ()
  else lst

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

let find_job wtoken =
  let host, compiler = Monitor.worker_env wtoken in
  let runnables = get_runnables ~host ?compiler () in
  if runnables = [] then None
  else (
    let id, _ = List.fold_left (fun (i, max_r) tid ->
        let deps = Hashtbl.find d_tbl tid in
        let r = Monitor.job_rank wtoken deps in
        if r > max_r then tid, r else i, max_r
      ) ("", (-1)) runnables
    in
    Hashtbl.replace s_tbl id (`Dispatched wtoken);
    debug "find_job:  -> %s" (task_info id);
    let job = Hashtbl.find j_tbl id in
    let deps = Hashtbl.find d_tbl id in
    let desp = Job.create_entry job deps |> Job.string_of_entry in
    let c, _ = Job.env job in
    Some (id, c, desp)
  )

let err_not_found id = Printf.ksprintf failwith "%s: not found" (sub_abbr id)

let publish_object_hook s id =
  if not (Hashtbl.mem h_tbl id) then Lwt.return_unit
  else (
    let ids = Hashtbl.find_all h_tbl id in
    let tups =
      List.rev_map (fun i ->
          i,
          try Hashtbl.find j_tbl i with Not_found ->
            debug "publish_hook: Not_found J_TBL %s" (sub_abbr i);
            err_not_found i
        ) ids
    in
    Lwt_list.fold_left_s (fun cache (i, job) ->
        let state =
          try Hashtbl.find s_tbl i with Not_found ->
            debug "publish_hook: Not_found S_TBL %s" (sub_abbr i);
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
  let info_lst = List.rev_map (task_info ~abbr:true) (get_runnables ()) in
  debug "publish: {%s}" (String.concat " ; " info_lst)

let update_tables s jobs =
  Lwt_list.filter_p (fun (id, _, _) ->
      Store.query_object s id >>= fun in_store ->
      Lwt.return (not (in_store || Hashtbl.mem j_tbl id))) jobs
  >>=fun new_jobs ->
  (* cache contains the results of Store.query_object, it can answer
     whether a previously queried dep is in the data store or not *)
  Lwt_list.fold_left_s (fun cache (id, j, deps) ->
      Store.log_job s id (j, deps) >>= fun () ->
      Hashtbl.replace j_tbl id j;
      Hashtbl.replace d_tbl id deps;
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
