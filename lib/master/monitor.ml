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

type t = [`Worker] Id.t

type id = t

let id x = x

module IdSet = struct
  include Set.Make(struct
      type t = Job.id
      let compare = Id.compare
    end)
  let of_list l = List.fold_left (fun set e -> add e set) empty l
end

module TMap = Map.Make(struct
  type t = [`Worker] Id.t
  let compare = Id.compare
end)

type status = Idle | Working of Job.id

(* id -> (host, optional compiler) *)
type worker_tbl = (t, Host.t * string option) Hashtbl.t

(* id -> check in times *)
type checkin_tbl = (t, int) Hashtbl.t

(* id -> worker status *)
type status_tbl = (t, status) Hashtbl.t

(* map from worker token to jobs completed by that worker *)
(* FIXME: why? *)
let t_map = ref TMap.empty

let check_round = 120.0
let default_compilers = ["4.00.1"]

let w_tbl : worker_tbl = Hashtbl.create 16
let c_tbl : checkin_tbl = Hashtbl.create 16
let s_tbl : status_tbl = Hashtbl.create 16

let compilers = ref []

let tick id =
  let times = Hashtbl.find c_tbl id in
  Hashtbl.replace c_tbl id (succ times)

let create h =
  let t = Id.of_uuid `Worker in
  t_map := TMap.add t IdSet.empty !t_map;
  Hashtbl.replace s_tbl t Idle;
  Hashtbl.replace w_tbl t (h, None);
  Hashtbl.replace c_tbl t (-1);
  tick t;
  t

let err fmt = Printf.ksprintf failwith fmt
let err_invalid_worker id = err "invalid worker: %s" (Id.to_string id)

let update_worker_env t compiler =
  Hashtbl.iter (fun id (h, _) ->
      let new_env = h, Some compiler in
      Hashtbl.replace w_tbl id new_env
    ) w_tbl

let new_job t ~compiler id =
  let status = Hashtbl.find s_tbl t in
  assert (status = Idle);
  Hashtbl.replace s_tbl t (Working id);
  update_worker_env t compiler

let err_worker_is_idle id =
  err "worker %s has no work in progress!" (Id.to_string id)

let complete t =
  match Hashtbl.find s_tbl t with
  | Idle      -> err_worker_is_idle t
  | Working i -> Hashtbl.replace s_tbl t Idle

let publish t id =
  let pkgs_set = TMap.find t !t_map in
  let n_set = IdSet.add id pkgs_set in
  t_map := TMap.add t n_set !t_map;
  complete t

let worker_statuses () =
  Hashtbl.fold (fun t (_, _) acc ->
      let status = Hashtbl.find s_tbl t in
      (t, status) :: acc
    ) w_tbl []

let object_rank token deps =
  let pkgs_set = TMap.find token !t_map in
  let deps_set = IdSet.of_list deps in
  IdSet.cardinal (IdSet.inter pkgs_set deps_set)

let worker_hosts () =
  Hashtbl.fold (fun _ (h, _) acc ->
      if List.mem h acc then acc
      else h :: acc
    ) w_tbl []

let worker_env token =
  Hashtbl.fold (fun _ (h, c_opt) acc -> (h, c_opt) :: acc) w_tbl []
  |> (fun lst -> assert (1 = List.length lst); List.hd lst)

let compiler t = snd (worker_env t)
let host t = fst (worker_env t)

let compilers () =
  if !compilers = [] then compilers := default_compilers;
  !compilers

let eliminate_workers workers =
  if workers <> [] then (
    let eliminate_one t =
      Hashtbl.remove w_tbl t;
      Hashtbl.remove c_tbl t;
      t_map := TMap.remove t !t_map;
      Hashtbl.remove s_tbl t;
    in
    List.iter eliminate_one workers;
    Hashtbl.fold (fun t _ acc -> t :: acc) c_tbl []
    |> List.rev_map (fun t -> "worker" ^ Id.to_string t)
    |> String.concat " "
    |> Printf.printf "\t[Alive workers]: %s\n%!"
  )

let worker_monitor store =
  let count () =
    Hashtbl.fold (fun id times acc -> (id, times) :: acc) c_tbl []
  in
  let rec loop last =
    Lwt_unix.sleep check_round >>= fun () ->
    let now = count () in
    let down_workers =
      List.fold_left (fun acc (id, now_times) ->
          let last_times =
            try List.assoc id last
            with Not_found -> pred now_times in
          if last_times = now_times then id :: acc else acc)
        [] now in
    if down_workers = [] then
      loop now
    else (
      eliminate_workers down_workers;
      Lwt.return down_workers
    )
  in
  loop (count ())
