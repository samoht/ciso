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

open Common_types

(* object id -> task
   if task A is supposed to produce object a,
   then there is the binding a.id -> A *)
type job_tbl

(* object id -> object id
   if task A has dependencies objects b, c, d,
   and task A is supposed to produce object a,
   then there will be bindins b.id -> a.id, c.id -> a.id, d.id -> a.id,
   whenever b/c/d is published in the obj_tbl,
   a hook function will examine if task A becomes runnable *)
type hook_tbl

(* task state *)
type state = [`Pending | `Runnable | `Completed | `Dispatched of Store.token]

(* object id -> task state
   if task A is supposed to produce object a,
   then there is binding a.id -> A.state *)
type state_tbl

(******************************************************************************)

(*  finds a suitable task based on given worker token, if there is one,
    return the task id and description *)
val find_job: Store.token -> (id * string * description) option

(* given an object id and a worker token, add them in the logmap *)
val publish_object:
  Store.t -> Store.token -> [`Success | `Fail of string | `Delegate of id] ->
  id -> unit Lwt.t

(* given a task id and return the pacakge name and version information *)
val task_info: ?abbr:bool -> id -> string

(* pickup any uncompleted tasks due to master failure *)
val bootstrap: Store.t -> unit Lwt.t

(* eliminate a worker's token when worker is down*)
val invalidate_token: Store.token -> unit

(* add new jobs into jobs/tables*)
val update_tables: Store.t -> (id * Job.t * id list) list -> unit Lwt.t

(* get related jobs of an id and the corresponding state *)
val progress_info: Store.t -> id -> string Lwt.t
(******************************************************************************)

(* given the pull request number from ocaml/opam-repository, resolve the task
   and add tasks into task table *)
(* val github_hook : Store.t -> int -> unit Lwt.t *)
