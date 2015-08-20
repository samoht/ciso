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

(** Schedule jobs. *)

val start: Store.t -> unit Lwt.t
(** [start s] starts the in-memory scheduler by reading incomple jobs
    (due to previous master failure) from the store [s]. *)

val find_job: Monitor.t -> (Job.t * Object.id list) option
(** [find_job m] finds a suitable job for the woker [m], based on its
    host kind. Return the job and the transitive closure of objects
    that it needs. *)

(* FIXME: doc and API *)


(* given a task id and return the pacakge name and version information *)
val job_info: ?abbr:bool -> Job.id -> string

(* given an object id and a worker token, add them in the logmap *)
val publish_object:
  Store.t -> [`Success | `Fail of string | `Delegate of Job.id] -> Job.id ->
  unit Lwt.t

(* add new jobs into jobs/tables*)
(* FIXME: the object list is the transitive closure of objects. *)
val update_tables: Store.t -> (Job.id * Job.t * Object.id list) list -> unit Lwt.t

(* get related jobs of an id and the corresponding state *)
val progress_info: Store.t -> Job.id -> string Lwt.t

(*



(* eliminate a worker's token when worker is down*)
val invalidate_token: Store.token -> unit


(******************************************************************************)

(* given the pull request number from ocaml/opam-repository, resolve the task
   and add tasks into task table *)
(* val github_hook : Store.t -> int -> unit Lwt.t *)

*)
