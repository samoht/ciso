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

(** Monitored workers. *)

type status = Idle | Working of Job.id
(** The type for worker status. It is either [Idle] or working on a
    given job. *)

type t = [`Worker] Id.t
(** The type for monitored worker. Values of that type are not
    persistent and should be regenarated when the master restarts. *)

val create: Host.t -> t
(** [create h] is a monitored worker, using the host kind [h]. *)

val rank: t -> Object.id list -> int
(** [rank t ids] compute the the number of objects in [js] already
    built by the given worker. *)

val start: t -> Job.id -> unit
(** [start t j] registers that the worker [t] starts to work on
    [j]. *)

val complete: t -> unit
(** [complete t] registers that the worker [t] has completed its
    job. *)

val publish: t -> Object.id -> unit
(** [publish t o] registers that the worker [t] has published the
    object [o]. This will call {!complete}. *)

val host: t -> Host.t
(** [host t] is [t]'s host kind. *)

val available_hosts: unit -> Host.t list
(** [available_hosts ()] is the list of hosts where a worker is
    available. *)

val status: unit -> (t * status) list
(** [status ()] is the status of the monitored workers. *)

(*


val info_of_status: worker_status -> string * string option

val worker_hosts: unit -> Host.t list

val worker_compiler: Store.token -> string option

val compilers: unit -> string list

val worker_monitor: Store.t -> (worker_id * Store.token) list Lwt.t

*)
