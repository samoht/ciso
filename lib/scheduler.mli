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

(** Scheduler.

    The scheduler looks for task, job and worker events in the store
    and distribute work to the workers.

*)

(** The signature for schedulers. *)
module type S = sig

  type t
  (** The type for schedulers. *)

  type value
  (** The type of values which are scheduled. *)

  val start: Store.t -> t Lwt.t
  (** [start s] starts the event scheduler. *)

  val list: t -> value list
  (** [list t] lists the values which are being scheduled. *)

  val peek: t -> value option
  (** [peel t] picks a value if it is available. *)

  val peek_s: t -> value Lwt.t
  (** [peek_s t] blocks until a value is available. *)

end

(** Task scheduler.

    Tasks can only be added. When a new task is submitted by the
    users, the task scheduler start managing it. A task can later be
    cancelled. *)
module Task: S with type value := Task.t

(** Job scheduler. *)
module Job: sig
  (** Jobs can only be added. Jobs are added by workers resolving new
      tasks (which then become pending). The job scheduler manages new
      jobs and check which ones are runnable. It also manage user
      cancellation. *)

  include S with type value := Job.t

  val peek: t -> Host.t -> Job.t option
  (** [peek t host] picks a job if it is runnable on the given host
      configuration. *)

  val peek_s: t -> Host.t -> Job.t Lwt.t
  (** [peek_s t host] blocks until a job become runnable on the given
      host configuration. *)

end

(** Worker scheduler.

    Workers can be added and can become inactive. The worker scheduler
    manage new workers, keep track of idle workers and remove inactive
    workers. *)
module Worker: S with type value := Worker.t

val start: Store.t -> unit Lwt.t
(** Start all the schedulers. *)
