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

module type S = sig

  (** The signature for schedulers. *)

  type t
  (** The type for schedulers. *)

  val start: Store.t -> t Lwt.t
  (** [start s] starts the event scheduler. *)

end

module Task: sig

(** Task scheduler.

    The task scheduler watches task events and distribute solver jobs
    to the workers. *)

  include S

  val pick: t -> Task.t option
  (** [pick t] picks a task if it is available. *)

  val find: t -> Task.t Lwt.t
  (** [find t] blocks until a task is available. *)

end

module Job: sig
  (** Job scheduler.

      The job scheduler watches job events and distribute build jobs
      to the workers. *)

  include S

  val pick: t -> Host.t -> Job.id option
  (** [pick t host] picks a job if it is runnable by the given
      host. *)

  val find: t -> Host.t -> Job.id Lwt.t
  (** [find_job t host] blocks until a job become runnable by the
      given host. *)

end

module Worker: S
(** The worker scheduler watches worker events. *)

val start: Store.t -> unit Lwt.t
(** Start all the schedulers. *)
