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

(** Store API *)

type t
(** The type for store handlers. *)

val create: ?uri:string -> unit -> t Lwt.t
(** Create a store handler, using Irmin's HTTP client. [uri] is the
    adress of the Irmin daemon. *)

module type S = sig

  (** The general signature to add and find values in the store. *)

  type id
  (** Type type for stable identifier of values kept in the store. *)

  type value
  (** The type for values kept in the store. *)

  val add: t -> value -> unit Lwt.t
  (** [add t v] adds [v] to the store [t]. *)

  val mem: t -> id -> bool Lwt.t
  (** [mem t id] is true if a value with the stable identifer [id] is
      stored in [t]. *)

  val find: t -> id -> value option Lwt.t
  (** [find t id] is the value stored in [t] with the stable
      identifier [id] . *)

end

module Task: sig

  (** {1 Persisting Task State} *)

  include S with type id := Task.id and type value := Task.t

  val status: t -> Task.id -> Task.status Lwt.t
  (** [status t task] is [task]'s status in [t]. *)

  val jobs: t -> Task.id -> Job.id list Lwt.t
  (** [jobs t task] are [task]'s jobs in [t]. *)

end

module Job: sig

  (** {1 Persisting Job State} *)

  include S with type id := Job.id and type value := Job.t

  val status: t -> Job.id -> Job.status Lwt.t
  (** [status t job] is [job]'s status in [t]. *)

  val running: t -> Job.id -> unit Lwt.t
  (** [runnning t id] sets [id]'s status to [`Running]. *)

  val success: t -> Job.id -> unit Lwt.t
  (** [success t id] sets [id]'s status to [`Success]. *)

  val failure: t -> Job.id -> string -> unit Lwt.t
  (** [failure t id msg] set [id]'s status to [`Failure msg]. *)

  val add_output: t -> Job.id -> Object.id -> unit Lwt.t
  (** [add_output t j o] adds [o] to the list of objects created by
      the job [j]. *)

  val outputs: t -> Job.id -> Object.id list Lwt.t
  (** [outputs t job] are [job]'s output objects. *)

end

module Object: S with type id := Object.id and type value := Object.t
