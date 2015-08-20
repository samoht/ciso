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

(** Workers.

    The workers process build {{!Job}jobs} to produce build
    {{!Object}objects}. A worker has a fixed {{!Host}host}
    configuration: an architecture, an operating system and a
    distribution.

*)

type id = [`Worker] Id.t
(** The type for worker identifiers. *)

type t
(** The type for worker configration .*)

val create: Host.t -> t
(** [create h] is the worker with host configuration [h]. *)

val id: t -> id
(** [id t] is [t]'s identifier. It is a 128 bits universally unique
    identifiers (UUID) version 4 (random based) according to
    {{:http://tools.ietf.org/html/rfc4122}RFC 4122}. *)

val host: t -> Host.t
(** [host t] is [t]'s host configuration. *)

val to_string: t -> string
(** [to_string t] is the string representation of [t]. *)

val of_string: string -> t
(** [of_string s] is the worker [t] such that [to_string t] is [s]. *)

(** {1 Worker Status} *)

type status = [
  | `Idle
  | `Job of Job.id
  | `Task of Task.id
]
(** The worker status. Can either be idle, or processing a build job,
    or converting a task into a sequence of jobs. *)

val string_of_status: status -> string
(** [string_of_status t] is the string representation of [t]. *)

val status_of_string: string -> status
(** [status_of_string s] is the status [t] such that [string_of_status
    t] is [s]. *)
