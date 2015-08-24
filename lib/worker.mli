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

val equal: t -> t -> bool
(** [equal] is the equality for workers. *)

val compare: t -> t -> int
(** [compare] compares workers. *)

val pp: t Fmt.t
(** [pp] formats workers. *)

val json: t Jsont.codec
(** [json] is the JSON coded for workers. *)

(** {1 Worker Status} *)

type status = [
  | `Idle
  | `Job of Job.id
  | `Task of Task.id
]
(** The worker status. Can either be idle, or processing a build job,
    or converting a task into a sequence of jobs. *)

val pp_status: status Fmt.t
(** [pp_status] formats worker status. *)

val json_status: status Jsont.codec
(** [json_status] is the JSON codec for worker status. *)
