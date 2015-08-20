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

(** Build jobs.

    Jobs are for a specific compiler version and host kind. Jobs have
    pre-requisites: these are objects which needs to be built and be
    put into the worker context before the job could start.

    Completed jobs produce output object(s) which will be consummed by
    other jobs.
*)

type t
(** The type for job values. *)

type id = [`Job] Id.t with sexp
(** The type for job identifiers. *)

val create:
  ?inputs:id list ->
  ?repos:Task.repository list ->
  ?pins:Task.pin list ->
  string -> Host.t -> Package.t list -> t
(** [create c h pkgs] is the job of building the list of packages
    [pkgs] using the OCaml compiler [c] on a worker having [h] as host
    kind.

    The job will be able to access the outputs objects created by the
    (optional) [inputs] jobs.

    If [repo] is specified, the worker will use it to set-up its list
    of known opam repositories (and it will remove the default
    repository). If [pins] is specified, the worker will update its
    opam configuration to use these packages pins. *)

val to_string: t -> string
(** [to_string t] is the string representation of [t]. *)

val of_string: string -> t
(** [of_string s] is the value [t] such that [to_string t] is [s]. *)

val id: t -> id
(** [id t] id [t]'s deterministic identifier. It is obtained by hasing
    a stable representation of [t]'s components. *)

val compiler: t -> string
(** [compiler t] is [t]'s compiler. *)

val host: t -> Host.t
(** [host t] is [t]'s host. *)

val inputs: t -> Object.id list
(** [input t] are [t]'s inputs. *)

val repos: t -> Task.repository list
(** [repos t] are [t]'s repositories. *)

val pins: t -> Task.pin list
(** [pins t] are [t]'s pinned packages. *)

(** {Job Status} *)

type status = [
  | `Success
  | `Failure of string
  | `Pending
  | `Running
]
(** The type for job status. *)

val pp_status: Format.formatter -> status -> unit
(** [pretty_status s] is a pretty representation of [s]. FIXME: use fmt *)

val string_of_status: status -> string
(** [string_of_result r] is the string representation of [r]. *)

val status_of_string: string -> status
(** [status_of_string s] is the status [t] such that [string_of_status
    t] is [s]. *)

val task_status: status list -> Task.status
(** [task_status s] is the status summary of s. If all status are
    [`Success] then it is a [`Success]. If all status are [`Failed]
    then it is also [`Failed]. Otherwise it is [`Pending]. *)
