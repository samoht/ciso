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

    Jobs are for a given {{!Compiler}compiler} version and
    {{!Host}host} configurations. Jobs have pre-requisites: these are
    {{!Object}objects} which needs to be built and be put into the
    {{!Worker}worker} context before the job could start.

    Completed jobs produce output {{!Object}object(s)} which will be
    consummed by other jobs.
*)

type id = [`Job] Id.t with sexp
(** The type for job identifiers. Job identifiers are deterministic,
    i.e. similar jobs will have the same identifiers. As for
    {{!Task.id}tasks}, the identifier is built by calling {!Id.digest}
    on the concatenation of {!create} arguments (after
    normalisation). *)

type t
(** The type for job values. *)

val create:
  ?inputs:id list ->
  Host.t -> Compiler.t -> (Package.t * Package.info) list -> t
(** [create h c pkgs] is the job of building the list of packages
    [pkgs] using the OCaml compiler [c] on a worker having [h] as host
    configuration.

    The job will be able to access the outputs objects created by the
    (optional) [inputs] jobs. *)

val to_string: t -> string
(** [to_string t] is the string representation of [t]. *)

val of_string: string -> t
(** [of_string s] is the value [t] such that [to_string t] is [s]. *)

val id: t -> id
(** [id t] id [t]'s deterministic identifier. It is obtained by hasing
    a stable representation of [t]'s components. *)

val compiler: t -> Compiler.t
(** [compiler t] is [t]'s compiler. *)

val host: t -> Host.t
(** [host t] is [t]'s host. *)

val inputs: t -> id list
(** [input t] are [t]'s job inputs. *)

val packages: t -> (Package.t * Package.info) list
(** [packages t] are the packages that [t] has to build. *)

(** {Job Status} *)

type status = [
  | `Success
  | `Failure of string
  | `Pending
  | `Running
  | `Cancelled
]
(** The type for job status. *)

val pp_status: status Fmt.t
(** [pp_status] formats jobs {!status}. *)

val string_of_status: status -> string
(** [string_of_result r] is the string representation of [r]. *)

val status_of_string: string -> status
(** [status_of_string s] is the status [t] such that [string_of_status
    t] is [s]. *)

val task_status: status list -> Task.status
(** [task_status s] is the status summary of s. If all status are
    [`Success] then it is a [`Success]. If all status are [`Failed]
    then it is also [`Failed]. Otherwise it is [`Pending]. *)
