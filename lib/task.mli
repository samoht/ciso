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

(** User-defined tasks.

    A task is a high-level description of user intents. It allows to
    express things like:

    {i "I want to compiler the package X to all supported host
    configurations and all OCaml compiler versions."}

    Tasks are later translated into more specific {{!module:Job}jobs}
    by {{!module:Worker}workers}, using the OPAM solver. These jobs
    are then processed by other {{!module:Worker}workers} to generate
    build {{!module:Object}objects}. The user can then access the jobs
    outputs and results, and the genarated objects.
*)

type id = [`Task] Id.t
(** The type for task identifiers. These identifiers are
    deterministic, i.e. similar tasks will have the same
    identifiers. This is done by hashing the concatenation of
    {!create} arguments (after normalisation) and calling {!Id.digest}
    on the result. *)

type t
(** The type for task values. *)

type repo = string * Uri.t
(** The type for remote opam repositories. *)

val pp_repo: repo Fmt.t
(** [pp_repository] formats a repository. *)

type pin = string * Uri.t option
(** The type for pinned packages. The first argument is a package
    name, the second one its pin target. It is either a version
    string, or a Git repository. The target is similar to what would
    be passed to {i opam pin add <name> <target>}. If the target is
    not specified, it means {i --dev}. *)

val pp_pin: pin Fmt.t
(** [pp_pin] formats a pin package. *)

val id: t -> id
(** [id t] is [t]'s deterministic identifier. Is it obtaining by
    hashing a stable representation of [t]'s components. *)

val packages: t -> Package.t list
(** [packages t]'s is the list of packages that [t] wants to
    install. *)

val create:
  ?repos:repo list -> ?pins:pin list ->
  ?switches:Switch.t list -> ?hosts:Host.t list ->
  Package.t list -> t
(** [create pkgs] is the task of building the packages [pkgs] on all
    possible compiler switches and on all possible host
    configurations. This task can somehow be attenuated by specifying
    some optional arguments:

    {ul
    {- [repos] is the list of (remote) repositories the the workers
       should use.}
    {- [pins] is the list of pinned packages that the worker should
       use.}
    {- [switches] restricts the list of compiler switches to test to
       only the ones appearing in the list. An empty list means all
       the {{!Switch.defaults}supported} compiler switches.}
    {- [hosts] restricts the list of host configurations to test to only
       the ones appearing in the list. An empty list means all the
       {{!Host.defaults}supported} hosts.}
    }
*)

val equal: t -> t -> bool
(** [equal] is the task equality. *)

val compare: t -> t -> int
(** [compare] compares tasks. *)

val pp: t Fmt.t
(** [pp] formats tasks. *)

val json: t Jsont.codec
(** [json] is the JSON codec for tasks. *)

(** {1 Task Status} *)

type status = [
  | `New
  | `Pending
  | `Success
  | `Failure
  | `Cancelled
]
(** The type for task status. *)

val pp_status: status Fmt.t
(** [pp_status] formats tasks {!status}. *)

val json_status: status Jsont.codec
(** [json_status] is the JSON coded for task status. *)
