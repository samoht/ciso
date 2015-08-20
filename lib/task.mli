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

(** User-defined tasks. *)

type t
(** The type for task values. *)

type id = [`Task] Id.t
(** The type for task identifiers. *)

type repository = Repository of string * string with sexp
(** The type for remote opam repositories. *)

type pin = Pin of string * string with sexp
(** The type for pinned packages. The first argument is a package
    name, the second one its pin target. It is either a version
    string, or a Git repository. The target is similar to what would
    be passed to {i opam pin add <name> <target>} *)

val id: t -> id
(** [id t] is [t]'s deterministic identifier. Is it obtaining by
    hashing a stable representation of [t]'s components. *)

val packages: t -> Package.t list
(** [packages t]'s is the list of packages that [t] wants to
    install. *)

val create:
  ?repos:repository list ->
  ?pins:pin list ->
  ?compilers:Compiler.t list ->
  ?hosts:Host.t list ->
  Package.t list -> t
(** [create pkgs] is the task of building the packages [pkgs] on all
    possible compiler version and on all possible host kinds. This
    task can somehow be attenuated by specifying some optional
    arguments:

    {ul
    {- [repos] is the list of (remote) repositories the the workers
       should use.}
    {- [pins] is the list of pinned packages that the worker should
       use.}
    {- [compilers] restricts the list of compilers to test to only the
       ones appearing in the list. An empty list means all the
       supported compilers.}
    {- [hosts] restricts the list of host kinds to test to only the
       ones appearing in the list. An empty list means all the
       supported hosts.}
    }
*)

val to_string: t -> string
(** [to_string t] is the string representation of [t]. *)

val of_string: string -> t
(** [of_string s] is the value [t] such that [to_string t] is [s]. *)

(** {1 Task Status} *)

type status = [
  | `Success
  | `Failure
  | `Pending
]
(** The type for task status. *)

val pp_status: status -> string
(** [pp_status s] is a pretty representation of [s]. FIXME: use fmt. *)

val string_of_status: status -> string
(** [string_of_result r] is the string representation of [r]. *)

val status_of_string: string -> status
(** [status_of_string s] is the status [t] such that [string_of_status
    t] is [s]. *)
