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

(** Interaction with OPAM *)

type plan
(** The type for OPAM's build plan. *)

val is_simple: plan -> Package.t option
(** [is_simple p] is [Some pkg] if the plan [p] consists of only one
    action: building and installing [p]. *)

val resolve: string list -> plan
(** [resolve atoms] returns a plan to build the list of (serialized) atoms
    [atoms]. An atom is either:

    {ul
    {- A package name.}
    {- A package name and a fixed version, e.g. ["name.version"]}
    {- A package name and a version constraint inequality,
       e.g. ["name<=version"]. This is similar to what OPAM uses on
       the command-line.}
    }  *)

val resolve_packages: Package.t list -> plan
(** Same as {!resolve} but without version constraint inequalities. *)

val installed: unit -> Package.t list
(** [installed ()] is the list of installed packages on the current switch. *)

val write_installed: Package.t list -> unit
(** [write_installed pkgs] update the OPAM state to state that the
    package [pkgs] are installed. *)

(** {1 OPAM commands} *)

val install: Package.t -> [ `Fail of string | `Success] Lwt.t
(** [install pkg] is similar to {i opam install pkg} but using
    opam-lib. *)

val remove: Package.t -> unit Lwt.t
(** [remove pkg] is similar to {i opam remove pkg} but using
    opam-lib. *)

(* FIXME *)

type job = Job.id * Job.t * Object.id list

val jobs: ?repos:Task.repository list -> ?pins:Task.pin list -> plan -> job list
(** [jobs p] is the list of jobs needed to fulfil the plan [p]. *)
(* FIXME: should take a user-defined task as argument ... *)

(* FIXME: review the doc *)

(* [parse_user_demand demand]:
   parse the user demand string [demand] into a package name string and
   package version string option *)
val parse_user_demand: string -> string * string option

(* [get_var v]
   as the command line `opam config var v`, to retrieve the variable `prefix`,
   most code copied from opamConfigCommand.ml*)
val get_var: string -> string

val update: unit -> unit Lwt.t

val compiler: unit -> string

val switch_to: string -> unit Lwt.t
val install_switch: string -> unit Lwt.t
val remove_switch: string -> unit Lwt.t
val export_switch: string -> unit Lwt.t

val clean_repos: unit -> unit
val add_repos: Task.repository list -> unit Lwt.t

val add_pins: Task.pin list -> unit Lwt.t
val show_repo_pin: unit -> unit Lwt.t