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

val host: plan -> Host.t
(** [host p] is the host configuration of workers executing [p]. *)

val switch: plan -> Switch.t
(** [switch p] is the compiler switchs of workers executing [p]. *)

val plans: ?hosts:Host.t list -> ?switches:Switch.t list -> Task.t -> plan list
(** [plans t] is the list of plans, for all the supported hosts and
    switches. If [hosts] is not set, use {!Host.defaults}. If
    [switches] is not set, use {!Switch.default}. *)

val jobs: Task.t -> Job.t list
(** [jobs p] are the jobs needed to execute the plan [p]. *)

(** {1 OPAM files} *)

val read_installed: Switch.t -> Package.t list
(** [read_installed s] is the list of installed packages on the switch
    [s] or [[]] if the switch does not exist. *)

val write_installed: Switch.t -> Package.t list -> unit
(** [write_installed pkgs] update the OPAM state to state that the
    package [pkgs] are installed. *)

val write_pinned: Switch.t -> Task.pin list -> unit
(** [write_pinned] write the list of pinned packages. *)

(** {1 OPAM commands} *)

val root: unit -> string
(** [root ()] is {i opam config var root}. *)

val install: Package.t list -> unit Lwt.t
(** [install pkgs] is {i opam install [pkgs]}. *)

val remove: Package.t list -> unit Lwt.t
(** [remove pkgs] is {i opam remove [pkgs]}. *)

val switch_to: Switch.t -> unit Lwt.t
(** [switch_to s] is {i opam switch [s]}. *)

val update: unit -> unit Lwt.t
(** [update ()] is {i opam update}. *)

(* FIXME: review the doc *)

(* [get_var v]
   as the command line `opam config var v`, to retrieve the variable `prefix`,
   most code copied from opamConfigCommand.ml*)
val get_var: string -> string

val clean_repos: unit -> unit
val add_repos: Task.repo list -> unit Lwt.t

val add_pins: Task.pin list -> unit Lwt.t
