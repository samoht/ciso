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

type t
(** The type for OPAM state. *)

val create: root:string -> Switch.t -> t
(** [create ~root s] create an OPAM state using [root] as OPAM's root
    and [s] as the current switch. *)

val jobs: t -> Task.t -> Job.t list
(** [jobs p] are the jobs needed to execute the plan [p]. *)

(** {1 OPAM files} *)

val read_installed: t -> Package.t list
(** [read_installed t] is the list of installed packages in on the
    current switch or [[]] if the switch does not exist. *)

val write_installed: t -> Package.t list -> unit
(** [write_installed t pkgs] update [t]'s metadata so that the packages
    [pkgs] are considered to be installed. *)

val write_pinned: t -> Task.pin list -> unit
(** [write_pinned t pkgs] update [t]'s metadata so that the packages
    [pkgs] are pinned. *)

(** {1 OPAM commands} *)

val install: t -> Package.t list -> unit Lwt.t
(** [install t pkgs] is {i opam install [pkgs]}. *)

val remove: t -> Package.t list -> unit Lwt.t
(** [remove t pkgs] is {i opam remove [pkgs]}. *)

val switch_to: t -> Switch.t -> unit Lwt.t
(** [switch_to t s] is {i opam switch [s]}. *)

val update: t -> unit Lwt.t
(** [update t] is {i opam update}. *)

(* FIXME: review the doc *)

val get_var: t -> string -> string
val clean_repos: t -> unit
val add_repos: t -> Task.repo list -> unit Lwt.t
val add_pins: t -> Task.pin list -> unit Lwt.t
