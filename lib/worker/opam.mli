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

(** Interface with opam-lib *)

(* [parse_user_demand demand]:
   parse the user demand string [demand] into a package name string and
   package version string option *)
val parse_user_demand: string -> string * string option

(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : string list -> OpamSolver.ActionGraph.t

(* [tasks_of_graph ?pull graph]
   given the action graph from resolv and return the id of corresponding
   task, the ids are deterministic *)
val jobs_of_graph: ?pull:Task.pull ->
  ?repositories:Task.repository list ->
  ?pins:Task.pin list ->
  OpamSolver.ActionGraph.t ->
  (Common_types.id * Job.t * (Common_types.id list)) list

val resolvable:
  name:string -> ?version:string -> ?depopts:(string * string option) list ->
  unit -> bool * OpamSolver.ActionGraph.t

(* [get_var v]
   as the command line `opam config var v`, to retrieve the variable `prefix`,
   most code copied from opamConfigCommand.ml*)
val get_var: string -> string

(* [findlib_conf prefix dest_path]
   if ocamlfind is installed under the current switch, ensure that the search
   and install path in the configuration file point to local lib/ *)
val findlib_conf: prefix:string -> write_path:string -> unit Lwt.t

(** [install n v]
    install package with name [n] and version [v] using OpamClient.SafeAPI *)
val install: name:string -> version:string ->
  [ `Fail of string | `Success | `Delegate of Common_types.id] Lwt.t

(** [uninstall n v] uninstall package with name [n] and version [v]
    using OpamClient.SafeAPI *)
val uninstall: name:string -> version:string -> unit Lwt.t

val update_metadata: install:bool -> path:string -> unit Lwt.t
val compiler: unit -> Common_types.compiler
val install_switch: Common_types.compiler -> unit Lwt.t
val remove_switch: Common_types.compiler -> unit Lwt.t
val switch: Common_types.compiler -> unit Lwt.t
val export_switch: Common_types.compiler -> unit Lwt.t
val clean_repositories: unit -> unit
val add_repositories: Task.repository list -> unit Lwt.t
val add_pins: Task.pin list -> unit Lwt.t
val update: unit -> unit Lwt.t
val show_repo_pin: unit -> unit Lwt.t
