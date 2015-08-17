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
  ?repository:Task.repository list ->
  ?pin:Task.pin list ->
  OpamSolver.ActionGraph.t ->
  (Common_types.id * Task.job * (Common_types.id list)) list


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
