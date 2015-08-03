(* [compiler ()]:
   detect current using ocaml compiler version *)
val compiler: ?state:OpamState.state -> unit -> Common_types.compiler

(* [parse_user_demand demand]:
   parse the user demand string [demand] into a package name string and
   package version string option *)
val parse_user_demand: string -> string * string option

(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : ?bare:bool -> OpamState.state -> string list ->
              OpamSolver.ActionGraph.t

(* [tasks_of_graph ?pull graph]
   given the action graph from resolv and return the id of corresponding
   task, the ids are deterministic *)
val jobs_of_graph: ?pull:Task.pull -> OpamSolver.ActionGraph.t ->
                    (Common_types.id * Task.job * (Common_types.id list)) list


val resolvable: name:string -> ?version:string -> ?depopts:(string * string option) list ->
                OpamState.state -> bool * OpamSolver.ActionGraph.t

(* [get_opam_var v]
   as the command line `opam config var v`, to retrieve the variable `prefix`,
   most code copied from opamConfigCommand.ml*)
val get_opam_var: string -> string

(* [load_state ?switch ()]
   load opam state, if [switch] is given, set the switch within opamGlobals *)
val load_state: ?switch:string -> unit -> OpamState.state


(* [findlib_conf prefix dest_path]
   if ocamlfind is installed under the current switch, ensure that the search
   and install path in the configuration file point to local lib/ *)
val findlib_conf: prefix:string -> write_path:string -> unit Lwt.t

(** [opam_install n v]
    install package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_install: OpamState.state -> name:string -> version:string ->
                  [> `Fail of string
                  | `Success
                  | `Delegate of Common_types.id] Lwt.t

(** [opam_uninstall n v]
    uninstall package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_uninstall: name:string -> version:string -> unit Lwt.t


val update_metadata: install:bool -> OpamState.state -> path:string ->
                     OpamState.state Lwt.t


val detect_root: unit -> Common_types.root


val opam_install_switch: Common_types.root -> Common_types.compiler ->
                         unit Lwt.t

val opam_remove_switch: Common_types.root -> Common_types.compiler ->
                        unit Lwt.t

val opam_switch_switch: Common_types.root -> Common_types.compiler ->
                        unit Lwt.t

val export_existed_switch: Common_types.root -> Common_types.compiler ->
                           unit Lwt.t
