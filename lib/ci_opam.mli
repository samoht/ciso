(* [compiler ()]:
   detect current using ocaml compiler version *)
val compiler: unit -> Common_types.compiler


(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : ?bare:bool -> OpamState.state -> string -> OpamSolver.ActionGraph.t

(* [tasks_of_graph ?pull graph]
   given the action graph from resolv and return the id of corresponding
   task, the ids are deterministic *)
val jobs_of_graph: ?pull:Task.pull -> OpamSolver.ActionGraph.t ->
                    (Common_types.id * Task.job * (Common_types.id list)) list


val resolvable: name:string -> ?version:string -> OpamState.state ->
                bool * OpamSolver.ActionGraph.t

(* [get_opam_var v]
   as the command line `opam config var v`, to retrieve the variable `prefix`,
   most code copied from opamConfigCommand.ml*)
val get_opam_var: string -> string

(* [load_state ?switch ()]
   load opam state, if [switch] is given, set the switch within opamGlobals *)
val load_state: ?switch:string -> unit -> OpamState.state

(*
(* [lock ()]
   for multiple workers test on one machine,
   during package build/remove, only one worker could patch its packages dir
   to the findlib.conf file, the return value lock contains the original values
   in the configuration files for `destdir` and `path` fields *)
val lock: unit -> ocamlfind_lock Lwt.t

(* [unlock lock]
   unlock the ocamlfind configuration file, allow other workers to build/remove
   their packages, assign the origin values to `destdir` and `path` by the
   values retained in the lock *)
val unlock: ocamlfind_lock -> unit Lwt.t *)


(* [findlib_conf prefix dest_path]
   if ocamlfind is installed under the current switch, ensure that the search
   and install path in the configuration file point to local lib/ *)
val findlib_conf: prefix:string -> write_path:string -> unit Lwt.t

(** [opam_install n v]
    install package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_install: OpamState.state -> name:string -> version:string ->
                  [> `Fail of string | `Success | `Delegate of Common_types.id] Lwt.t

(** [opam_uninstall n v]
    uninstall package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_uninstall: name:string -> version:string -> unit Lwt.t


val update_metadata: install:bool -> OpamState.state -> path:string ->
                     OpamState.state Lwt.t
