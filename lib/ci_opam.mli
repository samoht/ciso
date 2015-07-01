type ocamlfind_lock

(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : ?bare:bool -> OpamState.state -> string -> OpamSolver.ActionGraph.t

(* [tasks_of_graph ?pull graph]
   given the action graph from resolv and return the id of corresponding
   task, the ids are deterministic *)
val jobs_of_graph: ?pull:Task.pull -> OpamSolver.ActionGraph.t ->
                    (Common_types.id * Task.job * (Common_types.id list)) list

(* [get_opam_var v]
   as the command line `opam config var v`, to retrieve the variable `prefix`,
   most code copied from opamConfigCommand.ml*)
val get_opam_var: string -> string

(* [load_state ?root ()]
   root is for $OPAMROOT, load opam state *)
val load_state: ?root:string -> unit -> OpamState.state

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
val unlock: ocamlfind_lock -> unit Lwt.t

(** [opam_install n v]
    install package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_install: OpamState.state -> string -> string ->
                  [> `Fail of string | `Success ] Lwt.t

(** [opam_uninstall n v]
    uninstall package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_uninstall: string -> string -> unit Lwt.t


val update_metadata: install:bool -> OpamState.state -> string ->
                     OpamState.state Lwt.t
