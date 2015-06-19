val load_state: unit -> OpamState.state

(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : ?bare:bool -> OpamState.state -> string -> OpamSolver.ActionGraph.t

(* [tasks_of_graph ?pull graph]
   given the action graph from resolv and return the id of corresponding
   task, the ids are deterministic *)
val jobs_of_graph: ?pull:Task.pull -> OpamSolver.ActionGraph.t ->
                    (Common_types.id * Task.job) list

(** [opam_install s n v]
    install package with name [n] and version [v] using opam-lib without
    update of metadata  *)
val opam_install: OpamState.state -> string -> string
                  -> (OpamState.state * [> `Fail of string | `Success ]) Lwt.t

(** [opam_uninstall s n v]
    uninstall package with name [n] and version [v] using opam-lib *)
val opam_uninstall: OpamState.state -> string -> string -> unit


val update_metadata: OpamState.state -> string -> string -> OpamState.state Lwt.t
