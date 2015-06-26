val load_state: unit -> OpamState.state

(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : ?bare:bool -> OpamState.state -> string -> OpamSolver.ActionGraph.t

(* [tasks_of_graph ?pull graph]
   given the action graph from resolv and return the id of corresponding
   task, the ids are deterministic *)
val jobs_of_graph: ?pull:Task.pull -> OpamSolver.ActionGraph.t ->
                    (Common_types.id * Task.job * (Common_types.id list)) list

(** [opam_install n v]
    install package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_install: OpamState.state -> string -> string ->
                  [> `Fail of string | `Success ] Lwt.t

(** [opam_uninstall n v]
    uninstall package with name [n] and version [v] using OpamClient.SafeAPI *)
val opam_uninstall: string -> string -> unit Lwt.t


val update_metadata: install:bool -> OpamState.state -> string ->
                     OpamState.state Lwt.t

val installed_of_state: OpamState.state -> string
