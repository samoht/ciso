(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : string -> OpamSolver.ActionGraph.t

(* [tasks_of_graph ?pull graph]
   given the action graph from resolv and return the id of corresponding
   task, the ids are deterministic *)
val tasks_of_graph: ?pull:Task.pull -> OpamSolver.ActionGraph.t ->
                    (Common_types.id * Task.t) list
