(* given a package name with version constraint probably,
   produce an action graph based on the host's opam configuration but with
   no installed packages, nor pinned packages *)
val resolve : string -> OpamSolver.ActionGraph.t

(* [add_task new_task update_inputs ?pull graph]
   new_task is a callback function who takes package name and verison
   and optional a pull request to produce a new task, add it in task table,
   returns the object id this task will produce;
   update_inputs is a callback function who takes an object id and also
   the dependencies objects' id to update the task's field inputs *)
val tasks_of_graph: ?pull:Task.pull ->
                    (string -> string -> string list ->
                     string -> string -> string) ->
                    OpamSolver.ActionGraph.t ->
                    (string * Task.t) list
