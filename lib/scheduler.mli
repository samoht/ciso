(* object id -> task
   if task A is supposed to produce object a,
   then there is the binding a.id -> A *)
type task_tbl

(* object id -> object id
   if task A has dependencies objects b, c, d,
   and task A is supposed to produce object a,
   then there will be bindins b.id -> a.id, c.id -> a.id, d.id -> a.id,
   whenever b/c/d is published in the obj_tbl,
   a hook function will examine if task A becomes runnable *)
type hook_tbl

(* task state *)
type state = [`Pending | `Dispatched | `Runnable | `Completed]

(* object id -> task state
   if task A is supposed to produce object a,
   then there is binding a.id -> A.state *)
type state_tbl

(******************************************************************************)

(* given the ip and port number of a worker, return an object id for it,
   the task who produces the object must be runnable and also needs the least
   dependencies transfers from other workers *)
val find_task: string -> (string * string) option

(* given an object id and a worker token, add them in the logmap *)
val publish_object: string -> string -> unit Lwt.t

(* given a task id and return the pacakge name and version information *)
val task_info: string -> string

(* pickup any uncompleted tasks due to master failure *)
val bootstrap: unit -> unit Lwt.t

(******************************************************************************)

(* given the pull request number from ocaml/opam-repository, resolve the task
   and add tasks into task table *)
val github_hook : int -> unit Lwt.t

(* given a package number with version constraint probably, resolve the task
   and add tasks into task table *)
val user_demand: string -> unit Lwt.t
