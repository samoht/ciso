(* object id -> task
   if task A is supposed to produce object a,
   then there is the binding a.id -> A *)
type task_tbl

(* object id -> object
   when the object isn't produced, there will be no binding in the table,
   an object may have multiple copies,
   so there might be multiple bindings for one id *)
type obj_tbl

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
val find_task: string -> int -> int

(* return all the object copies correspond to the given object id*)
val get_objects: int -> Object.t list

(* find the task who produces an object with the given object id*)
val task_of_oid: int -> Task.t

(* calcute the "distance" between two ip addresses *)
val distance_of_ips: string -> string -> int

(* given an object id and an object, add them in the object table *)
val publish_object: int -> Object.t -> unit Lwt.t

(******************************************************************************)

(* given the pull request number from ocaml/opam-repository, resolve the task
   and add tasks into task table *)
val github_hook : int -> unit Lwt.t

(* given a package number with version constraint probably, resolve the task
   and add tasks into task table *)
val test_handler: string -> unit Lwt.t
