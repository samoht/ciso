type t
type store

(** [worker_register master_uri addr]:
    when started, register itself with ip and port as addr to master *)
val worker_register: Uri.t -> (string -> store Lwt.t) -> t Lwt.t

(** [worker_heartbeat master_uri worker]:
    sends heartbeat to master, the heartbeat contains the worker status: work
    or idle, if idle and the master has assigned a new task A to it,
    the task A produces object a, the function returns a thread holds
    Some (A.id, a.id) *)
val worker_heartbeat: Uri.t -> t -> (string * string) option Lwt.t


(** [worker_publish master_uri worker object]:
    if produces a new object or get a copy from other workers,
    publish it to master in the object tables *)
val worker_publish: Uri.t -> t -> string -> Object.t -> unit Lwt.t


(** [worker_request_object master_uri worker obj_id]:
    before task execution, the worker will gather all the dependencies by this
    function. If the object of obj_id isn't found locally,
    the worker will consult master about the location of the object,
    retrieve it from other workers, save it locally,
    publish it to master that a copy of this object has been made,
    then return the thread *)
val worker_request_object: Uri.t -> t -> string -> Object.t Lwt.t


(******************************************************************************)

(** [execution_loop master_uri worker cond]:
    infinite loop to execute tasks, the conditional variable this function waits
    for is task_id and obj_id *)
val execution_loop: Uri.t -> t -> (string * string) Lwt_condition.t -> 'a Lwt.t

(** [heartbeat_loop master_uri worker cond]:
    infinite loop to send out heartbeats to master,
    under the idle state, if gets the response of Some (task_id, obj_id),
    the function will send a signl to the conditional variable cond *)
val heartbeat_loop: Uri.t -> t -> (string * string) Lwt_condition.t -> 'a Lwt.t
