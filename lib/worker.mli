type t

(** [to_local_obj worker obj_id content]:
    when get a dependency object from other workers, save it locally *)
val to_local_obj: t -> int -> string -> Object.t Lwt.t

(******************************************************************************)

(** [worker_register master_uri addr]:
    when started, register itself with ip and port as addr to master *)
val worker_register: Uri.t -> string * int -> t Lwt.t

(** [worker_heartbeat master_uri worker]:
    sends heartbeat to master, the heartbeat contains the worker status: work
    or idle, if idle and the master has assigned a new task A to it,
    the task A produces object a, the function returns a thread holds
    Some (A.id, a.id) *)
val worker_heartbeat: Uri.t -> t -> (int * int) option Lwt.t

(** [worker_request_task master_uri worker task_id]:
    asks master to return the sexp of task whose id is task_id *)
val worker_request_task: Uri.t -> t -> int -> Task.t Lwt.t

(** [worker_publish master_uri worker object]:
    if produces a new object or get a copy from other workers,
    publish it to master in the object tables *)
val worker_publish: Uri.t -> t -> Object.t -> unit Lwt.t

(** [worker_consult_object master_uri worker obj_id]:
    when the worker doesn't hold a dependency locally, it asks master about
    the best location of a copy of the id obj_id, the returned thread
    is supposed to give the ip, addr of the worker to contact and also
    the path at that worker to retrieve the object *)
val worker_consult_object: Uri.t -> t -> int -> (string * int * string) Lwt.t

(** [worker_request_object master_uri worker obj_id]:
    before task execution, the worker will gather all the dependencies by this
    function. If the object of obj_id isn't found locally,
    the worker will consult master about the location of the object,
    retrieve it from other workers, save it locally,
    publish it to master that a copy of this object has been made,
    then return the thread *)
val worker_request_object: Uri.t -> t -> int -> string Lwt.t


(******************************************************************************)

(** [execution_loop master_uri worker cond]:
    infinite loop to execute tasks, the conditional variable this function waits
    for is task_id and obj_id *)
val execution_loop: Uri.t -> t -> (int * int) Lwt_condition.t -> 'a Lwt.t

(** [heartbeat_loop master_uri worker cond]:
    infinite loop to send out heartbeats to master,
    under the idle state, if gets the response of Some (task_id, obj_id),
    the function will send a signl to the conditional variable cond *)
val heartbeat_loop: Uri.t -> t -> (int * int) Lwt_condition.t -> 'a Lwt.t

(** [worker_file_server worker]:
    responsible for the requests from other workers for the objects produced
    by this worker,
    upon a request, find the file and respond with file content *)
val worker_file_server: t -> unit Lwt.t
