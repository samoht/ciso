open Common_types
type worker_status

val new_worker: host -> worker_id * worker_token

val verify_worker: worker_id -> worker_token -> unit

val job_rank: worker_token -> id list -> job_rank

val new_job: id -> compiler -> worker_token -> unit

val job_completed: id -> worker_token -> unit

val publish_object: id -> worker_token -> unit

val worker_statuses: unit -> (worker_id * worker_token * worker_status) list

val info_of_status: worker_status -> string * string option

val worker_environments: unit -> host list

val worker_env: worker_token -> host * compiler option

val compilers: unit -> compiler list

val worker_monitor: unit -> (worker_id * worker_token) list Lwt.t
