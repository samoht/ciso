open Common_types

val new_worker: compiler -> host -> worker_id * worker_token

val verify_worker: worker_id -> worker_token -> unit

val job_rank: worker_token -> id list -> job_rank

val publish_object: id -> worker_token -> unit

val worker_monitor: unit -> (worker_id * worker_token) list Lwt.t
