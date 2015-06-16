open Common_types

val initial_store: ?uri:string -> ?fresh:bool -> unit -> unit Lwt.t

val register_token: worker_token -> unit Lwt.t

val invalidate_token: worker_token -> unit Lwt.t

val query_object: id -> bool Lwt.t

val publish_object: worker_token -> id -> Object.t -> unit Lwt.t

val retrieve_object: id -> Object.t Lwt.t



val log_job: id -> Task.job -> unit Lwt.t

val unlog_job: id -> unit Lwt.t

val retrieve_jobs: unit -> (id * Task.job) list Lwt.t
