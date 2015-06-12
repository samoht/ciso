
val initial_store: ?uri:string -> ?fresh:bool -> unit -> unit Lwt.t

val register_token: string -> unit Lwt.t

val invalidate_token: string -> unit Lwt.t

val query_object: string -> bool Lwt.t

val publish_object: string -> string -> Object.t -> unit Lwt.t

val retrieve_object: string -> Object.t Lwt.t



val log_task: string -> Task.t -> unit Lwt.t

val unlog_task: string -> unit Lwt.t

val retrieve_tasks: unit -> (string * Task.t) list Lwt.t
