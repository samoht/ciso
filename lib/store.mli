
val initial_store: ?uri:string -> unit -> unit Lwt.t

val query_object: string -> bool Lwt.t

val publish_object: string -> Object.t -> unit Lwt.t

val retrieve_object: string -> Object.t Lwt.t
