(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix
open Irmin_unix

let debug fmt = Gol.debug ~section:"store" fmt

module Remote = Irmin.Basic(Irmin_http.Make)(Irmin.Contents.String)
module Local = Irmin.Basic(Irmin_git.FS)(Irmin.Contents.String)

type store =
  | Remote of Remote.t
  | Local of Local.t

type t = string -> store

module Store = struct

  let mem = function
    | Remote t -> Remote.mem t
    | Local t  -> Local.mem t

  let update = function
    | Remote t -> Remote.update t
    | Local t  -> Local.update t

  let read = function
    | Remote t -> Remote.read t
    | Local t  -> Local.read t

  let read_exn = function
    | Remote t -> Remote.read_exn t
    | Local t  -> Local.read_exn t

  let list = function
    | Remote t -> Remote.list t
    | Local t  -> Local.list t

  let remove = function
    | Remote t -> Remote.remove t
    | Local t  -> Local.remove t

end

module type S = sig
  type id
  type value
  val add: t -> value -> unit Lwt.t
  val mem: t -> id -> bool Lwt.t
  val find: t -> id -> value option Lwt.t
end

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = "ciso" in
  Irmin.Task.create ~date ~owner msg

let remote ?(uri = "http://127.0.0.1:8888") () =
  let config = Irmin_http.config (Uri.of_string uri) in
  Remote.create config task >|= fun t ->
  fun x -> Remote (t x)

let local ?root () =
  let config = Irmin_git.config ?root ~bare:true () in
  Local.create config task >|= fun t ->
  fun x -> Local (t x)

let mk t msg id = t (msg ^ " " ^ Id.to_string id)
let map_o f = function None -> None | Some x -> Some (f x)
let (/) dir file = List.append dir [file]

module XJob = struct

  let root = ["job"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"
  let status_p id = path id / "status"
  let outputs_p id = path id / "outputs"
  let output_p id obj = outputs_p id / "outputs" / Id.to_string obj

  let mem t id = Store.mem (mk t "mem job" id) (value_p id)

  let add t job =
    let id = Job.id job in
    debug "add: job %s" (Id.pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      Store.update (mk t "add job" id) (value_p id) (Job.to_string job)
      >|= fun () ->
      debug "add: job %s published!" (Id.pretty id)

  let find t id =
    Store.read (mk t "find job" id) (value_p id) >|= map_o Job.of_string

  let update_status status t id =
    let status = Job.string_of_status status in
    Store.update (mk t ("job " ^ status) id) (status_p id) status

  let success = update_status `Success
  let running = update_status `Running
  let failure t id msg = update_status (`Failure msg) t id

  let status t id =
    Store.read_exn (mk t "job status" id) (status_p id) >|= Job.status_of_string

  let add_output t id obj =
    Store.update (mk t "add job output" id) (output_p id obj) ""

  let outputs t id =
    Store.list (mk t "list job outputs" id) (outputs_p id) >|=
    List.map (fun path ->
        match List.rev path with
        | []    -> assert false
        | id::_ -> Id.of_string `Object id
      )

  let list t =
    Store.list (t "list jobs") root >|=
    List.map (fun path ->
        match List.rev path with
        | []    -> assert false
        | id::_ -> Id.of_string `Job id
      )

end

module XTask = struct

  let path id = ["task"; Id.to_string id]
  let value_p id = path id / "value"
  let status_p id = path id / "status"
  let jobs_p id = path id / "jobs"

  let mem t id = Store.mem (mk t "mem task" id) (value_p id)

  let add t task =
    let id = Task.id task in
    debug "add: task %s" (Id.pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      Store.update (mk t "add task" id) (value_p id) (Task.to_string task)
      >|= fun () ->
      debug "add: task %s published!" (Id.pretty id)

  let find t id =
    Store.read (mk t "find task" id) (value_p id) >|= map_o Task.of_string

  let jobs t id =
    Store.list (mk t "list jobs" id) (jobs_p id) >|=
    List.map (fun path ->
        match List.rev path with
        | []    -> assert false
        | id::_ -> Id.of_string `Job id
      )

  let update_status t id =
    jobs t id >>= fun jobs ->
    Lwt_list.map_p (XJob.status t) jobs >>= fun status ->
    let status = Job.task_status status |> Task.string_of_status in
    Store.update (mk t "update task status" id) (status_p id) status

  let status t id =
    update_status t id >>= fun () ->
    Store.read_exn (mk t "task status" id) (status_p id) >|=
    Task.status_of_string

end

module XObject = struct

  let path id = ["object"; Id.to_string id]
  let value_p id = path id / "value"

  let mem t id = Store.mem (mk t "mem object" id) (value_p id)

  let add t obj =
    let id = Object.id obj in
    debug "add: object %s" (Id.pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      let obj = Object.to_string obj in
      Store.update (mk t "publish object" id) (value_p id) obj >|= fun () ->
      debug "add: object %s published!" (Id.pretty id)

  let find t id =
    Store.read (mk t "retrieve object" id) (value_p id) >|=
    map_o Object.of_string

end

module XWorker = struct

  let path id = ["worker"; Id.to_string id]
  let value_p id = path id / "value"
  let tick_p id = path id / "tick"
  let job_p id = path id / "job"

  let mem t id = Store.mem (mk t "mem worker" id) (value_p id)

  let add t w =
    let id = Worker.id w in
    debug "add: worker %s" (Id.pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      let w = Worker.to_string w in
      Store.update (mk t "publish worker" id) (value_p id) w >|= fun () ->
      debug "add: worker %s published!" (Id.pretty id)

  let find t id =
    Store.read (mk t "retrieve worker" id) (value_p id) >|=
    map_o Worker.of_string

  let tick t id f =
    Store.update (mk t "tick worker" id) (tick_p id) (string_of_float f)

  let job t id =
    Store.read (mk t "worker job" id) (job_p id) >|= map_o (Id.of_string `Job)

  let start t id jid =
    Store.update (mk t "worker start" id) (job_p id) (Id.to_string jid)

  let stop t id =
    Store.remove (mk t "worker stop" id) (job_p id)

end

module Task = XTask
module Job = XJob
module Object = XObject
module Worker = XWorker
