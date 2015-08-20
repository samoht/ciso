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

module Store = Irmin.Basic(Irmin_http.Make)(Irmin.Contents.String)

type t = string -> Store.t

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

let create ?(uri = "http://127.0.0.1:8888") () =
  let config = Irmin_http.config (Uri.of_string uri) in
  Store.create config task

let mk t msg id = t (msg ^ " " ^ Id.to_string id)
let map_o f = function None -> None | Some x -> Some (f x)
let (/) dir file = List.append dir [file]

module XJob = struct

  let path id = ["job"; Id.to_string id]

  let mem t id = Store.mem (mk t "mem job" id) (path id)

  let add t job =
    let id = Job.id job in
    debug "add: job %s" (Id.pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      Store.update (mk t "add job" id) (path id) (Job.to_string job)
      >|= fun () ->
      debug "add: job %s published!" (Id.pretty id)

  let find t id =
    Store.read (mk t "find job" id) (path id) >|=
    map_o Job.of_string

  let update_status status t id =
    let status = Job.string_of_status status in
    Store.update (mk t ("job " ^ status) id) (path id / "status") status

  let success = update_status `Success
  let running = update_status `Running
  let failure t id msg = update_status (`Failure msg) t id

  let status t id =
    Store.read_exn (mk t "job status" id) (path id / "status") >|=
    Job.status_of_string

  let add_output t id o =
    let obj = Id.to_string o in
    Store.update (mk t "add job output" id) (path id / "outputs" / obj) ""

  let outputs t id =
    Store.list (mk t "list job outputs" id) (path id / "outputs") >|=
    List.map (fun path ->
        match List.rev path with
        | []    -> assert false
        | id::_ -> Id.of_string `Object id
      )

end

module XTask = struct

  let path id = ["task"; Id.to_string id]

  let mem t id = Store.mem (mk t "mem task" id) (path id)

  let add t task =
    let id = Task.id task in
    debug "add: task %s" (Id.pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      Store.update (mk t "add task" id) (path id) (Task.to_string task)
      >|= fun () ->
      debug "add: task %s published!" (Id.pretty id)

  let find t id =
    Store.read (mk t "find task" id) (path id) >|=
    map_o Task.of_string

  let jobs t id =
    Store.list (mk t "list jobs" id) (path id / "jobs") >|=
    List.map (fun path ->
        match List.rev path with
        | []    -> assert false
        | id::_ -> Id.of_string `Job id
      )

  let update_status t id =
    jobs t id >>= fun jobs ->
    Lwt_list.map_p (XJob.status t) jobs >>= fun status ->
    let status = Job.task_status status |> Task.string_of_status in
    Store.update (mk t "update task status" id) (path id / "status") status

  let status t id =
    update_status t id >>= fun () ->
    Store.read_exn (mk t "task status" id) (path id / "status")
    >|= Task.status_of_string

end

module XObject = struct

  let path id = ["object"; Id.to_string id]

  let mem t id = Store.mem (mk t "query object" id) (path id)

  let add t obj =
    let id = Object.id obj in
    debug "add: object %s" (Id.pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      Store.update (mk t "publish object" id) (path id) (Object.to_string obj)
      >|= fun () ->
      debug "add: object %s published!" (Id.pretty id)

  let find t id =
    Store.read (mk t "retrieve object" id) (path id) >|=
    map_o Object.of_string

end

module Task = XTask
module Job = XJob
module Object = XObject
