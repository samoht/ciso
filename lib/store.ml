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
let err fmt = Printf.ksprintf failwith ("Store: " ^^ fmt)
let (/) dir file = List.append dir [file]

module StringSet = struct
  include Set.Make(String)
  let of_list = List.fold_left (fun s e -> add e s) empty
end

module R = Irmin.Basic(Irmin_http.Make)(Irmin.Contents.String)
module L = Irmin.Basic(Irmin_git.FS)(Irmin.Contents.String)

module RV = Irmin.View(R)
module LV = Irmin.View(L)

type store =
  | R  of R.t
  | L  of L.t
  | RV of RV.t
  | LV of LV.t

type t = string -> store

type 'a callback = 'a -> unit Lwt.t

type cancel = unit callback

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = "ciso" in
  Irmin.Task.create ~date ~owner msg

let remote ?(uri = Uri.of_string "http://127.0.0.1:8888") () =
  let config = Irmin_http.config uri in
  R.create config task >|= fun t ->
  fun x -> R (t x)

let err_invalid_version v =
  err "invalid /version: got %s, expecting %s." v Version.current

let check_version t =
  L.read (t "read version") ["version"] >>= function
  | None   -> L.update (t "init") ["version"] Version.current
  | Some v ->
    if v <> Version.current then err_invalid_version v else Lwt.return_unit

let local ?root () =
  let config = Irmin_git.config ?root ~bare:true () in
  L.create config task >>= fun t ->
  check_version t >|= fun () ->
  fun x -> L (t x)

let rv t = fun _ -> (RV t)
let lv t = fun _ -> (LV t)

(* FIXME: should be in Irmin *)
let retry ?(n=5) f =
  let rec aux i =
    if i >= n then
      f () >|= function
      | `Ok ()      -> true
      | `Conflict _ -> false
    else
      f () >>= function
      | `Ok ()      -> Lwt.return_true
      | `Conflict _ ->
        Lwt_unix.sleep (float i /. 10.) >>= fun () ->
        aux (i+1)
  in
  aux 1

let err_cannot_commit_transaction () = err "Cannot commit the transaction"

(* FIXME: this doesn't really work as expected, see
   https://github.com/mirage/irmin/issues/272 *)
let with_transaction ?retry:n t msg f =
  let aux () = match t msg with
    | R t ->
      retry ?n (fun () ->
          RV.of_path t [] >>= fun v ->
          f (rv v) >>= fun () ->
          RV.merge_path t [] v
        )
    | L t ->
      retry ?n (fun () ->
          LV.of_path t [] >>= fun v ->
          f (lv v) >>= fun () ->
          LV.merge_path t [] v
        )
    | RV _ | LV _ ->
      (* no nested transactions *)
      Lwt.return_false
  in
  aux () >>= function
  | true  -> Lwt.return_unit
  | false -> err_cannot_commit_transaction ()

let to_str codec v =
  let b = Buffer.create 64 in
  let e = Jsonm.encoder (`Buffer b) in
  let e = Jsont.encoder e codec v in
  match Jsont.encode e with
  | `Ok      ->
    Buffer.add_char b '\n';
    Buffer.contents b
  | `Partial -> assert false

let of_str codec s =
  let e = Jsonm.decoder (`String s) in
  let e = Jsont.decoder e codec in
  match Jsont.decode e with
  | `Ok (_, v)    -> v
  | `Await        -> assert false
  | `Error (_, e) ->
    invalid_arg (Jsont.error_to_string e)

module Store = struct

  let mem = function
    | R t  -> R.mem t
    | L t  -> L.mem t
    | RV t -> RV.mem t
    | LV t -> LV.mem t

  let update = function
    | R t  -> R.update t
    | L t  -> L.update t
    | RV t -> RV.update t
    | LV t -> LV.update t

  let read = function
    | R t  -> R.read t
    | L t  -> L.read t
    | RV t -> RV.read t
    | LV t -> LV.read t

  let read_exn = function
    | R t  -> R.read_exn t
    | L t  -> L.read_exn t
    | RV t -> RV.read_exn t
    | LV t -> LV.read_exn t

  let last l = List.hd (List.rev l)

  let list t k = match t with
    | R t  -> R.list t k  >|= List.map last
    | L t  -> L.list t k  >|= List.map last
    | RV t -> RV.list t k >|= List.map last
    | LV t -> LV.list t k >|= List.map last

  let rmdir = function
    | R t  -> R.remove_rec t
    | L t  -> L.remove_rec t
    | RV t -> RV.remove_rec t
    | LV t -> LV.remove_rec t

  let head = function
    | R t  -> R.head t
    | L t  -> L.head t
    | RV _ | LV _ -> err "watch not supported on views"

  let don't_watch _k ?init:_ _f = Lwt.return (fun () -> Lwt.return_unit)

  let watch_key = function
    | R t  -> R.watch_key t
    | L t  -> L.watch_key t
    | RV _ | LV _ ->
      (* cannot watch transactions *)
      (* FIXME: fix this in Irmin *)
      don't_watch

  (* FIXME: move that into Irmin *)

  let rv t = RV.list t [] >|= List.map last
  let lv t = LV.list t [] >|= List.map last
  let list_of_view f (_, v) = f v

  let list_diff f = function
    | `Updated (x, y) ->
      list_of_view f x >>= fun x ->
      list_of_view f y >|= fun y ->
      `Updated (x, y)
    | `Added x   -> list_of_view f x >|= fun x -> `Added x
    | `Removed x -> list_of_view f x >|= fun x -> `Removed x

  let init_of_l t key = function
    | None   -> Lwt.return_none
    | Some h ->
      R.of_head (R.config t) (fun () -> R.task t) h >>= fun t ->
      RV.of_path (t ()) key >|= fun v ->
      Some (h, v)

  let init_of_r t key = function
    | None   -> Lwt.return_none
    | Some h ->
      L.of_head (L.config t) (fun () -> L.task t) h >>= fun t ->
      LV.of_path (t ()) key >|= fun v ->
      Some (h, v)

  let watch_key_rec t key f =
    head t >>= fun h ->
    match t with
    | R t ->
      init_of_l t key h >>= fun init ->
      RV.watch_path t key ?init (fun d -> list_diff rv d >>= f)
    | L t ->
      init_of_r t key h >>= fun init ->
      LV.watch_path t key ?init (fun d -> list_diff lv d >>=  f)
    | RV _ | LV _ ->
      (* cannot watch transactions *)
      don't_watch t key

  let watch t root f =
    let process a children =
      Lwt_list.fold_left_s (fun acc id -> match a with
          | `Removed -> Lwt.return (`Removed id :: acc)
          | `Added   ->
            read t (root / id / "value") >|= function
            | Some v -> `Added v :: acc
            | None   -> acc
        ) [] children >>=
      Lwt_list.iter_p f
    in
    watch_key_rec t root (function
        | `Added l   -> process `Added l
        | `Removed l -> process `Removed l
        | `Updated (o, n) ->
          let old_ids = StringSet.of_list o in
          let new_ids = StringSet.of_list n in
          let added = StringSet.diff new_ids old_ids in
          let removed = StringSet.diff old_ids new_ids in
          Lwt.join [
            process `Added (StringSet.elements added);
            process `Removed (StringSet.elements removed);
          ]
      )

end

module type S = sig
  type id
  type value
  val add: t -> value -> unit Lwt.t
  val mem: t -> id -> bool Lwt.t
  val get: t -> id -> value Lwt.t
  val list: t -> id list Lwt.t
end

let pretty id = Id.to_string id
let fmt msg id = msg ^ " " ^ pretty id
let mk t msg id = t (fmt msg id)

module XJob = struct

  let root = ["jobs"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"
  let status_p id = path id / "status"
  let outputs_p id = path id / "outputs"
  let output_p id obj = outputs_p id / "outputs" / Id.to_string obj

  let mem t id = Store.mem (mk t "mem job" id) (value_p id)

  let add t job =
    let id = Job.id job in
    debug "add: job %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      with_transaction t (fmt "add job" id) (fun t ->
          Store.update (t "") (value_p id) (to_str Job.json job) >>= fun () ->
          Store.update (t "") (status_p id) (to_str Job.json_status `Pending)
        )
      >|= fun () ->
      debug "add: job %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "find job" id) (value_p id) >|= of_str Job.json

  let update_status status t id =
    let status = to_str Job.json_status status in
    Store.update (mk t ("job " ^ status) id) (status_p id) status

  let success = update_status `Success
  let failure = update_status `Failure
  let pending = update_status `Pending
  let runnable = update_status `Runnable

  let dispatch_to t id w = update_status (`Dispatched (w, `Pending)) t id
  let ack t id w = update_status (`Dispatched (w, `Started)) t id

  let status t id =
    Store.read (mk t "job status" id) (status_p id) >|= function
    | None   -> `Pending
    | Some s -> of_str Job.json_status s

  let add_output t id obj =
    Store.update (mk t "add job output" id) (output_p id obj) ""

  let outputs t id =
    Store.list (mk t "list job outputs" id) (outputs_p id) >|=
    List.map (Id.of_string `Object)

  let list t =
    Store.list (t "list jobs") root >|= List.map (Id.of_string `Job)

  let watch_status t id f =
    Store.watch_key (mk t "watch job status" id) (status_p id) (function
        | `Updated (_, (_, s))
        | `Added (_, s) -> f (of_str Job.json_status s)
        | `Removed _    -> f `Cancelled
      )

  let watch t f =
    let mk v = of_str Job.json v in
    Store.watch (t "watch jobs") root (function
        | `Added v    -> f (mk v)
        | `Removed id ->
          debug "The job %s has been removed, skipping it." id;
          Lwt.return_unit
      )

end

module XTask = struct

  let root = ["tasks"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"
  let status_p id = path id / "status"
  let jobs_p id = path id / "jobs"

  let list t =
    Store.list (t "list tasks") root >|= List.map (Id.of_string `Task)

  let mem t id = Store.mem (mk t "mem task" id) (value_p id)

  let add t task =
    let id = Task.id task in
    debug "add: task %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      with_transaction t (fmt "add task" id) (fun t ->
          Store.update (t "") (value_p id) (to_str Task.json task) >>= fun () ->
          Store.update (t "") (status_p id) (to_str Task.json_status `New)
        )
      >|= fun () ->
      debug "add: task %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "find task" id) (value_p id) >|=
    of_str Task.json

  let jobs t id =
    Store.list (mk t "list jobs of task" id) (jobs_p id) >|=
    List.map (Id.of_string `Job)

  let update_status status t id =
    let status = to_str Task.json_status status in
    Store.update (mk t ("task " ^ status) id) (status_p id) status

  let reset = update_status `New

  let dispatch_to t id w = update_status (`Dispatched (w, `Pending)) t id
  let ack t id w = update_status (`Dispatched (w, `Started)) t id

  let refresh_status t id =
    jobs t id >>= fun jobs ->
    Lwt_list.map_p (XJob.status t) jobs >>= fun status ->
    let status = Job.task_status status |> to_str Task.json_status in
    (* FIXME: because of https://github.com/mirage/irmin/issues/272  *)
    begin Store.read (t "") (status_p id) >|= function
      | None    -> true
      | Some s -> s <> status
    end >>= function
    | true  -> Store.update (mk t "update task status" id) (status_p id) status
    | false -> Lwt.return_unit

  let status t id =
    Store.read (mk t "task status" id) (status_p id) >|= function
    | None   -> `New
    | Some s -> of_str Task.json_status s

  let watch_status t id f =
    Store.watch_key (mk t "watch task status" id) (status_p id) (function
        | `Updated (_, (_, s))
        | `Added (_, s) -> f (of_str Task.json_status s)
        | `Removed _    -> f `Cancelled
      )

  let watch t f =
    let mk v = of_str Task.json v in
    Store.watch (t "watch taks") root (function
        | `Added    v -> f (mk v)
        | `Removed id ->
          debug "The task %s has been removed, skipping it." id;
          Lwt.return_unit
      )

end

module XObject = struct

  let root = ["objects"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"

  let list t =
    Store.list (t "list objects") root >|= List.map (Id.of_string `Object)

  let mem t id = Store.mem (mk t "mem object" id) (value_p id)

  let add t obj =
    let id = Object.id obj in
    debug "add: object %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      let obj = to_str Object.json obj in
      Store.update (mk t "publish object" id) (value_p id) obj >|= fun () ->
      debug "add: object %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "get object" id) (value_p id) >|=
    of_str Object.json

end

module XWorker = struct

  type diff = [`Added of Worker.t | `Removed of Worker.id]

  let root = ["workers"]
  let path id = root / Id.to_string id
  let value_p id = path id / "value"
  let tick_p id = path id / "tick"
  let status_p id = path id / "status"

  let list t =
    Store.list (t "list workers") root >|= List.map (Id.of_string `Worker)

  let mem t id = Store.mem (mk t "mem worker" id) (value_p id)

  let add t w =
    let id = Worker.id w in
    debug "add: worker %s" (pretty id);
    mem t id >>= function
    | true  -> Lwt.return_unit
    | false ->
      let w = to_str Worker.json w in
      with_transaction t (fmt "publish worker" id) (fun t ->
          Store.update (t "") (value_p id) w >>= fun () ->
          Store.update (t "") (status_p id) (to_str Worker.json_status `Idle)
        ) >|= fun () ->
      debug "add: worker %s published!" (pretty id)

  let get t id =
    Store.read_exn (mk t "get worker" id) (value_p id) >|=
    of_str Worker.json

  let forget t id =
    debug "forget worker %s" (Id.to_string id);
    Store.rmdir (mk t "forget worker" id) (path id)

  let tick t id f =
    Store.update (mk t "tick worker" id) (tick_p id) (string_of_float f)

  let status t id =
    Store.read (mk t "worker status" id) (status_p id) >|= function
    | None   -> None
    | Some s -> Some (of_str Worker.json_status s)

  let start t id status =
    let status_s = to_str Worker.json_status status in
    let msg = match status with
      | `Idle -> "worker is idle"
      | _     -> "worker starts " ^ status_s
    in
    Store.update (mk t msg id) (status_p id) status_s

  let start_job t id jid = start t id (`Job jid)
  let start_task t id tid = start t id (`Task tid)
  let idle t id = start t id `Idle

  let watch_ticks t id f =
    Store.watch_key (mk t "watch_ticks" id) (tick_p id) (function
        | `Updated (_, (_, tick))
        | `Added   (_, tick) -> f (float_of_string tick)
        | `Removed _         -> f 0. (* FIXME: ? *)
      )

  let watch_status t id f =
    Store.watch_key (mk t "watch status" id) (status_p id) (function
        | `Updated (_, (_, s))
        | `Added (_, s) -> f (Some (of_str Worker.json_status s))
        | `Removed _    -> f None
      )

  let watch t f =
    Store.watch (t "watch workers") root (function
        | `Added v    -> f @@ `Added (of_str Worker.json v)
        | `Removed id -> f @@ `Removed (Id.of_string `Worker id)
      )

end

module Task = XTask
module Job = XJob
module Object = XObject
module Worker = XWorker
