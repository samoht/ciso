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

let debug fmt = Gol.debug ~section:"scheduler" fmt
let err fmt = Printf.ksprintf failwith ("Scheduler: " ^^ fmt)
let todo f = err "Scheduler: TODO %s" f

module XTask = struct

  module TSet = Set.Make(Task)

  type t = {
    cond: unit Lwt_condition.t;
    store: Store.t;                                            (* local store *)
    mutable tasks: TSet.t;                                       (* new tasks *)
  }

  let list t = TSet.elements t.tasks

  let remove_task t task = t.tasks <- TSet.remove task t.tasks

  (* FIXME: need to watch the related jobs status updates to update
     the task status. *)
  let watch_task_status t task =
    let id = Task.id task in
    let cancel = ref (fun () -> Lwt.return_unit) in
    Store.Task.watch_status t.store id (function
        | `New       -> assert (TSet.mem task t.tasks); Lwt.return_unit
        | `Cancelled -> todo "Task.cancel"
        | `Pending
        | `Success
        | `Failure   -> remove_task t task; !cancel ()
      ) >|= fun c ->
    cancel := c

  let add_task t task =
    t.tasks <- TSet.add task t.tasks;
    watch_task_status t task >|= fun () ->
    Lwt_condition.broadcast t.cond ()

  let create store =
    Store.Task.list store >>= fun ids ->
    Lwt_list.fold_left_s (fun tasks id ->
        Store.Task.get store id >>= fun ta ->
        Store.Task.status store id >|= function
        | `New       -> TSet.add ta tasks
        | `Cancelled -> todo "Task.cancel"
        | `Pending | `Success | `Failure -> tasks
      ) TSet.empty ids
    >>= fun tasks ->
    let cond = Lwt_condition.create () in
    let t = { cond; store; tasks } in
    Lwt_list.iter_p (watch_task_status t) (TSet.elements tasks) >|= fun () ->
    t

  let peek t =
    if TSet.cardinal t.tasks = 0 then None
    else Some (TSet.choose t.tasks)

  let rec peek_s t =
    match peek t with
    | None   -> Lwt_condition.wait t.cond >>= fun () -> peek_s t
    | Some h -> Lwt.return h

  let watch_task t = Store.Task.watch t.store (add_task t)

  let start store =
    debug "starting the task scheduler";
    create store >>= fun t ->
    watch_task t >|= fun _ ->
    t

end

module XJob = struct

  type t = {
    cond  : unit Lwt_condition.t;
    store : Store.t;
    jobs  : (Job.id, Job.t) Hashtbl.t;
    status: (Job.id, Job.status) Hashtbl.t;
  }

  let list t = Hashtbl.fold (fun _ v l -> v :: l) t.jobs []

  let runnables t host =
    Hashtbl.fold (fun id j acc ->
        if Hashtbl.find t.status id = `Runnable && Host.equal (Job.host j) host
        then j :: acc
        else acc
      ) t.jobs []

  let rec peek_s t host =
    let runnables = runnables t host in
    match runnables with
    | []       ->
      Lwt_condition.wait t.cond >>= fun () ->
      peek_s t host
    | jid :: _ -> Lwt.return jid

  let peek t host =
    match runnables t host with
    | []   -> None
    | h::_ -> Some h

  (* FIXME: use rev-deps to make it fast (a al TUP). *)
  let update_runnable t =
    let jobs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) t.jobs [] in
    let status j = Hashtbl.find t.status j in
    List.iter (fun (id, job) ->
        match status id with
        | `Pending ->
          let inputs = Job.inputs job in
          if List.for_all (fun j -> status j = `Success) inputs then (
            debug "%s is now runnable" (Id.to_string id);
            Hashtbl.replace t.status id `Runnable;
            (* FIXME: could broadcast only to the workers with the right
               host configuration *)
            Lwt_condition.broadcast t.cond ()
          )
        | _ -> ()
      ) jobs

  let remove_job t id =
    Hashtbl.remove t.jobs id;
    Hashtbl.remove t.status id

  let add_job_aux t job =
    let id = Job.id job in
    Store.Job.mem t.store id >>= function
    | false ->
      Store.Job.add t.store job >|= fun () ->
      Hashtbl.add t.jobs id job;
      Hashtbl.add t.status id `Pending
    | true ->
      Store.Job.status t.store id >|= fun status ->
      Hashtbl.replace t.jobs id job;
      Hashtbl.replace t.status id status

  let rec add_job t job =
    debug "add job %s" (Id.to_string @@ Job.id job);
    add_job_aux t job >>= fun () ->
    watch_job_status t job >|= fun () ->
    update_runnable t

  and watch_job_status t j =
    let id = Job.id j in
    debug "watch job status %s" (Id.to_string id);
    let cancel = ref (fun () -> Lwt.return_unit) in
    Store.Job.watch_status t.store id (function
        | `Cancelled -> failwith "TODO"
        | `Pending | `Running | `Runnable -> add_job t j
        | `Success | `Failure -> remove_job t id; !cancel ()
      ) >|= fun c ->
    cancel := c

  let watch_jobs t =
    Store.Job.watch t.store (add_job t) >|= fun _cancel ->
    ()

  let create store =
    let t =  {
      store;
      cond    = Lwt_condition.create ();
      jobs     = Hashtbl.create 16;
      status   = Hashtbl.create 16;
    } in
    Store.Job.list store >>=
    Lwt_list.map_p (Store.Job.get store) >>=
    Lwt_list.iter_p (add_job t) >|= fun () ->
    t

  let start store =
    debug "starting the job scheduler.";
    create store >>= fun t ->
    watch_jobs t >|= fun () ->
    t

end

module XWorker = struct

  module WSet = Set.Make(Worker)

  type t = {
    cond   : unit Lwt_condition.t;
    store  : Store.t;
    cancels: (Worker.id, (unit -> unit Lwt.t) list) Hashtbl.t;
    mutable workers: WSet.t;
  }

  let list t = WSet.elements t.workers

  let cancels t id = try Hashtbl.find t.cancels id with Not_found -> []

  let add_to_cancel t id f =
    let cs = cancels t id in
    Hashtbl.replace t.cancels id (f :: cs)

  (* reset the job a dead worker was working on. This will trigger the
     job scheduler to distribute it to someone else. *)
  let reset_job t id =
    Store.Worker.status t.store id >>= function
    | `Job id  -> Store.Job.pending t.store id
    | _ -> Lwt.return_unit

  let rem_worker t w =
    debug "remove worker %s" (Id.to_string @@ Worker.id w);
    t.workers <- WSet.remove w t.workers

  (* watch for worker ticks, clean-up the worker state if it's dead. *)
  let watch_woker_ticks t w =
    let id = Worker.id w in
    let now () = Unix.time () in
    let last_tick = ref (now ()) in
    let is_dead = 10. in
    Store.Worker.watch_ticks t.store id (fun _tick ->
        last_tick := now ();
        Lwt.return_unit
      )
    >|= fun cancel ->
    add_to_cancel t id cancel;
    let forget () =
      debug "%s is dead!" (Id.to_string id);
      rem_worker t w;
      reset_job t id >>= fun () ->
      Store.Worker.forget t.store id >>= fun () ->
      Lwt_list.iter_p (fun f -> f ()) (cancels t id)
    in
    let rec loop () =
      if now () -. !last_tick > is_dead then forget ()
      else
        Store.Worker.mem t.store id >>= function
        | false -> forget ()
        | true ->
          Lwt_unix.sleep is_dead >>= fun () ->
          loop ()
    in
    Lwt.async loop

  let add_worker t w =
    debug "add worker %s" (Id.to_string @@ Worker.id w);
    t.workers <- WSet.add w t.workers;
    Lwt_condition.broadcast t.cond ()

  let watch_worker_status t w =
    let id = Worker.id w in
    Store.Worker.watch_status t.store id (fun status ->
        let () = match status with
          | `Idle   -> add_worker t w
          | `Job _
          | `Task _ -> rem_worker t w
        in
        Lwt.return_unit
      )
    >|= fun cancel ->
    add_to_cancel t id cancel

  let add_worker_watches t w =
    watch_woker_ticks t w >>= fun () ->
    watch_worker_status t w

  let watch_workers t =
    Store.Worker.watch t.store (add_worker_watches t) >|= fun _cancel ->
    ()

  let peek t =
    if WSet.cardinal t.workers = 0 then None
    else Some (WSet.choose t.workers)

  let rec peek_s t =
    match peek t with
    | None   -> Lwt_condition.wait t.cond >>= fun () -> peek_s t
    | Some t -> Lwt.return t

  let create store =
    let cond = Lwt_condition.create () in
    let cancels = Hashtbl.create 12 in
    Store.Worker.list store >>=
    Lwt_list.fold_left_s (fun workers id ->
        Store.Worker.get store id >|= fun w ->
        WSet.add w workers
      ) WSet.empty
    >>= fun workers ->
    let t = { cond; store; cancels; workers } in
    Lwt_list.iter_p (add_worker_watches t) (WSet.elements workers) >|= fun () ->
    t

  let start store =
    debug "starting the work scheduler";
    create store >>= fun t ->
    watch_workers t >|= fun () ->
    t

end

let start store =
  XJob.start store    >>= fun j ->
  XTask.start store   >>= fun t ->
  XWorker.start store >|= fun w ->
  let peek_job host = XJob.peek_s j host >|= fun j -> `Job j in
  let peek_task () = XTask.peek_s t >|= fun t -> `Task t in
  let schedule w =
    let host = Worker.host w in
    let wid = Worker.id w in
    Lwt.pick [peek_job host; peek_task ()] >>= function
    | `Job j  -> Store.Worker.start_job store wid (Job.id j)
    | `Task t -> Store.Worker.start_task store wid (Task.id t)
  in
  let rec loop () =
    XWorker.peek_s w >>= fun w ->
    Lwt.join [schedule w; loop ()]
  in
  Lwt.async loop

module type S = sig
  type t
  type value
  val start: Store.t -> t Lwt.t
  val list: t -> value list
  val peek: t -> value option
  val peek_s: t -> value Lwt.t
end

module Job = XJob
module Worker = XWorker
module Task = XTask
