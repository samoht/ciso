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
let todo f = err "TODO %s" f

module XTask = struct

  module TSet = Set.Make(Task)

  type t = {
    cond: unit Lwt_condition.t;
    store: Store.t;                                            (* local store *)
    mutable new_tasks: TSet.t;                                   (* new tasks *)
    mutable pending_tasks: TSet.t;                           (* pending tasks *)
    cancels: (Task.id, unit -> unit Lwt.t) Hashtbl.t;
    mutable stop: unit -> unit Lwt.t;
  }

  let list t = TSet.(elements (union t.new_tasks t.pending_tasks))

  let remove_task t task =
    let id = Task.id task in
    debug "remove taks %s" (Id.to_string id);
    Hashtbl.remove t.cancels id;
    t.new_tasks <- TSet.remove task t.new_tasks;
    t.pending_tasks <- TSet.remove task t.pending_tasks

  let set_new t task =
    t.new_tasks <- TSet.add task t.new_tasks;
    t.pending_tasks <- TSet.remove task t.pending_tasks

  let set_pending t task =
    t.new_tasks <- TSet.remove task t.new_tasks;
    t.pending_tasks <- TSet.add task t.pending_tasks

  (* FIXME: need to watch the related jobs status updates to update
     the task status. *)
  let watch_task_status t task =
    let id = Task.id task in
    let cancel = ref (fun () -> Lwt.return_unit) in
    Store.Task.watch_status t.store id (function
        | `Pending   -> set_pending t task; Lwt.return_unit
        | `New       -> set_new t task; Lwt.return_unit
        | `Cancelled -> todo "Task.cancel"
        | `Success
        | `Failure   -> remove_task t task; !cancel ()
      ) >|= fun c ->
    assert (not (Hashtbl.mem t.cancels id));
    Hashtbl.add t.cancels id c;
    cancel := c

  let add_task t task =
    if TSet.mem task t.new_tasks || TSet.mem task t.pending_tasks then
      Lwt.return_unit
    else (
      let id = Task.id task in
      Store.Task.status t.store id >>= function
      | `Cancelled -> todo "cancelled tasks"
      | `Success | `Failure -> Lwt.return_unit
      | `Pending | `New as s ->
        debug "add task %s" (Id.to_string id);
        if s = `New then set_new t task else set_pending t task;
        watch_task_status t task >|= fun () ->
        Lwt_condition.broadcast t.cond ()
    )

  let empty store =
    let cond = Lwt_condition.create () in
    let stop () = Lwt.return_unit in
    let cancels = Hashtbl.create 16 in
    let new_tasks = TSet.empty in
    let pending_tasks = TSet.empty in
    { cond; store; new_tasks; pending_tasks; stop; cancels; }

  let peek t =
    if TSet.cardinal t.new_tasks = 0 then None
    else Some (TSet.choose t.new_tasks)

  let rec peek_s t =
    match peek t with
    | None   ->
      debug "waiting for a new task to appear ...";
      Lwt_condition.wait t.cond >>= fun () ->
      debug "there's a new task!";
      peek_s t
    | Some h -> Lwt.return h

  let watch_task t = Store.Task.watch t.store (add_task t)

  let start store =
    debug "starting the task scheduler";
    let t = empty store in
    Store.Task.list store >>=
    Lwt_list.map_p (Store.Task.get store) >>=
    Lwt_list.iter_p (add_task t) >>= fun () ->
    watch_task t >|= fun cancel ->
    t.stop <- cancel;
    t

  let stop t =
    debug "stopping the task scheduler";
    t.stop () >>= fun () ->
    let cancels = Hashtbl.fold (fun _ v l -> v :: l) t.cancels [] in
    Lwt_list.iter_p (fun f -> f ()) cancels

end

module XJob = struct

  type t = {
    cond  : unit Lwt_condition.t;
    store : Store.t;
    jobs  : (Job.id, Job.t) Hashtbl.t;
    status: (Job.id, Job.status) Hashtbl.t;
    cancels: (Job.id, unit -> unit Lwt.t) Hashtbl.t;
    mutable stop: unit -> unit Lwt.t;
  }

  let list t = Hashtbl.fold (fun _ v l -> v :: l) t.jobs []

  let status t j = try Hashtbl.find t.status j with Not_found -> `Pending

  let runnables t host =
    Hashtbl.fold (fun id j acc ->
        if status t id = `Runnable && Host.equal (Job.host j) host
        then j :: acc
        else acc
      ) t.jobs []

  let peek t host = match runnables t host with
    | []   -> None
    | h::_ -> Some h

  let rec peek_s t host =
    match peek t host with
    | Some jid -> Lwt.return jid
    | None     ->
      debug "waiting for a runnable job to appear ...";
      Lwt_condition.wait t.cond >>= fun () ->
      debug "there's a new runnable job!";
      peek_s t host

  (* FIXME: use rev-deps to make it fast (a al TUP). *)
  let update_runnable t =
    let jobs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) t.jobs [] in
    List.iter (fun (id, job) ->
        match status t id with
        | `Pending ->
          let inputs = Job.inputs job in
          if List.for_all (fun j -> status t j = `Success) inputs then (
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

  let watch_job_status t j =
    let id = Job.id j in
    let cancel = ref (fun () -> Lwt.return_unit) in
    Store.Job.watch_status t.store id (function
        | `Cancelled -> todo "job cancelled"
        | `Pending
        | `Running   (* we already watch it, so nothing to do *)
        | `Runnable  -> Lwt.return_unit
        | `Success
        | `Failure   -> remove_job t id; !cancel ()
      ) >|= fun c ->
    assert (not (Hashtbl.mem t.cancels id));
    Hashtbl.add t.cancels id c;
    cancel := c

  let add_job t job =
    if Hashtbl.mem t.jobs (Job.id job) then Lwt.return_unit
    else (
      Store.Job.status t.store (Job.id job) >>= function
      | `Cancelled -> todo "cancelled job"
      | `Failure | `Success -> Lwt.return_unit
      | `Pending | `Running | `Runnable  ->
        debug "add job %s" (Id.to_string @@ Job.id job);
        add_job_aux t job >>= fun () ->
        watch_job_status t job >|= fun () ->
        update_runnable t
    )

  let watch_jobs t = Store.Job.watch t.store (add_job t)

  let empty store =
    let stop () = Lwt.return_unit in
    { store; stop;
      cond    = Lwt_condition.create ();
      jobs     = Hashtbl.create 16;
      status   = Hashtbl.create 16;
      cancels  = Hashtbl.create 16; }

  let start store =
    debug "starting the job scheduler.";
    let t = empty store in
    Store.Job.list store >>=
    Lwt_list.map_p (Store.Job.get store) >>=
    Lwt_list.iter_p (add_job t) >>= fun () ->
    watch_jobs t >|= fun cancel ->
    t.stop <- cancel;
    t

  let stop t =
    debug "stoping the job scheduler";
    t.stop () >>= fun () ->
    let cancels = Hashtbl.fold (fun _ c acc -> c :: acc) t.cancels [] in
    Lwt_list.iter_p (fun f -> f ()) cancels

end

module XWorker = struct

  module WSet = Set.Make(Worker)

  type t = {
    cond   : unit Lwt_condition.t;
    store  : Store.t;
    cancels: (Worker.id, (unit -> unit Lwt.t) list) Hashtbl.t;
    mutable stop   : unit -> unit Lwt.t;    (* stop the main scheduler watch. *)
    mutable workers     : WSet.t;                         (* all the workers. *)
    mutable idle_workers: WSet.t;                            (* idle workers. *)
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
    | Some (`Job id)  -> Store.Job.pending t.store id
    | Some (`Task id) -> Store.Task.reset t.store id
    | _ -> Lwt.return_unit

  let cancel_worker t id =
    let fs = cancels t id in
    Hashtbl.remove t.cancels id;
    Lwt_list.iter_p (fun f -> f ()) fs

  let remove_worker t id =
    let has_id w = Id.equal id (Worker.id w) in
    let not_ f x = not (f x) in
    if not (WSet.exists has_id t.workers) then Lwt.return_unit
    else (
      debug "remove worker %s" (Id.to_string id);
      t.workers <- WSet.filter (not_ has_id) t.workers;
      t.idle_workers <- WSet.filter (not_ has_id) t.idle_workers;
      cancel_worker t id >>= fun () ->
      reset_job t id
    )

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
      remove_worker t id >>= fun () ->
      Store.Worker.forget t.store id
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

  let add_idle_worker t w =
    if not (WSet.mem w t.workers) then (
      debug "%s is idle!" (Id.to_string @@ Worker.id w);
      t.idle_workers <- WSet.add w t.idle_workers;
      Lwt_condition.broadcast t.cond ()
    )

  let remove_idle_worket t w =
    debug "%s is not idle!" (Id.to_string @@ Worker.id w);
    t.idle_workers <- WSet.remove w t.idle_workers

  let watch_worker_status t w =
    let id = Worker.id w in
    Store.Worker.watch_status t.store id (fun status ->
        match status with
        | Some `Idle     -> add_idle_worker t w; Lwt.return_unit
        | Some (`Job _)
        | Some (`Task _) -> remove_idle_worket t w; Lwt.return_unit
        | None           -> remove_worker t id
      )
    >|= fun cancel ->
    add_to_cancel t id cancel

  let add_worker t w =
    if WSet.mem w t.workers then Lwt.return_unit
    else (
      let id = Worker.id w in
      debug "add worker %s" (Id.to_string id);
      t.workers <- WSet.add w t.workers;
      watch_woker_ticks t w   >>= fun () ->
      watch_worker_status t w >>= fun () ->
      Store.Worker.status t.store id >>= function
      | Some `Idle -> add_idle_worker t w; Lwt.return_unit
      | None       -> remove_worker t id
      | _          -> Lwt.return_unit
    )

  let watch_workers t =
    Store.Worker.watch t.store (function
        | `Removed id -> remove_worker t id
        | `Added   w  -> add_worker t w
      )

  let peek t =
    if WSet.cardinal t.workers = 0 then None
    else Some (WSet.choose t.workers)

  let rec peek_s t =
    match peek t with
    | Some t -> Lwt.return t
    | None   ->
      debug "waiting for an idle worker ...";
      Lwt_condition.wait t.cond >>= fun () ->
      debug "there's a new idle worker!";
      peek_s t

  let empty store =
    let cond = Lwt_condition.create () in
    let cancels = Hashtbl.create 12 in
    let idle_workers = WSet.empty in
    let workers = WSet.empty in
    let stop () = Lwt.return_unit in
    { cond; store; cancels; idle_workers; workers; stop }

  let start store =
    debug "starting the work scheduler";
    let t = empty store in
    Store.Worker.list store >>=
    Lwt_list.map_s (Store.Worker.get store) >>=
    Lwt_list.iter_p (add_worker t) >>= fun () ->
    watch_workers t >|= fun cancel ->
    t.stop <- cancel;
    t

  let stop t =
    debug "stoping the work scheduler";
    t.stop () >>= fun () ->
    let cancels = Hashtbl.fold (fun _ l acc -> l @ acc) t.cancels [] in
    Lwt_list.iter_p (fun f -> f ()) cancels

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
  val stop: t -> unit Lwt.t
  val list: t -> value list
  val peek: t -> value option
  val peek_s: t -> value Lwt.t
end

module Job = XJob
module Worker = XWorker
module Task = XTask
