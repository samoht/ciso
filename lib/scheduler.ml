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
let count name =
  let c = ref 0 in fun () -> incr c; name ^ "-" ^ string_of_int !c

let opt ppf =
  let none ppf () = Fmt.pf ppf "-" in
  Fmt.(option ~none) ppf

module XTask = struct

  module TMap = Map.Make(Task)

  type status = [ Task.status | `Await ]

  let pp_status ppf = function
    | `Await -> Fmt.string ppf "await"
    | #Task.status as x -> Task.pp_status ppf x

  type t = {
    cond: unit Lwt_condition.t;
    store: Store.t;                                            (* local store *)
    mutable tasks: status TMap.t;        (* new, dispatched and pending tasks *)
    mutable stop: unit -> unit Lwt.t;
    cancels: (Task.id, unit -> unit Lwt.t) Hashtbl.t;
  }

  let list t = TMap.fold (fun t _ acc -> t :: acc) t.tasks []

  let is_runnable t task = try TMap.find task t.tasks = `New with Not_found -> false

  let status t task =
    TMap.fold (fun t s -> function
        | None -> if Task.equal t task then Some s else None
        | x    -> x
      ) t.tasks None

  let remove_task t task =
    let id = Task.id task in
    debug "remove taks:%a" Id.pp id;
    Hashtbl.remove t.cancels id;
    t.tasks <- TMap.remove task t.tasks

  let update_status t task s =
    let old_s = status t task in
    if old_s <> Some s && not (old_s = Some `Await && s = `New) then (
      debug "task:%a: %a => %a"
        Id.pp (Task.id task) (opt pp_status ) old_s pp_status s;
      t.tasks <- TMap.add task s t.tasks;
      if s = `New then Lwt_condition.broadcast t.cond ()
    )

  (* FIXME: need to watch the related jobs status updates to update
     the task status. *)
  let watch_task_status t task =
    let id = Task.id task in
    let cancel = ref (fun () -> Lwt.return_unit) in
    Store.Task.watch_status t.store id (function
        | `Success | `Failure ->
          remove_task t task;
          !cancel ()
        | `New | `Pending | `Dispatched _ as s ->
          update_status t task s;
          Lwt.return_unit
        | `Cancelled -> todo "Task.cancel"
      ) >|= fun c ->
    assert (not (Hashtbl.mem t.cancels id));
    Hashtbl.add t.cancels id c;
    cancel := c

  let add_task t task =
    if TMap.mem task t.tasks then Lwt.return_unit
    else (
      let id = Task.id task in
      Store.Task.status t.store id >>= function
      | `Cancelled -> todo "cancelled tasks"
      | `Success | `Failure -> Lwt.return_unit
      | `New | `Pending | `Dispatched _ as s ->
        update_status t task s;
        watch_task_status t task
    )

  let empty store =
    let cond = Lwt_condition.create () in
    let stop () = Lwt.return_unit in
    let cancels = Hashtbl.create 16 in
    let tasks = TMap.empty in
    { cond; store; tasks; stop; cancels; }

  let new_tasks t =
    TMap.fold (fun v status acc -> match status with
        | `New -> v :: acc
        | _    -> acc
      ) t.tasks []

  let peek t = match new_tasks t with
    | []   -> None
    | h::_ -> Some h

  let count_task = count "task"

  let peek_s t =
    let name = count_task () in
    let rec aux () = match peek t with
      | None ->
        debug "[%s] waiting for a new task ..." name;
        Lwt_condition.wait t.cond >>= fun () ->
        aux ()
      | Some ta ->
        let id = Task.id ta in
        debug "[%s] task:%a is new!" name Id.pp id;
        update_status t ta `Await;
        Lwt.return ta
    in
    aux ()

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

  module JSet = Set.Make(Job)
  module JMap = Map.Make(Job)

  type status = [ Job.status | `Await ]

  let pp_status ppf = function
    | `Await -> Fmt.string ppf "await"
    | #Job.status as x -> Job.pp_status ppf x

  (* FIXME: add formard deps caching, a la TUP. *)
  type t = {
    cond  : unit Lwt_condition.t;
    store : Store.t;
    mutable jobs: status JMap.t;
    mutable stop: unit -> unit Lwt.t;
    mutable cancels: (unit -> unit Lwt.t) JMap.t;
  }

  let list t = JMap.fold (fun k _ acc -> k :: acc) t.jobs []

  let runnable_jobs t h =
    JMap.fold (fun job status acc ->
        if not (Host.equal (Job.host job) h) then acc
        else match status with
          | `Runnable -> job :: acc
          | _ -> acc
      ) t.jobs []

  let is_runnable t j = try JMap.find j t.jobs = `Runnable with Not_found -> false

  let status t job =
    JMap.fold (fun j s -> function
        | None -> if Job.equal j job then Some s else None
        | x    -> x
      ) t.jobs None

  let remove_job t j = t.jobs <- JMap.remove j t.jobs

  let update_status t j s =
    let old_s = status t j in
    if old_s <> Some s && not (old_s = Some `Await && s = `Runnable) then (
      debug "job:%a: %a => %a"
        Id.pp (Job.id j) (opt pp_status) old_s pp_status s;
      t.jobs <- JMap.add j s t.jobs;
      if s = `Runnable then Lwt_condition.broadcast t.cond ()
    )

  let peek t host = match runnable_jobs t host with
    | []   -> None
    | h::_ -> Some h

  let count_job = count "job"

  let peek_s t host =
    let name = count_job () in
    let rec aux () = match peek t host with
      | Some j ->
        let id = Job.id j in
        debug "[%s] job:%a can run on %s." name Id.pp id (Host.short host);
        update_status t j `Await;
        Lwt.return j
      | None   ->
        debug "[%s] waiting for a job on %s ..." name (Host.short host);
        Lwt_condition.wait t.cond >>= fun () ->
        aux ()
    in
    aux ()

  let status t id =
    JMap.fold (fun j status -> function
        | None -> if Id.equal (Job.id j) id then Some status else None
        | s    -> s
      ) t.jobs None

  (* FIXME: use rev-deps to make it fast (a al TUP). *)
  let update_runnable t =
    let jobs = ref [] in
    JMap.iter (fun job -> function
        | `Pending ->
          let inputs = Job.inputs job in
          if List.for_all (fun j -> status t j = Some `Success) inputs then
            jobs := job :: !jobs
        | _ -> ()
      ) t.jobs;
    Lwt_list.iter_p (fun job ->
        Store.Job.runnable t.store (Job.id job) >|= fun () ->
        update_status t job `Runnable
      ) !jobs

  let watch_job_status t j =
    let id = Job.id j in
    let cancel = ref (fun () -> Lwt.return_unit) in
    Store.Job.watch_status t.store id (function
        | `Cancelled -> todo "job cancelled"
        | `Pending | `Started  as s ->
          update_status t j s;
          Lwt.return_unit
        | `Runnable -> Lwt.return_unit
        | `Success | `Failure  ->
          remove_job t j;
          !cancel ()
      ) >|= fun c ->
    assert (not (JMap.mem j t.cancels));
    t.cancels <- JMap.add j c t.cancels;
    cancel := c

  let add_job t job =
    if JMap.mem job t.jobs then Lwt.return_unit
    else (
      Store.Job.status t.store (Job.id job) >>= function
      | `Cancelled -> todo "cancelled job"
      | `Failure | `Success -> Lwt.return_unit
      | `Pending | `Started | `Runnable as s ->
        update_status t job s;
        update_runnable t >>= fun () ->
        watch_job_status t job
    )

  let watch_jobs t = Store.Job.watch t.store (add_job t)

  let empty store =
    let stop () = Lwt.return_unit in
    { store; stop;
      cond = Lwt_condition.create ();
      jobs = JMap.empty;
      cancels  = JMap.empty; }

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
    let cancels = JMap.fold (fun _ c acc -> c :: acc) t.cancels [] in
    Lwt_list.iter_p (fun f -> f ()) cancels

end

module XWorker = struct

  module WSet = Set.Make(Worker)
  module WMap = Map.Make(Worker)

  type status = [ Worker.status | `Await ]

  let pp_status ppf = function
    | `Await -> Fmt.string ppf "await"
    | #Worker.status as x -> Worker.pp_status ppf x

  type t = {
    cond   : unit Lwt_condition.t;
    store  : Store.t;
    cancels: (Worker.id, (unit -> unit Lwt.t) list) Hashtbl.t;
    mutable stop   : unit -> unit Lwt.t;    (* stop the main scheduler watch. *)
    mutable workers: status WMap.t;                       (* all the workers. *)
  }

  let list t = WMap.fold (fun e _ l -> e :: l)  t.workers []

  let cancels t id = try Hashtbl.find t.cancels id with Not_found -> []

  let add_to_cancel t id f =
    let cs = cancels t id in
    Hashtbl.replace t.cancels id (f :: cs)

  (* reset the job a dead worker was working on. This will trigger the
     job scheduler to distribute it to someone else. *)
  let reset_job t = function
    | `Job id  -> Store.Job.pending t.store id
    | `Task id -> Store.Task.reset t.store id
    | `Await
    | `Idle    -> Lwt.return_unit

  let cancel_worker t id =
    let fs = cancels t id in
    Hashtbl.remove t.cancels id;
    Lwt_list.iter_p (fun f -> f ()) fs

  let not_ f x = not (f x)
  let has_id id w = Id.equal id (Worker.id w)

  let status t id =
    WMap.fold (fun w s -> function
        | None -> if has_id id w then Some s else None
        | x    -> x
      ) t.workers None

  let remove_worker t id =
    let not_id w _ = not_ (has_id id) w in
    match status t id with
    | None   -> Lwt.return_unit
    | Some s ->
      debug "remove worker:%a" Id.pp id;
      t.workers <- WMap.filter not_id t.workers;
      cancel_worker t id >>= fun () ->
      reset_job t s

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
      debug "worker:%a is dead!" Id.pp id;
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

  let update_status t w s =
    let old_s = status t (Worker.id w) in
    if Some s <> old_s && not (old_s = Some `Await && s = `Idle) then (
      debug "worker:%a: %a => %a"
        Id.pp (Worker.id w) (opt pp_status) old_s pp_status s;
      t.workers <- WMap.add w s t.workers;
      if s = `Idle then Lwt_condition.broadcast t.cond ()
    )

  let watch_worker_status t w =
    let id = Worker.id w in
    Store.Worker.watch_status t.store id (function
        | None   -> remove_worker t id
        | Some s -> update_status t w (s :> status); Lwt.return_unit
      ) >|= fun cancel ->
    add_to_cancel t id cancel

  let add_worker t w =
    if WMap.mem w t.workers then Lwt.return_unit
    else (
      let id = Worker.id w in
      Store.Worker.status t.store id >>= function
      | None   ->
        Store.Worker.forget t.store id
      | Some s ->
        update_status t w (s :> status);
        watch_woker_ticks t w >>= fun () ->
        watch_worker_status t w
    )

  let watch_workers t =
    Store.Worker.watch t.store (function
        | `Removed id -> remove_worker t id
        | `Added   w  -> add_worker t w
      )

  let idle_workers t =
    WMap.fold (fun w status acc ->
        match status with
        | `Idle -> w :: acc
        | _     -> acc
      ) t.workers []

  let is_runnable t w = try WMap.find w t.workers = `Idle with Not_found -> false

  let peek t = match idle_workers t with
    | []   -> None
    | h::_ -> Some h

  let count_worker = count "worker"

  let peek_s t =
    let name = count_worker () in
    let rec aux () = match peek t with
      | Some w ->
        let id = Worker.id w in
        debug "[%s] worker:%a ready!" name Id.pp id;
        update_status t w `Await;
        Lwt.return w
    | None   ->
      debug "[%s] waiting for a worker ..." name;
      Lwt_condition.wait t.cond >>= fun () ->
      aux ()
    in
    aux ()

  let empty store =
    let cond = Lwt_condition.create () in
    let cancels = Hashtbl.create 12 in
    let workers = WMap.empty in
    let stop () = Lwt.return_unit in
    { cond; store; cancels; workers; stop }

  let start store =
    debug "starting the work scheduler";
    let t = empty store in
    Store.Worker.list store >>=
    Lwt_list.filter_map_p (fun id ->
        Store.Worker.mem store id >>= function
        | true  -> Store.Worker.get store id    >|= fun x  -> Some x
        | false -> Store.Worker.forget store id >|= fun () -> None
      ) >>=
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

let fmt s id = s ^ " " ^ Id.to_string id

type t = { j: XJob.t; t: XTask.t; w: XWorker.t }

let job t = t.j
let task t = t.t
let worker t = t.w

let start store =
  XJob.start store    >>= fun j ->
  XTask.start store   >>= fun t ->
  XWorker.start store >|= fun w ->
  let peek_job host = XJob.peek_s j host >|= fun j -> `Job j in
  let peek_task () = XTask.peek_s t >|= fun t -> `Task t in
  let peek_worker () = XWorker.peek_s w in
  let dispatch worker job =
    let wid = Worker.id worker in
    match job with
    | `Job j  ->
      let id = Job.id j in
      Store.with_transaction store (fmt "starting job" id) (fun t ->
          debug "DISPATCH: job %a to worker %a" Id.pp id Id.pp wid;
          Store.Job.has_started t id >>= fun () ->
          Store.Worker.start_job t wid id
        )
    | `Task t ->
      let id = Task.id t in
      Store.with_transaction store (fmt "starting task" id) (fun t ->
          debug "DISPATCH: task %a to worker %a" Id.pp id Id.pp wid;
          Store.Task.dispatch_to t id wid >>= fun () ->
          Store.Worker.start_task t wid id
        )
  in
  let rec loop () =
    peek_worker () >>= fun worker ->
    debug "DISPATCH: worker:%a is available" Id.pp (Worker.id worker);
    let host = Worker.host worker in
    Lwt.pick [peek_job host; peek_task ()] >>= fun job ->
    dispatch worker job >>= fun () ->
    loop ()
  in
  Lwt.async loop;
  { j; t; w }

let stop t =
  Lwt.join [
    XJob.stop t.j;
    XWorker.stop t.w;
    XTask.stop t.t;
  ]

module type S = sig
  type t
  type value
  val start: Store.t -> t Lwt.t
  val stop: t -> unit Lwt.t
  val list: t -> value list
  val peek: t -> value option
  val peek_s: t -> value Lwt.t
  val is_runnable: t -> value -> bool
end

module Job = XJob
module Worker = XWorker
module Task = XTask
