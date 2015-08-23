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

module XTask = struct

  let err_invalid_task id = err "invalid task %s" (Id.to_string id)

  type t = {
    cond: unit Lwt_condition.t;
    store: Store.t;                                            (* local store *)
    mutable tasks: Task.t list;                                  (* new tasks *)
  }

  let create store =
    Store.Task.list store >>= fun ids ->
    Lwt_list.fold_left_s (fun tasks id ->
        Store.Task.find store id >>= function
        | None    -> err_invalid_task id
        | Some ta ->
          Store.Task.status store id >|= function
          | `New -> ta :: tasks
          | `Cancelled -> failwith "TODO"
          | `Pending | `Success | `Failure -> tasks
      ) [] ids
    >|= fun tasks ->
    let cond = Lwt_condition.create () in
    { cond; store; tasks }

  let remove_task t task =
    t.tasks <- List.filter (fun t -> not (Task.equal task t)) t.tasks

  let watch_task_status t task =
    let id = Task.id task in
    let cancel = ref (fun () -> Lwt.return_unit) in
    Store.Task.watch_status t.store id (function
        | `Cancelled -> failwith "TODO"
        | `New       -> Lwt.return_unit
        | `Pending
        | `Success
        | `Failure   -> remove_task t task; !cancel ()
      ) >|= fun c ->
    cancel := c

  let add_task t task =
    t.tasks <- task :: t.tasks;
    watch_task_status t task >|= fun () ->
    Lwt_condition.broadcast t.cond ()

  let rec find t =
    match t.tasks with
    | []   -> Lwt_condition.wait t.cond >>= fun () -> find t
    | h::_ -> Lwt.return h

  let pick t =
    match t.tasks with
    | []   -> None
    | h::_ -> Some h

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

  let create store =
    { store;
      cond    = Lwt_condition.create ();
      jobs     = Hashtbl.create 16;
      status   = Hashtbl.create 16; }

  let runnables t host =
    Hashtbl.fold (fun id j acc ->
        if Hashtbl.find t.status id = `Runnable && Host.equal (Job.host j) host
        then id :: acc
        else acc
      ) t.jobs []

  let rec find t host =
    let runnables = runnables t host in
    match runnables with
    | []       ->
      Lwt_condition.wait t.cond >>= fun () ->
      find t host
    | jid :: _ -> Lwt.return jid

  let pick t host =
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
            Hashtbl.replace t.status id `Runnable;
            (* FIXME: could broadcast only to the workers with the right
               host *)
            Lwt_condition.broadcast t.cond ()
          )
        | _ -> ()
      ) jobs

  let remove_job t id =
    Hashtbl.remove t.jobs id;
    Hashtbl.remove t.status id

  let rec add_job t job =
    let id = Job.id job in
    debug "add job %s" (Id.to_string id);
    let add_one () =
      Store.Job.mem t.store id >>= function
      | false ->
        Store.Job.add t.store job >|= fun () ->
        Hashtbl.add t.jobs id job;
        Hashtbl.add t.status id `Pending
      | true ->
        Store.Job.status t.store id >|= fun status ->
        Hashtbl.replace t.jobs id job;
        Hashtbl.replace t.status id status
    in
    add_one () >>= fun () ->
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

  let start store =
    debug "starting the job scheduler.";
    let t = create store in
    Store.Job.list t.store >>=
    Lwt_list.map_p (fun id ->
        Store.Job.find t.store id >|= function
        | None   -> err "invalid job" id
        | Some j -> j
      )
    >>=
    Lwt_list.iter_p (add_job t) >>= fun () ->
    watch_jobs t >|= fun () ->
    t

end

module XWorker = struct

  type t = {
    jobs   : XJob.t;
    tasks  : XTask.t;
    store  : Store.t;
    cancels: (Worker.id, (unit -> unit Lwt.t) list) Hashtbl.t;
  }

  let create jobs tasks store =
    { jobs; tasks; store; cancels = Hashtbl.create 12; }

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

  let watch_worker_status t w =
    let id = Worker.id w in
    let host = Worker.host w in
    Store.Worker.watch_status t.store id (function
        | `Idle -> begin
            debug "%s id idle, finding it a new job!" (Id.to_string id);
            match XTask.pick t.tasks with
            | None ->
              XJob.find t.jobs host >>= fun jid ->
              Store.Worker.start_job t.store id jid
            | Some task ->
              Store.Worker.start_task t.store id (Task.id task)
          end
        | _ -> Lwt.return_unit
      )
    >|= fun cancel ->
    add_to_cancel t id cancel

  let watch_workers t =
    Store.Worker.watch t.store (fun w ->
        watch_woker_ticks t w >>= fun () ->
        watch_worker_status t w
      ) >|=
    fun _cancel ->
    ()

  let start store =
    debug "starting the work scheduler";
    XTask.start store >>= fun t ->
    XJob.start store  >>= fun j ->
    let t = create j t store in
    watch_workers t >|= fun () ->
    t

end

let start store =
  XWorker.start store >|= fun _t ->
  ()

module type S = sig
  type t
  val start: Store.t -> t Lwt.t
end

module Job = XJob
module Worker = XWorker
module Task = XTask
