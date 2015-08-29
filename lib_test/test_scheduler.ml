(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Test_common

let ts = 0.01

let () =
  Irmin_unix.install_dir_polling_listener ts;
  Fmt.(set_style_renderer stdout `Ansi_tty)

let sleep () = Lwt_unix.sleep (ts *. 2.)

let store () =
  let _ = Sys.command "rm -rf /tmp/ciso-tests" in
  Store.local ~root:"/tmp/ciso-tests" ()

open Lwt.Infix

let run f =
  let err e =
    Fmt.(pf stdout "%!");
    Fmt.(pf stderr "%!");
    flush stdout;
    flush stderr;
    raise e
  in
  Lwt.async_exception_hook := err;
  let protect f () = try f () with e -> Lwt.fail e in
  Lwt_main.run (Lwt.catch (protect f) err)

let basic_tasks () =
  let test () =
    store () >>= fun s ->
    Scheduler.Task.start s >>= fun t ->
    Alcotest.(check @@ tasks_t) "0 tasks" [] (Scheduler.Task.list t);
    Store.Task.add s t1 >>= fun () ->
    sleep () >>= fun () ->

    Alcotest.(check @@ tasks_t) "1 task" [t1] (Scheduler.Task.list t);
    Scheduler.Task.stop t >>= fun () ->

    Scheduler.Task.start s >>= fun t ->
    Alcotest.(check @@ tasks_t) "2 task" [t1] (Scheduler.Task.list t);
    Store.Task.status s (Task.id t1) >>= fun s ->
    Alcotest.(check @@ option task_status_t) "status" (Some `New) s;
    Scheduler.Task.stop t

  in
  run test

let basic_jobs () =
  let test () =
    let check_roots t =
      List.iter (fun h ->
          let root = job_root h in
          Alcotest.(check @@ option job_t) "root"
            (Some root) (Scheduler.Job.peek t h)
        ) hosts
    in

    store () >>= fun s ->
    Scheduler.Job.start s >>= fun t ->
    Alcotest.(check @@ list job_t) "0 jobs" [] (Scheduler.Job.list t);
    Lwt_list.iter_p (Store.Job.add s) jobs >>= fun () ->
    sleep () >>= fun () ->

    Alcotest.(check @@ jobs_t) "jobs" jobs (Scheduler.Job.list t);
    check_roots t;
    Scheduler.Job.stop t >>= fun () ->

    Scheduler.Job.start s >>= fun t ->
    Alcotest.(check @@ jobs_t) "jobs" jobs (Scheduler.Job.list t);
    check_roots t;
    Scheduler.Job.stop t

  in
  run test

let basic_workers () =
  let test () =
    store () >>= fun s ->
    Scheduler.Worker.start s >>= fun t ->
    Alcotest.(check @@ list worker_t) "0 workers" [] (Scheduler.Worker.list t);
    Lwt_list.iter_p (Store.Worker.add s) workers >>= fun () ->
    sleep () >>= fun () ->
    Alcotest.(check @@ workers_t) "workers" workers (Scheduler.Worker.list t);
    let w = match Scheduler.Worker.peek t with
      | None   -> Alcotest.fail "worker peek"
      | Some w -> w
    in
    Alcotest.(check bool_t) "worker" true (List.mem w workers);
    Scheduler.Worker.stop t >>= fun () ->

    Scheduler.Worker.start s >>= fun t ->
    Alcotest.(check @@ workers_t) "workers" workers (Scheduler.Worker.list t);
    Lwt_list.iter_s (fun w -> Store.Worker.forget s (Worker.id w)) workers
    >>= fun () ->
    sleep () >>= fun () ->

    Alcotest.(check @@ list worker_t) "0 workers again"
      [] (Scheduler.Worker.list t);
    Scheduler.Worker.stop t

  in
  run test

let task_check s ~section sched msg expected =
  let msg = Printf.sprintf "[%s] %s" section msg in
  let tasks = Scheduler.Task.list (Scheduler.task sched) in
  Alcotest.(check @@ tasks_t) "t1 is monitored" [t1] tasks;
  Store.Task.status s (Task.id t1) >>= fun status ->
  Alcotest.(check @@ option task_status_t) msg (Some expected) status;
  Lwt.return_unit

(* - add a task
   - start the scheduler
   - add a worker: the task is picked-up
   - kill the worker: the task is new again *)
let task_scheduler_1 () =
  let test () =
    store () >>= fun s ->
    Store.Task.add s t1  >>= fun () ->
    Scheduler.start s    >>= fun scheduler ->
    let check = task_check s ~section:"task -> scheduler -> worker" scheduler in
    check "init" `New >>= fun () ->
    Store.Worker.add s wj1 >>= fun () ->
    sleep () >>= fun () ->
    check "init" `New >>= fun () ->
    Store.Worker.add s wt1 >>= fun () ->
    sleep () >>= fun () ->
    check "start" (`Dispatched (Worker.id wt1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wt1) >>= fun () ->
    sleep () >>= fun () ->
    check "forget" `New >>= fun () ->
    Scheduler.stop scheduler
  in
  run test

(* - add a worker
   - start the scheduler
   - add a task: the task is picked-up
   - kill the worker: the task is new again *)
let task_scheduler_2 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wt1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = task_check s ~section:"worker -> scheduler -> task" scheduler in
    let tasks = Scheduler.Task.list (Scheduler.task scheduler) in
    Alcotest.(check @@ tasks_t) "t1 is not monitored" [] tasks;
    Store.Task.add s t1 >>= fun () ->
    sleep () >>= fun () ->
    check "start" (`Dispatched (Worker.id wt1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wt1) >>= fun () ->
    sleep () >>= fun () ->
    check "forget" `New >>= fun () ->
    Scheduler.stop scheduler
  in
  run test

(* - add a worker
   - add a task
   - start the scheduler
   - the task is picked-up
   - kill the worker: the task is new again *)
let task_scheduler_3 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wt1 >>= fun () ->
    Store.Task.add s t1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = task_check s ~section:"worker -> task -> scheduler" scheduler in
    sleep () >>= fun () ->
    check "start" (`Dispatched (Worker.id wt1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wt1) >>= fun () ->
    sleep () >>= fun () ->
    check "forget" `New >>= fun () ->
    Scheduler.stop scheduler
  in
  run test

let job_check s ~section sched msg expected =
  let msg = Printf.sprintf "[%s] %s" section msg in
  let jobs = Scheduler.Job.list (Scheduler.job sched) in
  Alcotest.(check @@ jobs_t) "jr1 is monitored" [jr1] jobs;
  Store.Job.status s (Job.id jr1) >>= fun status ->
  Alcotest.(check @@ option job_status_t) msg (Some expected) status;
  Lwt.return_unit

(* - add a job
   - start the scheduler
   - add a worker: the job is picked-up
   - kill the worker: the job is pending again *)
let job_scheduler_1 () =
  let test () =
    store () >>= fun s ->
    Store.Job.add s jr1  >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = job_check s ~section:"job -> scheduler -> worker" scheduler in
    check "init" `Runnable >>= fun () ->
    Store.Worker.add s wt1 >>= fun () ->
    sleep () >>= fun () ->
    check "init" `Runnable >>= fun () ->
    Store.Worker.add s wj1 >>= fun () ->
    sleep () >>= fun () ->
    check "start" (`Dispatched (Worker.id wj1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wj1) >>= fun () ->
    sleep () >>= fun () ->
    check "forget" `Runnable >>= fun () ->
    Scheduler.stop scheduler
  in
  run test

(* - add a worker
   - start the scheduler
   - add a job: the job is picked-up
   - kill the worker: the job is new again *)
let job_scheduler_2 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wj1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = job_check s ~section:"worker -> scheduler -> job" scheduler in
    let jobs = Scheduler.Job.list (Scheduler.job scheduler) in
    Alcotest.(check @@ jobs_t) "jr1 is not monitored" [] jobs;
    Store.Job.add s jr1 >>= fun () ->
    sleep () >>= fun () ->
    check "start" (`Dispatched (Worker.id wj1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wj1) >>= fun () ->
    sleep () >>= fun () ->
    check "forget" `Runnable >>= fun () ->
    Scheduler.stop scheduler
  in
  run test

(* - add a worker
   - add a job
   - start the scheduler
   - the job is picked-up
   - kill the worker: the job is new again *)
let job_scheduler_3 () =
  let test () =
    store () >>= fun s ->
    Store.Worker.add s wj1 >>= fun () ->
    Store.Job.add s jr1 >>= fun () ->
    Scheduler.start s >>= fun scheduler ->
    let check = job_check s ~section:"worker -> job -> scheduler" scheduler in
    sleep () >>= fun () ->
    check "start" (`Dispatched (Worker.id wj1, `Pending)) >>= fun () ->
    Store.Worker.forget s (Worker.id wj1) >>= fun () ->
    sleep () >>= fun () ->
    check "forget" `Runnable >>= fun () ->
    Scheduler.stop scheduler
  in
  run test

let suite = [
  "basic tasks"    , `Quick, basic_tasks;
  "basic jobs"     , `Quick, basic_jobs;
  "basic workers"  , `Quick, basic_workers;
  "task schduler 1", `Quick, task_scheduler_1;
  "task schduler 2", `Quick, task_scheduler_2;
  "task schduler 3", `Quick, task_scheduler_3;
  "job schduler 1" , `Quick, job_scheduler_1;
  "job schduler 2" , `Quick, job_scheduler_2;
  "job schduler 3" , `Quick, job_scheduler_3;
]
