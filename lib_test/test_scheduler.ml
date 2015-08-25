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
    Alcotest.(check task_status_t) "status" `New s;
    Scheduler.Task.stop t

  in
  run test

let basic_jobs () =
  let test () =
    let check_roots t =
      let roots = List.filter (fun j -> Job.inputs j = []) jobs in
      List.iter (fun h ->
          let root =
            try List.find (fun j -> Host.equal h (Job.host j)) roots
            with Not_found ->
              Alcotest.fail (Fmt.strf "no root for host %a" Host.pp h)
          in
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

let task_scheduler () =
  let test () =
    store () >>= fun s ->

    let check ~section sched msg expected =
      let msg = Printf.sprintf "[%s] %s" section msg in
      let tasks = Scheduler.Task.list (Scheduler.task sched) in
      Alcotest.(check @@ tasks_t) "t1 is monitored" [t1] tasks;
      Store.Task.status s (Task.id t1) >>= fun status ->
      Alcotest.(check task_status_t) msg expected status;
      Lwt.return_unit
    in

    (* - add a task
       - start the scheduler
       - add a worker: the task is picked-up
       - kill the  worker: the task is new again *)
    Store.Task.add s t1  >>= fun () ->
    Scheduler.start s    >>= fun scheduler ->
    let check = check ~section:"task -> scheduler -> worker" scheduler in

    check "init" `New >>= fun () ->

    Store.Worker.add s w1 >>= fun () ->
    sleep () >>= fun () ->
    check "start" (`Dispatched (Worker.id w1, `Pending)) >>= fun () ->

    Store.Worker.forget s (Worker.id w1) >>= fun () ->
    sleep () >>= fun () ->
    check "forget" `New >>= fun () ->

    Scheduler.stop scheduler
  in
  run test

let suite = [
  "basic tasks"  , `Quick, basic_tasks;
  "basic jobs"   , `Quick, basic_jobs;
  "basic workers", `Quick, basic_workers;
  "task schduler", `Quick, task_scheduler;
]
