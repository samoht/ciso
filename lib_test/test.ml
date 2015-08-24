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

let () =
  Irmin_unix.install_dir_polling_listener 0.2;
  Fmt.(set_style_renderer stdout `Ansi_tty)

let package_t: Package.t Alcotest.testable = (module Package)
let task_t: Task.t Alcotest.testable = (module Task)
let host_t: Host.t Alcotest.testable = (module Host)
let worker_t: Worker.t Alcotest.testable = (module Worker)
let switch_t: Switch.t Alcotest.testable = (module Switch)
let job_t: Job.t Alcotest.testable = (module Job)
let object_t: Object.t Alcotest.testable = (module Object)

let random_cstruct n =
  let t  = Unix.gettimeofday () in
  let cs = Cstruct.create 8 in
  Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.)) ;
  Nocrypto.Rng.reseed cs;
  Nocrypto.Rng.generate n

let random_ascii_string n =
  let s = Bytes.create n in
  for i = 0 to n-1 do
    Bytes.set s i (Char.chr (Random.int 128))
  done;
  s

let to_str codec v =
  let b = Buffer.create 64 in
  let e = Jsonm.encoder (`Buffer b) in
  let e = Jsont.encoder e codec v in
  match Jsont.encode e with
  | `Ok      -> Buffer.contents b
  | `Partial -> assert false

let of_str codec s =
  let e = Jsonm.decoder (`String s) in
  let e = Jsont.decoder e codec in
  match Jsont.decode e with
  | `Ok (_, v)    -> v
  | `Await        -> assert false
  | `Error (_, e) ->
    invalid_arg (Jsont.error_to_string e)

let json codec v =
  let s = to_str codec v in
  Fmt.(pf stdout) "%s\n" s;
  of_str codec s

let p1 = Package.create "foo"
let p2 = Package.create "foo" ~version:"bar"

let simple_package () =
  List.iter (fun p ->
      let name = Package.to_string p in
      Alcotest.(check package_t) name p1 (json Package.json p1)
    ) [p1; p2]

let t1 = Task.create [p1; p2]

let simple_task () =
  List.iter (fun t ->
      let id = Id.to_string (Task.id t) in
      Alcotest.(check task_t) id t1 (json Task.json t1)
    ) [t1]

let hosts = Host.detect () :: Host.defaults

let simple_host () =
  List.iter (fun h ->
      let name = Fmt.to_to_string Host.pp h in
      Alcotest.(check host_t) name h (json Host.json h)
    ) hosts

let simple_switch () =
  List.iter (fun c ->
      let name = Fmt.to_to_string Switch.pp c in
      Alcotest.(check switch_t) name c (json Switch.json c)
    ) Switch.defaults

let simple_worker () =
  List.iter (fun w ->
      let name = Id.to_string (Worker.id w) in
      Fmt.pf Fmt.stdout "%a\n%!" Worker.pp w;
      Alcotest.(check worker_t) name w (json Worker.json w)
    ) (List.map Worker.create hosts)

let jobs =
  let info opam url =
    Package.info ~opam:(Cstruct.of_string opam) ~url:(Cstruct.of_string url)
  in
  let pkgs = [
    (p1, info "build: [make]" "url: http://example.com");
    (p2, info "build: [make test]" "url: git://example.com");
  ] in
  List.fold_left (fun acc h ->
      List.fold_left (fun acc c ->
          let inputs = List.map Job.id acc in
          Job.create ~inputs h c pkgs :: acc
        ) acc Switch.defaults
    ) [] hosts

let simple_job () =
  List.iter (fun j ->
      let name = Id.to_string (Job.id j) in
      Alcotest.(check job_t) name j (json Job.json j)
    ) jobs

let obj () =
  let file name contents = name, Digest.string contents in
  let files = [
    file "foo.ml"  "let x = 3";
    file "foo.cmo" (random_ascii_string 1024)
  ] in
  Object.(archive files (random_cstruct 2049))

let lines n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (random_ascii_string 80 :: acc) (n-1)
  in
  let str = String.concat "\n" (aux [] n) in
  Cstruct.of_string str

let objects = [
  obj ();
  obj ();
  obj ();
  Object.file "foo" (lines 10);
  Object.file "bar" (lines 100);
]

let simple_object () =
  List.iter (fun o ->
      let id = Id.to_string (Object.id o) in
      Alcotest.(check object_t) id o (json Object.json o)
    ) objects

let store () =
  let _ = Sys.command "rm -rf /tmp/ciso-tests" in
  Store.local ~root:"/tmp/ciso-tests" ()

open Lwt.Infix
let run f =
  try Lwt_main.run (f ()) with e ->
    Fmt.(pf stdout "%a %s" (styled `Red string) "ERROR" (Printexc.to_string e))

let basic_tasks () =
  let test () =
    store () >>= fun s ->
    Scheduler.Task.start s >>= fun t ->
    Alcotest.(check @@ list task_t) "0 tasks" [] (Scheduler.Task.list t);
    Lwt.return_unit
  in
  run test

let basic_jobs () =
  let test () =
    store () >>= fun s ->
    Scheduler.Job.start s >>= fun t ->
    Alcotest.(check @@ list job_t) "0 jobs" [] (Scheduler.Job.list t);
    Lwt.return_unit
  in
  run test

let basic_workers () =
  let test () =
    store () >>= fun s ->
    Scheduler.Worker.start s >>= fun t ->
    Alcotest.(check @@ list worker_t) "0 workers" [] (Scheduler.Worker.list t);
    Lwt.return_unit
  in
  run test

let simple = [
  "package"  , `Quick, simple_package;
  "task"     , `Quick, simple_task;
  "host"     , `Quick, simple_host;
  "switch"   , `Quick, simple_switch;
  "worker"   , `Quick, simple_worker;
  "job"      , `Quick, simple_job;
  "object"   , `Quick, simple_object;
]

let scheduler = [
  "basic tasks"  , `Quick, basic_tasks;
  "basic jobs"   , `Quick, basic_jobs;
  "basic workers", `Quick, basic_workers;
]

let () =
  Alcotest.run "ciso" [
    "simple"   , simple;
    "scheduler", scheduler;
  ]
