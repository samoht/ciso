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



open Cmdliner
open Lwt.Infix
include Ciso_common

let one pp ppf (id, v) =
  match v with
  | None   ->
    Fmt.(styled `Cyan Id.pp ppf) id;
    Fmt.(pf ppf " --\n")
  | Some v ->
    Fmt.box ~indent:2 pp ppf v

let block title pp =
  let bar ppf = Fmt.pf ppf "\n=== %s ===\n\n" in
  Fmt.(styled `Yellow bar stdout) title;
  Fmt.(list (one pp) stdout)

let find get t x =
  Lwt.catch
    (fun () -> get t x >|= fun y -> x, Some y)
    (function Invalid_argument _ -> Lwt.return (x, None) | e -> Lwt.fail e)

let kind =
  let doc = "Select the kind of objects to list." in
  let choices = [
    "tasks"  , `Task;
    "workers", `Worker;
    "jobs"   , `Job;
    "hosts"  , `Host
  ] in
  Arg.(value & opt (enum choices) `Task & info ["k";"kind"] ~docv:"KIND" ~doc)

let main =
  let master t kind =
    Lwt_main.run begin
      t >>= fun { store; _ } ->
      match kind with
      | `Task ->
        Store.Task.list store >>= fun task_ids ->
        Lwt_list.map_p (find Store.Task.get store) task_ids >|= fun tasks ->
        block "Tasks" Task.pp tasks
      | `Job ->
        Store.Job.list store >>= fun job_ids ->
        Lwt_list.map_p (find Store.Job.get store) job_ids >|= fun jobs ->
        block "Jobs" Job.pp jobs
      | `Worker ->
        Store.Worker.list store >>= fun worker_ids ->
        Lwt_list.map_p (find Store.Worker.get store) worker_ids >|= fun workers ->
        block "Workers" Worker.pp workers
      | `Host ->
        let hosts = List.map (fun h -> Host.id h, Some h) Host.defaults in
        block "Hosts" Host.pp hosts;
        Lwt.return_unit
    end
  in
  Term.(pure master $ t $ kind),
  Term.info ~version:Version.current ~doc:"Add new tasks to CISO" "ciso-add"

let () =
  match Term.eval main with `Error _ -> exit 1 | _ -> exit 0
