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

let section = ref "worker"
let debug fmt = Gol.debug ~section:!section fmt

let () =
  (* FIXME: why this is even needed? *)
  Lwt_unix.set_default_async_method Lwt_unix.Async_none

type t = {
  worker   : Worker.t;                           (* the worker configuration. *)
  store    : Store.t;                                     (* the Irmin store. *)
  opam_root: string;                          (* the OPAM root of the worker. *)
  tick     : float;                  (* how often do the worker need to tick. *)
  mutable stop: unit -> unit Lwt.t;                    (* stop the scheduler. *)
  mutable alive: bool;
}

let opam t s = Opam.create ~root:t.opam_root s
let store t = t.store
let opam_root t = t.opam_root
let worker t = t.worker

let create ~tick ~store ~opam_root worker =
  let stop () = Lwt.return_unit in
  let alive = true in
  { worker; store; opam_root; tick; stop; alive }

let spawn f =
  Lwt_preemptive.detach (fun () -> Lwt.async (fun () ->  f ())) ()

let execution_loop t fn =
  Store.Worker.watch_status t.store (Worker.id t.worker) (function
      | Some s -> spawn (fun () -> fn t s)
      | None   ->
        Fmt.(pf stdout "%a" (styled `Cyan string) "Killed!\n");
        exit 1
    )

let rec heartbeat_loop t =
  debug "tick %.0fs" t.tick;
  let id = Worker.id t.worker in
  Store.Worker.tick t.store id (Unix.time ()) >>= fun () ->
  Lwt_unix.sleep t.tick >>= fun () ->
  if t.alive then heartbeat_loop t else Lwt.return_unit

let start fn ?(tick=5.) ~opam_root ~kind store =
  let w = Worker.create kind (Host.detect ()) in
  let t = create ~tick ~store ~opam_root w in
  Store.Worker.add t.store w >>= fun () ->
  Lwt.async (fun () -> heartbeat_loop t);
  execution_loop t fn >|= fun cancel ->
  t.stop <- cancel;
  t

let stop t =
  t.stop () >|= fun () ->
  t.alive <- false
