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

let (/) = Filename.concat

let tmp_dir = Filename.get_temp_dir_name ()

let task =
  let doc = "Start a task worker." in
  Arg.(value & flag & info ["task"] ~doc)

let start store opam_root = function
  | true  -> Task_worker.start ~opam_root store >>= block
  | false -> Job_worker.start ~opam_root store  >>= block

let mk_opam_root x =
  let return x = info "opam  " x; Lwt.return x in
  let x = match x with
    | Some r -> return r
    | None   ->
      config_file () >>= fun config ->
      match config "opam-root" with
      | None   -> return (tmp_dir / (Id.uuid `Worker |> Id.to_string))
      | Some r -> return r
  in
  x

let main =
  let worker store opam_root task =
    info "kind  " (if task then "task" else "job");
    Lwt_main.run begin
      store >>= fun store ->
      mk_opam_root opam_root >>= fun opam_root ->
      start store opam_root task
    end
  in
  Term.(global worker $ store $ opam_root $ task),
  term_info ~doc:"Start a new CISO worker" "ciso-worker"

let () =
  match Term.eval main with `Error _ -> exit 1 | _ -> exit 0
