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

let store_root ~test () =
  if test then Lwt.return "." else
    let home = Sys.getenv "HOME" in
    let path = Filename.concat home "ci-store" in
    (if not (Sys.file_exists path) then Lwt_unix.mkdir path 0o770
     else if not (Sys.is_directory path) then begin
         Sys.remove path;
         Lwt_unix.mkdir path 0o770; end
     else Lwt.return ()) >>= fun () ->
    Lwt.return path

let server test uri =
  let module SMaker = Irmin_unix.Irmin_git.FS in
  let module S = SMaker(Irmin.Contents.String)
                       (Irmin.Tag.String)
                       (Irmin.Hash.SHA1) in
  let module Server = Irmin_unix.Irmin_http_server.Make(S) in

  (store_root ~test () >>= fun root ->
   let config = Irmin_git.config ~root ~bare:true () in
   S.create config Irmin_unix.task >>= fun t ->
   Server.listen (t "start server") (Uri.of_string uri))
  |> Lwt_main.run

let uri = Cmdliner.Arg.(
  required & pos 0 (some string) None & info []
    ~doc:"the address the data store will serve http://IP:PORT"
    ~docv:"ADDR")

let test = Cmdliner.Arg.(
  value & flag & info ["test"; "t"]
    ~doc:"when set, data store will be installed in the current directory")

let () = Cmdliner.Term.(
  let main =
    pure server $ test $ uri,
    info ~doc:"start the data store" "store" in
  match eval main with
    `Error _ -> exit 1
  | `Help | `Ok _ | `Version -> exit 0)
