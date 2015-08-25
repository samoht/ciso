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

let package_c: Package.t Arg.converter =
  let parse str = `Ok (Package.of_string str) in
  let print ppf t = Package.pp ppf t in
  parse, print

let packages =
  let doc = "The package to install" in
  Arg.(value & pos_all package_c [] & info [] ~docv:"PKGS" ~doc)

let main =
  let master t packages =
    if packages = [] then ()
    else
      let task = Task.create packages in
      Lwt_main.run begin
        t >>= fun { store; _ } ->
        Store.Task.add store task
      end
  in
  Term.(pure master $ t $ packages),
  Term.info ~version:Version.current ~doc:"Add new tasks to CISO" "ciso-add"

let () =
  match Term.eval main with `Error _ -> exit 1 | _ -> exit 0
