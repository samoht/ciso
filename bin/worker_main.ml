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

let () =
  Irmin_unix.install_dir_polling_listener 0.2;
  Fmt.(set_style_renderer stdout `Ansi_tty)

let root =
  Arg.(value & opt (some string) None & info ["local"]
         ~docv:"DIR" ~doc:"the path to the local Irmin store.")

let uri =
  Arg.(value & opt (some string) None & info ["global"]
         ~docv:"URI" ~doc:"the URI of the global Irmin store.")

let with_default d f = function None -> d | Some x -> f x

let main =
  let worker root uri =
    let root =
      let default = Filename.(concat (get_temp_dir_name ()) "ciso-worker") in
      with_default default (fun x -> x) root
    in
    let uri =
      with_default (Uri.of_string "http://127.0.0.0:8080") Uri.of_string uri
    in
    Fmt.(pf stdout "%a: %s\n%a: %s\n%!"
           (styled `Cyan string) "root" root
           (styled `Cyan string) "uri " (Uri.to_string uri));
    Lwt_main.run (Ciso_worker.run ~root uri)
  in
  Term.(pure worker $ root $ uri),
  Term.info ~doc:"CISO worker" "ciso-worker"

let () =
  match Term.eval main with `Error _ -> exit 1 | _ -> exit 0
