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

let base =
  Arg.(required & pos 0 (some string) None & info []
         ~docv:"HOST" ~doc:"the uri string of master node")

let uri =
  Arg.(required & pos 1 (some string) None & info []
         ~docv:"URI" ~doc:"the uri string of data store")

let fresh =
  Arg.(value & flag & info ["fresh"; "f"]
         ~doc:"start with a fresh new local store")

let () =
  let worker base uri fresh = Lwt_main.run (Worker.run ~base ~uri ~fresh) in
  let term =
    Term.(pure worker $ base $ uri $ fresh,
          info ~doc:"start a worker" "ciso-worker")
  in
  match Term.eval term with `Error _ -> exit 1 | _ -> exit 0
