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

let info x y = Fmt.(pf stdout "%a %s\n%!" (styled `Cyan string) x y)
let err x = Fmt.(pf stderr "%a %s\n%!" (styled `Red string) "Error!" x)

let () =
  Irmin_unix.install_dir_polling_listener 0.2;
  Fmt.(set_style_renderer stdout `Ansi_tty);
  Fmt.(set_style_renderer stderr `Ansi_tty)

let opam_root =
  let doc = "The OPAM root to use to store the worker state." in
  Arg.(value & opt (some string) None & info ["r";"opam-root"] ~docv:"DIR" ~doc)

let local =
  let doc = "The path to the local Irmin store."in
  Arg.(value & opt (some string) None & info ["local"] ~docv:"DIR" ~doc)

let global =
  let doc = "The URI of the global Irmin store." in
  Arg.(value & opt (some string) None & info ["global"] ~docv:"URI" ~doc)

let err_invalid_line l = err (Printf.sprintf "invalide line: %S" l)

let config_file () =
  let dot_ciso = ".ciso" in
  if not (Sys.file_exists dot_ciso) then Lwt.return (fun _ -> None)
  else
    let lines = Lwt_io.lines_of_file dot_ciso in
    Lwt_stream.to_list lines >|= fun lines ->
    let kvs =
      List.fold_left (fun acc l ->
          if l.[0] = '#' then acc
          else match Stringext.cut l ~on:" " with
            | Some (k, v) -> (k, v) :: acc
            | _ -> err_invalid_line l; acc
        ) [] lines
    in
    let find name =
      try
        let v = List.assoc (name ^ ":") kvs in
        Some (String.trim v)
      with Not_found ->
        None
    in
    find

let choose_store local global = match local, global with
  | Some l, _    -> info "local " l ; Store.local ~root:l ()
  | None, Some r -> info "remote" r; Store.remote ~uri:(Uri.of_string r) ()
  | None, None   -> err "no store specified!"; exit 1

let store =
  let mk local global =
    match local, global with
    | None, None ->
      config_file () >>= fun config ->
      info "config" ".ciso";
      choose_store (config "local") (config "global")
    | _ -> choose_store local global
  in
  Term.(pure mk $ local $ global)

let block _ =
  let t, _ = Lwt.task () in
  t
