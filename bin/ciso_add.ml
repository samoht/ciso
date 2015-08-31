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

let rev_deps =
  let doc =
    "Disabled by default, use '*' to test every dependent packages allowed by \
     the constraints. If you want to test only specific dependent packages, \
     they may be provided in a comma-separated list."
  in
  Arg.(value & opt (list string) [] & info ["rev-deps"] ~doc)

let repos =
  let doc = "Specify opam repositories." in
  Arg.(value & opt (list string) [] & info ["repos"] ~docv:"REPOS" ~doc)

let pins =
  let doc = "Specify pinned packages." in
  Arg.(value & opt (list string) [] & info ["pins"] ~docv:"PKGS" ~doc)

let mk_repo str =
  match Stringext.cut str ~on:":" with
  | Some (n, r) -> n, Uri.of_string r
  | None        -> str ^ string_of_int (Random.int 1024), Uri.of_string str

let mk_pin str =
  match Stringext.cut str ~on:":" with
  | Some (n, r) -> n, Some (Uri.of_string r)
  | None        -> str, None

let mk_rev_deps = function
  | []    -> `None
  | ["*"] -> `All
  | l     -> `Packages (List.map Package.of_string l)

let to_option = function
  | [] -> None
  | l  -> Some l

let main =
  let master store packages repos pins rev_deps =
    if rev_deps <> [] then info "rev-deps" (String.concat "," rev_deps);
    if packages = [] then ()
    else
      let repos = List.map mk_repo repos |> to_option in
      let pins = List.map mk_pin pins |> to_option in
      let rev_deps = mk_rev_deps rev_deps in
      let task = Task.create ~rev_deps ?pins ?repos packages in
      Lwt_main.run begin
        store >>= fun store ->
        Store.Task.add store task
      end
  in
  Term.(pure master $ store $ packages),
  Term.info ~version:Version.current ~doc:"Add new tasks to CISO" "ciso-add"

let () =
  match Term.eval main with `Error _ -> exit 1 | _ -> exit 0
