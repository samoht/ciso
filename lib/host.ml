(*
 * Copyright (c) 2013 David Sheets <sheets@alum.mit.edu>
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
 *
 *)

open Sexplib.Std

type linux =
  | Suse
  | RedHat
  | Fedora
  | Slackware
  | Debian
  | Gentoo
  | Ubuntu
with sexp

type os =
  | Darwin
  | Linux of linux option
  | FreeBSD
  | OpenBSD
  | NetBSD
  | DragonFly
  | Cygwin
  | Win32
  | Unix
  | Other of string
with sexp

type arm =
  | V5tel
  | V6l
with sexp

type arch =
  | X86_64
  | I386
  | I686
  | Arm of arm
  | PPC64
  | Powerpc
  | Unknown
with sexp

(*
type opam =
  | Opam_1_0_0
with sexp

type ocaml =
  | OCaml_3_12_1
  | OCaml_4_00_1
with sexp
*)

type t = {
  os : os;
  arch : arch;
(*  opam : opam list;
  ocaml : ocaml list; *)
} with sexp

(*type isa_exts*)
(* TODO: differences? compatibilities? worth it? *)

let string_of_os = function
  | Linux None             -> "Linux"
  | Linux (Some Suse)      -> "SUSE Linux"
  | Linux (Some RedHat)    -> "Red Hat Linux"
  | Linux (Some Fedora)    -> "Fedora Linux"
  | Linux (Some Slackware) -> "Slackware Linux"
  | Linux (Some Debian)    -> "Debian Linux"
  | Linux (Some Gentoo)    -> "Gentoo Linux"
  | Linux (Some Ubuntu)    -> "Ubuntu Linux"
  | Darwin -> "Darwin"
  | FreeBSD -> "FreeBSD"
  | OpenBSD -> "OpenBSD"
  | NetBSD -> "NetBSD"
  | DragonFly -> "DragonFly"
  | Cygwin -> "Cygwin"
  | Win32 -> "Win32"
  | Unix -> "Unix"
  | Other s -> s

let os_of_string_opt = function
  | Some "Darwin" -> Darwin
  | Some "Linux" -> Linux None
  | Some "FreeBSD" -> FreeBSD
  | Some "OpenBSD" -> OpenBSD
  | Some "NetBSD" -> NetBSD
  | Some "DragonFly" -> DragonFly
  | Some "Cygwin" -> Cygwin
  | Some "Win32" -> Win32
  | Some "Unix" -> Unix
  | Some other -> Other other
  | None -> Other ""

let string_of_arch = function
  | X86_64 -> "x86_64"
  | I386 -> "i386"
  | I686 -> "i686"
  | Arm V5tel -> "armv5tel"
  | Arm V6l -> "armv6l"
  | PPC64 -> "ppc64"
  | Powerpc -> "powerpc"
  | Unknown -> "unknown"

let arch_of_string_opt = function
  | Some "x86_64" -> X86_64
  | Some "amd64" -> X86_64
  | Some "i386" -> I386
  | Some "i686" -> I686
  | Some "armv5tel" -> Arm V5tel
  | Some "armv6l" -> Arm V6l
  | Some "ppc64" -> PPC64
  | Some "powerpc" -> Powerpc
  | Some "macppc" -> Powerpc
  | Some _ | None -> Unknown

let to_string { os; arch } =
  Printf.sprintf "%s (%s)" (string_of_os os) (string_of_arch arch)

(* copied from OpamMisc :-/ *)
let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn

let ltws = Re.(compile (seq [
  bos ; rep space ; group (non_greedy (rep any)) ; rep space ; eos
]))
let strip s = Re.(get (exec ltws s) 1)

let uname_m () =
  try with_process_in "uname -m"
        (fun ic -> Some (strip (input_line ic)))
  with _ -> None

let uname_s () =
  try with_process_in "uname -s"
        (fun ic -> Some (strip (input_line ic)))
  with _ -> None

let archref = ref None
let osref = ref None
(*let ocamlref = ref None
let opamref = ref None
*)
let arch () = match !archref with
  | None ->
      let arch = match Sys.os_type with
        | "Unix" -> arch_of_string_opt (uname_m ())
        | _ -> Unknown
      in
      archref := Some arch;
      arch
  | Some arch -> arch

let os () = match !osref with
  | None ->
      let os = match Sys.os_type with
        | "Unix"   -> os_of_string_opt (uname_s ())
        | "Win32"  -> Win32
        | "Cygwin" -> Cygwin
        | s        -> Other s
      in
      osref := Some os;
      os
  | Some os -> os
(*
let ocaml () = match !ocamlref with
  | None ->
      let ocaml =

      in
      ocamlref = Some ocaml;
      ocaml
  | Some ocaml -> ocaml

let opam () = match !opamref with
  | None ->
      let opam =

      in
      opamref = Some opam;
      opam
  | Some opam -> opam
*)
let detect () = {
  os = os ();
  arch = arch ();
(*  ocaml = ocaml ();
  opam = opam (); *)
}
