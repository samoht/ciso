(*
  From opam-depext:
   - https://github.com/ocaml/opam-depext
   - tip: fc183489fb9ee2265b6d969fcab846d38bceb937
*)

let debug = ref false

let lines_of_channel ic =
  let rec aux acc =
    let line = try Some (input_line ic) with End_of_file -> None in
    match line with
    | Some s -> aux (s::acc)
    | None -> acc
  in
  List.rev (aux [])

let lines_of_command c =
  if !debug then Printf.eprintf "+ %s\n%!" c;
  let ic = Unix.open_process_in c in
  let lines = lines_of_channel ic in
  close_in ic;
  lines

let lines_of_file f =
  let ic = open_in f in
  let lines = lines_of_channel ic in
  close_in ic;
  lines

let command_output c =
  match lines_of_command c with
  | [s] -> s
  | _ -> failwith (Printf.sprintf "Command %S failed" c)

let string_split char str =
  let rec aux pos =
    try
      let i = String.index_from str pos char in
      String.sub str pos (i - pos) :: aux (succ i)
    with Not_found | Invalid_argument _ ->
        let l = String.length str in
        [ String.sub str pos (l - pos) ]
  in
  aux 0

let has_command c =
  let cmd = Printf.sprintf "command -v %s >/dev/null" c in
  try Sys.command cmd = 0 with Sys_error _ -> false

(* system detection *)

let arch () =
  match command_output "uname -m" with
  | "x86_64" -> `X86_64
  | "x86" | "i386" | "i586" | "i686" -> `X86
  | "armv7l" -> `Arm7
  | "PPC" | "PowerPC" -> `PPC
  | s -> `Other s

let os () = match Sys.os_type with
  | "Unix" ->
    (match command_output "uname -s" with
     | "Darwin"    -> `Darwin
     | "Linux"     -> `Linux
     | "FreeBSD"   -> `FreeBSD
     | "OpenBSD"   -> `OpenBSD
     | "NetBSD"    -> `NetBSD
     | "DragonFly" -> `DragonFly
     | _           -> `Unix)
  | "Win32"  -> `Win32
  | "Cygwin" -> `Cygwin
  | s        -> `Other s

let distribution = function
  | `Darwin ->
    if has_command "brew" then Some `Homebrew
    else if has_command "port" then Some `Macports
    else None
  | `Linux ->
    (try
       let name =
         if has_command "lsb_release" then
           command_output "lsb_release -i -s"
         else
         let release_file =
           List.find Sys.file_exists
             ["/etc/redhat-release"; "/etc/centos-release";
              "/etc/gentoo-release"; "/etc/issue"; "/etc/os-release"]
         in
         List.hd (string_split ' ' (List.hd (lines_of_file release_file)))
       in
       match String.lowercase name with
       | "debian" -> Some `Debian
       | "ubuntu" -> Some `Ubuntu
       | "centos" -> Some `Centos
       | "fedora" -> Some `Fedora
       | "mageia" -> Some `Mageia
       | "gentoo" -> Some `Gentoo
       | "archlinux" -> Some `Archlinux
       | s -> Some (`Other s)
     with Not_found | Failure _ -> None)
  | _ -> None

(* generate OPAM depexts flags *)

let archflags = function
  | `X86_64 -> ["x86_64"]
  | `X86 -> ["x86"]
  | `Arm7 -> ["arm";"armv7"]
  | `PPC -> ["ppc"]
  | `Other s -> [String.lowercase s]

let osflags = function
  | `Darwin -> ["osx"]
  | `Linux -> ["linux"]
  | `Unix -> ["unix"]
  | `FreeBSD -> ["bsd";"freebsd"]
  | `OpenBSD -> ["bsd";"openbsd"]
  | `NetBSD -> ["bsd";"netbsd"]
  | `DragonFly -> ["bsd";"dragonfly"]
  | `Win32 -> ["mswindows";"win32"]
  | `Cygwin -> ["mswindows";"cygwin"]
  | `Other s -> [String.lowercase s]

let distrflags = function
  | Some `Homebrew -> ["homebrew"]
  | Some `Macports -> ["macports"]
  | Some `Debian -> ["debian"]
  | Some `Ubuntu -> ["ubuntu"]
  | Some `Centos -> ["centos"]
  | Some `Fedora -> ["fedora"]
  | Some `Mageia -> ["mageia"]
  | Some `Archlinux -> ["archlinux"]
  | Some `Gentoo -> ["gentoo"]
  | Some (`Other s) -> [String.lowercase s]
  | None -> []

(* end of copy-pate *)

open Sexplib.Std

type t = {
  arch: string list;
  os: string list;
  distribution: string list;
} with sexp

let create arch os distr = {
  arch         = archflags arch;
  os           = osflags os;
  distribution = distrflags distr;
}

let detect () =
  let os = os () in
  create (arch ()) os (distribution os)

let to_string t =
  let l = String.concat "." in
  Printf.sprintf "%s:%s:%s" (l t.arch) (l t.os) (l t.distribution)

let pp = to_string

let defaults =
  List.map (fun (a, o, s) -> create a o s)
    [
      (`X86, `Linux , Some `Ubuntu);
      (`X86, `Linux , Some `Debian);
      (`X86, `Darwin, Some `Homebrew);
    ]
