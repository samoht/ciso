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

let distrib = function
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

type arch = [
  | `X86_64
  | `X86
  | `Arm7
  | `PPC
  | `Other of string
]

let string_of_arch = function
  | `X86_64 -> "x86_64"
  | `X86 -> "x86"
  | `Arm7 -> "armv7"
  | `PPC -> "ppc"
  | `Other s -> String.lowercase s

let arch_of_string = function
  | "x86_64" -> `Ok `X86_64
  | "x86" -> `Ok `X86
  | "armv7" -> `Ok `Arm7
  | "ppc" -> `Ok `PPC
  | s -> `Ok (`Other (String.lowercase s))

type os = [
  | `Darwin
  | `Linux
  | `Unix
  | `FreeBSD
  | `OpenBSD
  | `NetBSD
  | `DragonFly
  | `Win32
  | `Cygwin
  | `Other of string
]

let string_of_os = function
  | `Darwin -> "osx"
  | `Linux -> "linux"
  | `Unix -> "unix"
  | `FreeBSD -> "freebsd"
  | `OpenBSD -> "openbsd"
  | `NetBSD -> "netbsd"
  | `DragonFly -> "dragonfly"
  | `Win32 -> "win32"
  | `Cygwin -> "cygwin"
  | `Other s -> String.lowercase s

let os_of_string = function
  | "osx" -> `Ok `Darwin
  | "linux" -> `Ok `Linux
  | "unix" -> `Ok `Unix
  | "freebsd" -> `Ok `FreeBSD
  | "openbsd" -> `Ok `OpenBSD
  | "netbsd" -> `Ok `NetBSD
  | "dragonfly" -> `Ok `DragonFly
  | "win32" -> `Ok `Win32
  | "cygwin" -> `Ok `Cygwin
  | s -> `Ok (`Other (String.lowercase s))

type distr = [
  | `Homebrew
  | `Macports
  | `Debian
  | `Ubuntu
  | `Centos
  | `Fedora
  | `Mageia
  | `Archlinux
  | `Gentoo
  | `Other of string
]

let string_of_distr = function
  | `Homebrew -> "homebrew"
  | `Macports -> "macports"
  | `Debian -> "debian"
  | `Ubuntu -> "ubuntu"
  | `Centos -> "centos"
  | `Fedora -> "fedora"
  | `Mageia -> "mageia"
  | `Archlinux -> "archlinux"
  | `Gentoo -> "gentoo"
  | `Other s -> String.lowercase s

let distr_of_string = function
  | "homebrew" -> `Ok `Homebrew
  | "macports" -> `Ok `Macports
  | "debian" -> `Ok `Debian
  | "ubuntu" -> `Ok `Ubuntu
  | "centos" -> `Ok `Centos
  | "fedora" -> `Ok `Fedora
  | "mageia" -> `Ok `Mageia
  | "archlinux" -> `Ok `Archlinux
  | "gentoo" -> `Ok `Gentoo
  | s -> `Ok (`Other (String.lowercase s))

(* end of copy-pate *)

type t = {
  arch: arch;
  os: os;
  distr: distr option;
}

let create arch os distr = { arch; os; distr }

let detect () =
  let os = os () in
  create (arch ()) os (distrib os)

let pp_arch ppf x = Fmt.string ppf (string_of_arch x)
let pp_os ppf x = Fmt.string ppf (string_of_os x)
let pp_distr ppf x = Fmt.string ppf (string_of_distr x)

let pp ppf t =
  Fmt.pf ppf
    "@[<v>\
     arch:  %a@;\
     os:    %a@;\
     distr: %a@]"
    pp_arch t.arch
    pp_os t.os
    (Fmt.option pp_distr) t.distr

let json_arch = Jsont.view (arch_of_string, string_of_arch) Jsont.string
let json_os = Jsont.view (os_of_string, string_of_os) Jsont.string
let json_distr = Jsont.view (distr_of_string, string_of_distr) Jsont.string

let json =
  let o = Jsont.objc ~kind:"host" () in
  let arch = Jsont.(mem o "arch" json_arch) in
  let os = Jsont.(mem o "os" json_os) in
  let distr = Jsont.(mem_opt o "distr" json_distr) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get f = Jsont.get f o in
    `Ok { arch = get arch; os = get os; distr = get distr; }
  in
  let enc t =
    Jsont.(new_obj c [memv arch t.arch; memv os t.os; memv distr t.distr])
  in
  Jsont.view (dec, enc) c


let defaults =
  List.map (fun (a, o, s) -> create a o s)
    [
      (`X86, `Linux , Some `Ubuntu);
      (`X86, `Linux , Some `Debian);
      (`X86, `Darwin, Some `Homebrew);
    ]
