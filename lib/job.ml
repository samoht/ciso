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

type id = [`Job] Id.t

type t = {
  id      : id;                                                 (* the job id *)
  inputs  : id list;                 (* the transitive reduction of need jobs *)
  compiler: Compiler.t;                     (* switch on which to run the job *)
  host    : Host.t;                           (* host on which to run the job *)
  packages: (Package.t * Package.info) list; (* the list of packages to build *)
}

let json_package =
  let o = Jsont.objc ~kind:"job" () in
  let package = Jsont.(mem o "package" Package.json) in
  let info = Jsont.(mem o "info" Package.json_info) in
  let c = Jsont.obj ~seal:true o in
  let dec o = let get m = Jsont.get m o in `Ok (get package, get info) in
  let enc (p, i) = Jsont.(new_obj c [ memv package p; memv info i]) in
  Jsont.view (dec, enc) c

let json =
  let o = Jsont.objc ~kind:"job" () in
  let id = Jsont.mem o "id" Id.json in
  let inputs = Jsont.(mem ~opt:`Yes_rem o "inputs" @@ array Id.json) in
  let compiler = Jsont.(mem o "compiler" Compiler.json) in
  let host = Jsont.(mem o "host" Host.json) in
  let packages = Jsont.(mem o "packages" @@ array json_package) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok {
      id = get id; inputs = get inputs; compiler = get compiler;
      host = get host; packages = get packages
    } in
  let enc t =
    Jsont.(new_obj c [
        memv id t.id; memv inputs t.inputs;
        memv compiler t.compiler; memv host t.host;
        memv packages t.packages])
  in
  Jsont.view (dec, enc) c

let pp_package ppf (p, _) = Package.pp ppf p

let pp ppf t =
  Fmt.pf ppf
    "@[<v>\
     id:       %a@;\
     inputs:   %a@;\
     compiler: %a@;\
     host:     %a@;\
     packages: %a@]"
    Id.pp t.id
    (Fmt.list Id.pp) t.inputs
    Compiler.pp t.compiler
    Host.pp t.host
    (Fmt.list pp_package) t.packages

let id t = t.id
let inputs t = t.inputs
let compiler t = t.compiler
let host t = t.host
let packages t = t.packages

let digest buf = Cstruct.to_string (Nocrypto.Hash.SHA1.digest buf)

let hash ~host ~compiler ~packages =
  let x l = String.concat "+" (List.sort String.compare l) in
  let y   = String.concat "-" in
  let compilers = [Fmt.to_to_string Compiler.pp compiler] in
  let hosts = [Fmt.to_to_string Host.pp host] in
  let packages =
    List.map (fun (p, i) ->
        y [Package.to_string p; digest (Package.opam i); digest (Package.url i)]
      ) packages
  in
  let str = y [x compilers; x hosts; x packages] in
  Id.digest `Job str

let create ?(inputs=[]) host compiler packages =
  let id = hash ~host ~compiler ~packages in
  { id; inputs; compiler; host; packages; }

type status = [
  | `Success
  | `Failure
  | `Pending
  | `Running
  | `Cancelled
]

let json_status =
  Jsont.enum [
    "success", `Success; "failure", `Failure;
    "pending", `Pending; "concelled", `Cancelled
  ]

let pp_status ppf t =
  let s = match t with
  | `Success   -> "success"
  | `Failure   -> "failure"
  | `Pending   -> "pending"
  | `Running   -> "running"
  | `Cancelled -> "cancelled"
  in
  Fmt.string ppf s

let is_success = function `Success -> true | _ -> false
let is_failure = function `Failure -> true | _ -> false
let is_cancelled = function `Cancelled -> true | _ -> false

let task_status l: Task.status =
  if List.for_all is_success l then `Success
  else if List.exists is_failure l then `Failure     (* maybe a bit strong... *)
  else if List.exists is_cancelled l then `Cancelled
  else `Pending
