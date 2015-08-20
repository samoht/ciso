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

open Sexplib.Std

type id = [`Job] Id.t with sexp

type t = {
  id      : id;                                                 (* the job id *)
  inputs  : id list;                 (* the transitive reduction of need jobs *)
  compiler: Compiler.t;                     (* switch on which to run the job *)
  host    : Host.t;                           (* host on which to run the job *)
  repos   : Task.repository list;         (* list of opam repositories to use *)
  pins    : Task.pin list;                             (* list of pins to use *)
  packages: Package.t list;                  (* the list of packages to build *)
} with sexp

let id t = t.id
let inputs t = t.inputs
let compiler t = t.compiler
let host t = t.host
let repos t = t.repos
let pins t = t.pins

let to_string job = Sexplib.Sexp.to_string (sexp_of_t job)
let of_string s = t_of_sexp (Sexplib.Sexp.of_string s)

let hash ~repos ~pins ~host ~compiler ~packages =
  let x = String.concat "+" in
  let repos = List.map (function Task.Repository (n, add) -> n ^ add) repos in
  let pins =
    List.map (function Task.Pin (pkg, target) -> pkg ^ ":" ^ target) pins
  in
  let compilers = [Compiler.to_string compiler] in
  let hosts = [Host.to_string host] in
  let packages = List.map Package.to_string packages in
  let str = x [x repos; x pins; x compilers; x hosts; x packages] in
  Id.digest `Job str

let create ?(inputs=[]) ?(repos=[]) ?(pins=[]) host compiler packages =
  let id = hash ~repos ~pins ~host ~compiler ~packages in
  { id; inputs; compiler; host; repos; pins; packages; }


type status = [
  | `Success
  | `Failure of string
  | `Pending
  | `Running
] with sexp

let pp_status = function
  | `Success   -> "success"
  | `Failure s -> "failure: " ^ s
  | `Pending   -> "pending"
  | `Running   -> "running"

let string_of_status t = Sexplib.Sexp.to_string (sexp_of_status t)
let status_of_string s = status_of_sexp (Sexplib.Sexp.of_string s)

let is_success = function `Success -> true | _ -> false
let is_failure = function `Failure _ -> true | _ -> false

let task_status l: Task.status =
  if List.for_all is_success l then `Success
  else if List.for_all is_failure l then `Failure
  else `Pending
