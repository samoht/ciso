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

(* name, address option *)
type repository = Repository of string * string with sexp

(* package, target *)
type pin = Pin of string * string with sexp

type id = [`Task] Id.t with sexp

type t = {
  id: id;
  repos: repository list;
  pins:pin list;
  compilers:Compiler.t list;
  hosts:Host.t list;
  packages:Package.t list;
} with sexp

let id t = t.id
let packages t = t.packages

let hash ~repos ~pins ~compilers ~hosts ~packages =
  let x l = String.concat "+" (List.sort compare l) in
  let y   = String.concat "-" in
  let repos = List.map (function Repository (n, add) -> n ^ add) repos in
  let pins =
    List.map (function Pin (pkg, target) -> pkg ^ ":" ^ target) pins
  in
  let compilers = List.map Compiler.to_string compilers in
  let hosts = List.map Host.to_string hosts in
  let packages = List.map Package.to_string packages in
  let str = y [
      y repos; (* the order in which we stack the repos is important *)
      x pins; x compilers; x hosts; x packages
    ] in
  Id.digest `Task str

let create ?(repos=[]) ?(pins=[])
    ?(compilers=Compiler.defaults) ?(hosts=Host.defaults)
    packages =
  let id = hash ~repos ~pins ~compilers ~hosts ~packages in
  { id; repos; pins; compilers; hosts; packages }

let to_string t = Sexplib.Sexp.to_string (sexp_of_t t)
let of_string s = t_of_sexp (Sexplib.Sexp.of_string s)

type status = [
  | `Success
  | `Failure
  | `Pending
  | `Cancelled
] with sexp

let string_of_status t = Sexplib.Sexp.to_string (sexp_of_status t)
let status_of_string s = status_of_sexp (Sexplib.Sexp.of_string s)

let pp_status fmt x =
  let str = match x with
    | `Success  -> "success"
    | `Failure  -> "failure"
    | `Pending  -> "pending"
    | `Cancelled -> "canceled"
  in
  Fmt.string fmt str
