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
  compilers:string list;
  hosts:Host.t list;
  packages:Package.t list;
} with sexp

let packages t = t.packages

let id ~repos ~pins ~compilers ~hosts ~packages =
  let x = String.concat "+" in
  let repos = List.map (function Repository (n, add) -> n ^ add) repos in
  let pins =
    List.map (function Pin (pkg, target) -> pkg ^ ":" ^ target) pins
  in
  let hosts = List.map Host.to_string hosts in
  let packages = List.map Package.to_string packages in
  let str = x [x repos; x pins; x compilers; x hosts; x packages] in
  Id.digest `Task str

let create ~repos ~pins ~compilers ~hosts ~packages =
  let id = id ~repos ~pins ~compilers ~hosts ~packages in
  { id; repos; pins; compilers; hosts; packages }
