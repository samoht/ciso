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
open Common_types

type t = {
  id : id;           (* task is referenced by this id *)
  inputs : id list;  (* inputs are object ids *)
  compiler : compiler;
  host : host;
  repositories: Task.repository list;
  pins: Task.pin list;
  task : Task.t;
} with sexp

let inputs t = t.inputs
let task t = t.task
let env t = (t.compiler, t.host)
let repositories t = t.repositories
let pins t = t.pins
let to_string job = Sexplib.Sexp.to_string (sexp_of_t job)
let of_string s = t_of_sexp (Sexplib.Sexp.of_string s)

let create ~id ~inputs ~compiler ~host ~repositories ~pins task =
  { id; inputs; compiler; host; task; repositories; pins; }

type entry = {
  job : t;
  dependencies : id list;
} with sexp

let create_entry job dependencies = {job; dependencies}
let unwrap_entry {job; dependencies} = job, dependencies
let string_of_entry entry = Sexplib.Sexp.to_string (sexp_of_entry entry)
let entry_of_string s = entry_of_sexp (Sexplib.Sexp.of_string s)
