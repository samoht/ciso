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
  inputs  : Object.id list;      (* the immedidate objects needed for the job *)
  compiler: string;                         (* switch on which to run the job *)
  host    : Host.t;                           (* host on which to run the job *)
  repos   : Task.repository list;         (* list of opam repositories to use *)
  pins    : Task.pin list;                             (* list of pins to use *)
} with sexp

let id t = t.id
let inputs t = t.inputs
let output t = t.output
let compiler t = t.compiler
let host t = t.host
let repos t = t.repos
let pins t = t.pins
let result t = t.result
let task t = t.task

let to_string job = Sexplib.Sexp.to_string (sexp_of_t job)
let of_string s = t_of_sexp (Sexplib.Sexp.of_string s)

let create ~id ~inputs ~output ~compiler ~host ~repos ~pins ~result task =
  { id; output; inputs; compiler; host; repos; pins; task; result }

type entry = {
  job : t;
  dependencies : Object.id list;
} with sexp

let create_entry job dependencies = {job; dependencies}
let unwrap_entry {job; dependencies} = job, dependencies
let string_of_entry entry = Sexplib.Sexp.to_string (sexp_of_entry entry)
let entry_of_string s = entry_of_sexp (Sexplib.Sexp.of_string s)

let pretty_result = function
  | `Success -> "SUCCESS"
  | `Fail f -> "FAIL: " ^ f
  | `Delegate id ->"DELEGATE " ^ Id.to_string id
  | `Unknown -> "UNKNOWN"
