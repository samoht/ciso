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

type t = {
  id : Common_types.id;            (* object is referenced by id in scheduler *)
  result : [`Success |`Fail of string
           |`Delegate of Common_types.id];            (* result of opam build *)
  output: string list;      (* relative paths of stdout and stderr in archive *)
  installed : string list;    (* relative paths of installed files in archive *)
  archive: string * string;
              (* archive who holds output and installed files, name * content *)
} with sexp

let id_of_t t = t.id
let apply_info t = t.installed, t.archive
let installed_of_t t = t.installed
let result_of_t t = t.result

let make_obj id result ~output ~installed archive =
  {id; result; output; installed; archive}

let string_of_t obj =
  sexp_of_t obj |> Sexplib.Sexp.to_string

let t_of_string str =
  Sexplib.Sexp.of_string str |> t_of_sexp

let string_of_result = function
  | `Success -> "SUCCESS"
  | `Fail f -> "FAIL: " ^ f
  | `Delegate id ->"DELEGATE " ^ id
