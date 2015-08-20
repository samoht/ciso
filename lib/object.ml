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

module Digest = struct
  include Digest
  let sexp_of_t t = Sexplib.Conv.sexp_of_string (Digest.to_hex t)
  let t_of_sexp s = Digest.from_hex (Sexplib.Conv.string_of_sexp s)
end

type id = [`Object] Id.t with sexp

type kind = [
  | `Archive
  | `Stdout
  | `Stderr
]

type archive = {
  files: (string * Digest.t) list;
  raw  : Cstruct.t;
} with sexp

type contents =
  | Archive of archive
  | Stdout of string list
  | Stderr of string list
with sexp

type t = { id : id; contents: contents; } with sexp

let id t = t.id
let contents t = t.contents

let kind t = match t.contents with
  | Archive _ -> `Archive
  | Stdout _  -> `Stdout
  | Stderr _  -> `Stderr

let to_string obj = sexp_of_t obj |> Sexplib.Sexp.to_string
let of_string str = Sexplib.Sexp.of_string str |> t_of_sexp

let hash k =
  let l = match k with
    | `Lines lines -> lines
    | `Files files ->
      List.map (fun (f, d) -> f ^ ":" ^ Digest.to_hex d) files
      |> List.sort String.compare
  in
  Id.digest `Object (String.concat "+" l)

let archive files raw =
  let id = hash (`Files files) in
  { id; contents = Archive { files; raw } }

let stdout lines =
  let id = hash (`Lines lines) in
  { id; contents = Stdout lines }

let stderr lines =
  let id = hash (`Lines lines) in
  { id; contents = Stderr lines }
