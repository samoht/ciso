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

(* FIXME: doc *)

type t

val create:
  id:Common_types.id ->
  inputs:Common_types.id list ->
  compiler:string ->
  host:Host.t ->
  repos:Task.repository list ->
  pins:Task.pin list ->
  Task.t -> t

val to_string: t -> string
val of_string: string -> t

val compiler: t -> string
val host: t -> Host.t
val inputs: t -> Common_types.id list
val repos: t -> Task.repository list
val pins: t -> Task.pin list
val task: t -> Task.t

type entry

val create_entry: t -> Common_types.id list -> entry
val string_of_entry: entry -> string
val entry_of_string: string -> entry
val unwrap_entry: entry -> t * Common_types.id list
