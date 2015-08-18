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

(* FIXME: rework the API *)

type task
type job
type job_entry
type pull
type repository = string * string * int option
type pin = string * string
type depopt = string * string option

val make_job:
  Common_types.id ->
  Common_types.id list ->
  Common_types.compiler ->
  Common_types.host ->
  task -> ?repository:repository list -> ?pin:pin list -> unit -> job

val make_job_entry: job -> Common_types.id list -> job_entry

val string_of_job_entry: job_entry -> string
val job_entry_of_string: string -> job_entry
val unwrap_entry: job_entry -> job * Common_types.id list

val string_of_job: job -> string
val job_of_string: string -> job

val env_of_job: job -> Common_types.compiler * Common_types.host
val inputs_of_job: job -> Common_types.id list
val repo_of_job: job -> repository list option
val pin_of_job: job -> pin list option
val task_of_job: job -> task

(* FIXME: weird type *)
val info_of_task: task -> string * string option
val to_compiler: task -> string option

(* FIXME: weird *)
val info_of_pkg_task:
  task -> string * string option * (string * string option) list option

val make_gh_task: name:string -> ?version:string -> pull -> task
val make_pkg_task:
  name:string -> ?version:string -> ?depopts:depopt list -> unit -> task

val hash_id:
  ?repository:repository list -> ?pin:pin list ->
  task -> string list -> string -> string -> string
