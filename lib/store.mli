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

open Common_types

val initial_store: ?uri:string -> ?fresh:bool -> unit -> unit Lwt.t

val register_token: worker_token -> unit Lwt.t

val invalidate_token: worker_token -> unit Lwt.t

val query_object: id -> bool Lwt.t

val publish_object: worker_token -> id -> Object.t -> unit Lwt.t

val retrieve_object: id -> Object.t Lwt.t

val log_job: id -> Task.job * (id list) -> unit Lwt.t

val unlog_job: id -> unit Lwt.t

val retrieve_jobs: unit -> (id * Task.job * (id list)) list Lwt.t

val retrieve_job: id -> (Task.job * (id list)) Lwt.t

val query_compiler: id -> bool Lwt.t

val publish_compiler: worker_token -> id -> Object.t -> unit Lwt.t

val retrieve_compiler: id -> Object.t Lwt.t
