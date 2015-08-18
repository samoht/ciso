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

type t

val create: ?uri:string -> ?fresh:bool -> unit -> t Lwt.t

val register_token: t -> worker_token -> unit Lwt.t

val invalidate_token: t -> worker_token -> unit Lwt.t

val query_object: t -> id -> bool Lwt.t

val publish_object: t -> worker_token -> id -> Object.t -> unit Lwt.t

val retrieve_object: t -> id -> Object.t Lwt.t

val log_job: t -> id -> Task.job * (id list) -> unit Lwt.t

val unlog_job: t -> id -> unit Lwt.t

val retrieve_jobs: t -> (id * Task.job * (id list)) list Lwt.t

val retrieve_job: t -> id -> (Task.job * (id list)) Lwt.t

val query_compiler: t -> id -> bool Lwt.t

val publish_compiler: t -> worker_token -> id -> Object.t -> unit Lwt.t

val retrieve_compiler: t -> id -> Object.t Lwt.t
