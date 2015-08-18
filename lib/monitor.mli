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
type worker_status

val new_worker: host -> worker_id * worker_token

val verify_worker: worker_id -> worker_token -> unit

val job_rank: worker_token -> id list -> job_rank

val new_job: id -> compiler -> worker_token -> unit

val job_completed: id -> worker_token -> unit

val publish_object: id -> worker_token -> unit

val worker_statuses: unit -> (worker_id * worker_token * worker_status) list

val info_of_status: worker_status -> string * string option

val worker_environments: unit -> host list

val worker_env: worker_token -> host * compiler option

val compilers: unit -> compiler list

val worker_monitor: unit -> (worker_id * worker_token) list Lwt.t
