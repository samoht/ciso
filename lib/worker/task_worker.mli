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

(** Task workers.

    A task worker translate tasks into a graph of jobs to run, for the
    various host ocnfigurations and compiler switches.

*)

type t
(** The type for task workers. *)

val start: ?tick:float -> opam_root:string -> ?cache:bool -> Store.t -> t Lwt.t
(** [start ~opam_root s] starts a task worker process using the given
    OPAM root to store OPAM state. It uses [s] to synchronise with the
    scheduler and to store built objects. It also uses [s] to notify
    to the scheduler that it is alive.

    {ul
    {- [tick] specifies how often the worker write into the store to
       notify that it is alive (default is every 5s).}
    {- If [cache] is set (default is false), the {b highly
       experimental} caching feature is enable: every job will be cut
       into atomic actions (containing only one package to install)
       and the already built objects are used as a cache.}
    }

*)

val stop: t -> unit Lwt.t
(** [stop t] stops the task worker. *)
