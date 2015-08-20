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

(** Unique identifiers.

    This module handle both deterministic identifiers for
    {{!module:Object}objects}, {{!module:Job}jobs} and
    {{!module:Task}tasks} and randaom unique identifiers for
    {{!module:Worker}workers}.
*)

type 'a t with sexp
(** The type for deterministic or random dentifiers. *)

val of_uuid: 'a -> 'a t
(** [uuid k] is a a 128 bits universally unique identifiers (UUID)
    version 4 (random based) according to
    {{:http://tools.ietf.org/html/rfc4122}RFC 4122}. *)

val digest: 'a -> string -> 'a t
(** [digest k s] is [s]'s SHA1 digest. *)

val compare: 'a t -> 'a t -> int
(** [compare x y] compares the representation of two identifiers. *)

val to_string: 'a t -> string
(** [to_string t] is the string representation of [t]. *)

val of_string: 'a -> string -> 'a t
(** [of_string k s] is the identifier [t] of type [k] such that
    [to_string t] is [s]. *)

val pp: 'a t Fmt.t
(** [pp t] formats [t]. *)
