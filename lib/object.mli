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

(** Build objects.

    The objects are built by {{!module:Job}jobs} executed by
    {{!module:Worker}workers}.

*)

type id = [`Object] Id.t with sexp
(** The type for object identifiers. Object identifiers are
    deterministic, i.e. two similar objects will have the same
    identifiers. The notion of similiraty depends on the object
    type. *)

type archive = {
  files: (string * Digest.t) list;
  raw  : Cstruct.t;
}
(** The type for archive values. *)

type contents =
  | Archive of archive
  | Stdout of string list
  | Stderr of string list
(** The type for object contents. Can either be an archive, or stdout
    and stderr dumps. *)

type kind = [
  | `Archive
  | `Stdout
  | `Stderr
]
(** The type for object kinds. *)

type t
(** The type for object values. *)

val to_string: t -> string
(** [to_string t] is the string representing [t]. *)

val of_string: string -> t
(** [of_string s] is the object [t] whose string representation is
    [s]. *)

val id: t -> id
(** [id t] is [t]'s id. *)

val contents: t -> contents
(** [contents t] is [t]s contents. *)

val kind: t -> kind
(** [kind t] is [t]'s kind. *)

val archive: (string * Digest.t) list -> Cstruct.t -> t
(** [archive f c] is the archive containing the files [f] and with raw
    contents [c]. *)

val stdout: string list -> t
(** [stdout lines] is the object containing the [lines] of the
    standard output of a job. *)

val stderr: string list -> t
(** [stderr lines] is similar to {!stdout} but for the standard
    errors. *)
