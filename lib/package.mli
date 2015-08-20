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

(** Package name with optional version. *)

type t with sexp
(** The type for package names with an optional version. *)

val name: t -> string
(** [name t] is [t]'s name. *)

val version: t -> string option
(** [version t] is [t]'s version or [None] it [t] does not have any
    version. *)

val create: ?version:string -> string -> t
(** [create ?version name] is the opam package [name.version].  *)

val of_string: string -> t
(** [of_string "n.v"] is the package with name [n] and version [v]. If
    [s] does not contain any string, it is the package with name [s]
    and no version. *)

val to_string: t -> string
(** [to_string t] is [name t ^ "." v] if [t] has the version [v],
    otherwise it is [name t]. *)

(** {1 Package Information} *)

type info with sexp
(** The type for package information values. *)

val info: opam:Cstruct.t -> url:Cstruct.t -> info
(** [info ~opam ~url] is the package information value containing the
    given opam and url file contents. *)

val opam: info -> Cstruct.t
(** [opam i] is the contents of [i]'s opam file. *)

val url: info -> Cstruct.t
(** [url i] is the contents of [i]'s url file. *)
