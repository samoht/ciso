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

type id = [`Object] Id.t

type kind = [
  | `Archive
  | `Stdout
  | `Stderr
]

type archive = {
  files: (string * Digest.t) list;
  raw  : Cstruct.t;
}

let pp_file ppf (f, d) = Fmt.pf ppf "%s %s" f (Digest.to_hex d)
let pp_archive ppf t = Fmt.(pf ppf "[@[<v>files: %a@]]" (list pp_file) t.files)

let json_digest =
  let dec o = `Ok (Digest.from_hex o) in
  let enc s = Digest.to_hex s in
  Jsont.view ~default:(Digest.bytes "") (dec, enc) Jsont.string

let json_file =
  let o = Jsont.objc ~kind:"file" () in
  let name = Jsont.mem o "name" Jsont.string in
  let digest  = Jsont.mem o "digest" json_digest in
  let c = Jsont.obj ~seal:true o in
  let dec o =`Ok  (Jsont.get name o, Jsont.get digest o) in
  let enc (n, d) = Jsont.(new_obj c [memv name n; memv digest d]) in
  Jsont.view (dec, enc) c

(* FIXME: it's probably not a good idea to do that. *)
let json_cstruct =
  let dec o = `Ok (Cstruct.of_string (Hex.to_string (`Hex o))) in
  let enc c = let `Hex h = Hex.of_cstruct c in h in
  Jsont.view (dec, enc) Jsont.nat_string

let json_archive =
  let o = Jsont.objc ~kind:"archive" () in
  let files = Jsont.(mem o "files" @@ array json_file) in
  let raw  = Jsont.mem o "raw" json_cstruct in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok { files = Jsont.get files o; raw = Jsont.get raw o } in
  let enc a = Jsont.(new_obj c [memv files a.files; memv raw a.raw]) in
  Jsont.view (dec, enc) c

type contents =
  | Archive of archive
  | Stdout of string list
  | Stderr of string list

let pp_contents ppf = function
  | Archive a -> pp_archive ppf a
  | Stdout lines
  | Stderr lines -> Fmt.(list string) ppf lines

let json_contents =
  let o = Jsont.objc ~kind:"contents" () in
  let archive = Jsont.(mem_opt o "archive" json_archive) in
  let stdout = Jsont.(mem_opt o "stdout" @@ array string) in
  let stderr = Jsont.(mem_opt o "stderr" @@ array string) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get f = Jsont.get f o in
    match get archive, get stdout, get stderr with
    | Some a, None, None -> `Ok (Archive a)
    | None, Some l, None -> `Ok (Stdout l)
    | None, None, Some l -> `Ok (Stderr l)
    | _ -> `Error "json_contents"
  in
  let enc t =
    Jsont.(new_obj c [match t with
      | Archive a -> memv archive (Some a)
      | Stdout l  -> memv stdout (Some l)
      | Stderr l  -> memv stderr (Some l)])
  in
  Jsont.view ~default:(Stdout []) (dec, enc) c

type t = { id : id; contents: contents; }

let equal x y = Id.equal x.id y.id

let pp ppf t = Fmt.pf ppf
    "@[<v>\
     id:       %a@;\
     contents: %a@@]"
    Id.pp t.id
    pp_contents t.contents

let json =
  let o = Jsont.objc ~kind:"object" () in
  let id = Jsont.(mem o "id" Id.json) in
  let contents = Jsont.(mem o "contents" json_contents) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok { id = Jsont.get id o; contents = Jsont.get contents o } in
  let enc t = Jsont.(new_obj c [memv id t.id; memv contents t.contents]) in
  Jsont.view (dec, enc) c

let id t = t.id
let contents t = t.contents

let kind t = match t.contents with
  | Archive _ -> `Archive
  | Stdout _  -> `Stdout
  | Stderr _  -> `Stderr

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
