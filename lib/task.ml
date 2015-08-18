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

open Sexplib.Std

(* name, verison option *)
type depopt = string * string option with sexp

(* name, address option *)
type repository = string * string with sexp

(* package, target *)
type pin = string * string with sexp

type t =
  | Package of string * string option * depopt list option
  (* package name * version * depopts *)
  | Compiler of string
 (* compiler version[+tag] *)
with sexp

let info_of_task task =
  match task with
  | Package (p, v, None) -> p, v
  | Package (p, v, Some depopts) ->
     let depopt_info =
       depopts
       |> List.rev_map (fun (n, v_opt) ->
            match v_opt with
            | None -> n
            |Some v -> n ^ "." ^ v)
       |> String.concat ";" in
     p ^ "+" ^ depopt_info, v
  | Compiler c -> c, None


let info_of_pkg_task = function
  | Package (n, v, depopts) -> n, v, depopts
  | Compiler _ -> assert false

(* return a deterministic id, based on pakcage name, version, and dependencies
   could add os and architecture later *)
let hash_id ?(repositories=[]) ?(pins=[]) task inputs compiler host =
  let task_str = match task with
    | Compiler c -> c
    | Package (n, v_opt, depopt_opt) ->
      let v_str = match v_opt with None -> "" | Some v -> v in
      let depopt_str = match depopt_opt with
        | None -> ""
        | Some depopts ->
          List.rev_map (fun (n ,v_opt) ->
              match v_opt with
              | None -> n | Some v -> n ^ "." ^ v) depopts
          |> String.concat ";" in
      n ^ v_str ^ depopt_str
  in
  let repo_str = match (repositories: repository list) with
    | [] -> ""
    | _  ->
      List.rev_map (fun (n, add) -> n ^ add) repositories
      |> String.concat ";"
  in
  let pin_str = match (pins: pin list) with
    | [] -> ""
    | _  ->
      List.rev_map (fun (pkg, target) -> pkg ^ ":" ^ target) pins
      |> String.concat ";"
  in
  let input_str = String.concat ";" inputs in
  let str = task_str ^ repo_str ^ pin_str ^ input_str ^ compiler ^ host in
  let `Hex h =
    Hex.of_cstruct (Nocrypto.Hash.SHA1.digest (Cstruct.of_string str))
  in
  h

let make_pkg_task ~name ?version ?depopts () = Package (name, version, depopts)

let to_compiler = function
  | Compiler c -> Some c
  | _ -> None
