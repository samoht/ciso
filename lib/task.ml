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
open Common_types

type task =
  | Github of string * string option * pull
  (* package name * version * pull *)
  | Package of string * string option * depopt list option
  (* package name * version * depopts *)
  | Compiler of string
 (* compiler version[+tag] *)
and pull = {
    pull_num : int;
    repo_url : string;
    base_sha : string;
    head_sha : string;
}
(* name, verison option *)
and depopt = string * string option with sexp

type job = {
  id : id;           (* task is referenced by this id *)
  inputs : id list;  (* inputs are object ids *)
  compiler : compiler;
  host : host;
  repository: repository list option;
  pin: pin list option;
  task : task;
}
(* name, address, priority option *)
and repository = string * string * int option
(* package, target *)
and pin = string * string with sexp

type job_entry = {
  job : job;
  dependencies : id list;
} with sexp

let inputs_of_job t = t.inputs
let task_of_job t = t.task
let env_of_job t = (t.compiler, t.host)
let repo_of_job t = t.repository
let pin_of_job t = t.pin

let info_of_task task =
  match task with
  | Github (p, v, _)
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
  | Compiler _ | Github _ -> assert false

(*
let make_pull num url base head = {
    pull_num = num;
    repo_url = url;
    base_sha = base;
    head_sha = head;}
*)

(* return a deterministic id, based on pakcage name, version, and dependencies
   could add os and architecture later *)
let hash_id ?repository ?pin task inputs compiler host =
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
    | Github (n, v_opt, pull) ->
       let v_str = match v_opt with None -> "" | Some v -> v in
       let pull_str = string_of_int pull.pull_num in
       n ^ v_str ^ pull_str in
  let repo_str =
    match repository with
    | None -> ""
    | Some repos ->
       List.rev_map (fun (n, add, p_opt) ->
         n ^ add
         ^ match p_opt with None -> "" | Some p -> string_of_int p) repos
       |> String.concat ";" in
  let pin_str =
    match pin with
    | None -> ""
    | Some pins ->
       List.rev_map (fun (pkg, target) -> pkg ^ ":" ^ target) pins
       |> String.concat ";" in
  let input_str = String.concat ";" inputs in
  let str = task_str ^ repo_str ^ pin_str ^ input_str ^ compiler ^ host in
  let `Hex h =
    Hex.of_cstruct (Nocrypto.Hash.SHA1.digest (Cstruct.of_string str))
  in
  h

let make_pkg_task ~name ?version ?depopts () = Package (name, version, depopts)

let make_gh_task ~name ?version pull = Github (name, version, pull)

let make_job id inputs compiler host task ?repository ?pin () =
  {id; inputs; compiler; host; repository; pin; task}

let make_job_entry job dependencies = {job; dependencies}

let unwrap_entry {job; dependencies} = job, dependencies

let string_of_job_entry entry = Sexplib.Sexp.to_string (sexp_of_job_entry entry)
let job_entry_of_string s = job_entry_of_sexp (Sexplib.Sexp.of_string s)

let string_of_job job = Sexplib.Sexp.to_string (sexp_of_job job)

let to_compiler = function
  | Compiler c -> Some c
  | _ -> None

let job_of_string s = job_of_sexp (Sexplib.Sexp.of_string s)
