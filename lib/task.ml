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

(* name, address option *)
type repository = Repository of (string * Uri.t)

(* package, target *)
type pin = Pin of (string * Uri.t)

let json_uri =
  let dec s = `Ok (Uri.of_string s) in
  let enc u = Uri.to_string u in
  Jsont.(view (dec, enc) string)

let json_pair kind =
  let o = Jsont.objc ~kind () in
  let name = Jsont.mem o "name" Jsont.string in
  let url  = Jsont.mem o "uri"  json_uri in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get name o, Jsont.get url o) in
  let enc (n, u) = Jsont.(new_obj c [memv name n; memv url u]) in
  Jsont.view (dec, enc) c

let pp_pair ppf (n, u) = Fmt.(pf ppf "%s:%s" n (Uri.to_string u))

let json_repository =
  let dec s = `Ok (Repository s) in
  let enc (Repository s) = s in
  Jsont.view (dec, enc) (json_pair "repository")

let json_pin =
  let dec s = `Ok (Pin s) in
  let enc (Pin s) = s in
  Jsont.view (dec, enc) (json_pair "pin")

let pp_repo ppf (Repository s) = pp_pair ppf s
let pp_pin ppf (Pin s) = pp_pair ppf s

type id = [`Task] Id.t

type t = {
  id: id;
  repos: repository list;
  pins: pin list;
  compilers: Compiler.t list;
  hosts: Host.t list;
  packages: Package.t list;
}

let json =
  let o = Jsont.objc ~kind:"task" () in
  let id = Jsont.mem o "id" Id.json in
  let repos = Jsont.(mem ~opt:`Yes_rem o "repos" @@ array json_repository) in
  let pins = Jsont.(mem ~opt:`Yes_rem o "pins" @@ array json_pin) in
  let compilers = Jsont.(mem ~opt:`Yes_rem o "compilers" @@ array Compiler.json) in
  let hosts = Jsont.(mem o ~opt:`Yes_rem "hosts" @@ array Host.json) in
  let packages = Jsont.(mem o "packages" @@ array Package.json) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok {
      id = get id; repos = get repos; pins = get pins;
      compilers = get compilers; hosts = get hosts;
      packages = get packages
    } in
  let enc t =
    Jsont.(new_obj c [
        memv id t.id; memv repos t.repos; memv pins t.pins;
        memv compilers t.compilers; memv hosts t.hosts;
        memv packages t.packages])
  in
  Jsont.view (dec, enc) c

let pp ppf t =
  Fmt.(pf ppf
    "@[<v>\
     id:        %a@,\
     repos:     %a@,\
     pins:      %a@,\
     compilers: %a@,\
     hosts:     %a@,\
     packages:  %a@]"
    Id.pp t.id
    (list pp_repo) t.repos
    (list pp_pin) t.pins
    (list Compiler.pp) t.compilers
    (list Host.pp) t.hosts
    (list Package.pp) t.packages)

let id t = t.id
let packages t = t.packages

let hash ~repos ~pins ~compilers ~hosts ~packages =
  let x l = String.concat "+" (List.sort compare l) in
  let y   = String.concat "-" in
  let p (x, y) = x ^ ":" ^ Uri.to_string y in
  let repos = List.map (function Repository s -> p s) repos in
  let pins = List.map (function Pin s -> p s) pins in
  let compilers = List.map (Fmt.to_to_string Compiler.pp) compilers in
  let hosts = List.map (Fmt.to_to_string Host.pp) hosts in
  let packages = List.map Package.to_string packages in
  let str = y [
      y repos; (* the order in which we stack the repos is important *)
      x pins; x compilers; x hosts; x packages
    ] in
  Id.digest `Task str

let create ?(repos=[]) ?(pins=[])
    ?(compilers=Compiler.defaults) ?(hosts=Host.defaults)
    packages =
  let id = hash ~repos ~pins ~compilers ~hosts ~packages in
  { id; repos; pins; compilers; hosts; packages }

type status = [
  | `Success
  | `Failure
  | `Pending
  | `Cancelled
]

let pp_status fmt x =
  let str = match x with
    | `Success  -> "success"
    | `Failure  -> "failure"
    | `Pending  -> "pending"
    | `Cancelled -> "canceled"
  in
  Fmt.string fmt str

let json_status =
  Jsont.enum [
    "success", `Success; "failure", `Failure;
    "pending", `Pending; "concelled", `Cancelled
  ]
