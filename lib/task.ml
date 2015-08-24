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
type repo = string * Uri.t

(* package, target *)
type pin = string * Uri.t option

let json_uri =
  let dec s = `Ok (Uri.of_string s) in
  let enc u = Uri.to_string u in
  Jsont.(view (dec, enc) string)

let json_pair kind json_uri =
  let o = Jsont.objc ~kind () in
  let name = Jsont.mem o "name" Jsont.string in
  let uri  = Jsont.mem o "uri"  json_uri in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get name o, Jsont.get uri o) in
  let enc (n, u) = Jsont.(new_obj c [memv name n; memv uri u]) in
  Jsont.view (dec, enc) c

let pp_uri ppf x = Fmt.string ppf (Uri.to_string x)

let pp_pair ppf (n, u) pp_uri = Fmt.(pf ppf "%s:%a" n pp_uri u)

let json_repo = json_pair "repository" json_uri
let json_pin  = json_pair "pin" (Jsont.some json_uri)

let pp_repo ppf s = pp_pair ppf s pp_uri
let pp_pin ppf s  = pp_pair ppf s (Fmt.option pp_uri)

type id = [`Task] Id.t

type t = {
  id: id;
  repos: repo list;
  pins: pin list;
  switches: Switch.t list;
  hosts: Host.t list;
  packages: Package.t list;
}

let equal x y = Id.equal x.id y.id
let compare x y = Id.compare x.id y.id

let json =
  let o = Jsont.objc ~kind:"task" () in
  let id = Jsont.mem o "id" Id.json in
  let repos = Jsont.(mem ~opt:`Yes_rem o "repos" @@ array json_repo) in
  let pins = Jsont.(mem ~opt:`Yes_rem o "pins" @@ array json_pin) in
  let switches = Jsont.(mem ~opt:`Yes_rem o "switches" @@ array Switch.json) in
  let hosts = Jsont.(mem o ~opt:`Yes_rem "hosts" @@ array Host.json) in
  let packages = Jsont.(mem o "packages" @@ array Package.json) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok {
      id = get id; repos = get repos; pins = get pins;
      switches = get switches; hosts = get hosts;
      packages = get packages
    } in
  let enc t =
    Jsont.(new_obj c [
        memv id t.id; memv repos t.repos; memv pins t.pins;
        memv switches t.switches; memv hosts t.hosts;
        memv packages t.packages])
  in
  Jsont.view (dec, enc) c

let pp ppf t =
  Fmt.(pf ppf
    "@[<v>\
     id:       %a@,\
     repos:    %a@,\
     pins:     %a@,\
     switches: %a@,\
     hosts:    %a@,\
     packages: %a@]"
    Id.pp t.id
    (list pp_repo) t.repos
    (list pp_pin) t.pins
    (list Switch.pp) t.switches
    (list Host.pp) t.hosts
    (list Package.pp) t.packages)

let id t = t.id
let packages t = t.packages

let hash ~repos ~pins ~switches ~hosts ~packages =
  let x l = String.concat "+" (List.sort String.compare l) in
  let y   = String.concat "-" in
  let repos = List.map (Fmt.to_to_string pp_repo) repos in
  let pins = List.map (Fmt.to_to_string pp_pin) pins in
  let switches = List.map (Fmt.to_to_string Switch.pp) switches in
  let hosts = List.map (Fmt.to_to_string Host.pp) hosts in
  let packages = List.map Package.to_string packages in
  let str = y [
      y repos; (* the order in which we stack the repos is important *)
      x pins; x switches; x hosts; x packages
    ] in
  Id.digest `Task str

let create ?(repos=[]) ?(pins=[])
    ?(switches=Switch.defaults) ?(hosts=Host.defaults)
    packages =
  let id = hash ~repos ~pins ~switches ~hosts ~packages in
  { id; repos; pins; switches; hosts; packages }

type status = [
  | `New
  | `Pending
  | `Success
  | `Failure
  | `Cancelled
]

let to_string = function
  | `New       -> "new"
  | `Pending   -> "pending"
  | `Success   -> "success"
  | `Failure   -> "failure"
  | `Cancelled -> "canceled"

let status = [`New; `Pending; `Success; `Failure; `Cancelled ]
let pp_status = Fmt.of_to_string to_string

let json_status =
  Jsont.enum ~default:`New (List.map (fun x -> to_string x, x) status)
