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
let hosts t = t.hosts
let switches t = t.switches

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
  let mk pp = List.map (Fmt.to_to_string pp) in
  let block = [
    "id      ", [Id.to_string t.id];
    "repo    ", mk pp_repo t.repos;
    "pins    ", mk pp_pin t.pins;
    "switches", mk Switch.pp t.switches;
    "hosts   ", List.map Host.short t.hosts;
    "packages", mk Package.pp t.packages;
  ] in
  Gol.show_block ppf block

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

type core = [ `New | `Pending | `Cancelled ]
type dispatch =  [`Pending | `Started]
type complete = [`Success | `Failure]

type status = [
  | core
  | `Dispatched of  [`Worker] Id.t * dispatch
  | `Complete of complete
]

let to_string = function
  | `New       -> "new"
  | `Dispatched-> "dispatched"
  | `Pending   -> "pending"
  | `Started   -> "started"
  | `Resolving -> "resolving"
  | `Complete  -> "complete"
  | `Success   -> "success"
  | `Failure   -> "failure"
  | `Cancelled -> "canceled"

let core = [`New; `Dispatched; `Pending; `Complete; `Cancelled ]
let dispatch = [ `Pending; `Started]
let complete = [`Success; `Failure]

let mk_enum status =
  let default = List.hd status in
  Jsont.enum ~default @@ List.map (fun s -> to_string s, s) status

let json_params =
  let o = Jsont.objc ~kind:"task-status-params" () in
  let worker = Jsont.(mem_opt o "worker" Id.json) in
  let status = Jsont.(mem o "status" @@ mk_enum (dispatch @ complete)) in
  let c = Jsont.obj ~seal:true o in
  let dec o = `Ok (Jsont.get worker o, Jsont.get status o) in
  let enc (w, s) = Jsont.(new_obj c [memv worker w; memv status s]) in
  Jsont.view (dec, enc) c

let json_status =
  let o = Jsont.objc ~kind:"worker-status" () in
  let status = Jsont.(mem o "status" @@ mk_enum core) in
  let params = Jsont.(mem_opt o "params" json_params) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let params = match Jsont.get params o with
      | None -> `N
      | Some (Some id, (#dispatch as p)) -> `D (id, p)
      | Some (None   , (#complete as p)) -> `C p
      | _ -> `Error "task_params"
    in
    match Jsont.get status o, params with
    | `Dispatched, `D p -> `Ok (`Dispatched p)
    | `Complete  , `C p -> `Ok (`Complete p)
    | #core as x , `N   -> `Ok x
    | _ -> `Error "task_status"
  in
  let enc t =
    let cast t = (t :> [dispatch | complete]) in
    let s, i = match t with
      | `Dispatched (w, p) -> `Dispatched, Some (Some w, cast p)
      | `Complete p        -> `Complete  , Some (None  , cast p)
      | #core as x         -> x          , None
    in
    Jsont.(new_obj c [memv status s; memv params i])
  in
  Jsont.view (dec, enc) c

let pp_s ppf = Fmt.of_to_string to_string ppf

let pp_status ppf = function
  | `Dispatched (w, s) -> Fmt.pf ppf "dispatched to %a (%a)" Id.pp w pp_s s
  | `Complete s -> Fmt.pf ppf "complete: %a" pp_s s
  | #core as  x -> Fmt.of_to_string to_string  ppf x
