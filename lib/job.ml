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

type id = [`Job] Id.t

type t = {
  id      : id;                                                 (* the job id *)
  inputs  : id list;                 (* the transitive reduction of need jobs *)
  switch  : Switch.t;                       (* switch on which to run the job *)
  host    : Host.t;                           (* host on which to run the job *)
  packages: (Package.t * Package.info) list; (* the list of packages to build *)
}

let equal x y = Id.equal x.id y.id
let compare x y = Id.compare x.id y.id

let json_package =
  let o = Jsont.objc ~kind:"job" () in
  let package = Jsont.(mem o "package" Package.json) in
  let info = Jsont.(mem o "info" Package.json_info) in
  let c = Jsont.obj ~seal:true o in
  let dec o = let get m = Jsont.get m o in `Ok (get package, get info) in
  let enc (p, i) = Jsont.(new_obj c [ memv package p; memv info i]) in
  Jsont.view (dec, enc) c

let json =
  let o = Jsont.objc ~kind:"job" () in
  let id = Jsont.mem o "id" Id.json in
  let inputs = Jsont.(mem ~opt:`Yes_rem o "inputs" @@ array Id.json) in
  let switch = Jsont.(mem o "switch" Switch.json) in
  let host = Jsont.(mem o "host" Host.json) in
  let packages = Jsont.(mem o "packages" @@ array json_package) in
  let c = Jsont.obj ~seal:true o in
  let dec o =
    let get m = Jsont.get m o in
    `Ok {
      id = get id; inputs = get inputs; switch = get switch;
      host = get host; packages = get packages
    } in
  let enc t =
    Jsont.(new_obj c [
        memv id t.id; memv inputs t.inputs;
        memv switch t.switch; memv host t.host;
        memv packages t.packages])
  in
  Jsont.view (dec, enc) c

let pp_package ppf (p, _) = Package.pp ppf p

let pp ppf t =
  Fmt.pf ppf
    "@[<v>\
     id:       %a@;\
     inputs:   %a@;\
     switch:   %a@;\
     host:     %a@;\
     packages: %a@]"
    Id.pp t.id
    (Fmt.list Id.pp) t.inputs
    Switch.pp t.switch
    Host.pp t.host
    (Fmt.list pp_package) t.packages

let id t = t.id
let inputs t = t.inputs
let switch t = t.switch
let host t = t.host
let packages t = t.packages

let digest buf = Cstruct.to_string (Nocrypto.Hash.SHA1.digest buf)

let hash ~host ~switch ~packages =
  let x l = String.concat "+" (List.sort String.compare l) in
  let y   = String.concat "-" in
  let switches = [Fmt.to_to_string Switch.pp switch] in
  let hosts = [Fmt.to_to_string Host.pp host] in
  let packages =
    List.map (fun (p, i) ->
        y [Package.to_string p; digest (Package.opam i); digest (Package.url i)]
      ) packages
  in
  let str = y [x switches; x hosts; x packages] in
  Id.digest `Job str

let create ?(inputs=[]) host switch packages =
  let id = hash ~host ~switch ~packages in
  { id; inputs; switch; host; packages; }

type status = [
  | `Pending     (* the job is created *)
  | `Runnable    (* the job is dispatched to a worker to run *)
  | `Running     (* a worker is running the job *)
  | `Success
  | `Failure
  | `Cancelled
]

let to_string = function
  | `Success   -> "success"
  | `Failure   -> "failure"
  | `Pending   -> "pending"
  | `Runnable  -> "runnnable"
  | `Running   -> "running"
  | `Cancelled -> "cancelled"

let status = [ `Success; `Failure; `Pending; `Runnable; `Running; `Cancelled; ]
let pp_status = Fmt.of_to_string to_string

let json_status =
  Jsont.enum ~default:`Pending (List.map (fun x -> to_string x, x) status)

let is_success = function `Success -> true | _ -> false
let is_failure = function `Failure -> true | _ -> false
let is_cancelled = function `Cancelled -> true | _ -> false

let task_status = function
  | [] -> `New
  | l  ->
    if List.for_all is_success l then `Success
    else if List.exists is_failure l then `Failure   (* maybe a bit strong... *)
    else if List.exists is_cancelled l then `Cancelled
    else `Pending
