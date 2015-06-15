open Sexplib.Std
open Common_types

type t = {
  id : id;           (* task is referenced by this id *)
  inputs : id list;  (* inputs are object ids *)
  compiler : string;
  host : string;
  task : task;
}
 (* a task may be a github PR or a dependency resolved by opam solver *)
and task =
  | Github of string * string * pull   (* package name * version * pull *)
  | Package of string * string         (* package name * version *)
and pull = {
    pull_num : int;
    repo_url : string;
    base_sha : string;
    head_sha : string;
} with sexp

let id_of_t {id} = id

let inputs_of_t {inputs} = inputs

let info_of_t t =
  match t.task with
  | Github (p, v, _) | Package (p, v) -> p, v

let make_pull num url base head = {
    pull_num = num;
    repo_url = url;
    base_sha = base;
    head_sha = head;}

let make_task ?pull id package version inputs compiler host =
  let task = match pull with
    | Some pull -> Github (package, version, pull)
    | None -> Package (package, version) in
  {id; inputs; compiler; host; task;}
