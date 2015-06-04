open Sexplib.Std

type t = {
  id : int;           (* task is referenced by this id *)
  inputs : int list;  (* inputs are object ids *)
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

let make_pull num url base head = {
    pull_num = num;
    repo_url = url;
    base_sha = base;
    head_sha = head;}

let make_task ?pull id package version inputs =
  let task = match pull with
    | Some pull -> Github (package, version, pull)
    | None -> Package (package, version) in
  {id; inputs; task}

let update_task t inputs = {t with inputs}

let string_of_t {id; inputs; task} =
  let task_str = match task with
    | Github (pkg, version, pull) ->
       Printf.sprintf "%s.%s from github/%d" pkg version pull.pull_num
    | Package (pkg, version) ->
       Printf.sprintf "%s.%s" pkg version in
  let inputs_str = String.concat "," (List.rev_map string_of_int inputs) in
  Printf.sprintf "%s -> [%d: %s]" inputs_str id task_str
