open Sexplib.Std

type t = {
  id : int;
  inputs : int list;
  task : task;
}
and task =
  | Github of string * string * pull
  | Package of string * string
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

let make_task id ?pull package version =
  let task = match pull with
    | Some pull -> Github (package, version, pull)
    | None -> Package (package, version) in
  {id; inputs = []; task}

let update_task t inputs = {t with inputs}

let string_of_t t = ""
