open Sexplib.Std

type t = {
  id : int;
  pull : pull;
  inputs : Object.t list;
}
and pull = {
  pull_num : int;
  repo_url : string;
  base_sha : string;
  head_sha : string;
} with sexp

let id_of_t {id} = id

let inputs_of_t {inputs} = inputs

let string_of_t t =
  let pull_string =
    Printf.sprintf "PR%d from %s\nfrom%s\nto%s"
      t.pull.pull_num t.pull.repo_url t.pull.base_sha t.pull.head_sha in
  Printf.sprintf "I'm task %d,\n  [created from]:\n%s\n" t.id pull_string
