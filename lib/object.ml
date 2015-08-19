open Sexplib.Std

type t = {
  id : Common_types.id;            (* object is referenced by id in scheduler *)
  result : [`Success |`Fail of string
           |`Delegate of Common_types.id];            (* result of opam build *)
  output: string list;      (* relative paths of stdout and stderr in archive *)
  installed : string list;    (* relative paths of installed files in archive *)
  archive: string * string;
              (* archive who holds output and installed files, name * content *)
} with sexp

let id_of_t {id} = id
let apply_info {installed; archive} = installed, archive
let installed_of_t {installed} = installed
let result_of_t {result} = result

let make_obj id result ~output ~installed archive =
  {id; result; output; installed; archive}

let string_of_t obj =
  sexp_of_t obj |> Sexplib.Sexp.to_string

let t_of_string str =
  Sexplib.Sexp.of_string str |> t_of_sexp

let string_of_result = function
  | `Success -> "SUCCESS"
  | `Fail f -> "FAIL f"
  | `Delegate id ->"DELEGATE " ^ id

let info_of_result = function
  | `Success -> "SUCCESS", None
  | `Fail f -> "FAIL", Some f
  | `Delegate id -> "DELEGATE", Some id
