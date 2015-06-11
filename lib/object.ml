open Sexplib.Std

type t = {
  id : string;                (* object is referenced by id in scheduler *)
  result : [`Success |`Fail]; (* result of opam build *)
  output: string list;      (* relative paths of stdout and stderr in archive *)
  installed : string list;  (* relative paths of installed files in archive *)
  archive: string * string;
  (* archive who holds output and installed files, name * content *)
} with sexp

let id_of_t {id} = id

let create id = {
    id;
    result = `Success;
    output = [".stdout"; ".stderr"];
    installed = [".cmi"; ".cmx"; ".cmxa"; ".cmo"; ".cma";];
    archive =  ".tar.gz", "content" }

let string_of_t obj =
  sexp_of_t obj |> Sexplib.Sexp.to_string

let t_of_string str =
  Sexplib.Sexp.of_string str |> t_of_sexp
