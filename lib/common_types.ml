open Sexplib.Std

type id = string with sexp

type worker_token = string

(* for the string from Sexplib.Sexp.to_string *)
type description = string
