open Sexplib.Std

type id = string with sexp

type worker_id = int with sexp
type worker_token = string with sexp

(* for the string from Sexplib.Sexp.to_string *)
type description = string with sexp
