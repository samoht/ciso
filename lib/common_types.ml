open Sexplib.Std

type id = string with sexp

type worker_id = int with sexp
type worker_token = string with sexp
type job_rank = int

type compiler = string with sexp
type host = string with sexp

(* for the string from Sexplib.Sexp.to_string *)
type description = string with sexp
