open Sexplib.Std

type worker_msg =
  | Register
  | Heartbeat of string option
  | Publish of string
with sexp

type master_msg =
  | Ack_register of int * string
  | Ack_heartbeat
  | New_task of string * string
with sexp
