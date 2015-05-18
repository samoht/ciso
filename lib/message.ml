open Sexplib.Std

type worker_msg =
  | Register of string * int
  | Heartbeat of int option
  | Publish of (string * int) * (int * string)
with sexp

type master_msg =
  | Ack_register of int * string
  | Ack_heartbeat
  | New_task of int * int
with sexp
