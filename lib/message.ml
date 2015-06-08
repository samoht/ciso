open Sexplib.Std

type worker_msg =
  | Register of string * int
  | Heartbeat of string option
  | Publish of (string * int) * (string * string)
with sexp

type master_msg =
  | Ack_register of int * string
  | Ack_heartbeat
  | New_task of string * string
  | Best_object of string * int * string
with sexp
