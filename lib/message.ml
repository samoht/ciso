open Sexplib.Std
open Common_types

type worker_msg =
  | Register
  | Heartbeat of id option
  | Publish of id
  with sexp

type master_msg =
  | Ack_register of worker_id * worker_token
  | Ack_heartbeat
  | New_job of id * description
with sexp
