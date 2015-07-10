open Sexplib.Std
open Common_types

type worker_msg =
  | Register of compiler * host
  | Heartbeat of id option
  | Publish of [`Success | `Fail of string | `Delegate of id] * id
  with sexp

type master_msg =
  | Ack_register of worker_id * worker_token
  | Ack_heartbeat
  | New_job of id * description
with sexp
