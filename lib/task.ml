open Sexplib.Std
open Common_types

type task =
  | Github of string * string * pull   (* package name * version * pull *)
  | Package of string * string         (* package name * version *)
and pull = {
    pull_num : int;
    repo_url : string;
    base_sha : string;
    head_sha : string;
} with sexp

type job = {
  id : id;           (* task is referenced by this id *)
  inputs : id list;  (* inputs are object ids *)
  compiler : compiler;
  host : host;
  task : task;
} with sexp

type job_entry = {
  job : job;
  dependencies : id list;
} with sexp

let id_of_job {id} = id
let inputs_of_job {inputs} = inputs
let task_of_job {task} = task
let env_of_job {compiler; host} = (compiler, host)

let info_of_task task =
  match task with
  | Github (p, v, _) | Package (p, v) -> p, v

let make_pull num url base head = {
    pull_num = num;
    repo_url = url;
    base_sha = base;
    head_sha = head;}

(* return a deterministic id, based on pakcage name, version, and dependencies
   could add os and architecture later *)
let hash_id pkg v inputs compiler host =
  let str = pkg ^ v ^ (String.concat ";" inputs) ^ compiler ^ host in
  let hash str =
    let hex_of_cs cs =
      let buf = Buffer.create 16 in
      Cstruct.hexdump_to_buffer buf cs;
      Buffer.contents buf in
    let stripe_nl_space s = Re.(
      let re = compile (alt [compl [notnl]; space]) in
      replace_string re ~by:"" s) in
    Cstruct.of_string str |> Nocrypto.Hash.SHA1.digest
    |> hex_of_cs |> stripe_nl_space in
  hash str

let make_job ?pull id package version inputs compiler host =
  let task = match pull with
    | Some pull -> Github (package, version, pull)
    | None -> Package (package, version) in
  {id; inputs; compiler; host; task;}

let make_job_entry job dependencies = {
  job; dependencies}

let unwrap_entry {job; dependencies} = job, dependencies
