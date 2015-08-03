open Sexplib.Std
open Common_types

type task =
  | Github of string * string option * pull
  (* package name * version * pull *)
  | Package of string * string option * depopt list option
  (* package name * version * depopts *)
  | Compiler of string
 (* compiler version[+tag] *)
and pull = {
    pull_num : int;
    repo_url : string;
    base_sha : string;
    head_sha : string;
}
and depopt = string * string option with sexp

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
  | Github (p, v, _)
  | Package (p, v, None) -> p, v
  | Package (p, v, Some depopts) ->
     let depopt_info =
       depopts
       |> List.rev_map (fun (n, v_opt) ->
            match v_opt with
            | None -> n
            |Some v -> n ^ "." ^ v)
       |> String.concat ";" in
     p ^ "+" ^ depopt_info, v
  | Compiler c -> c, None


let info_of_pkg_task = function
  | Package (n, v, depopts) -> n, v, depopts
  | Compiler _ | Github _ -> assert false


let make_pull num url base head = {
    pull_num = num;
    repo_url = url;
    base_sha = base;
    head_sha = head;}

(* return a deterministic id, based on pakcage name, version, and dependencies
   could add os and architecture later *)
let hash_id task inputs compiler host =
  let task_str = match task with
    | Compiler c -> c
    | Package (n, v_opt, depopt_opt) ->
       let v_str = match v_opt with None -> "" | Some v -> v in
       let depopt_str = match depopt_opt with
         | None -> ""
         | Some depopts ->
            List.rev_map (fun (n ,v_opt) ->
              match v_opt with
              | None -> n | Some v -> n ^ "." ^ v) depopts
            |> String.concat ";" in
       n ^ v_str ^ depopt_str
    | Github (n, v_opt, pull) ->
       let v_str = match v_opt with None -> "" | Some v -> v in
       let pull_str = string_of_int pull.pull_num in
       n ^ v_str ^ pull_str in
  let str = task_str ^ (String.concat ";" inputs) ^ compiler ^ host in
  let `Hex h =
    str
    |> Cstruct.of_string
    |> Nocrypto.Hash.SHA1.digest
    |> Hex.of_cstruct in
  h


let make_pkg_task ~name ?version ?depopts () = Package (name, version, depopts)

let make_compiler_task compiler = Compiler compiler

let make_gh_task ~name ?version pull = Github (name, version, pull)


let make_job id inputs compiler host task =
  {id; inputs; compiler; host; task}

let make_job_entry job dependencies = {job; dependencies}

let unwrap_entry {job; dependencies} = job, dependencies
