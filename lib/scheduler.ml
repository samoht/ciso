open Lwt

type task_tbl = (int, Task.t) Hashtbl.t (* obj.id -> task *)
type obj_tbl = (int, Object.t) Hashtbl.t (* obj.id -> obj *)
type hook_tbl = (int, int) Hashtbl.t
(* input.id -> (task ->) obj.id  *)

let t_tbl : task_tbl = Hashtbl.create 16
let o_tbl : obj_tbl  = Hashtbl.create 16
let h_tbl : hook_tbl = Hashtbl.create 16

let task_cnt = ref 0
let obj_cnt = ref 0

let user = "ocaml"
let repo = "opam-repository"

let oid_of_task t =
  Hashtbl.fold (fun obj_id task acc ->
    if task = t then obj_id else acc) t_tbl (-1)

let task_of_oid obj_id =
  try Hashtbl.find t_tbl obj_id with _ -> failwith "task not found"

let get_objects obj_id =
  Hashtbl.find_all o_tbl obj_id

(* dispatch the task to the worker who holds the most of its inputs *)
let dispatch task = return ()

let publish_object_hook obj_id =
  if not (Hashtbl.mem h_tbl obj_id) then return ()
  else begin
      let oids = Hashtbl.find_all h_tbl obj_id in
      let tasks = List.map task_of_oid oids in
      Lwt_list.iter_p (fun task ->
        let inputs = Task.inputs_of_t task in
        if List.for_all (fun input -> Hashtbl.mem o_tbl input) inputs then
          dispatch task
        else return ()) tasks
    end

let publish_object obj_id obj =
  choose [return (Hashtbl.add o_tbl obj_id obj);
          publish_object_hook obj_id]

let init_gh_token name =
  Github_cookie_jar.init ()
  >>= fun jar -> Github_cookie_jar.get jar name
  >>= function
    | Some auth -> return (Github.Token.of_auth auth)
    | None -> fail (failwith "None auth")

(* /packages/<pkg>/<pkg.version>/{opam, url, descr, files/.., etc} *)
let packages_of_pull token num = Github.Monad.(
  Github.Pull.list_files ~token ~user ~repo ~num ()
  >>= fun files ->
    List.fold_left (fun acc file ->
        let parts = Array.of_list
          (Str.split (Str.regexp "/") file.Github_t.file_filename) in
        let pkg = try
            if parts.(0) = "packages" && parts.(3) <> "descr"
            then parts.(2) else ""
          with _ -> "" in
        if pkg <> "" && not (List.mem pkg acc) then pkg :: acc else acc)
      [] files
    |> return)

let pull_info token num = Github.Monad.(
  let open Github_t in
  Github.Pull.get ~token ~user ~repo ~num ()
  >>= fun pull ->
    let base = pull.pull_base and head = pull.pull_head in
    let base_repo =
      match base.branch_repo with
      | Some repo -> repo | None -> failwith "pr_info" in
    Task.make_pull num base_repo.repo_clone_url base.branch_sha head.branch_sha
    |> return)

let resolve_and_add pull pkg =
  let action_graph = Ci_opam.resolve pkg in
  let new_task ?pull pkg v =
    incr task_cnt;
    let id = !task_cnt in
    let t = Task.make_task id ?pull pkg v in
    incr obj_cnt;
    let oid = !obj_cnt in
    Hashtbl.add t_tbl oid t;
    oid in
  let update_inputs oid inputs =
    let t = task_of_oid oid in
    let new_task = Task.update_task t inputs in
    Hashtbl.replace t_tbl oid new_task;
    List.iter (fun input -> Hashtbl.add h_tbl input oid) inputs in
  Ci_opam.add_task new_task update_inputs pull action_graph

let github_hook token num = Github.Monad.(
  packages_of_pull token num
  >>= fun pkgs -> pull_info token num
  >>= fun pull ->
    List.iter (resolve_and_add pull) pkgs;
    return ())
