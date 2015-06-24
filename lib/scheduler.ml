open Lwt
open Common_types

(* set of task/object ids*)
module IdSet = Set.Make(struct
  type t = id
  let compare = String.compare
end)

(* map from worker token to tasks completed by that worker as IdSet.t *)
module LogMap = Map.Make(struct
  type t = worker_token
  let compare = String.compare
end)

type job_tbl = (id, Task.job) Hashtbl.t

type hook_tbl = (id, id) Hashtbl.t

type state = [`Pending | `Runnable | `Completed | `Dispatched of worker_token]
type state_tbl = (id, state) Hashtbl.t

let j_tbl : job_tbl = Hashtbl.create 16
let h_tbl : hook_tbl = Hashtbl.create 16
let s_tbl : state_tbl = Hashtbl.create 16
let w_map = ref LogMap.empty

let sub_abbr str = String.sub str 0 5

let log action func ~info =
  let title = Printf.sprintf "%s@%s" action func in
  if info = "" then Printf.eprintf "[%s]\n%!" title
  else Printf.eprintf "\t[%s]: %s\n%!" title info


let task_info id =
  let sub = String.sub id 0 5 in
  try
    let job = Hashtbl.find j_tbl id in
    let p, v = Task.(task_of_job job |> info_of_task) in
     sub ^ ":" ^ p ^ "." ^ v
  with Not_found -> Printf.sprintf "Object %s not in the j_tbl" sub
     | e -> raise e


let register_token wtoken =
  Printf.eprintf "\t[scheduler@register]: new worker %s\n%!"
                 (String.sub wtoken 0 5);
  w_map := LogMap.add wtoken IdSet.empty !w_map


let invalidate_token wtoken =
  Hashtbl.iter (fun id s ->
      if s = (`Dispatched wtoken)
      then Hashtbl.replace s_tbl id `Runnable) s_tbl;
  Printf.eprintf "\t[scheduler@invalidate]: %d/%d jobs\n%!"
   (Hashtbl.fold (fun id _ acc ->
         if `Runnable = Hashtbl.find s_tbl id then succ acc else acc) j_tbl 0)
   (Hashtbl.length j_tbl);
  w_map := LogMap.remove wtoken !w_map


let find_job wtoken =
  let runnables = Hashtbl.fold (fun id state acc ->
      if state = `Runnable then id :: acc else acc) s_tbl [] in
  if runnables = [] then None
  else begin
      let wset = LogMap.find wtoken !w_map in
      let to_depset inputs = List.fold_left (fun set input ->
          IdSet.add input set) IdSet.empty inputs in
      let id, _ = List.fold_left (fun (i, n) tid ->
          let job = Hashtbl.find j_tbl tid in
          let inputs = Task.inputs_of_job job in
          let dset = to_depset inputs in
          let d = IdSet.cardinal (IdSet.inter wset dset) in
          if d > n then tid, d else i, n) ("", (-1)) runnables in
      Hashtbl.replace s_tbl id (`Dispatched wtoken);
      Printf.eprintf "\t[scheduler@find_job]: -> %s\n%!" (task_info id);

      let job = Hashtbl.find j_tbl id in
      let desp = Sexplib.Sexp.to_string (Task.sexp_of_job job) in
      Some (id, desp) end


let publish_object_hook id =
  if not (Hashtbl.mem h_tbl id) then return ()
  else begin
      let ids = Hashtbl.find_all h_tbl id in
      let tups = List.rev_map (fun i -> i,
          try Hashtbl.find j_tbl i with Not_found ->
            let info = "Not_found J_TBL " ^ (sub_abbr i) in
            log "scheduler" "publish_hook" ~info;
            raise Not_found) ids in

      Lwt_list.fold_left_s (fun cache (i, job) ->
        let state =
          try Hashtbl.find s_tbl i with Not_found ->
            let info = "Not_found S_TBL " ^ (sub_abbr i) in
            log "scheduler" "publish_hook" ~info;
            raise Not_found in

        if state <> `Pending then return cache
        else begin
          let inputs = Task.inputs_of_job job in
          let cached, store = List.partition (fun input ->
              List.mem_assoc input cache) inputs in

          if List.exists (fun input -> false = List.assoc input cache) cached
          then return cache else
            Lwt_list.rev_map_s (fun input -> Store.query_object input) store
            >>= fun store_results ->
            let store_tups = List.combine store (List.rev store_results) in
            Lwt_list.fold_left_s (fun acc tup ->
                return (tup :: acc)) cache store_tups
            >>= fun new_cache ->
            if List.for_all (fun re -> re = true) store_results then
              Hashtbl.replace s_tbl i `Runnable;
            return new_cache end) [] tups
      >>= fun _ -> return ()
    end


let publish_object wtoken result id =
  (match result with
   | `Success ->
      let wset = LogMap.find wtoken !w_map in
      let n_wset = IdSet.add id wset in
      w_map := LogMap.add wtoken n_wset !w_map;
      Hashtbl.replace s_tbl id `Completed;
      Printf.eprintf "\t[scheduler@publish]: %s completed\n%!"
        (String.sub id 0 5);
      publish_object_hook id
   | `Fail _ -> begin
       Hashtbl.replace s_tbl id `Runnable;
       return_unit end) >>= fun () ->

  Printf.eprintf "\t[scheduler@publish]: publish hook completed\n%!";
  let runnables = Hashtbl.fold (fun oid state acc ->
      if state = `Runnable then oid :: acc else acc) s_tbl [] in
  let str = String.concat " ; " (List.rev_map task_info runnables) in
  Printf.eprintf "\t[scheduler@publish]: {%s}\n%!" str;
  return ()


let user = "ocaml"
let repo = "opam-repository"
let token = ref None


let init_gh_token name =
  Github_cookie_jar.init ()
  >>= fun jar -> Github_cookie_jar.get jar name
  >>= function
    | Some auth -> return (Github.Token.of_auth auth)
    | None -> fail (failwith "None auth")

(* /packages/<pkg>/<pkg.version>/{opam, url, descr, files/.., etc} *)
let packages_of_pull token num = Github.Monad.(
  Github.Pull.files ~token ~user ~repo ~num ()
  |> Github.Stream.to_list
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
  >>= fun pull_resp ->
    let pull = Github.Response.value pull_resp in
    let base = pull.pull_base and head = pull.pull_head in
    let base_repo =
      match base.branch_repo with
      | Some repo -> repo | None -> failwith "pr_info" in
    Task.make_pull
      num base_repo.repository_clone_url base.branch_sha head.branch_sha
    |> return)


let update_tables new_tasks =
  Lwt_list.iter_p (fun (id, t) ->
      Store.log_job id t >>= fun () ->
      Hashtbl.replace j_tbl id t;
      let inputs = Task.inputs_of_job t in
      Lwt_list.for_all_p (fun input -> Store.query_object input) inputs
      >>= fun runnable ->
      (if runnable then Hashtbl.replace s_tbl id `Runnable
       else begin
           List.iter (fun input -> Hashtbl.add h_tbl input id) inputs;
           Hashtbl.replace s_tbl id `Pending; end);
      return ()) new_tasks


let bootstrap () =
  Store.retrieve_jobs ()
  >>= update_tables >>= fun () ->
  Printf.eprintf "\t[scheduler@bootstrap]: %d/%d jobs\n%!"
   (Hashtbl.fold (fun id _ acc ->
         if `Runnable = Hashtbl.find s_tbl id then succ acc else acc) j_tbl 0)
   (Hashtbl.length j_tbl);
  return ()

let resolve_and_add ?pull pkg =
  let state = Ci_opam.load_state () in
  let action_graph = Ci_opam.resolve state pkg in

  let jobs = Ci_opam.jobs_of_graph ?pull action_graph in
  Lwt_list.filter_p (fun (id, _) ->
      Store.query_object id >>= fun in_store ->
      return (not (in_store || Hashtbl.mem j_tbl id))) jobs
  >>= update_tables
  >>= fun () ->
  Printf.eprintf "\t[scheduler@resolve]: %d/%d jobs\n%!"
    (Hashtbl.fold (fun id _ acc ->
         if `Runnable = Hashtbl.find s_tbl id then succ acc else acc) j_tbl 0)
    (Hashtbl.length j_tbl);
  return ()

let github_hook num =
  (match !token with
    | Some t -> return t
    | None -> begin
        init_gh_token "scry"
        >>= fun t ->
          token := Some t;
          return t
      end)
  >>= fun token -> Github.Monad.run (pull_info token num)
  >>= fun pull -> Github.Monad.run (packages_of_pull token num)
  >>= fun pkgs -> Lwt_list.iter_s (resolve_and_add ~pull) pkgs

let user_demand pkg =
  resolve_and_add pkg
