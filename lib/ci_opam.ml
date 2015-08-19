open OpamTypes
open Lwt


let log action ~info =
  let title = Printf.sprintf "ci_opam@%s" action in
  Printf.eprintf "[%s]: %s\n%!" title info

let time () = Unix.(
  let tm = localtime (time ()) in
  Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec)


let package p = OpamPackage.((name_to_string p) ^ "." ^ (version_to_string p))


(* copied from opam/src/client/opamArg.ml *)
let parse str =
  let re = Re_str.regexp
    "\\([^>=<.!]+\\)\\(>=?\\|<=?\\|=\\|\\.\\|!=\\)\\(.*\\)" in
  try
    if not (Re_str.string_match re str 0) then failwith "no_version";
    let sname = Re_str.matched_group 1 str in
    let sop = Re_str.matched_group 2 str in
    let sversion = Re_str.matched_group 3 str in
    let name = OpamPackage.Name.of_string sname in
    let sop = if sop = "." then "=" else sop in
    let op = OpamFormula.relop_of_string sop in
    (* may raise Invalid_argument *)
    let version = OpamPackage.Version.of_string sversion in
    OpamFormula.(name, Some (op, version))
  with Failure _ | Invalid_argument _ ->
    try OpamFormula.(OpamPackage.Name.of_string str, None)
    with Failure msg -> failwith msg


let get_opam_var variable =
  let v = OpamVariable.Full.of_string variable in
  let var = OpamVariable.Full.variable v in
  (* copied from src/client/opamConfigCommand.ml *)
  let root = OpamPath.root () in
  let switch = match !OpamGlobals.switch with
    | `Command_line s
    | `Env s   -> OpamSwitch.of_string s
    | `Not_set ->
       let config = OpamPath.config root in
       OpamFile.Config.switch (OpamFile.Config.read config) in
  let config = OpamPath.Switch.global_config root switch in
  let config = OpamFile.Dot_config.read config in
  (match OpamState.get_env_var v with
   | Some _ as c -> c
   | None ->
      if OpamVariable.to_string var = "switch" then
        Some (S (OpamSwitch.to_string switch))
      else
        OpamFile.Dot_config.variable config var)
  |> (function
       | Some c -> c
       | None ->
          let t = OpamState.load_state "config-variable" in
          OpamMisc.Option.default (S "#undefined")
            (OpamState.contents_of_variable t OpamVariable.Map.empty v))
  |> OpamVariable.string_of_variable_contents


let parse_user_demand pkg =
  match parse pkg with
  | name, None -> OpamPackage.Name.to_string name, None
  | name, Some (_, v) -> OpamPackage.Name.to_string name,
                         Some (OpamPackage.Version.to_string v)

let print_edges vx vy =
  let string_of_action v =
    let action, pkg = match v with
        | To_delete p -> "Delete", OpamPackage.to_string p
        | To_recompile p -> "Recompile", OpamPackage.to_string p
        | To_change (o_opt, t) ->
           let origin = match o_opt with
             | Some o -> OpamPackage.to_string o
             | None -> "" in
           let target = OpamPackage.to_string t in
           "Change", Printf.sprintf "%s to %s" origin target in
    Printf.sprintf "[%s]: %s" action pkg in
  let strx = string_of_action vx in
  let stry = string_of_action vy in
  Printf.printf "%s\t->\t%s\n%!" strx stry


let modify_state state =
  let open OpamState.Types in
  let base = OpamState.base_packages in
  let is_base pkg = List.mem (OpamPackage.name pkg) base in

  let open OpamPackage.Set.Op in
  { state with
    installed = OpamPackage.Set.filter is_base state.installed;
    installed_roots = OpamPackage.Set.filter is_base state.installed_roots;
    pinned = OpamPackage.Name.Map.empty; }


let resolve ?(bare = true) state str_lst =
  let state = if bare then modify_state state else state in
  let atoms = List.rev_map parse str_lst in
  let install_set = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let action = Install install_set in
  let universe = OpamState.universe state action in
  let request = {
    wish_install = atoms;
    wish_remove = [];
    wish_upgrade = [];
    criteria = `Default} in
  let switch = OpamSwitch.to_string state.OpamState.Types.switch in
  log "resolve" ~info:("@switch " ^ switch ^
    " preinstalled: " ^ get_opam_var "preinstalled");
  let result = OpamSolver.resolve
    ~orphans:OpamPackage.Set.empty ~requested:OpamPackage.Name.Set.empty
    universe request in
  let solution = match result with
    | Success s -> s
    | Conflicts c ->
       let info = OpamCudf.string_of_conflict OpamFormula.string_of_atom c in
       let str = String.concat ";" str_lst in
       failwith (Printf.sprintf "no solution for %s: %s" str info) in
  let graph = solution.to_process in
  let oc = open_out "solver_log" in
  OpamSolver.ActionGraph.Dot.output_graph oc graph; close_out oc;
  graph
  (* OpamSolver.ActionGraph.iter_edges print_edges graph *)


let load_state ?switch () =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  begin
    match switch with
    | None -> ()
    | Some s -> OpamGlobals.switch := `Env s
  end;
  OpamState.load_state "state"


let rec compiler ?state () =
  match state with
  | Some s ->
     s.OpamState.Types.compiler
     |> OpamCompiler.to_string
  | None ->
     let state = load_state () in
     compiler ~state ()


let jobs_of_graph ?pull ?repository ?pin graph =
  let module Graph = OpamSolver.ActionGraph in
  let module Pkg = OpamPackage in
  let package_of_action = function
    | To_change (None, target) -> target
    | To_change (Some origin, target) ->
       let info = Printf.sprintf "WARNING: %s -> %s"
                                 (package origin) (package target) in
       log "jobs_of_graph" ~info;
       origin
    | To_delete p | To_recompile p ->
       failwith ("Not expect delete/recompile " ^ (package p))  in
  let process_queue = Queue.create () in
  let add_stack = Stack.create () in
  Graph.iter_vertex (fun v ->
      if Graph.out_degree graph v = 0 then Queue.add v process_queue) graph;

  while not (Queue.is_empty process_queue) do
    let v = Queue.pop process_queue in
    Graph.iter_pred (fun pred -> Queue.add pred process_queue) graph v;
    Stack.push v add_stack;
  done;

  let compiler = compiler () in
  let host = Host.detect () |> Host.to_string in
  let module IdSet = struct
      include Set.Make(String)
      let to_list s = fold (fun e acc -> e :: acc) s [] end in
  let id_map = ref Pkg.Map.empty in
  let deps_map = ref Pkg.Map.empty in
  let j_lst = ref [] in
  while not (Stack.is_empty add_stack) do
    let v = Stack.pop add_stack in
    let pkg = package_of_action v in
    let name, version = Pkg.(name_to_string pkg, version_to_string pkg) in
    let task =
      if Graph.out_degree graph v = 0 && pull <> None then
        let pull = match pull with None -> assert false | Some p -> p in
        Task.make_gh_task ~name ~version pull
      else Task.make_pkg_task ~name ~version () in

    let inputs, deps = Graph.fold_pred (fun pred (i, d) ->
        let pred_pkg = package_of_action pred in
        let pred_id = Pkg.Map.find pred_pkg !id_map in
        let pred_deps = Pkg.Map.find pred_pkg !deps_map in

        pred_id :: i,
        IdSet.union d (IdSet.add pred_id pred_deps))
      graph v ([], IdSet.empty) in

    (* repo and pin, hash id? *)
    let id = Task.hash_id task inputs compiler host in
    let job = Task.make_job id inputs compiler host task ?repository ?pin () in

    id_map := Pkg.Map.add pkg id !id_map;
    deps_map := Pkg.Map.add pkg deps !deps_map;
    j_lst := (id, job, IdSet.to_list deps) :: !j_lst
  done;
  !j_lst


let resolvable ~name ?version ?depopts state =
  let str = match version with None -> name | Some v -> name ^ "." ^ v in
  let depopt_str = match depopts with
    | None -> []
    | Some lst ->
       List.rev_map (fun (n, v_opt) ->
         match v_opt with
         |None -> n | Some v -> n ^ "." ^ v) lst in
  let graph = resolve ~bare:false state (str :: depopt_str) in
  if 1 = OpamSolver.ActionGraph.nb_vertex graph then false, graph
  else true, graph


let installed_of_state s =
  OpamPackage.Set.fold (fun p acc -> (package p) :: acc)
    s.OpamState.Types.installed []
  |> String.concat " ; "


let conf_file ()=
  let path = Sys.getenv "PATH" in
  let rec find pos =
    let comma = String.index_from path pos ':' in
    let dir = String.sub path pos (comma - pos) in
    let fname = Filename.concat dir "ocamlfind" in
    if Sys.file_exists fname then dir
    else find comma in
  let bin = find 0 in
  let conf =
    Filename.concat (Filename.dirname bin) "lib/findlib.conf" in
  assert (Sys.file_exists conf);
  conf


let read_conf conf =
  let rec read_conf_aux acc ic =
    Lwt_io.read_line_opt ic >>= function
    | None -> return acc
    | Some line ->
       let eg = String.index line '=' in
       let name = String.sub line 0 eg in
       let value = String.sub line (eg + 2) (String.length line - eg - 3) in
       read_conf_aux ((name, value) :: acc) ic in
  Lwt_io.with_file Lwt_io.input conf (read_conf_aux [])


let write_conf conf tups =
  (* make all the non-existent parent directories *)
  let prepare conf =
    let rec base acc path =
      if not (Sys.file_exists path) then
        let basename = Filename.basename path in
        let dir = Filename.dirname path in
        base (basename :: acc) dir
      else acc, path in
    let rec prepare_dir path = Lwt_unix.(function
      | [] -> return_unit
      | [f] ->
         openfile f [O_RDWR; O_CREAT] 0o664 >>= fun fd -> close fd
      | hd :: tl ->
         (let new_dir = Filename.concat path hd in
          mkdir new_dir 0o775 >>= fun () ->
          prepare_dir new_dir tl)) in
    let names, dir = base [] conf in
    prepare_dir dir names in

  let write_conf_aux oc =
    List.rev_map (fun (n, v) -> Printf.sprintf "%s=\"%s\"" n v) tups
    |> String.concat "\n"
    |> Lwt_io.write oc in
  prepare conf >>= fun () ->
  Lwt_io.with_file Lwt_io.output conf write_conf_aux


let modify_conf conf conf_path ~destdir ~path =
  let remove_tup name tups =
    if List.mem_assoc name tups then
      List.remove_assoc name tups
    else tups in
  let add_tup name value tups =
    let ntups = remove_tup name tups in
    (name, value) :: ntups in

  read_conf conf >>= fun tups ->
  let d = List.assoc "destdir" tups in
  let p = List.assoc "path" tups in

  if d = destdir && p = path then return_unit
  else
    tups
    |> remove_tup "destdir"
    |> remove_tup "path"
    |> add_tup "destdir" destdir
    |> add_tup "path" path
    |> write_conf conf_path


let findlib_conf ~prefix ~write_path =
  let conf = Filename.concat prefix "lib/findlib.conf" in
  if not (Sys.file_exists conf) then return_unit
  else begin
      let destdir = Filename.concat prefix "lib" in
      let path = Filename.concat prefix "lib" in
      modify_conf conf write_path ~destdir ~path
    end


let opam_install state ~name ~version =
  let nv =
    let name = OpamPackage.Name.of_string name in
    let version = OpamPackage.Version.of_string version in
    OpamPackage.create name version in

  match Lwt_unix.fork () with
  | 0 ->
     OpamAction.download_package state nv;
     OpamAction.build_and_install_package state ~metadata:true nv;
     let installed, installed_roots, reinstall = OpamState.Types.(
       state.installed, state.installed_roots, state.reinstall) in
     ignore (OpamAction.update_metadata state ~installed_roots ~reinstall
       ~installed:(OpamPackage.Set.add nv installed));
     exit 0;
  | pid ->
     Lwt_unix.(waitpid [] pid >>= fun (_, stat) ->
     match stat with
     | WEXITED i when i = 0 -> return `Success
     | WEXITED i | WSIGNALED i | WSTOPPED i ->
        return (`Fail "opam build"))

(*
  try
    OpamAction.download_package state nv;
    OpamAction.build_and_install_package state ~metadata:true nv;
    let installed, installed_roots, reinstall = OpamState.Types.(
      state.installed, state.installed_roots, state.reinstall) in
    let _ =
      OpamAction.update_metadata state ~installed_roots ~reinstall
        ~installed:(OpamPackage.Set.add nv installed) in
    return `Success;
  with _ -> return (`Fail "opam build") *)


let opam_uninstall ~name ~version =
  let str = name ^ "." ^ version in
  let atom = parse str in
  OpamGlobals.yes := true;
  OpamClient.SafeAPI.remove ~autoremove:true ~force:true [atom];
  return_unit


let update_metadata ~install state ~path =
  let rec packages_of_file acc ic =
    Lwt_io.read_line_opt ic >>= function
      | None -> return acc
      | Some line ->
         let pos = Re_str.search_forward (Re_str.regexp_string " ") line 0 in
         let name = String.sub line 0 pos |> OpamPackage.Name.of_string in
         let version = String.sub line (pos + 1) (String.length line - pos - 1)
                       |> OpamPackage.Version.of_string in
         let nv = OpamPackage.create name version in
         packages_of_file (nv :: acc) ic in
  Lwt_io.with_file Lwt_io.input path (packages_of_file []) >>= fun pkg_lst ->

  let installed =
    List.fold_left (fun set p ->
        if install then OpamPackage.Set.add p set
        else OpamPackage.Set.remove p set)
      state.OpamState.Types.installed pkg_lst in
  let installed_roots = OpamPackage.Set.inter installed
      state.OpamState.Types.installed_roots in
  let reinstall = OpamPackage.Set.inter installed
      state.OpamState.Types.reinstall in
  OpamAction.update_metadata state ~installed ~installed_roots ~reinstall
  |> return


let export_existed_switch r c =
  OpamGlobals.root_dir := r;
  let root = OpamFilename.Dir.of_string r in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string c in
  if not (OpamSwitch.Map.mem switch aliases) then ()
  else begin
    OpamSwitchCommand.switch ~quiet:false ~warning:false switch;
    let file = Printf.sprintf "ci_%s_%s.export" c (time ()) in
    let path = OpamFilename.(
      let dir = OP.(root / "log") in
      if exists_dir dir then OP.( dir // file)
      else OP.(root // file)) in
    OpamSwitchCommand.export (Some path);
    OpamSwitchCommand.switch ~quiet:false ~warning:false (OpamSwitch.of_string "system");
    OpamGlobals.yes := true;
    OpamSwitchCommand.remove switch end;
  return_unit


let opam_eval_env () =
  let env_s = OpamState.load_env_state "env_state" in
  let env = OpamState.get_opam_env ~force_path:true env_s in
  List.iter (fun (n, v) -> Unix.putenv n v) env


let opam_install_switch r c =
  OpamGlobals.root_dir := r;
  let root = OpamFilename.Dir.of_string r in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string c in

  (if OpamSwitch.Map.mem switch aliases then export_existed_switch r c
   else return_unit) >>= fun () ->

  let compiler = OpamCompiler.of_string c in
  match Lwt_unix.fork () with
  | 0 ->
     OpamSwitchCommand.install
       ~quiet:false ~warning:true ~update_config:true switch compiler;
     exit 0
  | pid ->
     Lwt_unix.(waitpid [] pid >>= fun (_, stat) ->
       (match stat with
        | WEXITED i when i = 0 -> ()
        | WEXITED i | WSIGNALED i | WSTOPPED i ->
           failwith (Printf.sprintf "exited %d" i));
       opam_eval_env ();
       return_unit)


let opam_remove_switch r c =
  export_existed_switch r c >>= fun () ->
  OpamGlobals.root_dir := r;
  OpamGlobals.yes := true;
  let switch = OpamSwitch.of_string c in
  OpamSwitchCommand.switch ~quiet:false ~warning:false (OpamSwitch.of_string "system");
  OpamSwitchCommand.remove switch;
  return_unit


let opam_switch_switch r c =
  OpamGlobals.root_dir := r;
  let root = OpamFilename.Dir.of_string r in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string c in
  let compiler = OpamCompiler.of_string c in
  let new_aliases = OpamSwitch.Map.add switch compiler aliases in
  OpamFile.Aliases.write (OpamPath.aliases root) new_aliases;

  OpamSwitchCommand.switch ~quiet:false ~warning:false switch;
  opam_eval_env ();
  return_unit


let detect_root () =
  OpamGlobals.default_opam_dir


let detect_compiler () =
  let state = load_state () in
  let c = state.OpamState.Types.compiler in
  OpamCompiler.to_string c


let clean_repositories () =
  let t = OpamState.load_state "repository_clean" in
  let repos = OpamState.sorted_repositories t in
  List.iter (fun r ->
    let name = OpamRepositoryName.to_string r.repo_name in
    if name = "default" then ()
    else OpamRepositoryCommand.remove r.repo_name) repos


let add_repositories repo =
  let add_one_repo (name, address, priority) =
    log "repository" ~info:(Printf.sprintf "add %s %s" name address);
    let name = OpamRepositoryName.of_string name in
    let address = OpamTypesBase.address_of_string address in
    let address, kind2 = OpamTypesBase.parse_url address in
    let kind = OpamMisc.Option.default kind2 None in
    OpamRepositoryCommand.add name kind address ~priority in

  match Lwt_unix.fork () with
  | 0 ->
     clean_repositories ();
     List.iter add_one_repo repo;
     exit 0
  | pid ->
     Lwt_unix.(waitpid [] pid >>= fun (_, stat) ->
       (match stat with
        | WEXITED i when i = 0 -> return_unit
        | WEXITED i | WSIGNALED i | WSTOPPED i ->
           fail_with (Printf.sprintf "exited %d when add repos" i)))


let add_pins pin =
  let add_one_pin (pkg, target) =
    let name = OpamPackage.Name.of_string pkg in
    if target = "dev-repo" then
      OpamClient.SafeAPI.PIN.pin ~edit:false ~action:false name None
    else
      let pin_option = OpamTypesBase.pin_option_of_string ?kind:None target in
      let kind = OpamTypesBase.kind_of_pin_option pin_option in
      let () = assert (kind <> `local) in
      OpamClient.SafeAPI.PIN.pin
        ~edit:false ~action:false name (Some pin_option) in
  List.iter add_one_pin pin;
  return_unit


let opam_update ~repos_only () =
  match Lwt_unix.fork () with
  | 0 ->
     OpamClient.SafeAPI.update ~repos_only [];
     exit 0
  | pid ->
     Lwt_unix.(waitpid [] pid >>= fun (_, stat) ->
       (match stat with
        | WEXITED i when i = 0 -> ()
        | WEXITED i | WSIGNALED i | WSTOPPED i ->
           failwith (Printf.sprintf "exited %d when opam update" i));
       return_unit)


(* only for testing *)
let show_repo_pin state =
  let repo = state.OpamState.Types.repositories in
  let pin = state.OpamState.Types.pinned in

  let repo_str =
    OpamRepositoryName.Map.fold (fun name repo acc ->
      let name = OpamRepositoryName.to_string name in
      let kind = OpamTypesBase.string_of_repository_kind repo.repo_kind in
      let address =
        let url, segment = repo.repo_address in
        match segment with
        | None -> url
        | Some s -> url ^ "#" ^ s in
      let priority = string_of_int repo.repo_priority in
      Printf.sprintf "[%s] %s %s %s" priority name kind address :: acc) repo  []
    |> String.concat "\n" in

  let pin_str =
    OpamPackage.Name.Map.fold (fun name pin_option acc ->
      let name = OpamPackage.Name.to_string name in
      let pin_option = OpamTypesBase.string_of_pin_option pin_option in
      Printf.sprintf "%s %s" name pin_option :: acc) pin []
    |> String.concat "\n" in

  let info =
    Printf.sprintf "\n[Repo]:\n%s\n[Pin]:\n%s" repo_str pin_str in
  log "repo_pin" ~info;
  return_unit


let set_root root =
  OpamGlobals.root_dir := root
