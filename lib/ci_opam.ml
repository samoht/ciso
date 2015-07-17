open OpamTypes
open Lwt


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


let resolve ?(bare = true) state str =
  let state = if bare then modify_state state else state in
  let atom = parse str in
  let install_set = OpamPackage.Name.(Set.add (fst atom) Set.empty) in
  let action = Install install_set in
  let universe = OpamState.universe state action in
  let request = {
    wish_install = [atom];
    wish_remove = [];
    wish_upgrade = [];
    criteria = `Default} in
  let result = OpamSolver.resolve
    ~orphans:OpamPackage.Set.empty ~requested:OpamPackage.Name.Set.empty
    universe request in
  let solution = match result with
    | Success s -> s
    | Conflicts c ->
       let info = OpamCudf.string_of_conflict OpamFormula.string_of_atom c in
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


let jobs_of_graph ?pull graph =
  let module Graph = OpamSolver.ActionGraph in
  let module Pkg = OpamPackage in
  let package_of_action = function
    | To_change (origin, target) -> assert (origin = None); target
    | To_delete _ | To_recompile _ -> failwith "Not expect" in
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

    let id = Task.hash_id task inputs compiler host in
    let job = Task.make_job id inputs compiler host task in

    id_map := Pkg.Map.add pkg id !id_map;
    deps_map := Pkg.Map.add pkg deps !deps_map;
    j_lst := (id, job, IdSet.to_list deps) :: !j_lst
  done;
  !j_lst


let resolvable ~name ?version state =
  let str = match version with None -> name | Some v -> name ^ "." ^ v in
  let graph = resolve ~bare:false state str in
  if 1 = OpamSolver.ActionGraph.nb_vertex graph then false, graph
  else true, graph


let package p = OpamPackage.((name_to_string p) ^ "." ^ (version_to_string p))


let installed_of_state s =
  OpamPackage.Set.fold (fun p acc -> (package p) :: acc)
    s.OpamState.Types.installed []
  |> String.concat " ; "


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
(*
let lock_file () =
  let home = Sys.getenv "HOME" in
  let file = Filename.concat home ".ci_lock" in
  assert (Sys.file_exists file);
  file


let lock () =
  let file = lock_file () in
  Lwt_unix.openfile file [Lwt_unix.O_RDWR] 0o664 >>= fun fd ->
  Lwt_unix.lockf fd Lwt_unix.F_LOCK 0 >>= fun () ->

  let findlib_conf = conf_file () in
  let root = OpamFilename.Dir.to_string (OpamPath.root ()) in
  let path = Filename.concat root "system/lib" in
  modify_conf findlib_conf ~destdir:path ~path >>= fun (d, p) ->
  return (d, p, fd)


let unlock (destdir, path, fd) =
  let findlib_conf = conf_file () in
  modify_conf findlib_conf ~destdir ~path >>= fun (_, _) ->

  Lwt_unix.lockf fd Lwt_unix.F_ULOCK 0 >>= fun () ->
  Lwt_unix.close fd


let opam_install state p v =
  let install p v =
    let str = p ^ "." ^ v in
    let atom = parse str in
    OpamGlobals.yes := true;
    try
      OpamClient.SafeAPI.install [atom] None false;
      return `Success
    with _ -> return (`Fail "opam install") in

  let graph = resolve ~bare:false state (p ^ "." ^ v) in
  let nb_action = OpamSolver.ActionGraph.nb_vertex graph in
  if nb_action <> 1 then begin
    OpamSolver.ActionGraph.fold_vertex (fun v acc ->
        (match v with
         | To_change (origin, target) ->
            Printf.sprintf "%s -> %s"
              (match origin with Some o -> package o | None -> "none")
              (package target)
         | To_delete p -> Printf.sprintf "delete %s" (package p)
         | To_recompile p -> Printf.sprintf "recompile %s" (package p)) :: acc)
      graph []
    |> String.concat " ; "
    |> fun info ->
       Printf.eprintf "[%s@warning]: %s\n%!" p info end;
  install p v *)


let opam_install state ~name ~version =
  let nv =
    let name = OpamPackage.Name.of_string name in
    let version = OpamPackage.Version.of_string version in
    OpamPackage.create name version in
  try
    OpamAction.download_package state nv;
    OpamAction.build_and_install_package state ~metadata:true nv;
    let installed, installed_roots, reinstall = OpamState.Types.(
      state.installed, state.installed_roots, state.reinstall) in
    let _ =
      OpamAction.update_metadata state ~installed_roots ~reinstall
        ~installed:(OpamPackage.Set.add nv installed) in
    return `Success;
  with _ -> return (`Fail "opam build")


let opam_uninstall ~name ~version =
  let str = name ^ "." ^ version in
  let atom = parse str in
  OpamGlobals.yes := true;
  OpamClient.SafeAPI.remove ~autoremove:true ~force:true [atom];
  return_unit

(*
let opam_uninstall state p v =
  let str = p ^ "." ^ v in
  let atom = parse str in
  let request = {
    wish_install = [];
    wish_remove = [atom];
    wish_upgrade = [];
    criteria = `Default} in
  let universe = OpamState.universe state Remove in
  let result = OpamSolver.resolve
    ~orphans:OpamPackage.Set.empty ~requested:OpamPackage.Name.Set.empty
    universe request in
  let solution = match result with
    | Success s -> s
    | Conflicts _ ->
       failwith (Printf.sprintf "no solution for uninstall %s" str) in
  let (_, _), result =
    OpamAction.remove_all_packages ~metadata:true state solution in
  match result with
  | `Successful () -> return_unit
  | `Exception exn -> fail exn *)


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


let opam_install_switch ~compiler =
  let state = load_state () in
  let aliase_p = OpamPath.aliases state.OpamState.Types.root in
  let aliase = OpamFile.Aliases.read aliase_p in
  let is_installed_switch s =
    let switch = OpamSwitch.of_string s in
    OpamSwitch.Map.mem switch aliase in

  let rec switch s_name =
    if not (is_installed_switch s_name) then OpamSwitch.of_string s_name
    else switch (s_name ^ "_") in
  let switch = switch "ci_switch" in
  let compiler = OpamCompiler.of_string compiler in

  let open Lwt_unix in
  match Lwt_unix.fork () with
  | 0 ->
     OpamSwitchCommand.install
       ~quiet:false ~warning:false ~update_config:false switch compiler;
     exit 0
  | pid ->
     Lwt_unix.waitpid [] pid >>= fun (_, stat) ->
         match stat with
         | WEXITED i when i = 0 -> return (OpamSwitch.to_string switch)
         | WEXITED i | WSIGNALED i | WSTOPPED i ->
            fail_with (Printf.sprintf "exited %d" i)


let opam_remove_switch ~switch =
  let switch = OpamSwitch.of_string switch in
  OpamSwitchCommand.remove switch;
  return_unit

(*
let str = Arg.(
  required & pos 0 (some string) None & info
    ~docv:"PKG" ~doc:"package name (and version constraint) to solve" [])

let () = Term.(
  let tup = pure resolve $ str, info ~doc:"solve dependencies" "ci-solver" in
  match eval tup with `Error _ -> exit 1 | _ -> exit 0) *)
