open OpamTypes
open Lwt

let compiler () =
  OpamCompiler.Version.current ()
  |> function
    | None -> "Unknown"
    | Some v -> OpamCompiler.Version.to_string v


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


let load_state () =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  OpamState.load_state "state"


let modify_state state =
  let open OpamState.Types in
  let base = OpamState.base_packages in
  let ocamlfind =
    let name = OpamPackage.Name.of_string "ocamlfind" in
    let version = OpamPackage.Version.of_string "1.5.5" in
    OpamPackage.create name version in
  let is_base pkg = List.mem (OpamPackage.name pkg) base in

  let open OpamPackage.Set.Op in
  { state with
    installed = OpamPackage.Set.filter is_base state.installed
                ++ OpamPackage.Set.singleton ocamlfind;
    installed_roots = OpamPackage.Set.filter is_base state.installed_roots
                      ++ OpamPackage.Set.singleton ocamlfind;
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
    | Conflicts _ -> failwith (Printf.sprintf "no solution for %s" str) in
  let graph = solution.to_process in
  let oc = open_out "solver_log" in
  OpamSolver.ActionGraph.Dot.output_graph oc graph; close_out oc;
  graph
  (* OpamSolver.ActionGraph.iter_edges print_edges graph *)


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

  let module IdSet = struct
      include Set.Make(String)
      let to_list s = fold (fun e acc -> e :: acc) s [] end in
  let id_map = ref Pkg.Map.empty in
  let deps_map = ref Pkg.Map.empty in
  let t_lst = ref [] in
  while not (Stack.is_empty add_stack) do
    let v = Stack.pop add_stack in
    let pkg = package_of_action v in
    let name, version = Pkg.(name_to_string pkg, version_to_string pkg) in

    let inputs, deps = Graph.fold_pred (fun pred (i, d) ->
        let pred_pkg = package_of_action pred in
        let pred_id = Pkg.Map.find pred_pkg !id_map in
        let pred_deps = Pkg.Map.find pred_pkg !deps_map in

        pred_id :: i,
        IdSet.union d (IdSet.add pred_id pred_deps))
      graph v ([], IdSet.empty) in

    let compiler = compiler () in
    let host = Host.detect () |> Host.to_string in
    let id = hash_id name version inputs compiler host in
    let task =
      if Graph.out_degree graph v <> 0 then
        Task.make_job ?pull:None id name version inputs compiler host
      else Task.make_job ?pull id name version inputs compiler host in

    id_map := Pkg.Map.add pkg id !id_map;
    deps_map := Pkg.Map.add pkg deps !deps_map;
    t_lst := (id, task, IdSet.to_list deps) :: !t_lst
  done;
  !t_lst


let package p = OpamPackage.((name_to_string p) ^ "." ^ (version_to_string p))


let installed_of_state s =
  OpamPackage.Set.fold (fun p acc -> (package p) :: acc)
    s.OpamState.Types.installed []
  |> String.concat " ; "

(*
let opam_install state p v =
  let nv =
    let name = OpamPackage.Name.of_string p in
    let version = OpamPackage.Version.of_string v in
    OpamPackage.create name version in
  try
    OpamAction.download_package state nv;
    OpamAction.build_and_install_package state ~metadata:true nv;
    let installed, installed_roots, reinstall = OpamState.Types.(
      state.installed, state.installed_roots, state.reinstall) in
    let new_state =
      OpamAction.update_metadata state ~installed_roots ~reinstall
        ~installed:(OpamPackage.Set.add nv installed) in
    return (new_state, `Success);
  with _ -> return (state, `Fail "opam build")


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

let opam_install state p v =
  let graph = resolve ~bare:false state (p ^ "." ^ v) in
  let nb_action = OpamSolver.ActionGraph.nb_vertex graph in
  if nb_action <> 1 then return (`Fail "dependency incomplet")
  else begin
      let str = p ^ "." ^ v in
      let atom = parse str in
      OpamGlobals.yes := true;
      try
        OpamClient.SafeAPI.install [atom] None false;
        return `Success
      with _ -> return (`Fail "opam install")
    end


let opam_uninstall p v =
  let str = p ^ "." ^ v in
  let atom = parse str in
  OpamGlobals.yes := true;
  OpamClient.SafeAPI.remove ~autoremove:true ~force:true [atom];
  return_unit


let update_metadata ~install state file =
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

  Lwt_io.with_file Lwt_io.input file (packages_of_file []) >>= fun pkg_lst ->
  let installed_roots = state.OpamState.Types.installed_roots in
  let reinstall = state.OpamState.Types.reinstall in
  let installed =
    List.fold_left (fun set p -> if install then OpamPackage.Set.add p set
                                 else OpamPackage.Set.remove p set)
      state.OpamState.Types.installed pkg_lst in
  OpamAction.update_metadata state ~installed ~installed_roots ~reinstall
  |> return

(*
let str = Arg.(
  required & pos 0 (some string) None & info
    ~docv:"PKG" ~doc:"package name (and version constraint) to solve" [])

let () = Term.(
  let tup = pure resolve $ str, info ~doc:"solve dependencies" "ci-solver" in
  match eval tup with `Error _ -> exit 1 | _ -> exit 0) *)
