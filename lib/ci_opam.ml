open OpamTypes
open Cmdliner

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

let modify_state state =
  let open OpamState.Types in
  let base = OpamState.base_packages state in
  { state with
    installed = base;
    installed_roots = base;
    pinned = OpamPackage.Name.Map.empty; }

let resolve str =
  OpamGlobals.root_dir := OpamGlobals.default_opam_dir;
  let state = OpamState.load_state "solve" in
  let atom = parse str in
  let install_set = OpamPackage.Name.(Set.add (fst atom) Set.empty) in
  let action = Install install_set in
  let universe = OpamState.universe (modify_state state) action in
  let request = {
    wish_install = [atom];
    wish_remove = [];
    wish_upgrade = [];
    criteria = `Default} in
  let result = OpamSolver.resolve
    universe ~orphans:OpamPackage.Set.empty request in
  let solution = match result with
    | Success s -> s
    | Conflicts _ -> failwith (Printf.sprintf "no solution for %s" str) in
  let graph = OpamSolver.get_atomic_action_graph solution in
  let oc = open_out "solver_log" in
  OpamSolver.ActionGraph.Dot.output_graph oc graph; close_out oc;
  graph
  (* OpamSolver.ActionGraph.iter_edges print_edges graph *)

let add_task new_task update_inputs pull graph =
  let module Graph = OpamSolver.ActionGraph in
  let module Pkg = OpamPackage in
  let package_of_action = function
    | To_change (origin, target) -> assert (origin = None); target
    | _ -> failwith "Not expect" in
  let oid_map = Graph.fold_vertex (fun v map ->
      let pkg = package_of_action v in
      let name = Pkg.name_to_string pkg in
      let version = Pkg.version_to_string pkg in
      let degree = Graph.out_degree graph v in
      let oid = if degree = 0 then new_task ?pull:(Some pull) name version
                else new_task ?pull:None name version in
      Pkg.Map.add pkg oid map) graph Pkg.Map.empty in
  Graph.iter_vertex (fun v ->
      let pkg = package_of_action v in
      let oid = Pkg.Map.find pkg oid_map in
      let inputs = Graph.fold_pred (fun pred acc ->
          let pred_pkg = package_of_action pred in
          let input = Pkg.Map.find pred_pkg oid_map in
          input :: acc) graph v [] in
      update_inputs oid inputs) graph

let str = Arg.(
  required & pos 0 (some string) None & info
    ~docv:"PKG" ~doc:"package name (and version constraint) to solve" [])

let () = Term.(
  let tup = pure resolve $ str, info ~doc:"solve dependencies" "ci-solver" in
  match eval tup with `Error _ -> exit 1 | _ -> exit 0)
