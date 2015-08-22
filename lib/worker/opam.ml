(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c)      2015 Qi Li <liqi0425@gmail.com>
 * Copyright (c)      2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open OpamTypes
open OpamState.Types
open Lwt.Infix

type plan = OpamSolver.ActionGraph.t

type job = Job.id * Job.t * Object.id list

let debug fmt = Gol.debug ~section:"opam" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Opam: " ^^ fmt)

let package p = OpamPackage.((name_to_string p) ^ "." ^ (version_to_string p))

let parse_atom str =
  match fst OpamArg.atom str with `Ok a -> a | `Error s ->
    Printf.eprintf "Cannot parse %s: %s\n%!" str s;
    exit 1

(* FIXME: user OPAM's master when #2292 is merged. *)
let init ?switch ?root () =
  let root =
    match root with None -> OpamStateConfig.opamroot () | Some root -> root
  in
  OpamFormatConfig.init ();
  let init = OpamStateConfig.load_defaults root in
  if not init then (Printf.eprintf "OPAM is not initialized\n"; exit 1);
  let log_dir = OpamFilename.(Dir.to_string Op.(root / "log")) in
  OpamStd.Config.init ~answer:(Some true) ~log_dir ();
  OpamRepositoryConfig.init ();
  OpamSolverConfig.init ();
  OpamStateConfig.init ~root_dir:root ?current_switch:switch ();
  OpamClientConfig.init ()

let load_state ?switch dbg =
  init ?switch ();
  OpamState.load_state ("ci-opam-" ^ dbg)  OpamStateConfig.(!r.current_switch)

let get_var v =
  let t = load_state "get-var" in
  OpamVariable.Full.of_string v
  |> OpamState.contents_of_variable (lazy t)
  |> OpamVariable.string_of_variable_contents

let parse_user_demand pkg =
  match parse_atom pkg with
  | name, None        -> OpamPackage.Name.to_string name, None
  | name, Some (_, v) ->
    OpamPackage.Name.to_string name, Some (OpamPackage.Version.to_string v)

let resolve str_lst =
  let state = load_state "resolve" in
  let atoms = List.rev_map parse_atom str_lst in
  let install_set = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let action = Install install_set in
  let universe = OpamState.universe state action in
  let request = OpamSolver.request ~install:atoms () in
  let switch = OpamSwitch.to_string state.switch in
  debug "resolve: switch:%s preinstalled:%s" switch (get_var "preinstalled");
  let result =
    OpamSolver.resolve ~orphans:OpamPackage.Set.empty universe request
  in
  let solution = match result with
    | Success s -> s
    | Conflicts c ->
       let info = OpamCudf.string_of_conflict OpamFormula.string_of_atom c in
       let str = String.concat ";" str_lst in
       failwith (Printf.sprintf "no solution for %s: %s" str info) in
  let graph = OpamSolver.get_atomic_action_graph solution in
  let oc = open_out "solver_log" in
  OpamSolver.ActionGraph.Dot.output_graph oc graph; close_out oc;
  graph

let resolve_packages pkgs = resolve (List.map Package.to_string pkgs)

let compiler () =
  let s = load_state "compier" in
  s.compiler
  |> OpamCompiler.to_string
  |> Compiler.of_string

module IdSet = struct
  include Set.Make(struct
      type t = Job.id
      let compare = Id.compare
    end)
  let to_list s = fold (fun e acc -> e :: acc) s []
end

let package_of_opam p =
  let name = OpamPackage.(Name.to_string @@ name p) in
  let version = OpamPackage.(Version.to_string @@ version p) in
  Package.create ~version name

let jobs ?repos ?pins host graph =
  let module Graph = OpamSolver.ActionGraph in
  let module Pkg = OpamPackage in
  let package_of_action = function
    | `Install target -> target
    | `Change (_, origin, target) ->
      debug "jobs_of_graph: WARNING %s -> %s" (package origin) (package target);
      origin
    | `Remove p | `Reinstall p | `Build p ->
      failwith ("Not expect delete/recompile " ^ (package p))  in
  let process_queue = Queue.create () in
  let add_stack = Stack.create () in
  Graph.iter_vertex (fun v ->
      if Graph.out_degree graph v = 0 then Queue.add v process_queue
    ) graph;
  while not (Queue.is_empty process_queue) do
    let v = Queue.pop process_queue in
    Graph.iter_pred (fun pred -> Queue.add pred process_queue) graph v;
    Stack.push v add_stack;
  done;
  let compiler = compiler () in
  let host = Host.detect () in
  let id_map = ref Pkg.Map.empty in
  let deps_map = ref Pkg.Map.empty in
  let j_lst = ref [] in
  while not (Stack.is_empty add_stack) do
    let v = Stack.pop add_stack in
    let pkg = package_of_action v in
    let p = package_of_opam pkg in
    let job = Job.create ?repos ?pins host compiler [p] in
    let inputs, deps = Graph.fold_pred (fun pred (i, d) ->
        let pred_pkg  = package_of_action pred in
        let pred_id   = Pkg.Map.find pred_pkg !id_map in
        let pred_deps = Pkg.Map.find pred_pkg !deps_map in
        pred_id :: i,
        IdSet.union d (IdSet.add pred_id pred_deps)
      ) graph v ([], IdSet.empty)
    in
    id_map  := Pkg.Map.add pkg (Job.id job) !id_map;
    deps_map := Pkg.Map.add pkg deps !deps_map;
    j_lst := (job, IdSet.to_list deps) :: !j_lst
  done;
  !j_lst

let is_simple g =
  if OpamSolver.ActionGraph.nb_vertex g > 1 then None
  else
    let p = ref None in
    OpamSolver.ActionGraph.iter_vertex (fun v -> p := Some v) g;
    match !p with
    | Some (`Install p) -> Some (package_of_opam p)
    | _ -> failwith "invalid base action"

let (@@++) x f =
  let open OpamProcess.Job.Op in
  x @@+ function
  | Some err -> raise err
  | None     -> f ()

let install ~name ~version =
  let state = load_state "install" in
  let nv =
    let name = OpamPackage.Name.of_string name in
    let version = OpamPackage.Version.of_string version in
    OpamPackage.create name version in
  match Lwt_unix.fork () with
  | 0 ->
    let job =
      let open OpamProcess.Job.Op in
      OpamAction.download_package state nv @@+ function
      | `Error err         -> failwith err
      | `Successful source ->
        OpamAction.build_package state source nv @@++ fun () ->
        OpamAction.install_package state nv @@++ fun () ->
        let installed, installed_roots, reinstall =
          state.installed, state.installed_roots, state.reinstall
        in
        let _ =
          OpamAction.update_metadata state ~installed_roots ~reinstall
            ~installed:(OpamPackage.Set.add nv installed)
        in
        exit 0
    in
    OpamProcess.Job.run job
  | pid ->
      let open Lwt_unix in
      Lwt_unix.waitpid [] pid >>= fun (_, stat) ->
      match stat with
      | WEXITED i when i = 0 -> Lwt.return `Success
      | WEXITED _ | WSIGNALED _ | WSTOPPED _ ->
        Lwt.return (`Fail "opam build")

let uninstall ~name ~version =
  let str = name ^ "." ^ version in
  let atom = parse_atom str in
  init ();
  OpamClient.SafeAPI.remove ~autoremove:true ~force:true [atom];
  Lwt.return_unit

let export_switch c =
  init ();
  let root = OpamStateConfig.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string c in
  if not (OpamSwitch.Map.mem switch aliases) then ()
  else (
    OpamSwitchCommand.switch ~quiet:false ~warning:false switch;
    let file = Printf.sprintf "ci_%s_%s.export" c (Gol.timestamp ()) in
    let path = OpamFilename.(
      let dir = Op.(root / "log") in
      if exists_dir dir then Op.( dir // file)
      else Op.(root // file)) in
    OpamSwitchCommand.export (Some path);
    OpamSwitchCommand.switch ~quiet:false ~warning:false (OpamSwitch.of_string "system");
    OpamSwitchCommand.remove switch
  );
  Lwt.return_unit

let opam_eval_env ?switch () =
  let env_s = load_state ?switch "opam-eval-env" in
  let env = OpamState.get_opam_env ~force_path:true env_s in
  List.iter (fun (n, v) -> Unix.putenv n v) env

let install_switch c =
  init ();
  let root = OpamStateConfig.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string c in
  (if OpamSwitch.Map.mem switch aliases then export_switch c
   else Lwt.return_unit) >>= fun () ->
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
       Lwt.return_unit)

let remove_switch c =
  init ();
  export_switch c >>= fun () ->
  let switch = OpamSwitch.of_string c in
  OpamSwitchCommand.switch ~quiet:false ~warning:false (OpamSwitch.of_string "system");
  OpamSwitchCommand.remove switch;
  Lwt.return_unit

let switch_to c =
  init ();
  let root = OpamStateConfig.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string c in
  let compiler = OpamCompiler.of_string c in
  let new_aliases = OpamSwitch.Map.add switch compiler aliases in
  OpamFile.Aliases.write (OpamPath.aliases root) new_aliases;
  OpamSwitchCommand.switch ~quiet:false ~warning:false switch;
  opam_eval_env ~switch ();
  Lwt.return_unit

let compiler () =
  let state = load_state "compiler" in
  OpamCompiler.to_string state.compiler

let clean_repos () =
  let t = load_state "clean-repos" in
  let repos = OpamState.sorted_repositories t in
  List.iter (fun r ->
    let name = OpamRepositoryName.to_string r.repo_name in
    if name = "default" then ()
    else OpamRepositoryCommand.remove r.repo_name) repos

let add_repos repo =
  let add_one_repo (Task.Repository (name, address)) =
    debug "repository: add %s %s" name address;
    let name = OpamRepositoryName.of_string name in
    let address = OpamTypesBase.address_of_string address in
    let address, kind = OpamTypesBase.parse_url address in
    OpamRepositoryCommand.add name kind address ~priority:None
  in
  (* FIXME: review use of fork *)
  match Lwt_unix.fork () with
  | 0 ->
    clean_repos ();
    List.iter add_one_repo repo;
    exit 0
  | pid ->
    Lwt_unix.waitpid [] pid >>= fun (_, s) ->
    let open Lwt_unix in
    match s with
    | WEXITED i when i = 0 -> Lwt.return_unit
    | WEXITED i | WSIGNALED i | WSTOPPED i -> err "exited %d when add repos" i

let add_pins pin =
  let add_one_pin (Task.Pin (pkg, target)) =
    let name = OpamPackage.Name.of_string pkg in
    if target = "dev-repo" then
      OpamClient.SafeAPI.PIN.pin ~edit:false ~action:false name None
    else
      let pin_option = OpamTypesBase.pin_option_of_string ?kind:None target in
      let kind = OpamTypesBase.kind_of_pin_option pin_option in
      let () = assert (kind <> `local) in
      OpamClient.SafeAPI.PIN.pin
        ~edit:false ~action:false name (Some pin_option)
  in
  List.iter add_one_pin pin;
  Lwt.return_unit

let update () =
  match Lwt_unix.fork () with
  | 0 ->
     OpamClient.SafeAPI.update ~repos_only:false ~dev_only:false [];
     exit 0
  | pid ->
    Lwt_unix.waitpid [] pid >>= fun (_, s) ->
    let open Lwt_unix in
    (* FIXME: review use of fork *)
    match s with
    | WEXITED i when i = 0 -> Lwt.return_unit
    | WEXITED i | WSIGNALED i | WSTOPPED i -> err "exited %d when opam update" i

(* only for testing *)
let show_repo_pin () =
  let state = load_state "show-repo-pin" in
  let repo = state.repositories in
  let pin = state.pinned in
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
        Printf.sprintf "[%s] %s %s %s" priority name kind address :: acc
      ) repo  []
    |> String.concat "\n"
  in
  let pin_str =
    OpamPackage.Name.Map.fold (fun name pin_option acc ->
        let name = OpamPackage.Name.to_string name in
        let pin_option = OpamTypesBase.string_of_pin_option pin_option in
        Printf.sprintf "%s %s" name pin_option :: acc
      ) pin []
    |> String.concat "\n"
  in
  debug "repo_pin:\n[Repo]:\n%s\n[Pin]:\n%s" repo_str pin_str;
  Lwt.return_unit

let installed () =
  let t = load_state "installed" in
  t.installed
  |> OpamPackage.Set.elements
  |> List.map OpamPackage.to_string
  |> List.map Package.of_string

let write_installed installed =
  let installed =
    installed
    |> List.map Package.to_string
    |> List.map OpamPackage.of_string
    |> OpamPackage.Set.of_list
  in
  let t = load_state "write-installed" in
  let file = OpamPath.Switch.installed t.root t.switch in
  OpamFile.Installed.write file installed