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
open Lwt.Infix
module T = OpamState.Types

module Graph = OpamSolver.ActionGraph
let (/) = Filename.concat

type t = { root: string; switch: Switch.t }

let create ~root switch = { root; switch }

type plan = {
  g: Graph.t;
  h: Host.t;
  s: Switch.t;
}

let debug fmt = Gol.debug ~section:"opam" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Opam: " ^^ fmt)
let fail fmt = Printf.ksprintf failwith ("Ciso.Opam: " ^^ fmt)

let package p = OpamPackage.to_string p

let parse_atom str =
  match fst OpamArg.atom str with `Ok a -> a | `Error s ->
    fail "Cannot parse %s: %s\n%!" str s

let atom_of_package p = parse_atom (Package.to_string p)
let opam_switch s = OpamSwitch.of_string (Switch.to_string s)

let init t =
  let current_switch = opam_switch t.switch in
  let root_dir = OpamFilename.Dir.of_string t.root in
  OpamClientConfig.opam_init ~root_dir ~current_switch ~answer:None ()

let repo t name url =
  let repo_name = OpamRepositoryName.of_string name in
  let repo_priority = 0 in
  let repo_address, repo_kind = OpamTypesBase.parse_url url in
  let repo_root =
    OpamRepositoryPath.create (OpamFilename.Dir.of_string t.root) repo_name
  in
  { repo_root; repo_name; repo_kind; repo_address; repo_priority }

let load_state t dbg =
  init t;
  if not OpamFilename.(exists_dir Dir.(of_string (t.root / "system"))) then (
    let repo = repo t "default" ("https://opam.ocaml.org", None) in
    let comp = OpamCompiler.system in
    let root = OpamFilename.of_string t.root in
    OpamClient.SafeAPI.init repo comp `bash root `no;
  );
  let switch = opam_switch t.switch in
  let t = OpamState.load_state ("ci-opam-" ^ dbg) switch in
  t

let get_var t v =
  let t = load_state t "get-var" in
  OpamVariable.Full.of_string v
  |> OpamState.contents_of_variable (lazy t)
  |> OpamVariable.string_of_variable_contents

let resolve t atoms_s =
  let state = load_state t "resolve" in
  let atoms = List.rev_map parse_atom atoms_s in
  let install_set = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let action = Install install_set in
  let universe = OpamState.universe state action in
  let request = OpamSolver.request ~install:atoms () in
  let switch = OpamSwitch.to_string state.T.switch in
  debug "resolve: switch:%s preinstalled:%s" switch (get_var t "preinstalled");
  let result =
    OpamSolver.resolve ~orphans:OpamPackage.Set.empty universe request
  in
  let solution = match result with
    | Success s -> s
    | Conflicts c ->
       let info = OpamCudf.string_of_conflict OpamFormula.string_of_atom c in
       let str = String.concat ";" atoms_s in
       fail "no solution for %s: %s" str info
  in
  let graph = OpamSolver.get_atomic_action_graph solution in
  let oc = open_out "solver_log" in
  Graph.Dot.output_graph oc graph; close_out oc;
  graph

let resolve_packages t pkgs = resolve t (List.map Package.to_string pkgs)

let set_env h s =
  Unix.putenv "OPAMVAR_os"     Fmt.(to_to_string Host.pp_os @@ Host.os h);
  Unix.putenv "OPAMVAR_switch" (Switch.to_string s)

let plans t task =
  let one h s =
    set_env h s;
    resolve_packages t (Task.packages task)
  in
  List.fold_left (fun acc h ->
      List.fold_left (fun acc s ->
          { g = one h s; h; s } :: acc
        ) acc (Task.switches task)
    ) [] (Task.hosts task)

module IdSet = struct
  include Set.Make(struct
      type t = Job.id
      let compare = Id.compare
    end)
end

let package_of_opam p =
  let name = OpamPackage.(Name.to_string @@ name p) in
  let version = OpamPackage.(Version.to_string @@ version p) in
  Package.create ~version name

let opam_of_package p =
  let name = OpamPackage.(Name.of_string @@ Package.name p) in
  let version = match Package.version p with
    | None   -> failwith "no version!"
    | Some v -> OpamPackage.Version.of_string v
  in
  OpamPackage.create name version

let jobs_of_plan plan =
  let package_of_action = function
    | `Install target -> target
    | `Change (_, o, t) -> fail "change %s -> %s" (package o) (package t)
    | `Remove p | `Reinstall p | `Build p ->
      fail "Not expect delete/recompile %s" (package p)
  in
  let process_queue = Queue.create () in
  let add_stack = Stack.create () in
  Graph.iter_vertex (fun v ->
      if Graph.out_degree plan.g v = 0 then Queue.add v process_queue
    ) plan.g;
  while not (Queue.is_empty process_queue) do
    let v = Queue.pop process_queue in
    Graph.iter_pred (fun pred -> Queue.add pred process_queue) plan.g v;
    Stack.push v add_stack;
  done;
  let id_map = ref OpamPackage.Map.empty in
  let j_lst = ref [] in
  while not (Stack.is_empty add_stack) do
    let v = Stack.pop add_stack in
    let pkg = package_of_action v in
    let p = package_of_opam pkg in
    let inputs = Graph.fold_pred (fun pred i ->
        let pred_pkg  = package_of_action pred in
        let pred_id   = OpamPackage.Map.find pred_pkg !id_map in
        pred_id :: i
      ) plan.g v []
    in
    (* URGENT FIXME *)
    let opam = Cstruct.of_string "" and url = Cstruct.of_string "" in
    let job = Job.create ~inputs plan.h plan.s [p, Package.info ~opam ~url] in
    id_map  := OpamPackage.Map.add pkg (Job.id job) !id_map;
    j_lst := job :: !j_lst
  done;
  !j_lst

let jobs t p =
  List.fold_left (fun jobs p -> jobs_of_plan p @ jobs) [] (plans t p)

let (@@++) x f =
  let open OpamProcess.Job.Op in
  x @@+ function
  | Some err -> raise err
  | None     -> f ()

let install t pkgs =
  let state = load_state t "install" in
  if List.length pkgs = 0 then Lwt.return_unit
  else match Lwt_unix.fork () with
    | 0 ->
      begin match pkgs with
        | [pkg] ->
          let nv = opam_of_package pkg in
          let job =
            let open OpamProcess.Job.Op in
            OpamAction.download_package state nv @@+ function
            | `Error err         -> failwith err
            | `Successful source ->
              OpamAction.build_package state source nv @@++ fun () ->
              OpamAction.install_package state nv @@++ fun () ->
              let { T.installed; installed_roots; reinstall; _ } = state in
              let installed = OpamPackage.Set.add nv installed in
              OpamAction.update_metadata state ~installed_roots ~reinstall ~installed
              |> fun _ -> exit 0
          in
          OpamProcess.Job.run job
        | pkgs ->
          let atoms = List.map atom_of_package pkgs in
          let add_to_root = None in
          let deps_only = false in
          OpamClient.SafeAPI.install atoms add_to_root deps_only;
          Lwt.return_unit
      end
    | pid ->
      let open Lwt_unix in
      Lwt_unix.waitpid [] pid >>= fun (_, stat) ->
      match stat with
      | WEXITED i when i = 0 -> Lwt.return_unit
      | WEXITED _ | WSIGNALED _ | WSTOPPED _ ->
        err "opam install %s failed"
          (String.concat ", " @@ List.map Package.to_string pkgs)

let remove t = function
  | []   -> Lwt.return_unit
  | pkgs ->
    let atoms = List.map atom_of_package pkgs in
    init t;
    OpamClient.SafeAPI.remove ~autoremove:true ~force:true atoms;
    Lwt.return_unit

(*
let install_switch s =
  init ();
  let root = OpamStateConfig.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = OpamSwitch.of_string (Switch.to_string s) in
  if OpamSwitch.Map.mem switch aliases then Lwt.return_unit
  else match Lwt_unix.fork () with
    | 0 ->
      let compiler = OpamCompiler.of_string (OpamSwitch.to_string switch) in
      OpamSwitchCommand.install
        ~quiet:false ~warning:true ~update_config:true switch compiler;
      exit 0
    | pid ->
      let open Lwt_unix in
      Lwt_unix.waitpid [] pid >>= fun (_, stat) ->
      match stat with
      | WEXITED i when i = 0 -> Lwt.return ()
      | WEXITED i | WSIGNALED i | WSTOPPED i ->
        err "install switch %s failed" (Switch.to_string s)

let remove_switch c =
  init ();
  let switch = OpamSwitch.of_string c in
  OpamSwitchCommand.switch ~quiet:false ~warning:false (OpamSwitch.of_string "system");
  OpamSwitchCommand.remove switch;
  Lwt.return_unit

let eval_opam_config_env ?switch () =
  let env_s = load_state ?switch "opam-eval-env" in
  let env = OpamState.get_opam_env ~force_path:true env_s in
  List.iter (fun (n, v) -> Unix.putenv n v) env
*)

let switch_to t s =
  init t;
  let root = OpamStateConfig.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = opam_switch s in
  let compiler = OpamCompiler.of_string (Switch.to_string s) in
  let new_aliases = OpamSwitch.Map.add switch compiler aliases in
  OpamFile.Aliases.write (OpamPath.aliases root) new_aliases;
  OpamSwitchCommand.switch ~quiet:false ~warning:false switch;
  Lwt.return_unit

let clean_repos t =
  let t = load_state t "clean-repos" in
  let repos = OpamState.sorted_repositories t in
  List.iter (fun r ->
    let name = OpamRepositoryName.to_string r.repo_name in
    if name = "default" then ()
    else OpamRepositoryCommand.remove r.repo_name
    ) repos

let add_repos t repos =
  let add_one_repo (name, address) =
    let address = Uri.to_string address in
    debug "repository: add %s %s" name address;
    let name = OpamRepositoryName.of_string name in
    let address = OpamTypesBase.address_of_string address in
    let address, kind = OpamTypesBase.parse_url address in
    OpamRepositoryCommand.add name kind address ~priority:None
  in
  match Lwt_unix.fork () with
  | 0 ->
    clean_repos t;
    List.iter add_one_repo repos;
    exit 0
  | pid ->
    Lwt_unix.waitpid [] pid >>= fun (_, s) ->
    let open Lwt_unix in
    match s with
    | WEXITED i when i = 0 -> Lwt.return_unit
    | WEXITED i | WSIGNALED i | WSTOPPED i ->
      err "add_repo %s failed (exit %d)"
        (Fmt.(to_to_string (list Task.pp_repo) repos)) i

let add_pins t pin =
  init t;
  let add_one (pkg, target) =
    let name = OpamPackage.Name.of_string pkg in
    match target with
    | None -> (* --dev *)
      OpamClient.SafeAPI.PIN.pin ~edit:false ~action:false name None
    | Some target ->
      let target = Uri.to_string target in
      let pin_option = OpamTypesBase.pin_option_of_string ?kind:None target in
      let kind = OpamTypesBase.kind_of_pin_option pin_option in
      let () = assert (kind <> `local) in
      OpamClient.SafeAPI.PIN.pin
        ~edit:false ~action:false name (Some pin_option)
  in
  List.iter add_one pin;
  Lwt.return_unit

let update t =
  match Lwt_unix.fork () with
  | 0 ->
    init t;
    OpamClient.SafeAPI.update ~repos_only:false ~dev_only:false [];
    exit 0
  | pid ->
    Lwt_unix.waitpid [] pid >>= fun (_, s) ->
    let open Lwt_unix in
    (* FIXME: review use of fork *)
    match s with
    | WEXITED i when i = 0 -> Lwt.return_unit
    | WEXITED i | WSIGNALED i | WSTOPPED i -> err "exited %d when opam update" i

let read_installed t =
  let t = load_state t "installed" in
  t.T.installed
  |> OpamPackage.Set.elements
  |> List.map OpamPackage.to_string
  |> List.map Package.of_string

let write_installed t installed =
  let installed =
    installed
    |> List.map Package.to_string
    |> List.map OpamPackage.of_string
    |> OpamPackage.Set.of_list
  in
  let t = load_state t "write-installed" in
  let file = OpamPath.Switch.installed t.T.root t.T.switch in
  OpamFile.Installed.write file installed

let write_pinned t pinned =
  let t = load_state t "write-pinned" in
  let file = OpamPath.Switch.pinned t.T.root t.T.switch in
  let pinned =
    List.map (fun (n, t) ->
        let t = match t with None -> failwith "TODO" | Some t -> t in
        [n; Uri.to_string t]
      ) pinned
  in
  OpamFile.Lines.write file pinned
