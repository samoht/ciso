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
module T = OpamState.Types

module Graph = OpamSolver.ActionGraph
let (/) = Filename.concat

type t = { root: string; switch: Switch.t; }

type plan = {
  g: Graph.t;
  h: Host.t;
  s: Switch.t;
}

let debug fmt = Gol.debug ~section:"opam" fmt
let fail fmt = Printf.ksprintf failwith ("Ciso.Opam: " ^^ fmt)

let package p = OpamPackage.to_string p

let parse_atom str =
  match fst OpamArg.atom str with `Ok a -> a | `Error s ->
    fail "Cannot parse %s: %s\n%!" str s

let atom_of_package p = parse_atom (Package.to_string p)
let opam_switch s = OpamSwitch.of_string (Switch.to_string s)

let init_config t =
  let current_switch = opam_switch t.switch in
  let root_dir = OpamFilename.Dir.of_string t.root in
  OpamClientConfig.opam_init ~root_dir ~current_switch ~strict:false
    ~skip_version_checks:true ~answer:None ()

let default_repo = ("https://github.com/ocaml/opam-repository.git", None)

(*
let repo t name url =
  let repo_name = OpamRepositoryName.of_string name in
  let repo_priority = 0 in
  let repo_address, repo_kind = OpamTypesBase.parse_url url in
  let repo_root =
    OpamRepositoryPath.create (OpamFilename.Dir.of_string t.root) repo_name
  in
  { repo_root; repo_name; repo_kind; repo_address; repo_priority }
*)

let load_state t dbg =
  init_config t;
  if not OpamFilename.(exists_dir Dir.(of_string (t.root / "system"))) then (
    (* FIXME: we don't want opam 1.3, so shell out `opam init...`*)
    (*   let repo = repo t "default" default_repo in
         let comp = OpamCompiler.system in
         let root = OpamFilename.of_string t.root in
         OpamClient.SafeAPI.init repo comp `bash root `no; *)
    let cmd =
      Printf.sprintf "opam init --root=%s %s -n" t.root
        (match default_repo with s, None -> s | s, Some v -> s ^ "#" ^ v)
    in
    match Sys.command cmd with
    | 0 -> ()
    | i -> Printf.ksprintf failwith "%s failed (exit %d)!" cmd i
  );
  let switch = opam_switch t.switch in
  let t = OpamState.load_state ("ci-opam-" ^ dbg) switch in
  t

let create ~root = function
  | Some s -> { root; switch = s}
  | None   ->
    let aliases = root / "aliases"  in
    if Sys.file_exists aliases then
      let aliases = OpamFile.Aliases.read (OpamFilename.of_string aliases) in
      match OpamSwitch.Map.bindings aliases with
      | []        -> { root; switch = Switch.system }
      | (s, _)::_ -> { root; switch = Switch.of_string (OpamSwitch.to_string s)}
    else
      { root; switch = Switch.system }

let get_var t v =
  let t = load_state t "get-var" in
  OpamVariable.Full.of_string v
  |> OpamState.contents_of_variable (lazy t)
  |> OpamVariable.string_of_variable_contents

(* FIXME: this doesn't work as OPAM is caching the env variables
   lazily. *)
let _set_env t s h =
  let app k v = Unix.putenv ("OPAMVAR_" ^ k) v in
  let tts = Fmt.to_to_string in
  List.iter (fun (k, v) -> app k v) [
    "os"          , tts Host.pp_os @@ Host.os h;
    "switch"      , Switch.to_string s;
    "os"          , Switch.to_string s;
    "preinstalled", "false";
  ];
  Unix.putenv "OPAMROOT" t.root

let eval_opam_config_env t =
  let env_s = load_state t "opam-eval-env" in
  let env = OpamState.get_opam_env ~force_path:true env_s in
  List.iter (fun (n, v) -> Unix.putenv n v) env

let resolve t atoms_s =
  debug "resolve";
  let state = load_state t "resolve" in
  let atoms = List.rev_map parse_atom atoms_s in
  let install_set = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let action = Install install_set in
  let universe = OpamState.universe state action in
  let request = OpamSolver.request ~install:atoms () in
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

let plans t task =
  let one switch =
    (* FIXME *)
    eval_opam_config_env t;
    let i = Sys.command (Fmt.strf "opam switch %a" Switch.pp switch) in
    if i <> 0 then failwith "error while switching";
    resolve_packages { t with switch } (Task.packages task)
  in
  let h = Host.detect () in
  if List.mem h (Task.hosts task) then
    let switches = Task.switches task in
    List.fold_left (fun acc s -> { g = one s; h; s } :: acc) [] switches
  else
    []

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

(* URGENT FIXME *)
let package_info o pkg =
  let nv = opam_of_package pkg in
  let to_cstruct name f x =
    let file = Filename.temp_file name "tmp" in
    f (OpamFilename.of_string file) x;
    let fd = Unix.openfile file [Unix.O_RDONLY; Unix.O_NONBLOCK] 0o644 in
    let cstruct = Unix_cstruct.of_fd fd in
    Unix.close fd;
    cstruct
  in
  let opam = OpamState.opam o nv |> to_cstruct "opam" OpamFile.OPAM.write in
  let url  =
    match OpamState.url o nv with
    | None   -> None
    | Some x -> Some (to_cstruct "url" OpamFile.URL.write x)
  in
  Package.info ~opam ~url

let package_of_action (a:Graph.vertex) =
  let o = match a with
    | `Install target -> target
    | `Change (_, o, t) -> fail "change %s -> %s" (package o) (package t)
    | `Remove p | `Reinstall p | `Build p ->
      fail "Not expect delete/recompile %s" (package p)
  in
  package_of_opam o

module PMap = Map.Make(Package)

let atomic_jobs_of_plan t plan =
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
  let id_map = ref PMap.empty in
  let j_lst = ref [] in
  while not (Stack.is_empty add_stack) do
    let v = Stack.pop add_stack in
    let p = package_of_action v in
    let inputs = Graph.fold_pred (fun pred i ->
        let pred_pkg  = package_of_action pred in
        let pred_id   = PMap.find pred_pkg !id_map in
        pred_id :: i
      ) plan.g v []
    in
    let info = package_info t p in
    let job = Job.create ~inputs plan.h plan.s [p, info] in
    id_map  := PMap.add p (Job.id job) !id_map;
    j_lst := job :: !j_lst
  done;
  !j_lst

let jobs_of_plan t plan =
  let actions = Graph.fold_vertex (fun e l -> e :: l) plan.g [] in
  let pkgs =
    List.map (fun a ->
        let pkg = package_of_action a in
        pkg, package_info t pkg) actions
  in
  Job.create plan.h plan.s pkgs

let (@@++) x f =
  let open OpamProcess.Job.Op in
  x @@+ function
  | Some err -> raise err
  | None     -> f ()

let install t pkgs =
  debug "install";
  let state = load_state t "install" in
  if List.length pkgs = 0 then ()
  else match pkgs with
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
      OpamClient.SafeAPI.install atoms add_to_root deps_only

let remove t = function
  | []   -> ()
  | pkgs ->
    debug "remove";
    init_config t;
    let atoms = List.map atom_of_package pkgs in
    OpamClient.SafeAPI.remove ~autoremove:true ~force:true atoms

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

*)

let switch_to t s =
  debug "switch_to %a" Switch.pp s;
  init_config t;
  let root = OpamStateConfig.(!r.root_dir) in
  let aliases = OpamFile.Aliases.safe_read (OpamPath.aliases root) in
  let switch = opam_switch s in
  let compiler = OpamCompiler.of_string (Switch.to_string s) in
  let new_aliases = OpamSwitch.Map.add switch compiler aliases in
  OpamFile.Aliases.write (OpamPath.aliases root) new_aliases;
  OpamSwitchCommand.switch ~quiet:false ~warning:false switch

let repo_clean t =
  debug "repo_clean";
  let t = load_state t "repo_clean" in
  let repos = OpamState.sorted_repositories t in
  List.iter (fun r ->
    let name = OpamRepositoryName.to_string r.repo_name in
    if name = "default" then ()
    else OpamRepositoryCommand.remove r.repo_name
    ) repos

let repo_add t repos =
  debug "repo_add";
  let add_one_repo (name, address) =
    let address = Uri.to_string address in
    debug "repository: add %s %s" name address;
    let name = OpamRepositoryName.of_string name in
    let address = OpamTypesBase.address_of_string address in
    let address, kind = OpamTypesBase.parse_url address in
    OpamRepositoryCommand.add name kind address ~priority:None
  in
  repo_clean t;
  List.iter add_one_repo repos

let pin_clean t =
  debug "pin_clean";
  let t = load_state t "repo_clean" in
  let pins = OpamState.pinned_packages t in
  let pkgs = OpamPackage.Set.elements pins in
  let names = List.map OpamPackage.name pkgs in
  let _ = OpamPinCommand.unpin ~state:t names in
  ()

let pin_add t pin =
  debug "pin_add";
  init_config t;
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
  List.iter add_one pin

let update t =
  debug "update";
  init_config t;
  OpamClient.SafeAPI.update ~repos_only:false ~dev_only:false []

let read_installed t =
  debug "read_installed";
  let t = load_state t "installed" in
  t.T.installed
  |> OpamPackage.Set.elements
  |> List.map OpamPackage.to_string
  |> List.map Package.of_string

let write_installed t installed =
  debug "write_installed";
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
  debug "write_pinned";
  let t = load_state t "write-pinned" in
  let file = OpamPath.Switch.pinned t.T.root t.T.switch in
  let pinned =
    List.map (fun (n, t) ->
        let t = match t with None -> failwith "TODO" | Some t -> t in
        [n; Uri.to_string t]
      ) pinned
  in
  OpamFile.Lines.write file pinned

let atomic_jobs t p =
  debug "atomic_jobs";
  let o = load_state t "atomic-jobs" in
  List.fold_left (fun jobs p -> atomic_jobs_of_plan o p @ jobs) [] (plans t p)

let jobs t p =
  debug "jobs";
  let o = load_state t "atomic-jobs" in
  List.fold_left (fun jobs p -> jobs_of_plan o p :: jobs) [] (plans t p)
