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

let debug fmt = Gol.debug ~section:"opam" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Opam: " ^^ fmt)

let package p = OpamPackage.((name_to_string p) ^ "." ^ (version_to_string p))

let parse_atom str =
  match fst OpamArg.atom str with `Ok a -> a | `Error s ->
    Printf.eprintf "Cannot parse %s: %s\n%!" str s;
    exit 1

(* FIXME: this is crazy... *)
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
  | name, None -> OpamPackage.Name.to_string name, None
  | name, Some (_, v) -> OpamPackage.Name.to_string name,
                         Some (OpamPackage.Version.to_string v)

let resolve str_lst =
  let state = load_state "resolve" in
  let atoms = List.rev_map parse_atom str_lst in
  let install_set = OpamPackage.Name.Set.of_list (List.rev_map fst atoms) in
  let action = Install install_set in
  let universe = OpamState.universe state action in
  let request = OpamSolver.request ~install:atoms () in
  let switch = OpamSwitch.to_string state.OpamState.Types.switch in
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

let compiler () =
  let s = load_state "compier" in
  s.OpamState.Types.compiler |> OpamCompiler.to_string

let jobs_of_graph ?pull ?repository ?pin graph =
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
  let host = Host.detect () |> Host.to_string in
  let module IdSet = struct
    include Set.Make(String)
    let to_list s = fold (fun e acc -> e :: acc) s []
  end in
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
      else
        Task.make_pkg_task ~name ~version ()
    in
    let inputs, deps = Graph.fold_pred (fun pred (i, d) ->
        let pred_pkg = package_of_action pred in
        let pred_id = Pkg.Map.find pred_pkg !id_map in
        let pred_deps = Pkg.Map.find pred_pkg !deps_map in
        pred_id :: i,
        IdSet.union d (IdSet.add pred_id pred_deps)
      ) graph v ([], IdSet.empty)
    in
    let id = Task.hash_id ?repository ?pin task inputs compiler host in
    let job = Task.make_job id inputs compiler host task ?repository ?pin () in
    id_map := Pkg.Map.add pkg id !id_map;
    deps_map := Pkg.Map.add pkg deps !deps_map;
    j_lst := (id, job, IdSet.to_list deps) :: !j_lst
  done;
  !j_lst

let resolvable ~name ?version ?depopts () =
  let str = match version with None -> name | Some v -> name ^ "." ^ v in
  let depopt_str = match depopts with
    | None -> []
    | Some lst ->
       List.rev_map (fun (n, v_opt) ->
         match v_opt with
           |None -> n | Some v -> n ^ "." ^ v
        ) lst
  in
  let graph = resolve (str :: depopt_str) in
  if 1 = OpamSolver.ActionGraph.nb_vertex graph then false, graph
  else true, graph

let read_conf conf =
  let rec read_conf_aux acc ic =
    Lwt_io.read_line_opt ic >>= function
    | None -> Lwt.return acc
    | Some line ->
       let eg = String.index line '=' in
       let name = String.sub line 0 eg in
       let value = String.sub line (eg + 2) (String.length line - eg - 3) in
       read_conf_aux ((name, value) :: acc) ic
  in
  Lwt_io.with_file ~mode:Lwt_io.input conf (read_conf_aux [])

let write_conf conf tups =
  (* make all the non-existent parent directories *)
  let prepare conf =
    let rec base acc path =
      if not (Sys.file_exists path) then
        let basename = Filename.basename path in
        let dir = Filename.dirname path in
        base (basename :: acc) dir
      else acc, path in
    let rec prepare_dir path = function
      | [] -> Lwt.return_unit
      | [f] ->
        let open Lwt_unix in
        Lwt_unix.openfile f [O_RDWR; O_CREAT] 0o664 >>= Lwt_unix.close
      | hd :: tl ->
        let new_dir = Filename.concat path hd in
        Lwt_unix.mkdir new_dir 0o775 >>= fun () ->
        prepare_dir new_dir tl
    in
    let names, dir = base [] conf in
    prepare_dir dir names in

  let write_conf_aux oc =
    List.rev_map (fun (n, v) -> Printf.sprintf "%s=\"%s\"" n v) tups
    |> String.concat "\n"
    |> Lwt_io.write oc in
  prepare conf >>= fun () ->
  Lwt_io.with_file ~mode:Lwt_io.output conf write_conf_aux

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

  if d = destdir && p = path then Lwt.return_unit
  else
    tups
    |> remove_tup "destdir"
    |> remove_tup "path"
    |> add_tup "destdir" destdir
    |> add_tup "path" path
    |> write_conf conf_path

let findlib_conf ~prefix ~write_path =
  let conf = Filename.concat prefix "lib/findlib.conf" in
  if not (Sys.file_exists conf) then Lwt.return_unit
  else begin
      let destdir = Filename.concat prefix "lib" in
      let path = Filename.concat prefix "lib" in
      modify_conf conf write_path ~destdir ~path
    end

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
          let open OpamState.Types in
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

let update_metadata ~install ~path =
  let state = load_state "update_metadata" in
  let rec packages_of_file acc ic =
    Lwt_io.read_line_opt ic >>= function
      | None -> Lwt.return acc
      | Some line ->
         let pos = Re_str.search_forward (Re_str.regexp_string " ") line 0 in
         let name = String.sub line 0 pos |> OpamPackage.Name.of_string in
         let version = String.sub line (pos + 1) (String.length line - pos - 1)
                       |> OpamPackage.Version.of_string in
         let nv = OpamPackage.create name version in
         packages_of_file (nv :: acc) ic in
  Lwt_io.with_file ~mode:Lwt_io.input path (packages_of_file []) >>= fun pkg_lst ->
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
  |> fun _ -> Lwt.return_unit

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

let switch c =
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
  let c = state.OpamState.Types.compiler in
  OpamCompiler.to_string c

let clean_repositories () =
  let t = load_state "clean-repository" in
  let repos = OpamState.sorted_repositories t in
  List.iter (fun r ->
    let name = OpamRepositoryName.to_string r.repo_name in
    if name = "default" then ()
    else OpamRepositoryCommand.remove r.repo_name) repos

let add_repositories repo =
  let add_one_repo (name, address, priority) =
    debug "repository: add %s %s" name address;
    let name = OpamRepositoryName.of_string name in
    let address = OpamTypesBase.address_of_string address in
    let address, kind = OpamTypesBase.parse_url address in
    OpamRepositoryCommand.add name kind address ~priority
  in
  (* FIXME: review use of fork *)
  match Lwt_unix.fork () with
  | 0 ->
    clean_repositories ();
    List.iter add_one_repo repo;
    exit 0
  | pid ->
    Lwt_unix.waitpid [] pid >>= fun (_, s) ->
    let open Lwt_unix in
    match s with
    | WEXITED i when i = 0 -> Lwt.return_unit
    | WEXITED i | WSIGNALED i | WSTOPPED i -> err "exited %d when add repos" i

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
