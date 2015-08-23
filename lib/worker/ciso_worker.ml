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

open Lwt.Infix

let debug fmt = Gol.debug ~section:"worker" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Worker: " ^^ fmt)
let failwith fmt = Printf.ksprintf failwith ("Ciso.Worker: " ^^ fmt)

let chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  let fail () = failwith "%s is not a prefix of %s" prefix t in
  if lt < lp then fail ()
  else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then fail ()
    else String.sub t lp (lt - lp)

let (/) = Filename.concat

module Body = Cohttp_lwt_body
module Code = Cohttp.Code
module Client = Cohttp_lwt_unix.Client
module Response = Cohttp_lwt_unix.Response


module JSet = struct
  include Set.Make(struct
      type t = Job.id
      let compare = Id.compare
    end)
  let of_list = List.fold_left (fun s e -> add e s) empty
end

module OSet = Set.Make(struct
    type t = Object.id
    let compare = Id.compare
  end)

module System = struct

  let debug fmt = Printf.printf fmt
  open Lwt.Infix

  (* FIXME: use Bos? *)

  (* from ocaml-git/lib/unix/git_unix.ml *)

  let openfile_pool = Lwt_pool.create 200 (fun () -> Lwt.return_unit)

  let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> Lwt.fail (Failure (Printexc.to_string e))
    | e -> Lwt.fail e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit
    | e -> Lwt.fail e

  let protect f x = Lwt.catch (fun () -> f x) protect_unix_exn

  let safe f x = Lwt.catch (fun () -> f x) ignore_enoent

  let remove_file f = safe Lwt_unix.unlink f

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir && Sys.is_directory dir then Lwt.return_unit
      else (
        let clear =
          if Sys.file_exists dir then (
            debug "%s already exists but is a file, removing." dir;
            remove_file dir;
          ) else
            Lwt.return_unit
        in
        clear >>= fun () ->
        aux (Filename.dirname dir) >>= fun () ->
        debug "mkdir %s" dir;
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

  let list_files kind dir =
    Lwt_pool.use openfile_pool (fun () ->
        if Sys.file_exists dir then (
          let s = Lwt_unix.files_of_directory dir in
          let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
          let s = Lwt_stream.map (Filename.concat dir) s in
          let s = Lwt_stream.filter kind s in
          Lwt_stream.to_list s >>= fun l ->
          Lwt.return l
        ) else
          Lwt.return_nil
      )

  let directories dir =
    list_files (fun f ->
        try Sys.is_directory f with Sys_error _ -> false
      ) dir

  let files dir =
    list_files (fun f ->
        try not (Sys.is_directory f) with Sys_error _ -> false
      ) dir

  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds
    in
    aux [] dir

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then Lwt.fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return_unit in
    match Cstruct.len b with
    | 0   -> Lwt.return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let with_write_file ?temp_dir file fn =
    begin match temp_dir with
      | None   -> Lwt.return_unit
      | Some d -> mkdir d
    end >>= fun () ->
    let dir = Filename.dirname file in
    mkdir dir >>= fun () ->
    let tmp = Filename.temp_file ?temp_dir (Filename.basename file) "write" in
    Lwt_pool.use openfile_pool (fun () ->
        debug "Writing %s (%s)" file tmp;
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644) >>= fun fd ->
        Lwt.finalize
          (fun () -> protect fn fd >>= fun () -> Lwt_unix.rename tmp file)
          (fun _  -> Lwt_unix.close fd)
      )

  let write_file file ?temp_dir b =
    with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)

  let read_file file =
    Unix.handle_unix_error (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            debug "Reading %s" file;
            let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
            let ba = Lwt_bytes.map_file ~fd ~shared:false () in
            Unix.close fd;
            Lwt.return (Cstruct.of_bigarray ba)
          ))
      ()

  (* end of ocaml-git *)

  let niet () = Lwt.return_unit

  let exec ?on_failure ?(on_success=niet) cmd =
    let on_failure = match on_failure with
      | None   -> (fun () -> err "%S: failure" cmd)
      | Some f -> f
    in
    let open Lwt_unix in
    Lwt_unix.system cmd >>= function
    | WEXITED rc when rc = 0 -> on_success ()
    | WEXITED _ | WSIGNALED _ | WSTOPPED _ -> on_failure ()

  let install_archive (name, content) =
    let tmp = "/tmp/ciso-" / name in
    if Sys.file_exists tmp then Sys.remove tmp;
    write_file tmp content >>= fun () ->
    Lwt.return tmp

  let extract_archive tar =
    if not (Filename.check_suffix tar ".tar.gz") then
      raise (Invalid_argument tar);
    let cmd = Printf.sprintf "tar -xzf %s -C /" tar in
    let on_failure () = err "extract: cannot untar %s" tar in
    let on_success () = debug "extract: OK"; Lwt.return_unit in
    exec ~on_success ~on_failure cmd

  let install_files ~src ~dst files =
    let cp src dst =
      let comm = Printf.sprintf "cp %s %s" src dst in
      let on_failure () = err "cp: cannot copy %s to %s" src dst in
      let on_success () = debug "%s installed" src; Lwt.return_unit in
      exec ~on_success ~on_failure comm
    in
    Lwt_list.iter_s (fun (f, _) ->
        (* FIXME: verify integrity of the digest? *)
        let src_path = src / f in
        let dst_path = dst / f in
        mkdir (Filename.dirname (dst / f)) >>= fun () ->
        cp src_path dst_path
      ) files

  let name_of_archive name =
    assert (Filename.check_suffix name ".tar.gz");
    Filename.chop_extension (Filename.chop_extension name)

  let clean_tmp action name =
    let file = "/tmp" / name in
    let dir = "/tmp" / name_of_archive name in
    let comm = Printf.sprintf "rm -rf %s %s" file dir in
    let on_success () =
      debug "clean_tmp: %s done (%s %s)!" action file dir;
      Lwt.return_unit
    in
    let on_failure () = err "cannot remove %s and %s" file dir in
    exec ~on_failure ~on_success comm

end

type t = {
  w     : Worker.t;                              (* the worker configuration. *)
  local : Store.t;              (* the local store, which is used as a cache. *)
  global: Store.t;                   (* the global store, accessed over HTTP. *)
}

let tick_sleep = 1.00
let idle_sleep = 10.0

let archive_of_id id =
  Filename.get_temp_dir_name () / Id.to_string id  ^ ".tar.gz"

let prefix_of_job job =
  Opam.root () / Switch.to_string (Job.switch job)

let create ?root w uri =
  Store.local ?root () >>= fun local ->
  Store.remote ~uri () >|= fun global ->
  { w; local; global }

let snapshots ?white_list job =
  let prefix = prefix_of_job job in
  let rec loop checksums = function
    | [] -> checksums
    | path :: tl ->
      (* soft link to absent file *)
      if not (Sys.file_exists path) then loop checksums tl
      else if not (Sys.is_directory path) then
        loop ((path, Digest.file path) :: checksums) tl
      else
        let files =
          Sys.readdir path
          |> Array.to_list
          |> List.rev_map (fun f -> path / f)
        in
        loop checksums (List.rev_append files tl)
  in
  let sub_dirs =
    Sys.readdir prefix
    |> Array.to_list
    |> (fun lst -> match white_list with
        | Some wl -> List.filter (fun n -> List.mem n wl) lst
        | None    -> lst)
    |> List.rev_map (fun n -> prefix / n)
  in
  loop [] sub_dirs

let opam_snapshot job = Opam.read_installed (Job.switch job)

let collect_outputs job =
  let is_output f =
    List.exists
      (fun suffix -> Filename.check_suffix f suffix)
      [".info"; ".err"; ".out"; ".env"]
  in
  let prefix = prefix_of_job job in
  System.rec_files (prefix / "build") >>= fun files ->
  let files = List.filter is_output files in
  Lwt_list.map_p (fun f ->
      let name = chop_prefix ~prefix f in
      System.read_file f >|= fun raw ->
      Object.file name raw
    ) files

let collect_installed job ~before ~after =
  let module CsMap = Map.Make(String) in
  let cmap =
    List.fold_left
      (fun acc (f, checksum) -> CsMap.add f checksum acc)
      CsMap.empty before
  in
  (* TODO: collect deleted files *)
  let installed =
    List.fold_left (fun acc (f, checksum) ->
        if not (CsMap.mem f cmap) then (f, checksum) :: acc else
          let cs = CsMap.find f cmap in
          if cs <> checksum then (f, checksum) :: acc else acc
      ) [] after
  in
  (* 1 is for the delimiter *)
  let prefix = prefix_of_job job in
  let files = List.rev_map (fun (f, d) -> chop_prefix ~prefix f, d) installed in
  Lwt.return files

(* FIXME: console outputs should not be in the archive *)
let create_archive job files ~old_pkgs ~new_pkgs =
  let path = archive_of_id (Job.id job) in
  let dst  = Filename.dirname path in
  let src  = prefix_of_job job in
  System.install_files ~src ~dst files >>= fun () ->
  let installed = List.filter (fun p -> not (List.mem p old_pkgs)) new_pkgs in
  Opam.write_installed (Job.switch job) installed;
  let cmd = Printf.sprintf "tar -zcf %s %s" path dst in
  System.exec cmd >>= fun () ->
  System.read_file path >|= fun content ->
  Object.archive files content

let extract_object job obj =
  match Object.contents obj with
  | Object.File (name, raw) ->
    let path = prefix_of_job job / name in
    System.write_file path raw
  | Object.Archive { Object.files; raw } ->
    let path = archive_of_id (Object.id obj) in
    System.install_archive (path, raw) >>= fun arch_path ->
    System.extract_archive arch_path >>= fun () ->
    let src = System.name_of_archive arch_path in
    let dst = prefix_of_job job in
    System.install_files ~src ~dst files >>= fun () ->
    System.clean_tmp "extract_object" (Filename.basename arch_path)

(* FIXME: add caching *)
let find_job_deps t j =
  let rec aux todo deps =
    if JSet.is_empty todo then Lwt.return (JSet.elements deps)
    else
      let id = JSet.choose todo in
      let todo = JSet.remove id todo in
      Store.Job.find t.local id >>= function
      | None   -> aux todo deps
      | Some j ->
        let inputs = JSet.of_list (Job.inputs j) in
        let todo = JSet.(union todo (diff inputs deps)) in
        let deps = JSet.union inputs deps in
        aux todo deps
  in
  aux JSet.(singleton j) JSet.empty

let find_obj_deps t j =
  find_job_deps t j >>= fun jobs ->
  Lwt_list.fold_left_s (fun deps job ->
      if j = job then Lwt.return deps
      else
        Store.Job.outputs t.local job >|= fun objs ->
        List.fold_left (fun s e -> OSet.add e s) deps objs
    ) OSet.empty jobs
  >|= fun objs ->
  OSet.elements objs

let prepare t job  =
  find_obj_deps t (Job.id job) >>= fun objs ->
  Opam.switch_to (Job.switch job) >>= fun () ->
  (* URGENT FIXME: installation order IS important *)
  Lwt_list.iter_p (fun oid ->
      Store.Object.find t.local oid >>= function
      | None   -> err "cannot find object %s" (Id.to_string oid)
      | Some o -> extract_object job o
    ) objs

let default_white_list = ["lib"; "bin"; "sbin"; "doc"; "share"; "etc"; "man"]

let process_job ?(white_list=default_white_list) t job =
  prepare t job >>= fun () ->
  let dbg = Id.to_string (Job.id job) in
  debug "build: %s, pre-snapshot" dbg;
  let before = snapshots ~white_list job in
  let old_pkgs = opam_snapshot job in
  debug "build: %s, install." dbg;
  (* FIXME: handle the Package.info *)
  let pkgs = List.map fst (Job.packages job) in
  begin
    Lwt.catch
      (fun ()   -> Opam.install pkgs >|= fun () -> `Success)
      (fun _exn -> Lwt.return `Failure)
  end >>= fun result ->
  let () = match result with
    | `Success -> debug "build: %s Success!" dbg
    | `Failure -> debug "build: %s Failure!" dbg
  in
  debug "build: %s, post-snapshot" dbg;
  let after = snapshots ~white_list job in
  let new_pkgs = opam_snapshot job in
  collect_outputs job >>= fun outputs ->
  collect_installed job ~before ~after >>= fun installed ->
  create_archive job installed ~old_pkgs ~new_pkgs >>= fun archive ->
  System.clean_tmp "pkg_build" (archive_of_id @@ Job.id job) >>= fun () ->
  Opam.remove pkgs >>= fun () ->
  Store.with_transaction t.local "Job complete" (fun t ->
      let add_one o =
        Store.Object.add t archive >>= fun () ->
        Store.Job.add_output t (Job.id job) (Object.id o)
      in
      Lwt_list.iter_p add_one (archive :: outputs) >>= fun () ->
      match result with
      | `Success -> Store.Job.success t (Job.id job)
      | `Failure -> Store.Job.success t (Job.id job)
    ) >>= function
  | false -> err "cannot commit the job" (* FIXME: ? *)
  | true  -> Lwt.return_unit

(*
  Lwt.catch (fun () ->
      (match repos with
       | [] -> Opam.clean_repos (); Lwt.return_unit
       | _  -> Opam.add_repos repos)
      >>= fun () ->
      Opam.update () >>= fun () ->
      let c_curr = Opam.compiler () in
      let c = Job.compiler job in
      (if c = c_curr then Lwt.return_unit
       else
         Opam.export_switch c >>= fun () ->
         install_compiler worker c (Job.host job) >|= fun () ->
         debug "execute: compiler %s installed" c
      ) >>= fun () ->
      (match pins with
       | [] -> Lwt.return_unit
       | _  ->
         let build = root / Compiler.to_string c / "build" in
         Unix.mkdir build 0o775;
         Opam.add_pins pins
      ) >>= fun () ->
      let prefix = Opam.get_var "prefix" in
      debug "execute: opam load state";
      Opam.show_repo_pin () >>= fun () ->
      debug "execute: %d dependencies" (List.length deps);
      Lwt_list.iter_s (fun dep ->
          worker_request_object base worker dep >>= fun obj ->
          apply_object prefix obj
        ) deps
      >>= fun () ->
      (* FIXME: we should not have access to task here *)
      let pkgs = Task.packages (Job.task job) in
      let graph = Opam.resolve_packages pkgs in
      match Opam.is_simple graph with
      | None ->
        debug "execute: simple=false";
        let job_lst = Opam.jobs ~repos ~pins graph in
        worker_spawn base worker job_lst >|= fun () ->
        let delegate_id =
          List.fold_left (fun acc (id, job, _) ->
              let p = List.hd (Task.packages (Job.task job)) in
              if List.mem p pkgs then id :: acc else acc
            ) [] job_lst
          |> (fun id_lst -> assert (1 = List.length id_lst); List.hd id_lst)
        in
        let result = `Delegate delegate_id in
        (* FIXME: this is weird *)
        let archive = "", Cstruct.of_string "" in
        let id = Job.output job in
        let obj = Object.create ~id ~outputs:[] ~files:[] ~archive in
        switch_clean_up root c;
        result, obj
      | Some pkg ->
        debug "execute: simple=%s" (Package.to_string pkg);
        let v =
          match Package.version pkg with Some v -> v | None -> assert false
        in
        pkg_build prefix jid (Package.name pkg) v
        >|= fun (result, outputs, files, archive) ->
        (* FIXME: update job result *)
        let id = Job.output job in
        let obj = Object.create ~id ~outputs ~files ~archive in
        switch_clean_up root c;
        result, obj
    ) (fun exn ->
      let result = match exn with
        | Failure f -> `Fail f
        | _ -> `Fail "unknow execution failure" in
      (* FIXME: this is weird *)
      let archive = "", Cstruct.of_string "" in
      let id = Job.output job in
      (* FIXME: update job result *)
      let obj = Object.create ~id ~outputs:[] ~files:[] ~archive in
      switch_clean_up root (Job.compiler job);
      Lwt.return (result, obj))

let rec execution_loop base worker cond =
  Lwt_condition.wait cond >>= fun (jid, deps) ->
  local_query worker.local jid >>= fun completed ->
  if completed then execution_loop base worker cond
  else (
    worker.status <- Working jid;
    (* URGENT FIXME: we don't want to get the task here *)
    (match Task.to_compiler task with
     | None   -> pkg_job_execute base worker jid job deps
     | Some _ -> compiler_job_execute jid job)
    >>= fun (result, obj) ->
    let id = Job.output job in
    (match result with
     | `Delegate _ ->
       Store.publish_object worker.store id obj >>= fun () ->
       Store.unlog_job worker.store jid >>= fun () ->
       worker_publish base worker result jid
     | `Success ->
       Store.publish_object worker.store id obj >>= fun () ->
       Store.unlog_job worker.store jid >>= fun () ->
       Lwt.join [worker_publish base worker result jid;
                 local_publish worker.local id obj]
     | `Fail _ ->
       Store.publish_object worker.store id obj >>= fun () ->
       worker_publish base worker result jid
    ) >>= fun () ->
    worker.status <- Idle;
    execution_loop base worker cond
  )
*)

let execution_loop t =
  Store.Worker.watch_status t.local (Worker.id t.w) (function
      | `Task _ -> failwith "TODO"
      | `Idle   -> Lwt_unix.sleep idle_sleep
      | `Job id ->
        Store.Job.find t.local id >>= function
        | Some job -> process_job t job
        | None     -> err "cannot find the job %s" (Id.to_string id)
    ) >>= fun _cancel ->
  let t, _ = Lwt.task () in
  t

let rec heartbeat_loop t =
  let id = Worker.id t.w in
  Store.Worker.tick t.local id (Unix.time ()) >>= fun () ->
  Lwt_unix.sleep tick_sleep >>= fun () ->
  heartbeat_loop t

let run ?root uri =
  let w = Worker.create (Host.detect ()) in
  create ?root w uri >>= fun t ->
  Store.Worker.add t.local w >>= fun () ->
  Lwt.pick [
    heartbeat_loop t;
    execution_loop t;
  ]
