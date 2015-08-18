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

let debug fmt = Gol.debug ~section:"store" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Store:" ^^ fmt)

let store_handle = ref None

let sub len str = String.sub str 0 len
let sub_abbr = sub 5

let path_of_token t =  ["token"; t]
let path_of_obj id = ["object"; id]
let path_of_job id = ["job"; id]
let path_of_arc id = ["archive"; id]
let path_of_com id = ["compiler"; id]

let clean_up t =
  let print_path k =
    let path = String.concat "/" k in
    Lwt_io.printf "remove %s\n%!" path in
  let clean_dir = ["token"; "object"; "job"; "archive"] in
  Lwt_list.iter_s (fun dir ->
    Irmin.list (t ("list files of " ^ dir)) [dir] >>= fun paths ->
    Lwt_list.iter_s (fun path ->
      print_path path >>= fun () ->
      Irmin.remove (t ("remove " ^ (String.concat "/" path))) path) paths)
      clean_dir

let initial_store ?(uri = "http://127.0.0.1:8888") ?(fresh = false) () =
  let basic = Irmin.basic (module Irmin_unix.Irmin_http.Make)
                          (module Irmin.Contents.String) in
  let config = Irmin_unix.Irmin_http.config (Uri.of_string uri) in
  Irmin.create basic config Irmin_unix.task >>= fun t ->
  if fresh then clean_up t else Lwt.return () >>= fun () ->
  store_handle := Some t;
  Lwt.return ()

let rec get_store () =
  match !store_handle with
  | None       -> initial_store () >>=  get_store
  | Some store -> Lwt.return store

let register_token token =
  get_store () >>= fun t ->
  let path = path_of_token token in
  Irmin.update (t ("register token " ^ token)) path token

let invalidate_token token =
  get_store () >>= fun t ->
  let path = path_of_token token in
  Irmin.remove (t ("invalidate token" ^ token)) path

let query_object id =
  get_store () >>= fun t ->
  let path = path_of_obj id in
  Irmin.mem (t ("query object " ^ id)) path

let publish_object token id obj =
  debug "publish: object %s" (sub_abbr id);
  query_object id >>= fun exist ->
  (if exist then Lwt.return () else
     get_store () >>= fun t ->
     let token_path = path_of_token token in
     Irmin.read (t ("query token " ^ token)) token_path >>= fun t_opt ->
     match t_opt with
     | None -> Lwt.fail (raise (Invalid_argument token))
     | Some _ ->
       let path = path_of_obj id in
       let content = Sexplib.Sexp.to_string (Object.sexp_of_t obj) in
       Irmin.update (t ("publish object " ^ id)) path content)
  >|= fun () ->
  debug "publish: object %s published!" (sub_abbr id)

let err_not_found id = err "object %s not found" (sub_abbr id)

let retrieve_object id =
  debug "retrieve: object %s" (sub_abbr id);
  get_store () >>= fun t ->
  let path = path_of_obj id in
  Irmin.read (t ("retrieve object " ^ id)) path >>= function
  | None -> err_not_found id
  | Some v ->
    debug "retrieve: object %s found!" (sub_abbr id);
    Lwt.return (Object.t_of_sexp (Sexplib.Sexp.of_string v))

let log_job id (job, deps) =
  get_store () >>= fun t ->
  let path = path_of_job id in
  let entry = Task.make_job_entry job deps in
  let content = Sexplib.Sexp.to_string (Task.sexp_of_job_entry entry) in
  Irmin.update (t ("log job " ^ id)) path content

let archive_job id =
  get_store () >>= fun t ->
  let path = path_of_job id in
  Irmin.read (t ("retrieve job " ^ id)) path >>= function
  | None -> Lwt.fail (raise Not_found)
  | Some c ->
    let arc_path = path_of_arc id in
    Irmin.update (t ("archive job" ^ id)) arc_path c

let unlog_job id =
  Lwt.catch (fun () ->
      archive_job id >>= fun () ->
      get_store () >>= fun t ->
      let path = path_of_job id in
      Irmin.remove (t ("unlog job" ^ id)) path
    ) (function _ ->
      (* FIXME: avoid catch-all errors *)
      Lwt.return_unit)

let retrieve_job id =
  get_store () >>= fun t ->
  let jpath = path_of_job id in
  let apath = path_of_arc id in
  let read_job_content c =
    c |> Sexplib.Sexp.of_string
    |> Task.job_entry_of_sexp
    |> Task.unwrap_entry
  in
  Irmin.mem (t ("query job " ^ id)) jpath >>= fun is_jpath ->
  Irmin.mem (t ("query archive " ^ id)) apath >>= fun is_apath ->
  let path = if is_jpath then jpath else if is_apath then apath else [] in
  Irmin.read_exn (t ("get job entry for " ^ id)) path >|= fun c ->
  read_job_content c

let retrieve_jobs () =
  get_store () >>= fun t ->
  let par_dir = ["job"] in
  Irmin.list (t "list ids") par_dir >>= fun paths ->
  let rec id_of_path = function
    | [id]  -> id
    | _::tl -> id_of_path tl
    | []    -> raise (Invalid_argument "empty path")
  in
  let entry_of_content c = Task.job_entry_of_sexp (Sexplib.Sexp.of_string c) in
  Lwt_list.rev_map_p (fun p ->
      let id = id_of_path p in
      Irmin.read_exn (t ("retrieve job" ^ id)) p >|= fun c ->
      let job, deps = Task.unwrap_entry (entry_of_content c) in
      id, job, deps
    ) paths

let query_compiler id =
  debug "query: compiler %s" (sub_abbr id);
  get_store () >>= fun t ->
  let path = path_of_com id in
  Irmin.mem (t ("query compiler " ^ id)) path

let err_invalid_token tok = err "invalid token: %s" tok

let publish_compiler token id obj =
  debug "publish: compiler %s" (sub_abbr id);
  query_compiler id >>= fun exist ->
  (if exist then Lwt.return_unit
   else
     get_store () >>= fun t ->
     let token_path = path_of_token token in
     Irmin.mem (t ("query token " ^ token)) token_path >>= function
     | false -> err_invalid_token token
     | true  ->
       let path = path_of_com id in
       let content = Sexplib.Sexp.to_string (Object.sexp_of_t obj) in
       Irmin.update (t ("publish compiler " ^ id)) path content)
  >|= fun () ->
  debug "publish: compiler %s published! " (sub_abbr id)

let retrieve_compiler id =
  debug "retrieve: compiler %s" (sub_abbr id);
  get_store () >>= fun t ->
  let path = path_of_com id in
  Irmin.read_exn (t ("retrieve compiler " ^ id)) path >|= fun v ->
  debug "retrieve: compiler %s found!" (sub_abbr id);
  Object.t_of_sexp (Sexplib.Sexp.of_string v)
