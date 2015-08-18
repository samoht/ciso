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
open Irmin_unix
open Sexplib.Conv

let debug fmt = Gol.debug ~section:"store" fmt
let err fmt = Printf.ksprintf Lwt.fail_with ("Ciso.Store:" ^^ fmt)

let sub len str = String.sub str 0 len
let sub_abbr = sub 5

module Store = Irmin.Basic(Irmin_http.Make)(Irmin.Contents.String)

type t = string -> Store.t

type token = Token of string with sexp
let string_of_token (Token t) = t
let token_of_string t = Token t

(* FIXME: duplicate code *)
let hash_token str =
  let `Hex h =
    Hex.of_cstruct (Nocrypto.Hash.SHA1.digest (Cstruct.of_string str))
  in
  h

let create_token info =
  let time = string_of_float (Sys.time ()) in
  token_of_string (hash_token (info ^ time))

let task msg =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let owner = "ciso" in
  Irmin.Task.create ~date ~owner msg

let path_of_token t =  ["token"; t]
let path_of_obj id = ["object"; id]
let path_of_job id = ["job"; id]
let path_of_arc id = ["archive"; id]
let path_of_com id = ["compiler"; id]

let clean_up t =
  let print_path k =
    let path = String.concat "/" k in
    Lwt_io.printf "remove %s\n%!" path
  in
  let clean_dir = ["token"; "object"; "job"; "archive"] in
  Lwt_list.iter_s (fun dir ->
    Store.list (t ("list files of " ^ dir)) [dir] >>= fun paths ->
    Lwt_list.iter_s (fun path ->
      print_path path >>= fun () ->
      Store.remove (t ("remove " ^ (String.concat "/" path))) path
      ) paths
    ) clean_dir

let create ?(uri = "http://127.0.0.1:8888") ?(fresh = false) () =
  let config = Irmin_http.config (Uri.of_string uri) in
  Store.create config task >>= fun t ->
  (if fresh then clean_up t else Lwt.return_unit) >|= fun () ->
  t

let register_token t (Token token) =
  let path = path_of_token token in
  Store.update (t ("register token " ^ token)) path token

let invalidate_token t (Token token) =
  let path = path_of_token token in
  Store.remove (t ("invalidate token" ^ token)) path

let query_object t id =
  let path = path_of_obj id in
  Store.mem (t ("query object " ^ id)) path

let err_invalid_token tok = err "invalid token: %s" tok

let publish_object t (Token token) id obj =
  debug "publish: object %s" (sub_abbr id);
  query_object t id >>= fun exist ->
  (if exist then Lwt.return () else
     let token_path = path_of_token token in
     Store.mem (t ("query token " ^ token)) token_path >>= function
     | false -> err_invalid_token token
     | true  ->
       let path = path_of_obj id in
       let content = Object.to_string obj in
       Store.update (t ("publish object " ^ id)) path content
  ) >|= fun () ->
  debug "publish: object %s published!" (sub_abbr id)

let retrieve_object t id =
  debug "retrieve: object %s" (sub_abbr id);
  let path = path_of_obj id in
  Store.read_exn (t ("retrieve object " ^ id)) path >|= fun v ->
  debug "retrieve: object %s found!" (sub_abbr id);
  Object.of_string v

let log_job t id (job, deps) =
  let path = path_of_job id in
  let entry = Job.create_entry job deps in
  let content = Job.string_of_entry entry in
  Store.update (t ("log job " ^ id)) path content

let archive_job t id =
  let path = path_of_job id in
  Store.read_exn (t ("retrieve job " ^ id)) path >>= fun c ->
  let arc_path = path_of_arc id in
  Store.update (t ("archive job" ^ id)) arc_path c

let unlog_job t id =
  Lwt.catch (fun () ->
      archive_job t id >>= fun () ->
      let path = path_of_job id in
      Store.remove (t ("unlog job" ^ id)) path
    ) (function _ ->
      (* FIXME: avoid catch-all errors *)
      Lwt.return_unit)

let retrieve_job t id =
  let jpath = path_of_job id in
  let apath = path_of_arc id in
  let read_job_content c = c |> Job.entry_of_string |> Job.unwrap_entry in
  Store.mem (t ("query job " ^ id)) jpath >>= fun is_jpath ->
  Store.mem (t ("query archive " ^ id)) apath >>= fun is_apath ->
  let path = if is_jpath then jpath else if is_apath then apath else [] in
  Store.read_exn (t ("get job entry for " ^ id)) path >|= fun c ->
  read_job_content c

let err_empty_path () = err "empty path"

let retrieve_jobs t =
  let par_dir = ["job"] in
  Store.list (t "list ids") par_dir >>= fun paths ->
  let rec id_of_path = function
    | [id]  -> Lwt.return id
    | _::tl -> id_of_path tl
    | []    -> err_empty_path ()
  in
  let entry_of_content c = Job.entry_of_string c in
  Lwt_list.rev_map_p (fun p ->
      id_of_path p >>= fun id ->
      Store.read_exn (t ("retrieve job" ^ id)) p >|= fun c ->
      let job, deps = Job.unwrap_entry (entry_of_content c) in
      id, job, deps
    ) paths

let query_compiler t id =
  debug "query: compiler %s" (sub_abbr id);
  let path = path_of_com id in
  Store.mem (t ("query compiler " ^ id)) path

let err_invalid_token tok = err "invalid token: %s" tok

let publish_compiler t (Token token) id obj =
  debug "publish: compiler %s" (sub_abbr id);
  query_compiler t id >>= fun exist ->
  (if exist then Lwt.return_unit
   else
     let token_path = path_of_token token in
     Store.mem (t ("query token " ^ token)) token_path >>= function
     | false -> err_invalid_token token
     | true  ->
       let path = path_of_com id in
       let content = Object.to_string obj in
       Store.update (t ("publish compiler " ^ id)) path content)
  >|= fun () ->
  debug "publish: compiler %s published! " (sub_abbr id)

let retrieve_compiler t id =
  debug "retrieve: compiler %s" (sub_abbr id);
  let path = path_of_com id in
  Store.read_exn (t ("retrieve compiler " ^ id)) path >|= fun v ->
  debug "retrieve: compiler %s found!" (sub_abbr id);
  Object.of_string v
