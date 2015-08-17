open Lwt.Infix

let store_handle = ref None

let sub len str = String.sub str 0 len
let sub_abbr = sub 5

let log action obj ~info =
  Printf.eprintf "[%s~%s]: %s\n%!" action obj info

let time () = Unix.(
  let tm = localtime (time ()) in
  Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec)


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
    | None -> initial_store () >>= fun () ->
              get_store ()
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
  let info = Printf.sprintf "object %s %s" (sub_abbr id) (time ()) in
  log "publish" "remotely" ~info;
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
  >>= fun () ->
    Lwt.return (log "publish" "remotely" ~info:("completed " ^ (time ())))


let retrieve_object id =
  let info = Printf.sprintf "object %s %s" (sub_abbr id) (time ()) in
  log "retrieve" "remotely" ~info;
  get_store () >>= fun t ->
  let path = path_of_obj id in
  Irmin.read (t ("retrieve object " ^ id)) path >>= function
  | None -> Lwt.fail Not_found
  | Some v ->
     log "retrieve" "remotely" ~info:("get one " ^ (time ()));
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
  Lwt.catch (fun () -> archive_job id >>= fun () ->
    get_store () >>= fun t ->
    let path = path_of_job id in
    Irmin.remove (t ("unlog job" ^ id)) path)
    (function | _ -> Lwt.return_unit)


let retrieve_job id =
  get_store () >>= fun t ->
  let jpath = path_of_job id in
  let apath = path_of_arc id in
  let read_job_content c =
    c |> Sexplib.Sexp.of_string
    |> Task.job_entry_of_sexp
    |> Task.unwrap_entry in

  Irmin.mem (t ("query job " ^ id)) jpath >>= fun is_jpath ->
  Irmin.mem (t ("query archive " ^ id)) apath >>= fun is_apath ->
  let path = if is_jpath then jpath
             else if is_apath then apath
             else [] in
  Irmin.read (t ("get job entry for " ^ id)) path >>= function
  | Some c -> Lwt.return (read_job_content c)
  | None -> Lwt.fail_with "job missing"


let retrieve_jobs () =
  get_store () >>= fun t ->
  let par_dir = ["job"] in
  Irmin.list (t "list ids") par_dir >>= fun paths ->
  let rec id_of_path = function
    | [id] -> id
    | _ :: tl -> id_of_path tl
    | [] -> raise (Invalid_argument "empty path") in
  let entry_of_content c =
    Task.job_entry_of_sexp (Sexplib.Sexp.of_string c) in
  Lwt_list.rev_map_p (fun p ->
      let id = id_of_path p in
      Irmin.read (t ("retrieve job" ^ id)) p >>= function
      | None -> Lwt.fail (raise Not_found)
      | Some c ->
         let job, deps = Task.unwrap_entry (entry_of_content c) in
         Lwt.return (id, job, deps)) paths


let query_compiler id =
  let info = Printf.sprintf "compiler %s %s" (sub_abbr id) (time ()) in
  log "query" "remotely" ~info;
  get_store () >>= fun t ->
  let path = path_of_com id in
  Irmin.mem (t ("query compiler " ^ id)) path


let publish_compiler token id obj =
  let info = Printf.sprintf "compiler %s %s" (sub_abbr id) (time ()) in
  log "publish" "remotely" ~info;
  query_compiler id >>= fun exist ->
    (if exist then Lwt.return () else
      get_store () >>= fun t ->
      let token_path = path_of_token token in
      Irmin.read (t ("query token " ^ token)) token_path >>= fun t_opt ->
      match t_opt with
      | None -> Lwt.fail (raise (Invalid_argument token))
      | Some _ ->
         let path = path_of_com id in
         let content = Sexplib.Sexp.to_string (Object.sexp_of_t obj) in
         Irmin.update (t ("publish compiler " ^ id)) path content)
  >>= fun () ->
    Lwt.return (log "publish" "remotely" ~info:("completed " ^ (time ())))


let retrieve_compiler id =
  let info = Printf.sprintf "compiler %s %s" (sub_abbr id) (time ()) in
  log "retrieve" "remotely" ~info;
  get_store () >>= fun t ->
  let path = path_of_com id in
  Irmin.read (t ("retrieve compiler " ^ id)) path >>= function
  | None -> Lwt.fail Not_found
  | Some v ->
     log "retrieve" "remotely" ~info:("get one " ^ (time ()));
     Lwt.return (Object.t_of_sexp (Sexplib.Sexp.of_string v))
