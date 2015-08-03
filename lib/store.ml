open Lwt

let store_handle = ref None

let sub len str = String.sub str 0 len
let sub_abbr = sub 5

let log action obj ~info =
  Printf.eprintf "[%s~%s]: %s\n%!" action obj info

let time () = Unix.(
  let tm = localtime (time ()) in
  Printf.sprintf "%d:%d:%d" tm.tm_hour tm.tm_min tm.tm_sec)

let clean_up t =
  let print_path k =
    let path = String.concat "/" k in
    Lwt_io.printf "%s -> value\n%!" path in
  let remove_kv k v_t =
    v_t >>= fun _ ->
    print_path k >>= fun () ->
    let dir = List.hd k in
    if dir <> "compiler" then Irmin.remove (t "remove kv") k
    else return_unit in
  Irmin.iter (t "iter kv") remove_kv


let initial_store ?(uri = "http://127.0.0.1:8888") ?(fresh = false) () =
  let basic = Irmin.basic (module Irmin_unix.Irmin_http.Make)
                          (module Irmin.Contents.String) in
  let config = Irmin_unix.Irmin_http.config (Uri.of_string uri) in
  Irmin.create basic config Irmin_unix.task >>= fun t ->
  if fresh then clean_up t else return () >>= fun () ->
  store_handle := Some t;
  return ()


let rec get_store () =
  match !store_handle with
    | None -> initial_store () >>= fun () ->
              get_store ()
    | Some store -> return store


let path_of_token t =
  let sub_dir = String.sub t 0 2 in
  ["token"; sub_dir; t]


let register_token token =
  get_store () >>= fun t ->
  let path = path_of_token token in
  Irmin.update (t ("register token " ^ token)) path token


let invalidate_token token =
  get_store () >>= fun t ->
  let path = path_of_token token in
  Irmin.remove (t ("invalidate token" ^ token)) path


let path_of_obj id =
  let sub_dir = String.sub id 0 2 in
  ["object"; sub_dir; id]


let query_object id =
  get_store () >>= fun t ->
  let path = path_of_obj id in
  Irmin.read (t ("query object " ^ id)) path >>= fun v_opt ->
  if v_opt <> None then return true else return false


let publish_object token id obj =
  let info = Printf.sprintf "object %s %s" (sub_abbr id) (time ()) in
  log "publish" "remotely" ~info;
  query_object id >>= fun exist ->
    (if exist then return () else
      get_store () >>= fun t ->
      let token_path = path_of_token token in
      Irmin.read (t ("query token " ^ token)) token_path >>= fun t_opt ->
      match t_opt with
      | None -> fail (raise (Invalid_argument token))
      | Some _ ->
         let path = path_of_obj id in
         let content = Sexplib.Sexp.to_string (Object.sexp_of_t obj) in
         Irmin.update (t ("publish object " ^ id)) path content)
  >>= fun () ->
    return (log "publish" "remotely" ~info:("completed " ^ (time ())))


let retrieve_object id =
  let info = Printf.sprintf "object %s %s" (sub_abbr id) (time ()) in
  log "retrieve" "remotely" ~info;
  get_store () >>= fun t ->
  let path = path_of_obj id in
  Irmin.read (t ("retrieve object " ^ id)) path >>= function
  | None -> fail Not_found
  | Some v ->
     log "retrieve" "remotely" ~info:("get one " ^ (time ()));
     return (Object.t_of_sexp (Sexplib.Sexp.of_string v))


let path_of_job id =
  ["job"; id]


let path_of_arc id =
  let dir = String.sub id 0 2 in
  ["archive"; dir; id]


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
  | None -> fail (raise Not_found)
  | Some c ->
     let arc_path = path_of_arc id in
     Irmin.update (t ("archive job" ^ id)) arc_path c


let unlog_job id =
  catch (fun () -> archive_job id >>= fun () ->
    get_store () >>= fun t ->
    let path = path_of_job id in
    Irmin.remove (t ("unlog job" ^ id)) path)
    (function | _ -> return_unit)


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
  | Some c -> return (read_job_content c)
  | None -> fail_with "job missing"


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
      | None -> fail (raise Not_found)
      | Some c ->
         let job, deps = Task.unwrap_entry (entry_of_content c) in
         return (id, job, deps)) paths


let path_of_com id =
  ["compiler"; id]


let query_compiler id =
  let info = Printf.sprintf "compiler %s %s" (sub_abbr id) (time ()) in
  log "query" "remotely" ~info;
  get_store () >>= fun t ->
  let path = path_of_com id in
  Irmin.read (t ("query compiler " ^ id)) path >>= fun v_opt ->
  if v_opt <> None then return true else return false


let publish_compiler token id obj =
  let info = Printf.sprintf "compiler %s %s" (sub_abbr id) (time ()) in
  log "publish" "remotely" ~info;
  query_compiler id >>= fun exist ->
    (if exist then return () else
      get_store () >>= fun t ->
      let token_path = path_of_token token in
      Irmin.read (t ("query token " ^ token)) token_path >>= fun t_opt ->
      match t_opt with
      | None -> fail (raise (Invalid_argument token))
      | Some _ ->
         let path = path_of_com id in
         let content = Sexplib.Sexp.to_string (Object.sexp_of_t obj) in
         Irmin.update (t ("publish compiler " ^ id)) path content)
  >>= fun () ->
    return (log "publish" "remotely" ~info:("completed " ^ (time ())))


let retrieve_compiler id =
  let info = Printf.sprintf "compiler %s %s" (sub_abbr id) (time ()) in
  log "retrieve" "remotely" ~info;
  get_store () >>= fun t ->
  let path = path_of_com id in
  Irmin.read (t ("retrieve compiler " ^ id)) path >>= function
  | None -> fail Not_found
  | Some v ->
     log "retrieve" "remotely" ~info:("get one " ^ (time ()));
     return (Object.t_of_sexp (Sexplib.Sexp.of_string v))
