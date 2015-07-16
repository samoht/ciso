open Lwt

let store_handle = ref None

let path_of_id id =
  let sub_dir = String.sub id 0 2 in
  ["object"; sub_dir; id]

let path_of_token t =
  let sub_dir = String.sub t 0 2 in
  ["token"; sub_dir; t]

let clean_up t =
  let print_path k =
    let path = String.concat "/" k in
    Lwt_io.printf "%s -> value\n%!" path in
  let remove_kv k v_t =
    v_t >>= fun _ ->
    print_path k >>= fun () ->
    Irmin.remove (t "remove kv") k in
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

let query_object id =
  get_store () >>= fun t ->
  let path = path_of_id id in
  Irmin.read (t ("query object " ^ id)) path >>= fun v_opt ->
  if v_opt <> None then return true else return false

let publish_object token id obj =
  query_object id >>= fun exist ->
    if exist then return () else
      get_store () >>= fun t ->
      let token_path = path_of_token token in
      Irmin.read (t ("query token " ^ token)) token_path >>= fun t_opt ->
      match t_opt with
      | None -> fail (raise (Invalid_argument token))
      | Some _ ->
         let path = path_of_id id in
         let content = Sexplib.Sexp.to_string (Object.sexp_of_t obj) in
         Irmin.update (t ("publish object " ^ id)) path content

let retrieve_object id =
  get_store () >>= fun t ->
  let path = path_of_id id in
  Irmin.read (t "retrieve") path >>= function
  | None -> fail Not_found
  | Some v -> return (Object.t_of_sexp (Sexplib.Sexp.of_string v))

let register_token token =
  get_store () >>= fun t ->
  let path = path_of_token token in
  Irmin.update (t ("register token " ^ token)) path token

let invalidate_token token =
  get_store () >>= fun t ->
  let path = path_of_token token in
  Irmin.remove (t ("invalidate token" ^ token)) path

let jpath_of_id id =
  ["job"; id]

let log_job id (job, deps) =
  get_store () >>= fun t ->
  let path = jpath_of_id id in
  let entry = Task.make_job_entry job deps in
  let content = Sexplib.Sexp.to_string (Task.sexp_of_job_entry entry) in
  Irmin.update (t ("log job " ^ id)) path content

let unlog_job id =
  get_store () >>= fun t ->
  let path = jpath_of_id id in
  Irmin.remove (t ("unlog job" ^ id)) path

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
