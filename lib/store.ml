open Lwt

let store_handle = ref None

let path_of_id id =
  let sub_dir = String.sub id 0 2 in
  ["object"; sub_dir; id]

let path_of_token t =
  let sub_dir = String.sub t 0 2 in
  ["token"; sub_dir; t]

let initial_store ?(uri = "http://127.0.0.1:8888") () =
  let basic = Irmin.basic (module Irmin_unix.Irmin_http.Make)
                          (module Irmin.Contents.String) in
  let config = Irmin_unix.Irmin_http.config (Uri.of_string uri) in
  Irmin.of_tag basic config Irmin_unix.task "objects" >>= fun t ->
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
