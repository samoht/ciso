open Sexplib.Std

type t = {
  id : int;
  addr : string * int;
  path : string
} with sexp

let path_of_t {path} = path
let id_of_t {id} = id
let addr_of_t {addr} = addr

let create id addr path = {id; addr; path}
