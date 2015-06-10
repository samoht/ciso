open Sexplib.Std

type t = {
  id : string;          (* object is referenced by id in scheduler *)
  addr : string * int;  (* ip * port of the machine who has a copy of object *)
  path : string         (* the path to the object on the machine at addr *)
} with sexp

let path_of_t {path} = path
let id_of_t {id} = id
let addr_of_t {addr} = addr
let ip_of_t t = fst (addr_of_t t)

let create id addr path = {id; addr; path}

let string_of_t obj =
  sexp_of_t obj |> Sexplib.Sexp.to_string

let t_of_string str =
  Sexplib.Sexp.of_string str |> t_of_sexp
