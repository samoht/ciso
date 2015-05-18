type t = {
  id : int;
  addr : string * int;
  path : string
}

let path_of_t {path} = path
let id_of_t {id} = id
let addr_of_t {}

let create id addr path = {id; addr; path}
