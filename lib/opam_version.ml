type t = [`V2_0 | `V2_1] [@@deriving ord,yojson, eq]

let to_string = function `V2_0 -> "2.0" | `V2_1 ->  "2.1"
let to_string_with_minor t = to_string t ^ ".0"

let pp = Fmt.of_to_string to_string

let default = `V2_1

let of_string str = match String.sub str 0 3 with
  | "2.0" -> Ok `V2_0
  | "2.1" -> Ok `V2_1
  | _ | exception Invalid_argument _ -> Error (`Msg "invalid opam version")
