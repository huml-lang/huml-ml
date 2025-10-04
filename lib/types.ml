module Ast = struct
  type t =
    [ `String of string
    | `Float of float
    | `Int of int
    | `Bool of bool
    | `Null
    | `Assoc of (string * t) list
    | `List of t list ]

  let rec to_string = function
    | `String s -> Printf.sprintf "%S" s
    | `Float f -> string_of_float f
    | `Int i -> string_of_int i
    | `Bool b -> string_of_bool b
    | `Null -> "null"
    | `Assoc obj ->
        let items =
          List.map (fun (k, v) -> Printf.sprintf "%S: %s" k (to_string v)) obj
        in
        Printf.sprintf "{ %s }" (String.concat "; " items)
    | `List lst ->
        let items = List.map to_string lst in
        Printf.sprintf "[ %s ]" (String.concat "; " items)
end

exception ParseError of string * Lexing.position