open Js_of_ocaml

let parse_string s =
  let result = Js.to_string s |> Lexing.from_string |> Huml.parse in
  let output, error =
    match result with
    | Ok ast ->
        let rec convert_ast = function
          | `String s -> `String s
          | `Float f -> `Float f
          | `Int i -> `Int i
          | `Intlit s -> `Intlit s (* Convert large int literals to strings *)
          | `Bool b -> `Bool b
          | `Null -> `Null
          | `Assoc lst ->
              `Assoc (List.map (fun (k, v) -> (k, convert_ast v)) lst)
          | `List lst -> `List (List.map convert_ast lst)
        in
        let json = convert_ast ast in
        let json_string = Yojson.Safe.pretty_to_string json in
        (Js.string json_string, Js.string "")
    | Error err -> (Js.string "", Js.string err)
  in
  object%js
    val output = output
    val error = error
  end

let _ =
  Js.export "huml"
    (object%js
       method parse s = parse_string s
    end)
