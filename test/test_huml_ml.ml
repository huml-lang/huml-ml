open Alcotest

type test_case = { name : string; input : string; error : bool }

let load_tests_from_json filename =
  let ic = open_in filename in
  let json_str = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let json = Yojson.Safe.from_string json_str in
  let json_list = Yojson.Safe.Util.to_list json in
  List.map
    (fun test_json ->
      let open Yojson.Safe.Util in
      {
        name = test_json |> member "name" |> to_string;
        input = test_json |> member "input" |> to_string;
        error = test_json |> member "error" |> to_bool;
      })
    json_list

let test_huml_parsing test_case () =
  let lexbuf = Lexing.from_string test_case.input in
  let result = Huml.parse lexbuf in
  match (result, test_case.error) with
  | Ok _, true -> failf "Expected parsing to fail, but it succeeded"
  | Error _, false -> failf "Expected parsing to succeed, but it failed"
  | Ok _, false ->
      (* Test passed: parsing succeeded as expected *)
      ()
  | Error _, true ->
      (* Test passed: parsing failed as expected *)
      ()

let create_test_case test_case =
  (test_case.name, `Quick, test_huml_parsing test_case)

let create_test_suite test_cases = List.map create_test_case test_cases

let () =
  let test_cases = load_tests_from_json "../../../test/tests.json" in
  let test_suite = create_test_suite test_cases in
  run "HUML Parser Tests" [ ("parsing", test_suite) ]

