open Alcotest

type test_input = String of string | InChannel of in_channel

type test_case = {
  name : string;
  input : test_input;
  error : bool;
  output : Yojson.Safe.t option;
}

let tests_dir = "../../../tests"

let rec yojson_of_huml huml : Yojson.Safe.t =
  match huml with
  | `String s -> `String s
  | `Int i -> `Int i
  | `Intlit s -> `Intlit s
  | `Float f -> `Float f
  | `Bool b -> `Bool b
  | `Null -> `Null
  | `Assoc assoc ->
      `Assoc (List.map (fun (k, v) -> (k, yojson_of_huml v)) assoc)
  | `List lst -> `List (List.map yojson_of_huml lst)

let string_of_yojson v =
  match v with
  | `String s -> Printf.sprintf "String: %S" s
  | `Int i -> Printf.sprintf "Int: %d" i
  | `Intlit i -> Printf.sprintf "Intlit: %s" i
  | `Float f -> Printf.sprintf "Float: %f" f
  | `Bool b -> Printf.sprintf "Bool: %b" b
  | `Null -> "Null"
  | `Assoc _ -> "[Object]"
  | `List _ -> "[Array]"
  | _ -> failwith "Unknown Yojson type"

let load_document_tests_from_dir ~check_output dir =
  let make_test_case ?json_filename name huml_filename =
    let huml_ic = open_in huml_filename in
    {
      name;
      input = InChannel huml_ic;
      error = false;
      output =
        (match json_filename with
        | Some json_file when check_output ->
            let ic = open_in json_file in
            Some (Yojson.Safe.from_channel ic)
        | _ -> None);
    }
  in
  let filenames = Sys.readdir dir |> Array.to_list in
  let huml_filenames =
    filenames
    |> List.filter (fun fname -> Filename.check_suffix fname ".huml")
    |> List.map (fun fname -> (Filename.chop_suffix fname ".huml", fname))
  in
  let json_filenames =
    filenames
    |> List.filter (fun fname -> Filename.check_suffix fname ".json")
    |> List.map (fun fname -> (Filename.chop_suffix fname ".json", fname))
  in
  let ( // ) = Filename.concat in
  let test_cases =
    huml_filenames
    |> List.map (fun (name, huml_fname) ->
           match List.assoc_opt name json_filenames with
           | None -> make_test_case name (dir // huml_fname)
           | Some json_fname ->
               make_test_case name (dir // huml_fname)
                 ~json_filename:(dir // json_fname))
  in
  test_cases

let load_tests_from_json ~check_output filename =
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
        input = String (test_json |> member "input" |> to_string);
        error = test_json |> member "error" |> to_bool;
        output =
          (if check_output then
             test_json |> member "output" |> to_option (fun j -> j)
           else None);
      })
    json_list

let get_deep_diffs a b =
  let rec aux acc path a' b' =
    match (a', b') with
    | `Assoc a_assoc, `Assoc b_assoc ->
        let a_keys = List.map fst a_assoc in
        let b_keys = List.map fst b_assoc in
        let all_keys = List.sort_uniq String.compare (a_keys @ b_keys) in
        List.fold_left
          (fun acc key ->
            let new_path = path ^ "->" ^ key in
            match (List.assoc_opt key a_assoc, List.assoc_opt key b_assoc) with
            | Some a_val, Some b_val -> aux acc new_path a_val b_val
            | Some _, None ->
                acc
                @ [
                    Printf.sprintf "Key %s missing in second JSON at %s" key
                      path;
                  ]
            | None, Some _ ->
                acc
                @ [
                    Printf.sprintf "Key %s missing in first JSON at %s" key path;
                  ]
            | None, None -> acc)
          acc all_keys
    | `List a_list, `List b_list ->
        let len = max (List.length a_list) (List.length b_list) in
        let rec loop i acc =
          if i >= len then acc
          else
            let new_path = Printf.sprintf "%s[%d]" path i in
            let a_val =
              if i < List.length a_list then Some (List.nth a_list i) else None
            in
            let b_val =
              if i < List.length b_list then Some (List.nth b_list i) else None
            in
            match (a_val, b_val) with
            | Some a_v, Some b_v -> loop (i + 1) (aux acc new_path a_v b_v)
            | Some _, None ->
                loop (i + 1)
                  (acc
                  @ [
                      Printf.sprintf "Index %d missing in second JSON at %s" i
                        path;
                    ])
            | None, Some _ ->
                loop (i + 1)
                  (acc
                  @ [
                      Printf.sprintf "Index %d missing in first JSON at %s" i
                        path;
                    ])
            | None, None -> loop (i + 1) acc
        in
        loop 0 acc
    | _ ->
        if a' <> b' then
          acc
          @ [
              Printf.sprintf "Value mismatch at %s: %s vs %s" path
                (string_of_yojson a') (string_of_yojson b');
            ]
        else acc
  in
  aux [] "root" a b

let test_huml_parsing test_case () =
  let lexbuf =
    match test_case.input with
    | String input -> Lexing.from_string input
    | InChannel ic -> Lexing.from_channel ic
  in
  let result = Huml.parse lexbuf in
  match (result, test_case.error) with
  | Ok _, true -> failf "Expected parsing to fail, but it succeeded"
  | Error _, false -> failf "Expected parsing to succeed, but it failed"
  | Ok result, false -> (
      match test_case.output with
      | Some expected_json ->
          let result_json = yojson_of_huml result in
          if not (Yojson.Safe.equal result_json expected_json) then
            let diffs = get_deep_diffs result_json expected_json in
            let msg = String.concat "\n" diffs in
            failf "Parsed output does not match expected output:\n%s" msg
      | None ->
          (* Test passed: parsing succeeded without errors*)
          ())
  | Error _, true ->
      (* Test passed: parsing failed as expected *)
      ()

let create_test_case test_case =
  (test_case.name, `Quick, test_huml_parsing test_case)

let create_test_suite test_cases = List.map create_test_case test_cases

let () =
  let ( // ) = Filename.concat in
  let check_output = Sys.word_size = 64 in
  let assertions = tests_dir // "assertions" // "mixed.json" in
  let assertion_tests =
    load_tests_from_json ~check_output assertions |> create_test_suite
  in
  let documents_dir = tests_dir // "documents" in
  let document_tests =
    load_document_tests_from_dir ~check_output documents_dir
    |> create_test_suite
  in
  run "parsing"
    [ ("assertions", assertion_tests); ("documents", document_tests) ]
