let version = "0.1.0"

let usage_msg =
  "huml [OPTIONS] INPUT_FILE\n\n" ^ "Parse HUML files and output JSON.\n\n"
  ^ "Examples:\n" ^ "  huml input.huml\n" ^ "  huml input.huml -o output.json\n"

let help_msg =
  usage_msg ^ "\nOptions:\n"
  ^ "  -o, --output FILE   Output file (default: stdout)\n"
  ^ "  -h, --help          Show this help\n"
  ^ "  -v, --version       Show version\n"

let error_and_exit msg code =
  Printf.eprintf "Error: %s\n" msg;
  exit code

let parse_args () =
  let input_file = ref None in
  let output_file = ref None in

  let rec parse_args_rec = function
    | [] -> ()
    | "-h" :: _ | "--help" :: _ ->
        print_string help_msg;
        exit 0
    | "-v" :: _ | "--version" :: _ ->
        Printf.printf "huml %s\n" version;
        exit 0
    | "-o" :: file :: rest | "--output" :: file :: rest ->
        output_file := Some file;
        parse_args_rec rest
    | file :: rest when not (String.starts_with ~prefix:"-" file) ->
        input_file := Some file;
        parse_args_rec rest
    | unknown :: _ -> error_and_exit ("Unknown option: " ^ unknown) 1
  in

  parse_args_rec (List.tl (Array.to_list Sys.argv));

  match !input_file with
  | None ->
      Printf.eprintf "%s" help_msg;
      exit 1
  | Some file -> (file, !output_file)

let () =
  let input_file, output_file = parse_args () in

  (* Read input file *)
  let ic =
    try open_in input_file with Sys_error msg -> error_and_exit msg 2
  in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_file };

  (* Parse the file *)
  let result = Huml.parse lexbuf in
  close_in ic;

  match result with
  | Error err -> error_and_exit ("Parse error: " ^ err) 3
  | Ok ast -> (
      (* Convert to Yojson and format as JSON *)
      let json = (ast :> Yojson.Safe.t) in
      let json_string = Yojson.Safe.pretty_to_string json in

      (* Output to stdout or file *)
      match output_file with
      | None -> print_endline json_string
      | Some path ->
          let oc =
            try open_out path with Sys_error msg -> error_and_exit msg 2
          in
          output_string oc json_string;
          output_char oc '\n';
          close_out oc)
