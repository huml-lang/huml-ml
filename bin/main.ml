let version = "0.1.0"

let usage_msg =
  "huml [OPTIONS] INPUT_FILE\n\n" ^ "Parse HUML files and output JSON.\n\n"
  ^ "Examples:\n" ^ "  huml input.huml\n" ^ "  huml input.huml -o output.json\n"
  ^ "  huml input.huml --lex\n"

let help_msg =
  usage_msg ^ "\nOptions:\n"
  ^ "  -o, --output FILE   Output file (default: stdout)\n"
  ^ "      --lex           Only output lexed tokens, don't parse\n"
  ^ "  -h, --help          Show this help\n"
  ^ "  -v, --version       Show version\n"

let error_and_exit msg code =
  Printf.eprintf "Error: %s\n" msg;
  exit code

let token_to_string = function
  | Huml__Parser.STRING s -> Printf.sprintf "STRING(%S)" s
  | FLOAT f -> Printf.sprintf "FLOAT(%g)" f
  | INT i -> Printf.sprintf "INT(%d)" i
  | INT_LIT s -> Printf.sprintf "INT_LIT(%S)" s
  | BOOL b -> Printf.sprintf "BOOL(%b)" b
  | IDENT s -> Printf.sprintf "IDENT(%S)" s
  | NEWLINE -> "NEWLINE"
  | NULL -> "NULL"
  | LIST_EMPTY -> "LIST_EMPTY"
  | DICT_EMPTY -> "DICT_EMPTY"
  | COMMA -> "COMMA"
  | DASH -> "DASH"
  | INDENT -> "INDENT"
  | DEDENT -> "DEDENT"
  | SCALAR_START -> "SCALAR_START"
  | INLINE_VECTOR_START -> "INLINE_VECTOR_START"
  | MULTILINE_VECTOR_START -> "MULTILINE_VECTOR_START"
  | EOF -> "EOF"

let lex_only lexbuf output_file =
  let tokens = ref [] in
  let rec collect_tokens () =
    let token = Huml__Lexer.lex lexbuf in
    tokens := token :: !tokens;
    if token <> Huml__Parser.EOF then collect_tokens ()
  in
  collect_tokens ();
  let tokens = List.rev !tokens in

  (* Group tokens by lines, splitting on NEWLINE but including NEWLINE in output *)
  let rec group_by_lines acc current_line = function
    | [] ->
        if current_line = [] then List.rev acc
        else List.rev (List.rev current_line :: acc)
    | Huml__Parser.NEWLINE :: rest ->
        let line = List.rev (Huml__Parser.NEWLINE :: current_line) in
        group_by_lines (line :: acc) [] rest
    | token :: rest -> group_by_lines acc (token :: current_line) rest
  in

  let lines = group_by_lines [] [] tokens in
  let format_line tokens =
    let token_strings = List.map token_to_string tokens in
    String.concat ", " token_strings
  in
  let output_lines = List.map format_line lines in
  let output = String.concat "\n" output_lines in

  match output_file with
  | None -> print_endline output
  | Some path ->
      let oc = try open_out path with Sys_error msg -> error_and_exit msg 2 in
      output_string oc output;
      output_char oc '\n';
      close_out oc

let parse_args () =
  let input_file = ref None in
  let output_file = ref None in
  let lex_only_flag = ref false in

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
    | "--lex" :: rest ->
        lex_only_flag := true;
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
  | Some file -> (file, !output_file, !lex_only_flag)

let () =
  let input_file, output_file, lex_only_flag = parse_args () in

  (* Read input file *)
  let ic =
    try open_in input_file with Sys_error msg -> error_and_exit msg 2
  in
  let lexbuf = Lexing.from_channel ic in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_file };

  if lex_only_flag then (
    lex_only lexbuf output_file;
    close_in ic)
  else
    (* Parse the file *)
    let result = Huml.parse lexbuf in
    close_in ic;

    match result with
    | Error err -> error_and_exit ("Parse error: " ^ err) 3
    | Ok ast -> (
        (* Convert to Yojson and format as JSON *)
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
