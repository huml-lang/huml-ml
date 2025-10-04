open Lexing
open Types

let (let*) = Result.bind

let supported_huml_version = "v0.1.0"

let show_position pos =
  Printf.sprintf "%sline %d, column %d"
    (if pos.pos_fname = "" then "" else pos.pos_fname ^ ": ")
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let check_version v =
  if v <> "" && v <> supported_huml_version then
    let msg =
      Printf.sprintf
        "Unsupported HUML version %s. Supported version is %s.\n" v
        supported_huml_version
    in
    Error msg
  else Ok ()

let parse lexbuf =
  Lexer.init_state ();
  let version = Lexer.lex_version lexbuf in
  let* _ = check_version version in
  try match Parser.main Lexer.lex lexbuf with v -> Ok v with
  | Lexer.SyntaxError msg ->
      let msg' =
        Printf.sprintf "Syntax error at %s: %s\n"
          (show_position lexbuf.lex_start_p)
          msg
      in
      Error msg'
  | ParseError (msg, pos) ->
      let msg' =
        Printf.sprintf "Parse error at %s: %s\n" (show_position pos) msg
      in
      Error msg'
  | exn ->
      let msg =
        Printf.sprintf "Unexpected error at %s: %s\n"
          (show_position lexbuf.lex_start_p)
          (Printexc.to_string exn)
      in
      Error msg
