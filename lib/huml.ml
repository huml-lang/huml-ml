open Lexing
open Types

let show_position pos =
  Printf.sprintf "%sline %d, column %d"
    (if pos.pos_fname = "" then "" else pos.pos_fname ^ ": ")
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse lexbuf =
  try
    match Parser.main Lexer.lex lexbuf with
    | v -> Ok v
  with
    | Lexer.SyntaxError msg ->
        let msg' = Printf.sprintf "Syntax error at %s: %s\n" (show_position lexbuf.lex_start_p) msg in
        Error (msg')
    | ParseError (msg, pos) ->
        let msg' = Printf.sprintf "Parse error at %s: %s\n" (show_position pos) msg in
        Error (msg')
    | exn ->
        let msg = Printf.sprintf "Unexpected error at %s: %s\n"
          (show_position lexbuf.lex_start_p)
          (Printexc.to_string exn)
        in
        Error (msg)