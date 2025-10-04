open Lexing
open Types

let show_token t =
  let open Parser in
  match t with
  | INT i -> Printf.sprintf "INT(%d)" i
  | FLOAT f -> Printf.sprintf "FLOAT(%f)" f
  | STRING s -> Printf.sprintf "STRING(%S)" s
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | BOOL b -> Printf.sprintf "BOOL(%b)" b
  | NULL -> "NULL"
  | SCALAR_START -> "SCALAR_START"
  | INLINE_VECTOR_START -> "INLINE_VECTOR_START"
  | MULTILINE_VECTOR_START -> "MULTILINE_VECTOR_START"
  | COMMA -> "COMMA"
  | DASH -> "DASH"
  | NEWLINE -> "NEWLINE"
  | LIST_EMPTY -> "[]"
  | DICT_EMPTY -> "{}"
  | INDENT -> "INDENT"
  | DEDENT -> "DEDENT"
  | EOF -> "EOF"

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
        let msg' = Printf.sprintf "%s: %s\n" (show_position lexbuf.lex_start_p) msg in
        Error (msg')
    | ParseError (msg, pos) ->
        let msg' = Printf.sprintf "Parse error at %s: %s\n" (show_position pos) msg in
        Error (msg')
    | _ ->
        let msg = Printf.sprintf "Unexpected token at %s\n" (show_position lexbuf.lex_start_p) in
        Error (msg)