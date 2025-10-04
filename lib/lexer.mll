{
open Parser
open Lexing

exception SyntaxError of string

let trailing_spaces_not_allowed = "trailing spaces are not allowed"
let expected_single_space_after s found =
  if found = "" then Printf.sprintf "expected single space before %S" s else
  Printf.sprintf "expected single space after %S, found multiple" s
let bad_indent expected found =
  Printf.sprintf "bad indent %d, expected %d" found expected

let indent_width = 2
let indent_level = ref 0
let queued_tokens : token list ref = ref []

let add_indent_tokens ?(expect_indent=false) ?extra ws =
  let mklist extra n token =
    let lst = List.init n (fun _ -> token) in
    match extra with
    | Some t -> t :: lst
    | None -> lst
  in
  let len = String.length ws in
  match expect_indent with
  | true when len = !indent_level + indent_width ->
      let n_tokens = (len - !indent_level) / indent_width in
      queued_tokens := !queued_tokens @ mklist extra n_tokens INDENT;
      indent_level := len
  | false when len <= !indent_level && len mod 2 = 0 ->
      let n_tokens = (!indent_level - len) / indent_width in
      queued_tokens := !queued_tokens @ (mklist extra n_tokens DEDENT |> List.rev);
      indent_level := len
  | true -> raise (SyntaxError (bad_indent (!indent_level + indent_width) len))
  | false ->
      (* expected_indent:
            min(level, len) if len is even
            min(level, len-1) if n is odd
      *)
      let expected_indent = min !indent_level (len/2 * 2) in
      raise (SyntaxError (bad_indent expected_indent len))

let check_indentation indent_level ws =
  let len = String.length ws in
  if len <> indent_level then
    raise (SyntaxError (bad_indent indent_level len))
  else
    ()

let int_or_intlit_of_string s =
  match int_of_string_opt s with
  | Some i -> INT i
  | None -> INT_LIT s

let dedent ws =
  let len = String.length ws in
  if len >= !indent_level then
    String.sub ws !indent_level (len - !indent_level)
  else ws
}

let int = ('+'|'-')? ['0'-'9' '_']+
let float = "nan" | ('+'|'-')? "inf" | ('+'|'-')? int '.' int (('e'|'E') ('+'|'-')? int)?
let hex = ('+'|'-')? "0x" (['0'-'9' 'a'-'f' 'A'-'F' '_']+)
let octal = ('+'|'-')? "0o" (['0'-'7' '_']+)
let binary = ('+'|'-')? "0b" (['0'-'1' '_']+)

let ident = ['a'-'z' 'A'-'Z'] (['0'-'9' 'a'-'z' 'A'-'Z' '_' '-'])*

let whitespace = [' ' '\t' '\r']
let newline = '\n'

let escapable = '\\' | 'b' | 'f' | 'n' | 'r' | 't' | 'v'

let comment = whitespace* "# " [^ '\n']*

rule lex =
  parse    
  | "" {
      if !queued_tokens <> [] then
        let token = List.hd !queued_tokens in
        queued_tokens := List.tl !queued_tokens;
        token
      else lex_really lexbuf
  }
and expect_single_space symbol token =
  parse
  | ' ' { token }
  | ' '* as s { raise (SyntaxError (expected_single_space_after symbol s))}
and lex_newline expect_indent =
  parse
  | comment? newline {
      new_line lexbuf;
      lex_newline expect_indent lexbuf
  }
  | whitespace+ newline {
      raise (SyntaxError trailing_spaces_not_allowed)
  }
  | whitespace* {
      add_indent_tokens ~expect_indent ~extra:NEWLINE (lexeme lexbuf);
      lex lexbuf
  }
and lex_really =
  (* yes *)
  parse
  | comment { lex lexbuf }
  | newline { new_line lexbuf; lex_newline false lexbuf }
  | whitespace+ { raise (SyntaxError trailing_spaces_not_allowed) }
  | int { lexeme lexbuf |> int_or_intlit_of_string }
  | hex { lexeme lexbuf |> int_or_intlit_of_string }
  | octal { lexeme lexbuf |> int_or_intlit_of_string }
  | binary { lexeme lexbuf |> int_or_intlit_of_string }
  | float {
      let f = (lexeme lexbuf |> float_of_string) in
      let i = int_of_float f in
      if float_of_int i = f then INT i else FLOAT f
    }
  | "true" { BOOL true }
  | "false" { BOOL false }
  | "null" { NULL }
  | '"' { STRING (lex_string (Buffer.create 256) lexbuf) }
  | ':' { expect_single_space ":" SCALAR_START lexbuf }
  | "::" comment? newline {
        new_line lexbuf;
        queued_tokens := !queued_tokens @ [MULTILINE_VECTOR_START];
        lex_newline true lexbuf
    }
  | "::" { expect_single_space "::" INLINE_VECTOR_START lexbuf }
  | "```" { STRING (lex_start_multiline_string lex_start_triple_backtick_string lexbuf) }
  | "\"\"\"" { STRING (lex_start_multiline_string lex_start_triple_quote_string lexbuf) }
  | ident { IDENT (lexeme lexbuf) }
  | "[]" { LIST_EMPTY }
  | "{}" { DICT_EMPTY }
  | '-' { expect_single_space "-" DASH lexbuf }
  | ',' { expect_single_space "," COMMA lexbuf }
  | _ { raise (SyntaxError ("Unexpected character: " ^ lexeme lexbuf)) }
  | eof { EOF }
and lex_string buf =
  parse
  | '"' { Buffer.contents buf }
  | '\\' ('"' | '\\' as c) {Buffer.add_char buf c; lex_string buf lexbuf }
  | '\n' {raise (SyntaxError "unterminated string literal")}
  | _ as c {Buffer.add_char buf c; lex_string buf lexbuf }
and lex_start_multiline_string f =
  parse
  | comment? newline { indent_level := !indent_level + indent_width; f (Buffer.create 256) lexbuf }
  | whitespace+ { raise (SyntaxError trailing_spaces_not_allowed) }
  | [^ '\n'] { raise (SyntaxError "unexpected content at end of line") }
and lex_start_triple_backtick_string buf =
  parse
  | ' '* as ws {
      let s = dedent ws in
      Buffer.add_string buf s;
      lex_triple_backtick_string buf lexbuf
  }
and lex_start_triple_quote_string buf =
  parse
  | whitespace* { lex_triple_quote_string buf lexbuf }
and lex_triple_backtick_string buf =
  parse
  | '\\' ('`' | escapable as c) {Buffer.add_char buf c; lex_triple_backtick_string buf lexbuf }
  | newline (' '* as indentation) "```" {
        check_indentation (!indent_level - indent_width) indentation;
        new_line lexbuf;
        indent_level := !indent_level - indent_width;
        Buffer.contents buf
    }
  | newline (' '* as ws) {
        new_line lexbuf;
        let s = "\n" ^ (dedent ws) in
        Buffer.add_string buf s;
        lex_triple_backtick_string buf lexbuf
  }
  | _ as c {
        Buffer.add_char buf c;
        lex_triple_backtick_string buf lexbuf
    }
and lex_triple_quote_string buf =
  parse
  | '\\' ("\"\"\"" | "\\" as s) {
        Buffer.add_string buf s;
        lex_triple_quote_string buf lexbuf
    }
  | whitespace* newline (whitespace* as indentation) "\"\"\"" {
        check_indentation (!indent_level - indent_width) indentation;
        new_line lexbuf;
        indent_level := !indent_level - indent_width;
        Buffer.contents buf
    }
  | whitespace* newline whitespace* {
        new_line lexbuf;
        Buffer.add_char buf '\n';
        lex_triple_quote_string buf lexbuf
    }
  | _ as c {
        Buffer.add_char buf c;
        lex_triple_quote_string buf lexbuf
    }