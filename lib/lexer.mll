{
open Parser
open Lexing

(* State Monad for indent level & queued tokens *)
module State = struct
  type state = { indent_level: int ; queued_tokens: token list }
  type 'a t = state -> 'a * state

  let return v = fun s -> (v, s)
  let bind m f =
    fun s ->
      let (v, s') = m s in
      (f v) s'

  let get = fun s -> (s, s)
  let set s' = fun _ -> ((), s')

  let ( let* ) = bind
end

open State

exception SyntaxError of string

let trailing_spaces_not_allowed = "trailing spaces are not allowed"
let expected_single_space_after s found =
  if found = "" then Printf.sprintf "expected single space after %S" s else
  Printf.sprintf "expected single space after %S, found multiple" s
let bad_indent expected found =
  Printf.sprintf "bad indent %d, expected %d" found expected

let indent_width = 2
let initial_state = { indent_level = 0; queued_tokens = [] }

let add_indent_tokens ?(expect_indent=false) ?extra ws =
  let mklist extra n token =
    let lst = List.init n (fun _ -> token) in
    match extra with
    | Some t -> t :: lst
    | None -> lst
  in
  let len = String.length ws in
  let* s = State.get in
  match expect_indent with
  | true when len = s.indent_level + indent_width ->
      let n_tokens = (len - s.indent_level) / indent_width in
      let queued_tokens = s.queued_tokens @ mklist extra n_tokens INDENT in
      let* () = State.set { queued_tokens; indent_level = len } in
      State.return ()
  | false when len <= s.indent_level && len mod 2 = 0 ->
      let n_tokens = (s.indent_level - len) / indent_width in
      let queued_tokens = s.queued_tokens @ (mklist extra n_tokens DEDENT |> List.rev) in
      let* () = State.set { queued_tokens; indent_level = len } in
      State.return ()
  | true -> raise (SyntaxError (bad_indent (s.indent_level + indent_width) len))
  | false ->
      (* expected_indent:
            min(level, len) if len is even
            min(level, len-1) if n is odd
      *)
      let expected_indent = min s.indent_level (len/2 * 2) in
      raise (SyntaxError (bad_indent expected_indent len))

let check_indentation indent_level ws =
  let len = String.length ws in
  if len <> indent_level then
    raise (SyntaxError (bad_indent indent_level len))
  else
    ()

let remove_underscores s =
  String.concat "" (String.split_on_char '_' s)

let int_or_intlit_of_string s =
  match int_of_string_opt s with
  | Some i -> INT i
  | None -> INT_LIT (s |> remove_underscores)

let dedent ws =
  let* s = State.get in
  let len = String.length ws in
  if len >= s.indent_level then
    State.return (String.sub ws s.indent_level (len - s.indent_level))
  else State.return ws
}

let int = ('+'|'-')? ['0'-'9' '_']+
let float = ('+'|'-') "inf" | int '.' ['0'-'9']+ (('e'|'E') int)? | int (('e'|'E') int)
let hex = ('+'|'-')? "0x" (['0'-'9' 'a'-'f' 'A'-'F' '_']+)
let octal = ('+'|'-')? "0o" (['0'-'7' '_']+)
let binary = ('+'|'-')? "0b" (['0'-'1' '_']+)

let ident = ['a'-'z' 'A'-'Z'] (['0'-'9' 'a'-'z' 'A'-'Z' '_' '-'])*

let whitespace = [' ' '\t' '\r']
let newline = '\n'

let comment = whitespace* "# " [^ '\n']*

(* val lex : Lexing.lexbuf -> token State.t *)
rule lex =
  parse
  | "" {
      let* st = State.get in
      match st.queued_tokens with
      | [] -> let* token = lex_really lexbuf in return token
      | token :: rest ->
          let* () = State.set { st with queued_tokens = rest } in
          return token
  }
(* val lex_version : Lexing.lexbuf -> string option *)
and lex_version = parse
  | "%HUML " ('v'? ['0'-'9']+ ('.' ['0'-'9']+)* as version) { Some version }
  | "" { None }
(* val lex_really : Lexing.lexbuf -> token State.t *)
and lex_really =
  (* yes, really *)
  parse
  | comment { lex lexbuf }
  | newline { new_line lexbuf; lex_newline false lexbuf }
  | whitespace+ { raise (SyntaxError trailing_spaces_not_allowed) }
  | int { lexeme lexbuf |> int_or_intlit_of_string |> return }
  | hex { lexeme lexbuf |> int_or_intlit_of_string |> return }
  | octal { lexeme lexbuf |> int_or_intlit_of_string |> return }
  | binary { lexeme lexbuf |> int_or_intlit_of_string |> return }
  | float {
      let f = (lexeme lexbuf |> float_of_string) in
      let i = int_of_float f in
      let token = if float_of_int i = f then INT i else FLOAT f in
      return token
    }
  | '"' { return (STRING (lex_string (Buffer.create 256) lexbuf)) }
  | ':' { expect_single_space ":" SCALAR_START lexbuf }
  | "::" comment? newline {
        new_line lexbuf;
        let* st = State.get in
        let queued_tokens = st.queued_tokens @ [MULTILINE_VECTOR_START] in
        let* () = State.set { st with queued_tokens } in
        lex_newline true lexbuf
    }
  | "::" { expect_single_space "::" INLINE_VECTOR_START lexbuf }
  | "\"\"\"" {
      let* s = lex_start_multiline_string lexbuf in
      return (STRING s)
  }
  | ident { return (IDENT (lexeme lexbuf)) }
  | "[]" { return LIST_EMPTY }
  | "{}" { return DICT_EMPTY }
  | '-' { expect_single_space "-" DASH lexbuf }
  | ',' { expect_single_space "," COMMA lexbuf }
  | _ { raise (SyntaxError ("Unexpected character: " ^ lexeme lexbuf)) }
  | eof { let* () = add_indent_tokens ~extra:EOF "" in lex lexbuf }
(* val lex_string : char Buffer.t -> Lexing.lexbuf -> string *)
and lex_string buf =
  parse
  | '"' { Buffer.contents buf }
  | '\\' ('"' | '\\' | '/' as c) {Buffer.add_char buf c; lex_string buf lexbuf }
  | '\\' 'n' {Buffer.add_char buf '\n'; lex_string buf lexbuf }
  | '\\' 't' {Buffer.add_char buf '\t'; lex_string buf lexbuf }
  | '\\' 'r' {Buffer.add_char buf '\r'; lex_string buf lexbuf }
  | '\\' 'b' {Buffer.add_char buf '\b'; lex_string buf lexbuf }
  | '\\' 'f' {Buffer.add_char buf '\012'; lex_string buf lexbuf }
  | '\\' 'v' {Buffer.add_char buf '\011'; lex_string buf lexbuf }
  | ('\\' _ as s) { raise (SyntaxError (Printf.sprintf "invalid escape sequence %S" s)) }
  | '\n' {raise (SyntaxError "unterminated string literal")}
  | _ as c {Buffer.add_char buf c; lex_string buf lexbuf }
(* val lex_multiline_string : Lexing.lexbuf -> string State.t *)
and lex_start_multiline_string =
  parse
  | comment? newline {
      new_line lexbuf;
      let* st = State.get in
      let* () = State.set { st with indent_level = st.indent_level + indent_width } in
      lex_multiline_string_indent true (Buffer.create 256) lexbuf
    }
  | whitespace+ { raise (SyntaxError trailing_spaces_not_allowed) }
  | [^ '\n'] { raise (SyntaxError "unexpected content at end of line") }
(* val lex_multiline_string_indent : bool -> char Buffer.t -> Lexing.lexbuf -> string State.t *)
and lex_multiline_string_indent is_first_line buf =
  parse
  | (' '* as ws) "\"\"\"" {
      let* st = State.get in
      check_indentation (st.indent_level - indent_width) ws;
      let* () = State.set { st with indent_level = st.indent_level - indent_width } in
      return (Buffer.contents buf)
  }
  | (' '* as ws) {
      if (not is_first_line) then Buffer.add_char buf '\n';
      let* ws' = dedent ws in
      Buffer.add_string buf ws';
      lex_multiline_string buf lexbuf
    }
(* val lex_multiline_string : char Buffer.t -> Lexing.lexbuf -> string State.t *)
and lex_multiline_string buf =
  parse
  | ([^ '\n']* as s) newline {
      new_line lexbuf;
      Buffer.add_string buf s;
      lex_multiline_string_indent false buf lexbuf
    }
  | _ { raise (SyntaxError ("Unexpected character " ^ lexeme lexbuf)) }
(* val expect_single_space : string -> token -> Lexing.lexbuf -> token State.t *)
and expect_single_space symbol token =
  parse
  | ' ' { return token }
  | ' '* as s { raise (SyntaxError (expected_single_space_after symbol s))}
(* val lex_newline : bool -> Lexing.lexbuf -> token State.t *)
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
      let* () = add_indent_tokens ~expect_indent ~extra:NEWLINE (lexeme lexbuf) in
      lex lexbuf
    }
