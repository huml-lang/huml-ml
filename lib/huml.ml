open Lexing
open Types

type t = Ast.t

module I = Parser.MenhirInterpreter

let supported_spec_version = "v0.2.0"

let show_position pos =
  Printf.sprintf "%sline %d, column %d"
    (if pos.pos_fname = "" then "" else pos.pos_fname ^ ": ")
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let check_version v =
  match v with
  | Some s when s <> supported_spec_version ->
      let msg =
        Printf.sprintf "Unsupported HUML version %s. Supported version is %s.\n" s
          supported_spec_version
      in
      Error msg
  | _ -> Ok ()

let rec loop state lexbuf (checkpoint : Ast.t I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token, state' = Lexer.lex lexbuf state in
      let startp = lexbuf.lex_start_p
      and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
      loop state' lexbuf checkpoint
  | I.Shifting _ | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
      loop state lexbuf checkpoint
  | I.HandlingError _env ->
      raise (ParseError ("Unexpected error", lexbuf.lex_start_p))
  | I.Accepted v -> v
  | I.Rejected -> raise (ParseError ("Parser rejected input", lexbuf.lex_start_p))

let parse lexbuf =
  let ( let* ) = Result.bind in
  let version = Lexer.lex_version lexbuf in
  let* _ = check_version version in
  try
    let v = loop Lexer.initial_state lexbuf (Parser.Incremental.main lexbuf.lex_curr_p) in
    Ok v
  with
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
