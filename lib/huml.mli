type t = Types.Ast.t

val supported_spec_version : string

val parse : Lexing.lexbuf -> (t, string) result
