type t = Types.Ast.t

val parse : Lexing.lexbuf -> (t, string) result
