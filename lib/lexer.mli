exception SyntaxError of string

val lex_version : Lexing.lexbuf -> string
val lex : Lexing.lexbuf -> Parser.token
val init_state : unit -> unit