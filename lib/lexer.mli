exception SyntaxError of string

module State : sig
  type state = { indent_level: int ; queued_tokens: Parser.token list }
  type 'a t = state -> 'a * state

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val get : state t
  val set : state -> unit t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

val lex_version : Lexing.lexbuf -> string option
val lex : Lexing.lexbuf -> Parser.token State.t
val initial_state : State.state
