open Types

type text_token
(** The type representing text tokens *)

val lex_text: string -> text_token list
(** [lex_text text] is a stream of tokens representing [text] *)

val parse_text_tokens: text_token list -> raw_expr * text_token list
(** [parse_text_tokens tokens] is the unresolved AST that [tokens] represents *)

val parse_text: string -> raw_expr
(** [parse_text text] is the unresolved AST that [text] represents *)