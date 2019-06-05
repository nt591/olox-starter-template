module Parser : sig
  type parser_context
  type expr
  type token_type
  type token

  val equality: parser_context -> expr * parser_context

  val match_types: token_type list -> parser_context -> bool * parser_context
  val check: token_type -> parser_context -> bool
  val advance: parser_context -> token * parser_context
  val is_at_end: parser_context -> bool
  val peek: parser_context -> token
  val previous: parser_context -> token
end