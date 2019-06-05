module Scanner : sig
  type token_type

  type literal_type

  type token

  type scanner_context

  val scan_tokens: string -> token list

  val is_at_end: scanner_context -> bool

  val add_token: token_type -> string -> literal_type option -> int -> scanner_context -> scanner_context

  val advance: scanner_context -> scanner_context

  val current_char: scanner_context -> char option

  val add_literal_token: token_type -> literal_type option -> scanner_context -> scanner_context

  val add_non_literal_token: token_type -> scanner_context -> scanner_context

  val scan_token: scanner_context -> scanner_context

  val next_char: scanner_context -> char option

  val match_character: char -> scanner_context -> bool * scanner_context

  val add_conditional_non_literal_token: char -> token_type -> token_type -> scanner_context -> scanner_context

  val add_conditional_slash: scanner_context -> scanner_context

  val increment_line_if_newline: scanner_context -> scanner_context

  val add_string_literal: scanner_context -> scanner_context

  val is_digit: char option -> bool

  val add_number_literal: scanner_context -> scanner_context

  val peek_next: scanner_context -> char option

  val capture_digits: scanner_context -> scanner_context

  val capture_decimal: scanner_context -> scanner_context

  val is_alpha: char option -> bool

  val is_alpha_numeric: char option -> bool

  val add_identifier_literal: scanner_context -> scanner_context
end