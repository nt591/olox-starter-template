module Scanner = struct
  type token_type =
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR

    (* One or two character tokens *)
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL

    (* Literals *)
    | IDENTIFIER
    | STRING
    | NUMBER
    (* Keywords *)
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | EOF

  type literal_type = STRING_LITERAL of string | NUMBER_LITERAL of float | IDENTIFIER_LITERAL of string

  type token = {
    lexeme: string;
    literal: literal_type option;
    line: int;
    token_type: token_type;
  }

  type scanner_context = {
    source: string;
    start: int;
    current: int;
    line: int;
    tokens: token list;
  }

  let get_token_type token = token.token_type

  let is_at_end ctx = ctx.current >= (String.length ctx.source)

  (* runs in O(n) time *)
  let add_token token_type lexeme literal line ctx =
    {ctx with tokens = ctx.tokens @ [{
        token_type;
        lexeme;
        literal;
        line
      }]
    }

  let advance context = {context with current = context.current + 1}

  let current_char context =
    try Some (String.get context.source (context.current - 1)) with Invalid_argument _ -> None

  let next_char context =
    try Some (String.get context.source (context.current)) with Invalid_argument _ -> None

  let add_literal_token token_type literal_type context =
    let text = (String.sub context.source context.start (context.current - context.start)) in
    add_token token_type text literal_type context.line context

  let add_non_literal_token token_type context = add_literal_token token_type None context

  let match_character expected_char context = if (is_at_end context) then (false, context) else
    match next_char context with
      | Some c when c = expected_char -> (true, advance context)
      | _ -> (false, context)

  let add_conditional_non_literal_token expected_char token_if_true token_if_false context =
    match (match_character expected_char context) with
    | (true, new_context) -> add_non_literal_token token_if_true new_context
    | (false, new_context) -> add_non_literal_token token_if_false new_context

  let add_conditional_slash context =
    let rec consume_line context =
      match ((not (next_char context = Some '\n')), not (is_at_end context)) with
      | (true, true) -> consume_line (advance context)
      | _ -> context
    in
    match (match_character '/' context) with
    | (true, new_context) -> consume_line new_context
    | (false, new_context) -> add_non_literal_token SLASH new_context

  (* add string literals *)
  let peek_next context =
    try Some (String.get context.source (context.current + 1)) with Invalid_argument _ -> None

  let is_digit character = match character with
    | Some c when c >= '0' && c <= '9' -> true
    | _ -> false

  let rec capture_digits context =
    if is_digit (next_char context) then context |> advance |> capture_digits else context

  let capture_decimal context = match (next_char context) with
    | Some '.' when (is_digit (peek_next context)) -> context |> advance |> capture_digits
    | _ -> context

  let add_number_literal context =
    let new_context = context |> capture_digits |> capture_decimal in
    let stringified_number = (String.sub new_context.source new_context.start (new_context.current - new_context.start)) in
    let parsed_number = float_of_string stringified_number in
    add_literal_token NUMBER (Some (NUMBER_LITERAL parsed_number)) new_context


  (* Functions to add string literals *)
  let increment_line_if_newline context =
    match next_char context with
    | Some '\n' -> {context with line = context.line+1}
    | _ -> context

  let rec add_string_literal context =
     if( not ( next_char context = Some('"')) && (not (is_at_end context)) ) then
      context |> increment_line_if_newline |> advance |> add_string_literal
    else if (is_at_end context) then failwith "Unterminated string" else
    let str = (String.sub context.source (context.start + 1) (context.current - 1)) in
    add_literal_token STRING (Some (STRING_LITERAL str)) context

  (* add identifier literals *)

  let is_alpha character = match character with
  | Some c when c >= 'a' && c <='z' -> true
  | Some c when c >= 'A' && c <='Z' -> true
  | Some c when c = '_' -> true
  | _ -> false

  let is_alpha_numeric character = (is_alpha character) || (is_digit character)

  let rec add_identifier_literal context =
    if (is_alpha_numeric (next_char context)) then context |> advance |> add_identifier_literal
    else
    let substring = (String.sub context.source context.start (context.start - context.current)) in
    match substring with
    | "and" -> add_non_literal_token AND context
    | "class" -> add_non_literal_token CLASS context
    | "else" -> add_non_literal_token ELSE context
    | "false" -> add_non_literal_token FALSE context
    | "for" -> add_non_literal_token FOR context
    | "fun" -> add_non_literal_token FUN context
    | "if" -> add_non_literal_token IF context
    | "nil" -> add_non_literal_token NIL context
    | "or" -> add_non_literal_token OR context
    | "print" -> add_non_literal_token PRINT context
    | "return" -> add_non_literal_token RETURN context
    | "super" -> add_non_literal_token SUPER context
    | "this" -> add_non_literal_token THIS context
    | "true" -> add_non_literal_token TRUE context
    | "var" -> add_non_literal_token VAR context
    | "while" -> add_non_literal_token WHILE context
    | _ -> add_literal_token IDENTIFIER (Some (IDENTIFIER_LITERAL substring)) context

  let scan_token context =
    let advanced_context = advance context in
    match (current_char advanced_context) with
    | Some '(' -> add_non_literal_token LEFT_PAREN advanced_context
    | Some ')' -> add_non_literal_token RIGHT_PAREN advanced_context
    | Some '{' -> add_non_literal_token LEFT_BRACE advanced_context
    | Some '}' -> add_non_literal_token RIGHT_BRACE advanced_context
    | Some ',' -> add_non_literal_token COMMA advanced_context
    | Some '.' -> add_non_literal_token DOT advanced_context
    | Some '-' -> add_non_literal_token MINUS advanced_context
    | Some '+' -> add_non_literal_token PLUS advanced_context
    | Some ';' -> add_non_literal_token SEMICOLON advanced_context
    | Some '*' -> add_non_literal_token STAR advanced_context
    | Some '!' -> add_conditional_non_literal_token '=' BANG_EQUAL BANG advanced_context
    | Some '=' -> add_conditional_non_literal_token '=' EQUAL_EQUAL EQUAL advanced_context
    | Some '>' -> add_conditional_non_literal_token '=' GREATER_EQUAL GREATER advanced_context
    | Some '<' -> add_conditional_non_literal_token '=' LESS_EQUAL LESS advanced_context
    | Some ' ' -> advanced_context
    | Some '\r' -> advanced_context
    | Some '\t' -> advanced_context
    | Some '\n' -> {advanced_context with line = advanced_context.line + 1}
    | Some '"' -> add_string_literal advanced_context
    | digit when (is_digit digit) -> add_number_literal advanced_context
    | alpha when (is_alpha alpha) -> add_identifier_literal advanced_context
    | _ -> failwith "Disallowed character"

  let scan_tokens source =
    let ctx = {
      source;
      start = 0;
      current = 0;
      line = 1;
      tokens = [];
    } in
    let rec scan context = match is_at_end context with
    | false ->
      (* we are at the beginning of next lexeme *)
      {context with start = context.current} |> scan_token |> scan
    | true -> context |> (add_token EOF "" None context.line)
    in let get_tokens context = context.tokens
    in get_tokens (scan ctx)
end