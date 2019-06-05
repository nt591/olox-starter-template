open Scanner

module Parser = struct

  type parser_context = {
    current: int;
    tokens: Scanner.token list
  }

  type token_type = Scanner.token_type

  let peek context = List.nth context.tokens context.current
  let previous context = List.nth context.tokens (context.current - 1)

  let is_at_end context =
    let token = peek context in
    Scanner.get_token_type token = Scanner.token_type

  let check token_type context = match is_at_end context with
  | true -> false
  | false ->
    let token = (peek context) in
    token.token_type == token_type

  let advance context = match is_at_end context with
    | false ->
      let new_ctx = {context with current = context.current + 1} in
      (previous new_ctx), new_ctx
    | true -> (previous context), context

  let rec match_types types context = match types with
  | [] -> false, context
  | x::xs -> if (check x)
    then (true, {context with current = context.current +1})
    else match_types xs context *)
  (* let equality context = match (match_types context [Scanner.token]) with
  |
  let expression = equality
  *)
end