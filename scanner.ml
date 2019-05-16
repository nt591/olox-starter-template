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

  type literal_type = STRING_LITERAL of string | NUMBER_LITERAL of float

  type token = Token of {
    lexeme: string;
    literal: literal_type option;
    line: int;
    token_type: token_type;
  }
end