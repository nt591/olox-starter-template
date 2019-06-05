open Scanner

module Expr = struct

  type literal =
    | LiteralInt of int
    | LiteralString of string
    | LiteralBool of bool

  type expr =
    | Binary of expr * Scanner.token * expr
    | Grouping of expr
    | Literal of literal option
    | Unary of Scanner.token * expr

end