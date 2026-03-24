type literal = Number of float | String of string | Boolean of bool | Nil

type expr =
  | Binary of expr * Token.t * expr
  | Grouping of expr
  | Literal of literal
  | Unary of Token.t * expr

let rec print_ast = function
  | Literal (Number n) -> string_of_float n
  | Literal (String s) -> s
  | Literal (Boolean b) -> string_of_bool b
  | Literal Nil -> "nil"
  | Grouping e -> "(group " ^ print_ast e ^ ")"
  | Unary (op, right) -> "(" ^ op.lexeme ^ " " ^ print_ast right ^ ")"
  | Binary (left, op, right) ->
      "(" ^ op.lexeme ^ " " ^ print_ast left ^ " " ^ print_ast right ^ ")"
