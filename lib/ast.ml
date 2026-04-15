type literal = Number of float | String of string | Boolean of bool | Nil

let string_of_literal literal =
  match literal with
  | Number n ->
      let s = string_of_float n in
      if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
      else s
  | String s -> s
  | Boolean b -> string_of_bool b
  | Nil -> "nil"

type expr =
  | Assign of Token.t * expr
  | Binary of expr * Token.t * expr
  | Grouping of expr
  | Literal of literal
  | Unary of Token.t * expr
  | Variable of Token.t

let rec print_ast = function
  | Literal (Number n) -> string_of_float n
  | Literal (String s) -> s
  | Literal (Boolean b) -> string_of_bool b
  | Literal Nil -> "nil"
  | Grouping e -> "(group " ^ print_ast e ^ ")"
  | Unary (op, right) -> "(" ^ op.lexeme ^ " " ^ print_ast right ^ ")"
  | Binary (left, op, right) ->
      "(" ^ op.lexeme ^ " " ^ print_ast left ^ " " ^ print_ast right ^ ")"
  | Variable name -> name.lexeme
  | Assign (name, value) -> "(= " ^ name.lexeme ^ " " ^ print_ast value ^ ")"

type stmt =
  | Block of stmt list
  | Expression of expr
  | Print of expr
  | Var of Token.t * expr option
