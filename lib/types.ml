type literal =
  | Number of float
  | String of string
  | Boolean of bool
  | Nil
  | Callable of lox_callable

and lox_callable = {
  arity : int;
  repr : string;
  call : interpreter_state -> literal list -> literal;
}

and environment = {
  values : (string, literal) Hashtbl.t;
  enclosing : environment option;
}

and interpreter_state = { globals : environment; mutable env : environment }

type expr =
  | Assign of Token.t * expr
  | Binary of expr * Token.t * expr
  | Call of expr * Token.t * expr list
  | Grouping of expr
  | Literal of literal
  | Logical of expr * Token.t * expr
  | Unary of Token.t * expr
  | Variable of Token.t

let string_of_literal literal =
  match literal with
  | Number n ->
      let s = string_of_float n in
      if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
      else s
  | String s -> s
  | Boolean b -> string_of_bool b
  | Nil -> "nil"
  | Callable { repr; _ } -> repr

type stmt =
  | Block of stmt list
  | Expression of expr
  | Function of { name : Token.t; params : Token.t list; body : stmt list }
  | If of expr * stmt * stmt option
  | Print of expr
  | Return of Token.t * expr
  | Var of Token.t * expr option
  | While of expr * stmt

(* let rec print_ast = function *)
(* | Literal (Number n) -> string_of_float n *)
(* | Literal (String s) -> s *)
(* | Literal (Boolean b) -> string_of_bool b *)
(* | Literal Nil -> "nil" *)
(* | Grouping e -> "(group " ^ print_ast e ^ ")" *)
(* | Unary (op, right) -> "(" ^ op.lexeme ^ " " ^ print_ast right ^ ")" *)
(* | Binary (left, op, right) -> *)
(* "(" ^ op.lexeme ^ " " ^ print_ast left ^ " " ^ print_ast right ^ ")" *)
(* | Variable name -> name.lexeme *)
(* | Assign (name, value) -> "(= " ^ name.lexeme ^ " " ^ print_ast value ^ ")" *)
