type literal =
  | Number of float
  | String of string
  | Boolean of bool
  | Nil
  | Callable of lox_callable
  | LoxClass of lox_class
  | LoxInstance of lox_instance

and lox_callable = {
  arity : int;
  repr : string;
  closure : environment;
  call : interpreter_state -> literal list -> literal;
  params : Token.t list option;
  body : stmt list option;
  is_initializer : bool;
}

and environment = {
  values : (string, literal) Hashtbl.t;
  enclosing : environment option;
}

and interpreter_state = {
  globals : environment;
  mutable env : environment;
  locals : (expr * int) list ref;
}

and function_type = Type_None | Type_Function | Type_Method | Type_Init
and class_type = No_Class | In_Class | In_Subclass

and resolver_state = {
  interpreter : interpreter_state;
  scopes : (string, bool) Hashtbl.t list;
  current_function : function_type;
  current_class : class_type;
}

and expr =
  | Assign of Token.t * expr
  | Binary of expr * Token.t * expr
  | Call of expr * Token.t * expr list
  | Get of expr * Token.t
  | Grouping of expr
  | Literal of literal
  | Logical of expr * Token.t * expr
  | Set of { obj : expr; name : Token.t; value : expr }
  | Super of { keyword : Token.t; method_ : Token.t }
  | This of Token.t
  | Unary of Token.t * expr
  | Variable of Token.t

and lox_class = {
  name : string;
  repr : string;
  methods : (string, lox_callable) Hashtbl.t;
  superclass : lox_class option;
}

and lox_instance = { klass : lox_class; fields : (string, literal) Hashtbl.t }

and stmt =
  | Block of stmt list
  | Class of { name : Token.t; superclass : expr option; methods : stmt list }
  | Expression of expr
  | Function of { name : Token.t; params : Token.t list; body : stmt list }
  | If of expr * stmt * stmt option
  | Print of expr
  | Return of Token.t * expr
  | Var of Token.t * expr option
  | While of expr * stmt

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
  | LoxClass { repr; _ } -> repr
  | LoxInstance { klass; _ } -> klass.repr ^ " instance "

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
