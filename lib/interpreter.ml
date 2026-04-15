open Errors

type interpreter_state = { mutable env : Environment.t }

let is_truthy = function Ast.Nil | Ast.Boolean false -> false | _ -> true

let is_equal a b =
  match (a, b) with
  | Ast.Nil, Ast.Nil -> true
  | Ast.Number x, Ast.Number y -> x = y
  | Ast.String x, Ast.String y -> String.equal x y
  | Ast.Boolean x, Ast.Boolean y -> x = y
  | _ -> false

let unwrap_number op literal =
  match literal with
  | Ast.Number n -> n
  | _ -> raise @@ RuntimeError (op, "Operand must be a number.")

let rec evaluate s expr =
  match expr with
  | Ast.Literal v -> v
  | Ast.Grouping group -> evaluate s group
  | Ast.Unary (op, expr) -> evaluate_unary op (evaluate s expr)
  | Ast.Binary (left, op, right) ->
      evaluate_binary op (evaluate s left) (evaluate s right)
  | Ast.Variable expr -> Environment.get s.env expr
  | Ast.Assign (name, value) ->
      let value = evaluate s value in
      Environment.assign s.env name value;

      value

and evaluate_unary op expr =
  match op.kind with
  | Token.MINUS -> begin
      let n = unwrap_number op expr in
      Ast.Number (-.n)
    end
  | Token.BANG -> Ast.Boolean (not @@ is_truthy expr)
  | _ -> Ast.Nil

and evaluate_binary op left right =
  match op.kind with
  | Token.MINUS ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Ast.Number (left -. right)
  | Token.SLASH ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Ast.Number (left /. right)
  | Token.STAR ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Ast.Number (left *. right)
  | Token.PLUS -> begin
      match (left, right) with
      | Ast.Number x, Ast.Number y -> Ast.Number (x +. y)
      | Ast.String s1, Ast.String s2 -> Ast.String (s1 ^ s2)
      | _ ->
          raise
          @@ RuntimeError (op, "Operands must be two numbers or two strings.")
    end
  | Token.GREATER ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Ast.Boolean (left > right)
  | Token.GREATER_EQUAL ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Ast.Boolean (left >= right)
  | Token.LESS ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Ast.Boolean (left < right)
  | Token.LESS_EQUAL ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Ast.Boolean (left <= right)
  | Token.BANG_EQUAL -> Ast.Boolean (not @@ is_equal left right)
  | Token.EQUAL_EQUAL -> Ast.Boolean (is_equal left right)
  | _ -> failwith "Unreachable"

(* let interpret expression = *)
(* try *)
(* begin *)
(* let value = evaluate expression in *)
(* print_endline @@ Ast.string_of_literal value; *)
(* false *)
(* end *)
(* with RuntimeError (t, e) -> *)
(* print_endline (e ^ "\n[line " ^ string_of_int t.line ^ "]"); *)
(* true *)

let rec execute s (stmt : Ast.stmt) =
  match stmt with
  | Ast.Block stmts ->
      let local_env = Environment.create_with_enclosing s.env in
      execute_block s stmts local_env
  | Ast.Expression e -> ignore @@ evaluate s e
  | Ast.Print e ->
      let value = evaluate s e in
      print_endline @@ Ast.string_of_literal value
  | Ast.Var (key, value) ->
      let v = match value with Some v -> evaluate s v | _ -> Ast.Nil in
      Environment.define s.env key.lexeme v

and execute_block s stmts new_env =
  let previous = s.env in
  try
    s.env <- new_env;
    List.iter (execute s) stmts;
    s.env <- previous
  with _ -> s.env <- previous

let interpret state statements =
  let rec interpret_h stmts had_err =
    match stmts with
    | [] -> had_err
    | h :: t -> (
        try
          begin
            execute state h;
            interpret_h t had_err
          end
        with RuntimeError (token, e) ->
          print_endline (e ^ "\n[line " ^ string_of_int token.line ^ "]");
          true)
  in

  interpret_h statements false
