exception RuntimeError of Token.t * string

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

let rec evaluate = function
  | Ast.Literal v -> v
  | Ast.Grouping group -> evaluate group
  | Ast.Unary (op, expr) -> evaluate_unary op (evaluate expr)
  | Ast.Binary (left, op, right) ->
      evaluate_binary op (evaluate left) (evaluate right)

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

let interpret expression =
  try
    begin
      let value = evaluate expression in
      print_endline @@ Ast.string_of_literal value;
      false
    end
  with RuntimeError (t, e) ->
    print_endline (e ^ "\n[line " ^ string_of_int t.line ^ "]");
    true
