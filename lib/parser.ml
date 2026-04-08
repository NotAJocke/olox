type parser_state = { tokens : Token.t list; mutable current : int }

(* HELPERS *)
let peek (s : parser_state) = List.nth s.tokens s.current
let previous (s : parser_state) = List.nth s.tokens (s.current - 1)

let is_at_end (s : parser_state) =
  let t = peek s in
  t.kind = Token.EOF

let advance (s : parser_state) =
  if not (is_at_end s) then s.current <- s.current + 1;

  previous s

(* PARSER METHODS *)

let parse_primary (s : parser_state) =
  match (peek s).kind with
  | Token.FALSE ->
      ignore @@ advance s;
      Ast.Literal (Ast.Boolean false)
  | Token.TRUE ->
      ignore @@ advance s;
      Ast.Literal (Ast.Boolean true)
  | Token.NIL ->
      ignore @@ advance s;
      Ast.Literal Ast.Nil
  | Token.NUMBER f ->
      ignore @@ advance s;
      Ast.Literal (Ast.Number f)
  | Token.STRING str ->
      ignore @@ advance s;
      Ast.Literal (Ast.String str)
  | _ -> failwith "TODO"

let rec parse_unary s =
  match (peek s).kind with
  | Token.BANG | Token.MINUS ->
      let op = advance s in
      let right = parse_unary s in
      Ast.Unary (op, right)
  | _ -> parse_primary s

let parse_factor s =
  let rec handle_factor current =
    match (peek s).kind with
    | Token.SLASH | Token.STAR ->
        let op = advance s in
        let right = parse_unary s in
        handle_factor @@ Ast.Binary (current, op, right)
    | _ -> current
  in

  let expr = parse_unary s in
  handle_factor expr

let parse_term s =
  let rec handle_term current =
    match (peek s).kind with
    | Token.PLUS | Token.MINUS ->
        let op = advance s in
        let right = parse_factor s in
        handle_term @@ Ast.Binary (current, op, right)
    | _ -> current
  in

  let expr = parse_factor s in
  handle_term expr

let parse_comparaison s =
  let rec handle_comp current =
    match (peek s).kind with
    | Token.GREATER | Token.GREATER_EQUAL | Token.LESS | Token.LESS_EQUAL ->
        let op = advance s in
        let right = parse_term s in
        handle_comp @@ Ast.Binary (current, op, right)
    | _ -> current
  in

  let expr = parse_term s in
  handle_comp expr

let parse_equality (s : parser_state) =
  let rec handle_eq curr_expr =
    match (peek s).kind with
    | Token.EQUAL_EQUAL | Token.BANG_EQUAL ->
        let op = advance s in
        let right = parse_comparaison s in

        handle_eq (Ast.Binary (curr_expr, op, right))
    | _ -> curr_expr
  in

  let expr = parse_comparaison s in
  handle_eq expr

let parse_expression s = parse_equality s
