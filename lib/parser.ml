type parser_state = { tokens : Token.t list; mutable current : int }

exception ParseError of Token.t * string

(* HELPERS *)

let report line where message =
  Printf.eprintf "[line %d] Error %s: %s\n%!" line where message

let peek (s : parser_state) = List.nth s.tokens s.current
let previous (s : parser_state) = List.nth s.tokens (s.current - 1)

let is_at_end (s : parser_state) =
  let t = peek s in
  t.kind = Token.EOF

let advance (s : parser_state) =
  if not (is_at_end s) then s.current <- s.current + 1;

  previous s

let error (token : Token.t) msg =
  let location =
    match token.kind with
    | Token.EOF -> " at end"
    | _ -> " at '" ^ token.lexeme ^ "'"
  in
  report token.line location msg;
  raise (ParseError (token, msg))

let consume (s : parser_state) expected_kind msg =
  if (peek s).kind = expected_kind then advance s else error (peek s) msg

(* PARSER METHODS *)
let rec parse_expression s = parse_equality s

and parse_equality (s : parser_state) =
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

and parse_comparaison s =
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

and parse_term s =
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

and parse_factor s =
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

and parse_unary s =
  match (peek s).kind with
  | Token.BANG | Token.MINUS ->
      let op = advance s in
      let right = parse_unary s in
      Ast.Unary (op, right)
  | _ -> parse_primary s

and parse_primary (s : parser_state) =
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
  | Token.LEFT_PAREN ->
      ignore @@ advance s;
      let expr = parse_expression s in

      ignore @@ consume s Token.RIGHT_PAREN "Expect ')' after expression";

      Ast.Grouping expr
  | _ -> error (peek s) "Expect expression."

let synchronize (s : parser_state) =
  let rec discard_tokens () =
    if is_at_end s then ()
    else if (previous s).kind = Token.SEMICOLON then ()
    else
      match (peek s).kind with
      | Token.CLASS | Token.FUN | Token.VAR | Token.FOR | Token.IF | Token.WHILE
      | Token.PRINT | Token.RETURN ->
          ()
      | _ ->
          ignore @@ advance s;
          discard_tokens ()
  in

  ignore @@ advance s;
  discard_tokens ()

let parse tokens =
  let state = { tokens; current = 0 } in

  try Some (parse_expression state) with ParseError _ -> None
