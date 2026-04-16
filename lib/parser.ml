open Errors

type parser_state = { tokens : Token.t list; mutable current : int }

(* HELPERS *)
(*
  There's no `match` function like in the book as OCaml already has a match.
  BUT, the match function in the book does more than matching, if a match 
  is found, it calls the `advance` function and consume the token. So to mimic
  this behaviour, i must match on the peek result's kind.

  Ex:
  ```java
  if (match(IDENTIFIER)) {
    return new Expr.Variable(previous());
  }
  ```

  ```ocaml
  match (peek s).kind with
  | Token.IDENTIFIER _ -> Ast.Variable (advance s)
                                        ^^^^^^^^^
  ```

  Note the fact that we need to consume the token ourselves.
  
  Same for the `check` method that is just a java `match` without consuming
  the token. So it's just a simple OCaml match 
 *)

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
    | Token.EOF -> "at end"
    | _ -> "at '" ^ token.lexeme ^ "'"
  in
  report token.line location msg;
  raise (ParseError (token, msg))

let consume (s : parser_state) expected_kind msg =
  if (peek s).kind = expected_kind then advance s else error (peek s) msg

(* PARSER METHODS *)
let rec parse_declaration s =
  try
    begin match (peek s).kind with
    | Token.VAR ->
        ignore @@ advance s;
        Some (parse_var_declaration s)
    | _ -> Some (parse_statement s)
    end
  with ParseError (token, error) ->
    synchronize s;
    None

and parse_var_declaration s =
  let name =
    match (peek s).kind with
    | Token.IDENTIFIER _ -> advance s
    | _ -> error (peek s) "Expect variable name"
  in

  let init =
    match (peek s).kind with
    | Token.EQUAL ->
        ignore @@ advance s;
        Some (parse_expression s)
    | _ -> None
  in

  ignore @@ consume s Token.SEMICOLON "Expect ';' after variable declaration.";

  Ast.Var (name, init)

and parse_statement s =
  match (peek s).kind with
  | Token.FOR ->
      advance s |> ignore;
      parse_for_stmt s
  | Token.IF ->
      advance s |> ignore;
      parse_if_stmt s
  | Token.PRINT ->
      ignore @@ advance s;
      parse_print_stmt s
  | Token.WHILE ->
      advance s |> ignore;
      parse_while_stmt s
  | Token.LEFT_BRACE ->
      ignore @@ advance s;
      Ast.Block (parse_block s)
  | _ -> parse_expr_stmt s

and parse_for_stmt s =
  consume s Token.LEFT_PAREN "Expect '(' after 'for'." |> ignore;

  let init =
    match (peek s).kind with
    | Token.SEMICOLON ->
        advance s |> ignore;
        None
    | Token.VAR ->
        advance s |> ignore;
        Some (parse_var_declaration s)
    | _ -> Some (parse_expr_stmt s)
  in

  let condition =
    match (peek s).kind with
    | Token.SEMICOLON -> None
    | _ -> Some (parse_expression s)
  in

  consume s Token.SEMICOLON "Expect ';' after loop condition." |> ignore;

  let increment =
    match (peek s).kind with
    | Token.RIGHT_PAREN -> None
    | _ -> Some (parse_expression s)
  in
  consume s Token.RIGHT_PAREN "Expect ')' after for clauses." |> ignore;

  let body = parse_statement s in

  (* Desugaring *)
  let body =
    match increment with
    | Some expr -> Ast.Block [ body; Ast.Expression expr ]
    | None -> body
  in

  let cond = Option.value condition ~default:(Ast.Literal (Ast.Boolean true)) in
  let body = Ast.While (cond, body) in

  let body =
    match init with Some stmt -> Ast.Block [ stmt; body ] | None -> body
  in

  body

and parse_block s =
  let rec parse_block_h acc =
    if (not (is_at_end s)) && (peek s).kind <> Token.RIGHT_BRACE then
      parse_block_h (parse_declaration s :: acc)
    else List.rev acc
  in

  let statements = parse_block_h [] |> List.filter_map (fun x -> x) in

  consume s Token.RIGHT_BRACE "Expect '}' after block." |> ignore;

  statements

and parse_if_stmt s =
  consume s Token.LEFT_PAREN "Expect '(' after 'if'." |> ignore;
  let condition = parse_expression s in
  consume s Token.RIGHT_PAREN "Expect ')' after if condition." |> ignore;

  let then_branch = parse_statement s in
  let else_branch =
    match (peek s).kind with
    | Token.ELSE -> Some (parse_statement s)
    | _ -> None
  in

  Ast.If (condition, then_branch, else_branch)

and parse_print_stmt s =
  let value = parse_expression s in
  ignore @@ consume s Token.SEMICOLON "Expect ';' after value.";
  Ast.Print value

and parse_while_stmt s =
  consume s Token.LEFT_PAREN "Expect '(' after 'while'." |> ignore;
  let condition = parse_expression s in
  consume s Token.RIGHT_PAREN "Expect ')' after while condition." |> ignore;
  let body = parse_statement s in

  Ast.While (condition, body)

and parse_expr_stmt s =
  let expr = parse_expression s in
  ignore @@ consume s Token.SEMICOLON "Expect ';' after expression.";
  Ast.Expression expr

and parse_expression s = parse_assignment s

and parse_assignment s =
  let expr = parse_or s in

  match (peek s).kind with
  | Token.EQUAL ->
      let equals = advance s in
      let value = parse_assignment s in

      begin match expr with
      | Ast.Variable name -> Ast.Assign (name, value)
      | _ -> error equals "Invalid assignment target."
      end
  | _ -> expr

and parse_or s =
  let rec parse_or_h expr =
    match (peek s).kind with
    | Token.OR ->
        let op = advance s in
        let right = parse_and s in
        parse_or_h @@ Ast.Logical (expr, op, right)
    | _ -> expr
  in

  let expr = parse_and s in
  parse_or_h expr

and parse_and s =
  let rec parse_and_h expr =
    match (peek s).kind with
    | Token.AND ->
        let op = advance s in
        let right = parse_equality s in
        parse_and_h @@ Ast.Logical (expr, op, right)
    | _ -> expr
  in

  let expr = parse_equality s in
  parse_and_h expr

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
  | Token.IDENTIFIER _ -> Ast.Variable (advance s)
  | Token.LEFT_PAREN ->
      ignore @@ advance s;
      let expr = parse_expression s in

      ignore @@ consume s Token.RIGHT_PAREN "Expect ')' after expression";

      Ast.Grouping expr
  | _ -> error (peek s) "Expect expression."

and synchronize (s : parser_state) =
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

  let rec build_statements (acc : Ast.stmt option list) =
    if is_at_end state then List.rev acc
    else build_statements (parse_declaration state :: acc)
  in

  build_statements [] |> List.filter_map (fun x -> x)

(* try Some (parse_expression state) with ParseError _ -> None *)
