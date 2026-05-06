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
  | Token.IDENTIFIER _ -> Types.Variable (advance s)
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
    | Token.CLASS ->
        advance s |> ignore;
        Some (parse_class s)
    | Token.FUN ->
        advance s |> ignore;
        Some (parse_fun s "function")
    | Token.VAR ->
        ignore @@ advance s;
        Some (parse_var_declaration s)
    | _ -> Some (parse_statement s)
    end
  with ParseError (token, error) ->
    synchronize s;
    None

and parse_class s =
  let rec parse_methods acc =
    if (peek s).kind = Token.RIGHT_BRACE || is_at_end s then List.rev acc
    else parse_methods (parse_fun s "method" :: acc)
  in

  let name =
    match (peek s).kind with
    | Token.IDENTIFIER _ -> advance s
    | _ -> error (peek s) "Expect class name."
  in

  let superclass =
    match (peek s).kind with
    | Token.LESS ->
        advance s |> ignore;
        begin match (peek s).kind with
        | Token.IDENTIFIER _ -> Some (Types.Variable (advance s))
        | _ ->
            Errors.error (peek s).line "Expect superclass name.";
            None
        end
    | _ -> None
  in

  consume s Token.LEFT_BRACE "Expect '{' before class body." |> ignore;

  let methods = parse_methods [] in

  consume s Token.RIGHT_BRACE "Expect '}' after class body." |> ignore;

  Types.Class { name; methods; superclass }

and parse_fun s kind =
  let rec parse_fun_params acc =
    if List.length acc >= 255 then
      error (peek s) "Can't have more than 255 parameters.";

    match (peek s).kind with
    | Token.COMMA ->
        advance s |> ignore;
        let ident =
          match (peek s).kind with
          | Token.IDENTIFIER _ -> advance s
          | _ -> error (peek s) "Expect parameter name."
        in
        parse_fun_params (ident :: acc)
    | _ -> List.rev acc
  in

  let name =
    match (peek s).kind with
    | Token.IDENTIFIER _ -> advance s
    | _ -> error (peek s) ("Expect " ^ kind ^ " name.")
  in

  consume s Token.LEFT_PAREN ("Expect '(' after " ^ kind ^ " name.") |> ignore;

  let params =
    if (peek s).kind <> RIGHT_PAREN then begin
      let ident =
        match (peek s).kind with
        | Token.IDENTIFIER _ -> advance s
        | _ -> error (peek s) "Expect parameter name."
      in
      parse_fun_params [ ident ]
    end
    else []
  in

  consume s Token.RIGHT_PAREN "Expect ')' after parameters." |> ignore;
  consume s Token.LEFT_BRACE ("Expect '{' before" ^ kind ^ "body.") |> ignore;

  let body = parse_block s in

  Types.Function { name; params; body }

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

  Types.Var (name, init)

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
  | Token.RETURN -> parse_return_stmt s
  | Token.WHILE ->
      advance s |> ignore;
      parse_while_stmt s
  | Token.LEFT_BRACE ->
      ignore @@ advance s;
      Types.Block (parse_block s)
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
    | Some expr -> Types.Block [ body; Types.Expression expr ]
    | None -> body
  in

  let cond =
    Option.value condition ~default:(Types.Literal (Types.Boolean true))
  in
  let body = Types.While (cond, body) in

  let body =
    match init with Some stmt -> Types.Block [ stmt; body ] | None -> body
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

  Types.If (condition, then_branch, else_branch)

and parse_print_stmt s =
  let value = parse_expression s in
  ignore @@ consume s Token.SEMICOLON "Expect ';' after value.";
  Types.Print value

and parse_return_stmt s =
  let keyword = advance s in
  let value =
    if (peek s).kind <> SEMICOLON then parse_expression s
    else Types.Literal Types.Nil
  in

  consume s Token.SEMICOLON "Expect ';' after return value." |> ignore;

  Types.Return (keyword, value)

and parse_while_stmt s =
  consume s Token.LEFT_PAREN "Expect '(' after 'while'." |> ignore;
  let condition = parse_expression s in
  consume s Token.RIGHT_PAREN "Expect ')' after while condition." |> ignore;
  let body = parse_statement s in

  Types.While (condition, body)

and parse_expr_stmt s =
  let expr = parse_expression s in
  ignore @@ consume s Token.SEMICOLON "Expect ';' after expression.";
  Types.Expression expr

and parse_expression s = parse_assignment s

and parse_assignment s =
  let expr = parse_or s in

  match (peek s).kind with
  | Token.EQUAL ->
      let equals = advance s in
      let value = parse_assignment s in

      begin match expr with
      | Types.Variable name -> Types.Assign (name, value)
      | Types.Get (obj, name) -> Types.Set { obj; name; value }
      | _ -> error equals "Invalid assignment target."
      end
  | _ -> expr

and parse_or s =
  let rec parse_or_h expr =
    match (peek s).kind with
    | Token.OR ->
        let op = advance s in
        let right = parse_and s in
        parse_or_h @@ Types.Logical (expr, op, right)
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
        parse_and_h @@ Types.Logical (expr, op, right)
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

        handle_eq (Types.Binary (curr_expr, op, right))
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
        handle_comp @@ Types.Binary (current, op, right)
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
        handle_term @@ Types.Binary (current, op, right)
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
        handle_factor @@ Types.Binary (current, op, right)
    | _ -> current
  in

  let expr = parse_unary s in
  handle_factor expr

and parse_unary s =
  match (peek s).kind with
  | Token.BANG | Token.MINUS ->
      let op = advance s in
      let right = parse_unary s in
      Types.Unary (op, right)
  | _ -> parse_call s

and parse_call s =
  let rec parse_call_h expr =
    match (peek s).kind with
    | Token.LEFT_PAREN ->
        advance s |> ignore;
        parse_call_h @@ finish_call s expr
    | Token.DOT ->
        advance s |> ignore;
        let name =
          match (peek s).kind with
          | Token.IDENTIFIER _ -> advance s
          | _ -> error (peek s) "Expect property name after '.'."
        in
        parse_call_h @@ Types.Get (expr, name)
    | _ -> expr
  in

  let expr = parse_primary s in

  parse_call_h expr

and finish_call s callee =
  let rec parse_args (args : Types.expr list) =
    match (peek s).kind with
    | Token.COMMA ->
        advance s |> ignore;
        if List.length args >= 255 then
          error (peek s) "Can't have more than 255 arguments."
        else parse_args (parse_expression s :: args)
    | _ -> List.rev args
  in

  let arguments =
    match (peek s).kind with
    | Token.RIGHT_PAREN -> []
    | _ -> parse_args [ parse_expression s ]
  in

  let paren = consume s Token.RIGHT_PAREN "Expect ')' after arguments." in

  Types.Call (callee, paren, arguments)

and parse_primary (s : parser_state) =
  match (peek s).kind with
  | Token.FALSE ->
      ignore @@ advance s;
      Types.Literal (Types.Boolean false)
  | Token.TRUE ->
      ignore @@ advance s;
      Types.Literal (Types.Boolean true)
  | Token.NIL ->
      ignore @@ advance s;
      Types.Literal Types.Nil
  | Token.NUMBER f ->
      ignore @@ advance s;
      Types.Literal (Types.Number f)
  | Token.STRING str ->
      ignore @@ advance s;
      Types.Literal (Types.String str)
  | Token.IDENTIFIER _ -> Types.Variable (advance s)
  | Token.LEFT_PAREN ->
      ignore @@ advance s;
      let expr = parse_expression s in

      ignore @@ consume s Token.RIGHT_PAREN "Expect ')' after expression";

      Types.Grouping expr
  | Token.THIS -> Types.This (advance s)
  | Token.SUPER ->
      let keyword = advance s in

      consume s Token.DOT "Expect '.' after 'super'." |> ignore;

      let method_ =
        match (peek s).kind with
        | Token.IDENTIFIER _ -> advance s
        | _ -> raise @@ ParseError (peek s, "Expect superclass method name.")
      in

      Types.Super { keyword; method_ }
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

  let rec build_statements (acc : Types.stmt option list) =
    if is_at_end state then List.rev acc
    else build_statements (parse_declaration state :: acc)
  in

  build_statements [] |> List.filter_map (fun x -> x)

(* try Some (parse_expression state) with ParseError _ -> None *)
