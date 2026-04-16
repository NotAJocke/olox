open Errors

let create () : Types.interpreter_state =
  let globals = Environment.create () in

  Environment.define globals "clock"
    (Types.Callable
       {
         repr = "<builtin clock>";
         arity = 0;
         call = (fun _ _ -> Types.Number (Unix.gettimeofday ()));
       });

  { globals; env = globals }

let is_truthy = function Types.Nil | Types.Boolean false -> false | _ -> true

let is_equal a b =
  match (a, b) with
  | Types.Nil, Types.Nil -> true
  | Types.Number x, Types.Number y -> x = y
  | Types.String x, Types.String y -> String.equal x y
  | Types.Boolean x, Types.Boolean y -> x = y
  | _ -> false

let unwrap_number op literal =
  match literal with
  | Types.Number n -> n
  | _ -> raise @@ RuntimeError (op, "Operand must be a number.")

let rec evaluate (s : Types.interpreter_state) expr =
  match expr with
  | Types.Literal v -> v
  | Types.Grouping group -> evaluate s group
  | Types.Unary (op, expr) -> evaluate_unary op (evaluate s expr)
  | Types.Binary (left, op, right) ->
      evaluate_binary op (evaluate s left) (evaluate s right)
  | Types.Variable expr -> Environment.get s.env expr
  | Types.Logical (left, op, right) ->
      let left = evaluate s left in

      begin match op.kind with
      | Token.OR when is_truthy left -> left
      | Token.AND when not @@ is_truthy left -> left
      | Token.AND | Token.OR -> evaluate s right
      | _ -> failwith "Invalid logical operator"
      end
  | Types.Assign (name, value) ->
      let value = evaluate s value in
      Environment.assign s.env name value;

      value
  | Types.Call (callee, paren, args) -> (
      let callee = evaluate s callee in
      let args = List.map (evaluate s) args in

      match callee with
      | Types.Callable fn_obj ->
          if List.length args <> fn_obj.arity then
            raise
              (RuntimeError
                 ( paren,
                   "Expected " ^ string_of_int fn_obj.arity
                   ^ " arguments but got "
                   ^ string_of_int (List.length args)
                   ^ "." ));

          fn_obj.call s args
      | _ ->
          raise (RuntimeError (paren, "Can only call functions and classes.")))

and evaluate_unary op expr =
  match op.kind with
  | Token.MINUS -> begin
      let n = unwrap_number op expr in
      Types.Number (-.n)
    end
  | Token.BANG -> Types.Boolean (not @@ is_truthy expr)
  | _ -> Types.Nil

and evaluate_binary op left right =
  match op.kind with
  | Token.MINUS ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Types.Number (left -. right)
  | Token.SLASH ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Types.Number (left /. right)
  | Token.STAR ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Types.Number (left *. right)
  | Token.PLUS -> begin
      match (left, right) with
      | Types.Number x, Types.Number y -> Types.Number (x +. y)
      | Types.String s1, Types.String s2 -> Types.String (s1 ^ s2)
      | _ ->
          raise
          @@ RuntimeError (op, "Operands must be two numbers or two strings.")
    end
  | Token.GREATER ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Types.Boolean (left > right)
  | Token.GREATER_EQUAL ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Types.Boolean (left >= right)
  | Token.LESS ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Types.Boolean (left < right)
  | Token.LESS_EQUAL ->
      let left = unwrap_number op left in
      let right = unwrap_number op right in

      Types.Boolean (left <= right)
  | Token.BANG_EQUAL -> Types.Boolean (not @@ is_equal left right)
  | Token.EQUAL_EQUAL -> Types.Boolean (is_equal left right)
  | _ -> failwith "Unreachable"

let rec execute (s : Types.interpreter_state) (stmt : Types.stmt) =
  match stmt with
  | Types.Block stmts ->
      let local_env = Environment.create_with_enclosing s.env in
      execute_block s stmts local_env
  | Types.Expression e -> ignore @@ evaluate s e
  | Types.Function { name; params; body } ->
      let fn = create_lox_fun name params body s.env in
      Environment.define s.env name.lexeme fn
  | Types.Print e ->
      let value = evaluate s e in
      print_endline @@ Types.string_of_literal value
  | Types.Return (keyword, value) ->
      let evaluated_value = evaluate s value in
      raise @@ RuntimeReturn evaluated_value
  | Types.Var (key, value) ->
      let v = match value with Some v -> evaluate s v | _ -> Types.Nil in
      Environment.define s.env key.lexeme v
  | Types.If (cond, then_b, else_b) ->
      if is_truthy @@ evaluate s cond then execute s then_b
      else Option.iter (execute s) else_b
  | Types.While (cond, body) ->
      let rec exec_while () =
        if is_truthy @@ evaluate s cond then begin
          execute s body;
          exec_while ()
        end
        else ()
      in

      exec_while ()

and execute_block s stmts new_env =
  let previous = s.env in
  s.env <- new_env;
  try
    List.iter (execute s) stmts;
    s.env <- previous
  with e ->
    s.env <- previous;
    raise e

and create_lox_fun name params body closure =
  Types.Callable
    {
      arity = List.length params;
      call =
        (fun state arguments ->
          let env = Environment.create_with_enclosing closure in

          List.iter2
            (fun param arg -> Environment.define env param.Token.lexeme arg)
            params arguments;

          try
            execute_block state body env;
            Types.Nil
          with RuntimeReturn value -> value);
      repr = "<fn " ^ name.lexeme ^ ">";
    }

let interpret (state : Types.interpreter_state) statements =
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
