open Errors

let create () : Types.interpreter_state =
  let globals = Environment.create () in

  Environment.define globals "clock"
    (Types.Callable
       {
         repr = "<builtin clock>";
         arity = 0;
         call = (fun _ _ -> Types.Number (Unix.gettimeofday ()));
         closure = Environment.create ();
         params = None;
         body = None;
         is_initializer = false;
       });

  { globals; env = globals; locals = ref [] }

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

let rec find_local_depth locals expr =
  match locals with
  | [] -> None
  | (candidate, depth) :: rest ->
      if candidate == expr then Some depth else find_local_depth rest expr

let lookup_var (s : Types.interpreter_state) (name : Token.t)
    (expr : Types.expr) =
  match find_local_depth !(s.locals) expr with
  | Some distance -> Environment.get_at s.env distance name.lexeme
  | None -> Environment.get s.globals name

let rec evaluate (s : Types.interpreter_state) expr =
  match expr with
  | Types.Get (obj, name) ->
      let evaluated_obj = evaluate s obj in
      begin match evaluated_obj with
      | Types.LoxInstance { klass; fields } -> (
          match Hashtbl.find_opt fields name.lexeme with
          | Some f -> f
          | None -> begin
              match Hashtbl.find_opt klass.methods name.lexeme with
              | Some meth -> Types.Callable (bind_method { klass; fields } meth)
              | None ->
                  raise
                  @@ RuntimeError
                       (name, "Undefined property '" ^ name.lexeme ^ "'.")
            end)
      | _ -> failwith "unreachable"
      end
  | Types.Set { obj; value; name } ->
      let evaluated_obj = evaluate s obj in

      begin match evaluated_obj with
      | Types.LoxInstance { klass; fields } ->
          let evaluated_value = evaluate s value in
          Hashtbl.replace fields name.lexeme evaluated_value;
          evaluated_value
      | _ -> raise @@ RuntimeError (name, "Only instances have fields.")
      end
  | Types.This keyword -> lookup_var s keyword expr
  | Types.Literal v -> v
  | Types.Grouping group -> evaluate s group
  | Types.Unary (op, expr) -> evaluate_unary op (evaluate s expr)
  | Types.Binary (left, op, right) ->
      evaluate_binary op (evaluate s left) (evaluate s right)
  | Types.Variable name -> lookup_var s name expr
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
      begin match find_local_depth !(s.locals) expr with
      | Some distance -> Environment.assign_at s.env distance name value
      | None -> Environment.assign s.globals name value
      end;

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
      | Types.LoxClass klass ->
          let arity =
            match Hashtbl.find_opt klass.methods "init" with
            | Some f -> f.arity
            | None -> 0
          in

          if List.length args <> arity then
            raise
              (RuntimeError
                 ( paren,
                   "Expected " ^ string_of_int arity ^ " arguments but got "
                   ^ string_of_int (List.length args)
                   ^ "." ));
          let instance = { Types.klass; fields = Hashtbl.create 0 } in

          begin match Hashtbl.find_opt klass.methods "init" with
          | Some f -> (bind_method instance f).call s args |> ignore
          | None -> ()
          end;

          Types.LoxInstance instance
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

and execute (s : Types.interpreter_state) (stmt : Types.stmt) =
  match stmt with
  | Types.Class { name; methods } ->
      Environment.define s.env name.lexeme Types.Nil;

      let meths = Hashtbl.create 0 in
      List.iter
        (fun meth ->
          match meth with
          | Types.Function { name; params; body } ->
              let fn =
                create_lox_fun name params body s.env
                  (String.equal name.lexeme "init")
              in
              Hashtbl.add meths name.lexeme fn
          | _ -> failwith "Class method must be a function")
        methods;

      let (klass : Types.lox_class) =
        { name = name.lexeme; repr = name.lexeme; methods = meths }
      in
      Environment.assign s.env name (Types.LoxClass klass)
  | Types.Block stmts ->
      let local_env = Environment.create_with_enclosing s.env in
      execute_block s stmts local_env
  | Types.Expression e -> ignore @@ evaluate s e
  | Types.Function { name; params; body } ->
      let fn = create_lox_fun name params body s.env false in
      Environment.define s.env name.lexeme (Types.Callable fn)
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

and create_lox_fun name params body closure is_initializer =
  {
    arity = List.length params;
    closure;
    is_initializer;
    params = Some params;
    body = Some body;
    call =
      (fun state arguments ->
        let env = Environment.create_with_enclosing closure in

        List.iter2
          (fun param arg -> Environment.define env param.Token.lexeme arg)
          params arguments;

        try
          execute_block state body env;

          if is_initializer then Environment.get_at closure 0 "this"
          else Types.Nil
        with RuntimeReturn value ->
          if is_initializer then Environment.get_at closure 0 "this" else value);
    repr = "<fn " ^ name.lexeme ^ ">";
  }

and bind_method (instance : Types.lox_instance) (meth : Types.lox_callable) :
    Types.lox_callable =
  let env = Environment.create_with_enclosing meth.closure in
  Environment.define env "this" (Types.LoxInstance instance);
  match (meth.params, meth.body) with
  | Some params, Some body ->
      {
        meth with
        closure = env;
        call =
          (fun state args ->
            let call_env = Environment.create_with_enclosing env in
            List.iter2
              (fun p a -> Environment.define call_env p.Token.lexeme a)
              params args;
            try
              execute_block state body call_env;
              Types.Nil
            with RuntimeReturn v -> v);
      }
  | _ -> meth

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

let resolve (s : Types.interpreter_state) (expr : Types.expr) (depth : int) =
  s.locals := (expr, depth) :: !(s.locals)
