let begin_scope (s : Types.resolver_state) =
  { s with scopes = Hashtbl.create 0 :: s.scopes }

let end_scope (s : Types.resolver_state) = { s with scopes = List.tl s.scopes }

let declare (s : Types.resolver_state) (name : Token.t) : Types.resolver_state =
  match s.scopes with
  | [] -> s
  | scope :: _ ->
      if Hashtbl.mem scope name.lexeme then
        Errors.error name.line
          "Already a variable with this name in this scope."
      else Hashtbl.add scope name.lexeme false;
      s

let define (s : Types.resolver_state) (name : Token.t) : Types.resolver_state =
  match s.scopes with
  | [] -> s
  | scope :: _ ->
      Hashtbl.replace scope name.lexeme true;
      s

let rec resolve (s : Types.resolver_state) (stmts : Types.stmt list) :
    Types.resolver_state =
  List.fold_left resolve_stmt s stmts

and resolve_stmt s stmt =
  match stmt with
  | Types.Class { name; methods; superclass } ->
      let enclosing = s.current_class in
      let (s : Types.resolver_state) =
        { s with current_class = Types.In_Class }
      in

      let s_inner = declare s name in
      let after_define = define s_inner name in

      begin match superclass with
      | Some sc -> begin
          match sc with
          | Types.Variable iden ->
              if String.equal iden.lexeme name.lexeme then
                Errors.error iden.line "A class can't inherit from itself."
          | _ -> failwith "Unreachable"
        end
      | None -> ()
      end;

      let after_superclass =
        match superclass with
        | Some sc ->
            let s = { s with current_class = Types.In_Subclass } in
            let sc_resolved = resolve_expr s sc in
            let after_scope = begin_scope sc_resolved in
            Hashtbl.add (List.hd after_scope.scopes) "super" true;
            after_scope
        | None -> after_define
      in

      let after_scope = begin_scope after_superclass in
      Hashtbl.add (List.hd after_scope.scopes) "this" true;

      let after_methods =
        List.fold_left
          (fun s meth ->
            match meth with
            | Types.Function { name; params; body } ->
                let declaration =
                  if String.equal name.lexeme "init" then Types.Type_Init
                  else Types.Type_Method
                in
                resolve_function s name params body declaration
            | _ -> failwith "Unreachable")
          after_scope methods
      in

      let after_scope = end_scope after_methods in

      let after_after_sc =
        match superclass with
        | Some sc -> end_scope after_scope
        | _ -> after_scope
      in

      { after_after_sc with current_class = enclosing }
  | Types.Block stmts ->
      let s_inner = begin_scope s in
      let s_after_stmts = resolve s_inner stmts in
      end_scope s_after_stmts
  | Types.Var (name, init) ->
      let inner_s = declare s name in
      let after_init_s =
        match init with Some i -> resolve_expr inner_s i | None -> inner_s
      in

      define after_init_s name
  | Types.Function { name; params; body } ->
      let after_declare_s = declare s name in
      let after_define_s = define after_declare_s name in

      resolve_function after_define_s name params body Types.Type_Function
  | Types.Expression expr -> resolve_expr s expr
  | Types.If (cond, thenb, elseb) ->
      let after_cond = resolve_expr s cond in
      let after_then = resolve_stmt after_cond thenb in
      let after_else =
        match elseb with
        | Some b -> resolve_stmt after_then b
        | None -> after_then
      in

      after_else
  | Types.Print expr -> resolve_expr s expr
  | Types.Return (keyword, value) ->
      if s.current_function = Types.Type_None then
        Errors.error 0 "Can't return from top-level code.";

      begin match value with
      | Types.Literal Types.Nil -> s
      | _ ->
          if s.current_function = Types.Type_Init then
            Errors.error keyword.line
              "Can't return a value from an initializer.";
          resolve_expr s value
      end
  | Types.While (cond, body) ->
      let after_cond = resolve_expr s cond in
      resolve_stmt after_cond body

and resolve_expr (s : Types.resolver_state) (expr : Types.expr) =
  match expr with
  | Types.Set { obj; name; value } ->
      let inner_s = resolve_expr s value in
      resolve_expr inner_s obj
  | Types.This keyword ->
      if s.current_class = Types.No_Class then
        Errors.error keyword.line "Can't use 'this' outside of a class.";
      resolve_local s expr keyword
  | Types.Super { keyword; _ } ->
      begin match s.current_class with
      | Types.No_Class ->
          Errors.error keyword.line "Can't use 'super' outside of a class."
      | Types.In_Class ->
          Errors.error keyword.line
            "Can't use 'super' in a class with no superclass."
      | Types.In_Subclass -> ()
      end;

      resolve_local s expr keyword
  | Types.Get (obj, name) -> resolve_expr s obj
  | Types.Variable name ->
      if
        (not @@ List.is_empty s.scopes)
        && Hashtbl.find_opt (List.hd s.scopes) name.lexeme = Some false
      then
        Errors.error name.line
          "Can't read local variable in its own initializer.";

      resolve_local s expr name
  | Types.Assign (name, value) ->
      let inner_s = resolve_expr s value in
      resolve_local inner_s expr name
  | Types.Binary (left, _op, right) ->
      let after_left = resolve_expr s left in
      resolve_expr after_left right
  | Types.Call (callee, _, args) ->
      let after_callee = resolve_expr s callee in

      List.fold_left (fun s a -> resolve_expr s a) after_callee args
  | Types.Grouping expr -> resolve_expr s expr
  | Types.Literal _ -> s
  | Types.Logical (left, _, right) ->
      let after_left = resolve_expr s left in
      resolve_expr after_left right
  | Types.Unary (_, right) -> resolve_expr s right

and resolve_local s (expr : Types.expr) (name : Token.t) =
  let rec search depth = function
    | [] -> s
    | scope :: rest ->
        if Hashtbl.mem scope name.lexeme then begin
          Interpreter.resolve s.interpreter expr depth;
          s
        end
        else search (depth + 1) rest
  in
  search 0 s.scopes

and resolve_function s name params body fn_type =
  let enclosing = s.current_function in
  let s = { s with current_function = fn_type } in

  let inner_s = begin_scope s in

  let handle_param s param =
    let inner_s = declare s param in
    define inner_s param
  in
  let after_params_s = List.fold_left handle_param inner_s params in

  let after_resolve_s = resolve after_params_s body in
  let after_scope = end_scope after_resolve_s in

  { after_scope with current_function = enclosing }
