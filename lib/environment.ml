open Errors

type t = { values : (string, Ast.literal) Hashtbl.t; enclosing : t option }

let create () : t = { values = Hashtbl.create 0; enclosing = None }

let create_with_enclosing enclosing : t =
  { values = Hashtbl.create 0; enclosing = Some enclosing }

let define (env : t) name value = Hashtbl.add env.values name value

let rec get (env : t) (name : Token.t) : Ast.literal =
  match Hashtbl.find_opt env.values name.lexeme with
  | Some value -> value
  | None -> (
      match env.enclosing with
      | Some outer -> get outer name
      | None ->
          raise
          @@ RuntimeError (name, "Undefined variable '" ^ name.lexeme ^ "'."))

let rec assign (env : t) (name : Token.t) value =
  if Hashtbl.mem env.values name.lexeme then
    Hashtbl.replace env.values name.lexeme value
  else
    match env.enclosing with
    | Some outer -> assign outer name value
    | None ->
        raise @@ RuntimeError (name, "Undefined variable '" ^ name.lexeme ^ "'.")
