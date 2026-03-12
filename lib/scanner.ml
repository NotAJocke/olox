open Token

let report line where message =
  Printf.eprintf "[line %d] Error %s: %s" line where message

let error line message = report line "" message

let scan source =
  let hadError = ref false in
  let total = String.length source in
  let start = ref 0 in
  let current = ref 0 in
  let line = ref 1 in
  let tokens : token list ref = ref [] in

  let isAtEnd () = !current >= total in

  let scanToken () =
    let advance () =
      let c = String.get source !current in
      current := !current + 1;
      c
    in

    let addToken kind literal =
      let text = String.sub source !start (!current - !start) in
      tokens := { kind; lexeme = text; literal; line = !line } :: !tokens
    in

    let expect expected =
      if isAtEnd () then false
      else if String.get source !current <> expected then false
      else begin
        current := !current + 1;
        true
      end
    in

    let peek () =
      if isAtEnd () then None else Some (String.get source !current)
    in

    let rec skip_to_newline () =
      match peek () with
      | Some '\n' | None -> ()
      | Some _ ->
          ignore (advance ());
          skip_to_newline ()
    in

    let addTokenWoLiteral kind = addToken kind No in

    let c = advance () in
    match c with
    | '(' -> addTokenWoLiteral LEFT_PAREN
    | ')' -> addTokenWoLiteral RIGHT_PAREN
    | '{' -> addTokenWoLiteral LEFT_BRACE
    | '}' -> addTokenWoLiteral RIGHT_BRACE
    | ',' -> addTokenWoLiteral COMMA
    | '.' -> addTokenWoLiteral DOT
    | '+' -> addTokenWoLiteral PLUS
    | '-' -> addTokenWoLiteral MINUS
    | '*' -> addTokenWoLiteral STAR
    | ';' -> addTokenWoLiteral SEMICOLON
    | '!' -> addTokenWoLiteral (if expect '=' then BANG_EQUAL else BANG)
    | '=' -> addTokenWoLiteral (if expect '=' then EQUAL_EQUAL else EQUAL)
    | '<' -> addTokenWoLiteral (if expect '=' then LESS_EQUAL else LESS)
    | '>' -> addTokenWoLiteral (if expect '=' then GREATER_EQUAL else GREATER)
    | '/' -> if expect '/' then skip_to_newline () else addTokenWoLiteral SLASH
    | ' ' -> ()
    | '\r' -> ()
    | '\t' -> ()
    | '\n' -> line := !line + 1
    | _ ->
        error !line "Unexpected character.";
        hadError := true
  in

  while not (isAtEnd ()) do
    start := !current;
    scanToken ()
  done;

  tokens :=
    { kind = EOF; lexeme = ""; literal = (No : literal); line = !line }
    :: !tokens;

  List.rev !tokens
