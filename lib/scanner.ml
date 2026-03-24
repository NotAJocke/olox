(*

TODO: Rewrite the scan method in a more functional way.

1. Redefine scanToken to be Pure:
- Change scanToken so it no longer reads/writes global ref variables.
- It should take (source, current_index) as input.
- It should return a tuple or a record: (new_token_option, next_index, line_increment).

2. Replace the while loop with a Recursive loop:
- Create a nested rec function (often called aux or loop).
- Input: current_index, current_line, and the token_list accumulator.
- Base Case: If isAtEnd, append EOF and return List.rev tokens.
- Recursive Step: Call your new scanToken, prepend the result to your list, and call loop with the new index.

3. Handle Multi-character Tokens Functionally:
- Refactor parse_string and parse_number to be recursive. Instead of while loops, they should call themselves with current + 1 until they hit the delimiter, then return the final index and the extracted string.

4. Adopt the Option pattern for Errors:
- Instead of hadError being a global boolean, consider having your final scan function return a Result type: Ok(token_list) or Error(error_report).

*)

let report line where message =
  Printf.eprintf "[line %d] Error %s: %s\n%!" line where message

let error line message = report line "" message

module StringMap = Map.Make (String)

let keywords =
  StringMap.of_list
    [
      ("and", Token.AND);
      ("class", Token.CLASS);
      ("else", Token.ELSE);
      ("false", Token.FALSE);
      ("for", Token.FOR);
      ("fun", Token.FUN);
      ("if", Token.IF);
      ("nil", Token.NIL);
      ("or", Token.OR);
      ("print", Token.PRINT);
      ("return", Token.RETURN);
      ("super", Token.SUPER);
      ("this", Token.THIS);
      ("true", Token.TRUE);
      ("var", Token.VAR);
      ("while", Token.WHILE);
    ]

let scan source =
  let hadError = ref false in
  let total = String.length source in
  let start = ref 0 in
  let current = ref 0 in
  let line = ref 1 in
  let tokens : Token.t list ref = ref [] in

  let isAtEnd () = !current >= total in

  let scanToken () =
    let advance () =
      let c = String.get source !current in
      current := !current + 1;
      c
    in

    let addToken (kind : Token.kind) =
      let text = String.sub source !start (!current - !start) in
      tokens := { kind; lexeme = text; line = !line } :: !tokens
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

    let peekNext () =
      if !current + 1 >= total then None
      else Some (String.get source (!current + 1))
    in

    let rec skip_to_newline () =
      match peek () with
      | Some '\n' | None -> ()
      | Some _ ->
          ignore (advance ());
          skip_to_newline ()
    in

    let parse_string () =
      while peek () <> Some '"' && not (isAtEnd ()) do
        if peek () = Some '\n' then line := !line + 1;
        ignore (advance ())
      done;

      if isAtEnd () then begin
        error !line "Unterminated string.";
        hadError := true
      end
      else begin
        ignore (advance ());

        let value = String.sub source (!start + 1) (!current - !start - 2) in
        addToken (STRING value)
      end
    in

    let is_digit c = c >= '0' && c <= '9' in

    let is_alpha c =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    in

    let is_alpha_numeric c = is_alpha c || is_digit c in

    let parse_number () =
      let rec skip_number () =
        match peek () with
        | Some c when is_digit c ->
            ignore (advance ());
            skip_number ()
        | _ -> ()
      in

      skip_number ();

      if peek () = Some '.' then
        match peekNext () with
        | Some c when is_digit c ->
            ignore (advance ());
            skip_number ()
        | _ -> ()
      else ();

      let text = String.sub source !start (!current - !start) in
      addToken (NUMBER (float_of_string text))
    in

    let parse_identifier () =
      let rec skip_ident () =
        match peek () with
        | Some c when is_alpha_numeric c ->
            ignore (advance ());
            skip_ident ()
        | _ -> ()
      in

      skip_ident ();

      let text = String.sub source !start (!current - !start) in
      match StringMap.find_opt text keywords with
      | Some t -> addToken t
      | _ -> addToken (IDENTIFIER text)
    in

    let c = advance () in
    match c with
    | '(' -> addToken LEFT_PAREN
    | ')' -> addToken RIGHT_PAREN
    | '{' -> addToken LEFT_BRACE
    | '}' -> addToken RIGHT_BRACE
    | ',' -> addToken COMMA
    | '.' -> addToken DOT
    | '+' -> addToken PLUS
    | '-' -> addToken MINUS
    | '*' -> addToken STAR
    | ';' -> addToken SEMICOLON
    | '!' -> addToken (if expect '=' then BANG_EQUAL else BANG)
    | '=' -> addToken (if expect '=' then EQUAL_EQUAL else EQUAL)
    | '<' -> addToken (if expect '=' then LESS_EQUAL else LESS)
    | '>' -> addToken (if expect '=' then GREATER_EQUAL else GREATER)
    | '/' -> if expect '/' then skip_to_newline () else addToken SLASH
    | ' ' -> ()
    | '\r' -> ()
    | '\t' -> ()
    | '\n' -> line := !line + 1
    | '"' -> parse_string ()
    | c when is_digit c -> parse_number ()
    | c when is_alpha c -> parse_identifier ()
    | _ ->
        error !line "Unexpected character.";
        hadError := true
  in

  while not (isAtEnd ()) do
    start := !current;
    scanToken ()
  done;

  tokens := { kind = EOF; lexeme = ""; line = !line } :: !tokens;

  List.rev !tokens
