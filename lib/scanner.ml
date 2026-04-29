type scanner_state = {
  source : string;
  source_len : int;
  start : int;
  current : int;
  line : int;
  tokens : Token.t list;
  errors : string list;
}

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

let is_at_end s = s.current >= s.source_len

let advance s =
  let c = String.get s.source s.current in
  (c, { s with current = s.current + 1 })

let peek s = if is_at_end s then None else Some (String.get s.source s.current)

let peek_next s =
  if s.current + 1 >= s.source_len then None
  else Some (String.get s.source (s.current + 1))

let match_char s char =
  if is_at_end s then (false, s)
  else if peek s <> Some char then (false, s)
  else (true, { s with current = s.current + 1 })

let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
let is_alpha_numeric c = is_alpha c || is_digit c

let add_token (s : scanner_state) kind =
  let text = String.sub s.source s.start (s.current - s.start) in
  { s with tokens = { kind; lexeme = text; line = s.line } :: s.tokens }

let rec skip_to_newline s =
  match peek s with
  | Some '\n' | None -> s
  | _ -> skip_to_newline @@ snd @@ advance s

let parse_string s =
  let rec to_end s =
    match peek s with
    | Some '"' | None -> s
    | Some '\n' -> to_end { s with line = s.line + 1; current = s.current + 1 }
    | _ -> to_end @@ snd @@ advance s
  in

  let s = to_end s in

  if is_at_end s then begin
    Errors.error s.line "Unterminated string.";
    { s with errors = "Unterminated string." :: s.errors }
  end
  else begin
    let _, s = advance s in

    let value = String.sub s.source (s.start + 1) (s.current - s.start - 2) in
    add_token s (Token.STRING value)
  end

let parse_number s =
  let rec skip_number s =
    match peek s with
    | Some c when is_digit c -> skip_number @@ snd @@ advance s
    | _ -> s
  in

  let s = skip_number s in

  let s =
    match (peek s, peek_next s) with
    | Some '.', Some c when is_digit c ->
        let _, s = advance s in
        skip_number s
    | _ -> s
  in

  let text = String.sub s.source s.start (s.current - s.start) in
  add_token s (Token.NUMBER (float_of_string text))

let parse_identifier s =
  let rec skip_identifier s =
    match peek s with
    | Some c when is_alpha_numeric c -> skip_identifier @@ snd @@ advance s
    | _ -> s
  in

  let s = skip_identifier s in

  let text = String.sub s.source s.start (s.current - s.start) in
  match StringMap.find_opt text keywords with
  | Some t -> add_token s t
  | _ -> add_token s (Token.IDENTIFIER text)

let rec scan_h (s : scanner_state) : Token.t list * string list =
  let s = { s with start = s.current } in

  if is_at_end s then
    ( List.rev
        ({ Token.kind = Token.EOF; lexeme = ""; line = s.line } :: s.tokens),
      List.rev s.errors )
  else begin
    let c, s = advance s in
    let s =
      match c with
      | '(' -> add_token s Token.LEFT_PAREN
      | ')' -> add_token s Token.RIGHT_PAREN
      | '{' -> add_token s Token.LEFT_BRACE
      | '}' -> add_token s Token.RIGHT_BRACE
      | ',' -> add_token s Token.COMMA
      | '.' -> add_token s DOT
      | '+' -> add_token s PLUS
      | '-' -> add_token s MINUS
      | '*' -> add_token s STAR
      | ';' -> add_token s SEMICOLON
      | '!' -> (
          match match_char s '=' with
          | true, s -> add_token s Token.BANG_EQUAL
          | false, s -> add_token s Token.BANG)
      | '=' -> (
          match match_char s '=' with
          | true, s -> add_token s Token.EQUAL_EQUAL
          | false, s -> add_token s Token.EQUAL)
      | '<' -> (
          match match_char s '=' with
          | true, s -> add_token s Token.LESS_EQUAL
          | false, s -> add_token s Token.LESS)
      | '>' -> (
          match match_char s '=' with
          | true, s -> add_token s Token.GREATER_EQUAL
          | false, s -> add_token s Token.GREATER)
      | '/' -> (
          match match_char s '/' with
          | true, s -> skip_to_newline s
          | false, s -> add_token s Token.SLASH)
      | ' ' -> s
      | '\r' -> s
      | '\t' -> s
      | '\n' -> { s with line = s.line + 1 }
      | '"' -> parse_string s
      | c when is_digit c -> parse_number s
      | c when is_alpha c -> parse_identifier s
      | _ ->
          Errors.error s.line "Unexpected character.";
          { s with errors = "Unexpected character." :: s.errors }
    in

    scan_h s
  end

let scan source =
  scan_h
    {
      source;
      source_len = String.length source;
      start = 0;
      current = 0;
      line = 1;
      tokens = [];
      errors = [];
    }

(* Previous imperative version, keeping for science *)

(* let scann source = *)
(* let hadError = ref false in *)
(* let total = String.length source in *)
(* let start = ref 0 in *)
(* let current = ref 0 in *)
(* let line = ref 1 in *)
(* let tokens : Token.t list ref = ref [] in *)

(* let isAtEnd () = !current >= total in *)

(* let scanToken () = *)
(* let advance () = *)
(* let c = String.get source !current in *)
(* current := !current + 1; *)
(* c *)
(* in *)

(* let addToken (kind : Token.kind) = *)
(* let text = String.sub source !start (!current - !start) in *)
(* tokens := { kind; lexeme = text; line = !line } :: !tokens *)
(* in *)

(* let expect expected = *)
(* if isAtEnd () then false *)
(* else if String.get source !current <> expected then false *)
(* else begin *)
(* current := !current + 1; *)
(* true *)
(* end *)
(* in *)

(* let peek () = *)
(* if isAtEnd () then None else Some (String.get source !current) *)
(* in *)

(* let peekNext () = *)
(* if !current + 1 >= total then None *)
(* else Some (String.get source (!current + 1)) *)
(* in *)

(* let rec skip_to_newline () = *)
(* match peek () with *)
(* | Some '\n' | None -> () *)
(* | Some _ -> *)
(* ignore (advance ()); *)
(* skip_to_newline () *)
(* in *)

(* let parse_string () = *)
(* while peek () <> Some '"' && not (isAtEnd ()) do *)
(* if peek () = Some '\n' then line := !line + 1; *)
(* ignore (advance ()) *)
(* done; *)

(* if isAtEnd () then begin *)
(* error !line "Unterminated string."; *)
(* hadError := true *)
(* end *)
(* else begin *)
(* ignore (advance ()); *)

(* let value = String.sub source (!start + 1) (!current - !start - 2) in *)
(* addToken (STRING value) *)
(* end *)
(* in *)

(* let is_digit c = c >= '0' && c <= '9' in *)

(* let is_alpha c = *)
(* (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' *)
(* in *)

(* let is_alpha_numeric c = is_alpha c || is_digit c in *)

(* let parse_number () = *)
(* let rec skip_number () = *)
(* match peek () with *)
(* | Some c when is_digit c -> *)
(* ignore (advance ()); *)
(* skip_number () *)
(* | _ -> () *)
(* in *)

(* skip_number (); *)

(* if peek () = Some '.' then *)
(* match peekNext () with *)
(* | Some c when is_digit c -> *)
(* ignore (advance ()); *)
(* skip_number () *)
(* | _ -> () *)
(* else (); *)

(* let text = String.sub source !start (!current - !start) in *)
(* addToken (NUMBER (float_of_string text)) *)
(* in *)

(* let parse_identifier () = *)
(* let rec skip_ident () = *)
(* match peek () with *)
(* | Some c when is_alpha_numeric c -> *)
(* ignore (advance ()); *)
(* skip_ident () *)
(* | _ -> () *)
(* in *)

(* skip_ident (); *)

(* let text = String.sub source !start (!current - !start) in *)
(* match StringMap.find_opt text keywords with *)
(* | Some t -> addToken t *)
(* | _ -> addToken (IDENTIFIER text) *)
(* in *)

(* let c = advance () in *)
(* match c with *)
(* | '(' -> addToken LEFT_PAREN *)
(* | ')' -> addToken RIGHT_PAREN *)
(* | '{' -> addToken LEFT_BRACE *)
(* | '}' -> addToken RIGHT_BRACE *)
(* | ',' -> addToken COMMA *)
(* | '.' -> addToken DOT *)
(* | '+' -> addToken PLUS *)
(* | '-' -> addToken MINUS *)
(* | '*' -> addToken STAR *)
(* | ';' -> addToken SEMICOLON *)
(* | '!' -> addToken (if expect '=' then BANG_EQUAL else BANG) *)
(* | '=' -> addToken (if expect '=' then EQUAL_EQUAL else EQUAL) *)
(* | '<' -> addToken (if expect '=' then LESS_EQUAL else LESS) *)
(* | '>' -> addToken (if expect '=' then GREATER_EQUAL else GREATER) *)
(* | '/' -> if expect '/' then skip_to_newline () else addToken SLASH *)
(* | ' ' -> () *)
(* | '\r' -> () *)
(* | '\t' -> () *)
(* | '\n' -> line := !line + 1 *)
(* | '"' -> parse_string () *)
(* | c when is_digit c -> parse_number () *)
(* | c when is_alpha c -> parse_identifier () *)
(* | _ -> *)
(* error !line "Unexpected character."; *)
(* hadError := true *)
(* in *)

(* while not (isAtEnd ()) do *)
(* start := !current; *)
(* scanToken () *)
(* done; *)

(* tokens := { kind = EOF; lexeme = ""; line = !line } :: !tokens; *)

(* List.rev !tokens *)
