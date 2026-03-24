open Alcotest
open Olox

let kind_testable =
  Alcotest.testable (fun ppf v -> Fmt.string ppf (Token.string_of_kind v)) ( = )

(* Helper to run the scanner and extract kinds for comparison *)
let get_kinds input =
  let tokens, _ = Scanner.scan input in
  List.map (fun (t : Token.t) -> t.kind) tokens

let test_basic_ops_and_grouping () =
  let input = "(){},.-+* != == <= >= ! = < > /" in
  let expected =
    Token.
      [
        LEFT_PAREN;
        RIGHT_PAREN;
        LEFT_BRACE;
        RIGHT_BRACE;
        COMMA;
        DOT;
        MINUS;
        PLUS;
        STAR;
        BANG_EQUAL;
        EQUAL_EQUAL;
        LESS_EQUAL;
        GREATER_EQUAL;
        BANG;
        EQUAL;
        LESS;
        GREATER;
        SLASH;
        EOF;
      ]
  in

  let kinds = get_kinds input in

  check (list kind_testable) "token list matches" expected kinds

let test_string_literals () =
  let input = "\"hello\" \"\" \"multi\nline\"" in
  let expected =
    Token.[ STRING "hello"; STRING ""; STRING "multi\nline"; EOF ]
  in

  check (list kind_testable) "strings" expected (get_kinds input)

let test_numbers () =
  let input = "123 123.456" in
  let expected = Token.[ NUMBER 123.0; NUMBER 123.456; EOF ] in
  check (list kind_testable) "numbers" expected (get_kinds input)

let test_identifiers_keywords () =
  let input = "var language = \"lox\";" in
  let expected =
    Token.[ VAR; IDENTIFIER "language"; EQUAL; STRING "lox"; SEMICOLON; EOF ]
  in
  check (list kind_testable) "identifiers and keywords" expected
    (get_kinds input)

let test_comments () =
  let input = "(( )) // hidden\n !" in
  let expected =
    Token.[ LEFT_PAREN; LEFT_PAREN; RIGHT_PAREN; RIGHT_PAREN; BANG; EOF ]
  in
  check (list kind_testable) "comments and whitespace" expected
    (get_kinds input)

let () =
  run "Olox"
    [
      ( "lexer",
        [
          test_case "Operators" `Quick test_basic_ops_and_grouping;
          test_case "Strings" `Quick test_string_literals;
          test_case "Numbers" `Quick test_numbers;
          test_case "ID_Key" `Quick test_identifiers_keywords;
          test_case "Comments" `Quick test_comments;
        ] );
    ]
