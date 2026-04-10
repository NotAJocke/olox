open Olox

let printUsage () = print_endline "Usage: olox [script]"

let run source =
  let tokens, errors = Scanner.scan source in
  let expression = Parser.parse tokens in

  if List.length errors > 0 || Option.is_none expression then ()
  else print_endline @@ Ast.print_ast @@ Option.get expression

(* for i = 0 to List.length tokens - 1 do *)
(* let token = List.nth tokens i in *)
(* print_endline (Token.to_string token) *)
(* done *)

let runFile filename =
  print_endline ("Running file: " ^ filename);

  let source = In_channel.with_open_bin filename In_channel.input_all in
  run source

let rec runPrompt () =
  print_string "> ";
  flush stdout;

  match In_channel.input_line stdin with
  | None -> ()
  | Some line ->
      run line;
      runPrompt ()

let () =
  (* let ex = *)
  (* Ast.Binary *)
  (* ( Ast.Unary *)
  (* ( { kind = Token.MINUS; lexeme = "-"; line = 1 }, *)
  (* Ast.Literal (Ast.Number 123.0) ), *)
  (* { kind = Token.STAR; lexeme = "*"; line = 1 }, *)
  (* Ast.Grouping (Ast.Literal (Ast.Number 45.67)) ) *)
  (* in *)

  (* print_endline @@ Ast.print_ast ex *)
  let args = Sys.argv in

  match Array.length args with
  | 1 -> runPrompt ()
  | 2 -> runFile args.(1)
  | _ -> printUsage ()
