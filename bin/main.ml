open Olox

let printUsage () = print_endline "Usage: olox [script]"

let run source =
  let tokens = Scanner.scan source in

  for i = 0 to List.length tokens - 1 do
    let token = List.nth tokens i in
    print_endline (Token.string_of_token token)
  done

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
  let args = Sys.argv in

  match Array.length args with
  | 1 -> runPrompt ()
  | 2 -> runFile args.(1)
  | _ -> printUsage ()
