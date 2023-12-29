open Token
open Lexer

let start_repl () =
  let rec repl () =
    print_string "☞  ";
    flush stdout;
    try
      let line = input_line stdin in
      let lexer = new_lexer line in
      let rec print_tokens () =
        let token = next_token lexer in
        match token.typ with
        | Some EOF -> ()
        | _ -> Printf.printf "%s\n" (string_of_token_type token.typ); print_tokens ()
      in
      print_tokens ();
      repl ()
    with End_of_file -> ()
  in
  repl ()
