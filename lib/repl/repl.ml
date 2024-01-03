(* PLAGA REPL *)
(* open Token *)
open Lexer
open Ast
open Parser

(* let start_repl () = *)
(*   let rec repl () = *)
(*     print_string "☞  "; *)
(*     flush stdout; *)
(*     try *)
(*       let line = input_line stdin in *)
(*       let lexer = new_lexer line in *)
(*       let rec print_tokens () = *)
(*         let token = next_token lexer in *)
(*         match token.typ with *)
(*         | Some EOF -> () *)
(*         | _ -> Printf.printf "%s\n" (string_of_token_type token.typ); print_tokens () *)
(*       in *)
(*       print_tokens (); *)
(*       repl () *)
(*     with End_of_file -> () *)
(*   in *)
(*   repl () *)

let start_repl () =
  let rec repl () =
    print_string "☞  ";
    flush stdout;
    try
      let line = input_line stdin in
      let lexer = new_lexer line in
      let parser = new_parser lexer in
      let program = parse_program parser in
      let errors = get_errors parser in
      match errors with
      | [] ->
        print_program program; repl ()
      | _ ->
        print_endline ("Oh no, your input is infected! " ^ "\n" ^ String.concat "\n" errors); repl ()
    with
    | End_of_file -> ()
    | exn -> print_endline ("Your input created a monster: " ^ "\n" ^ Printexc.to_string exn); repl ()
  in
  repl ()
