open Token
open Lexer

let () = print_endline "Starting Lexer Test..."

let test_next_token () =
  let input = "=+(){},;" in
  let expected_tokens = [
    (Assign, "=");
    (Plus, "+");
    (LParen, "(");
    (RParen, ")");
    (LBrace, "{");
    (RBrace, "}");
    (Comma, ",");
    (Semicolon, ";");
    (EOF, "");
  ] in
  let lexer = Lexer.new_instance input in

  List.iteri (fun i (expected_type, expected_literal) ->
    let token = Lexer.next_token lexer in
    Alcotest.(check token_type) ("Type test for token " ^ string_of_int i)
      expected_type token.typ;
    Alcotest.(check string) ("Literal test for token " ^ string_of_int i)
      expected_literal token.literal;
  ) expected_tokens

let () =
  let open Alcotest in
  run "Lexer Tests" [
    "test_next_token", [test_case "Next token" `Quick test_next_token];
  ]
