open Token
open Lexer

let () = print_endline "Starting Lexer Test..."

let test_next_token () =
  let input =
    "let four = 4;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(four, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;"
  in
  let expected_tokens = [
    (Some(Let), "let");
    (Some(Ident), "four");
    (Some(Assign), "=");
    (Some(Int), "4");
    (Some(Semicolon), ";");
    (Some(Let), "let");
    (Some(Ident), "ten");
    (Some(Assign), "=");
    (Some(Int), "10");
    (Some(Semicolon), ";");
    (Some(Let), "let");
    (Some(Ident), "add");
    (Some(Assign), "=");
    (Some(Function), "fn");
    (Some(LParen), "(");
    (Some(Ident), "x");
    (Some(Comma), ",");
    (Some(Ident), "y");
    (Some(RParen), ")");
    (Some(LBrace), "{");
    (Some(Ident), "x");
    (Some(Plus), "+");
    (Some(Ident), "y");
    (Some(Semicolon), ";");
    (Some(RBrace), "}");
    (Some(Semicolon), ";");
    (Some(Let), "let");
    (Some(Ident), "result");
    (Some(Assign), "=");
    (Some(Ident), "add");
    (Some(LParen), "(");
    (Some(Ident), "four");
    (Some(Comma), ",");
    (Some(Ident), "ten");
    (Some(RParen), ")");
    (Some(Semicolon), ";");
    (Some(Bang), "!");
    (Some(Minus), "-");
    (Some(Slash), "/");
    (Some(Asterisk), "*");
    (Some(Int), "5");
    (Some(Semicolon), ";");
    (Some(Int), "5");
    (Some(LT), "<");
    (Some(Int), "10");
    (Some(GT), ">");
    (Some(Int), "5");
    (Some(Semicolon), ";");
    (Some(If), "if");
    (Some(LParen), "(");
    (Some(Int), "5");
    (Some(LT), "<");
    (Some(Int), "10");
    (Some(RParen), ")");
    (Some(LBrace), "{");
    (Some(Return), "return");
    (Some(True), "true");
    (Some(Semicolon), ";");
    (Some(RBrace), "}");
    (Some(Else), "else");
    (Some(LBrace), "{");
    (Some(Return), "return");
    (Some(False), "false");
    (Some(Semicolon), ";");
    (Some(RBrace), "}");
    (Some(Int), "10");
    (Some(EQ), "==");
    (Some(Int), "10");
    (Some(Semicolon), ";");
    (Some(Int), "10");
    (Some(Not_EQ), "!=");
    (Some(Int), "9");
    (Some(Semicolon), ";");
    (Some(EOF), "");
  ] in
  let lexer = new_instance input in

  List.iteri (fun i (expected_type, expected_literal) ->
    let token = next_token lexer in
    let token_type_testable = Alcotest.of_pp (fun fmt t -> Format.fprintf fmt "%s" (string_of_token_type t)) in
    Alcotest.(check token_type_testable) ("Type test for token " ^ string_of_int i)
      expected_type token.typ;
    Alcotest.(check string) ("Literal test for token " ^ string_of_int i)
      expected_literal token.literal;
  ) expected_tokens

let () =
  let open Alcotest in
  run "Lexer Tests" [
    "test_next_token", [test_case "Next token" `Quick test_next_token];
  ]