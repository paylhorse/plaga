open Token
open Ast
open Alcotest

let test_string_representation () =
  let program = {
    statements = [
      BindStatement {
        token = { typ = Some Bind; literal = "bind" };
        name = "myVar";
        value = Identifier {
          token = { typ = Some Ident; literal = "anotherVar" };
          value = "anotherVar"
        }
      }
    ]
  } in
  let expected = "\240\159\151\158  Bind Statement: (name: myVar, value: anotherVar)" in
  let program_str = string_of_program program in
  check string "String representation" expected program_str

let () =
  let open Alcotest in
  run "AST Tests" [
    "string_representation", [test_case "Program string representation" `Quick test_string_representation];
  ]
