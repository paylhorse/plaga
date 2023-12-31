open Lexer
open Ast
open Parser

let check_parser_errors parser =
  if List.length parser.errors > 0 then begin
    print_endline "PARSER ERRORS as follows:";
    List.iter print_endline parser.errors;
    Alcotest.fail "Parser errors!"
  end

let test_bind_statements () =
  let input =
    "bind x = 4;
     bind y = 10;
     bind foobar = 646464;"
  in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let expected_identifiers = ["x"; "y"; "foobar"] in
  let num_statements = List.length program.statements in
  let num_expected = List.length expected_identifiers in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i expected_id ->
    match List.nth program.statements i with
    | BindStatement { name; _ } ->
      Alcotest.(check string) ("Identifier in statement " ^ string_of_int i) expected_id name
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a BindStatement")
  ) expected_identifiers

let test_return_statements () =
  let input =
    "return 4;
     return 10;
     return 646464;"
  in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 3  (* Expected number of return statements *)
  in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ReturnStatement _ -> ()  (* This is expected, do nothing *)
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a ReturnStatement")
  ) program.statements

let () =
  let open Alcotest in
  run "Parser Tests" [
    "test_bind_statements", [test_case "Bind statements" `Quick test_bind_statements];
    "test_return_statements", [test_case "Return statements" `Quick test_return_statements];
  ]
