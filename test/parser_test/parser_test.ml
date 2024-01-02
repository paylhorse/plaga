open Lexer
open Ast
open Parser

let check_parser_errors parser =
  if List.length parser.errors > 0 then begin
    print_endline "PARSER ERRORS as follows:";
    List.iter print_endline parser.errors;
    Alcotest.fail "Parser errors!"
  end

(* --- BIND TEST --- *)
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

(* --- RETURN TEST --- *)
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

  let num_expected = 3
  in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ReturnStatement _ -> ()
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a ReturnStatement")
  ) program.statements

(* --- IDENT TEST --- *)
let test_identifier_expression () =
  let input = "foobar;"
  in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1
  in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ExpressionStatement { expression; _ } ->
      begin match expression with
        | Identifier _ -> ()
        | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a Identifier")
      end
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a ExpressionStatement")
  ) program.statements

(* --- INTEGER LITERAL TEST --- *)
let test_integer_literal_expression () =
  let input = "4;"
  in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1
  in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ExpressionStatement { expression; _ } ->
      begin match expression with
      | IntegerLiteral { value; _ } ->
        begin
          if value != 4 then Alcotest.fail ("IntegerLiteral value " ^ string_of_int value ^ " is wrong!");
        end
      | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a IntegerLiteral")
      end
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a ExpressionStatement")
  ) program.statements

(* --- PREFIX TEST --- *)
let test_parsing_prefix_expressions () =
  let prefix_tests = [
    ("!4;", "!", 4);
    ("-14;", "-", 14);
  ] in

  List.iter (fun (input, expected_operator, expected_value) ->
    let lexer = new_lexer input in
    let parser = new_parser lexer in
    let program = parse_program parser in

    let errors = get_errors parser in
    (match errors with
    | [] -> ()
    | _ -> Alcotest.fail ("Parser errors: " ^ String.concat ", " errors));

    let num_statements = List.length program.statements in
    Alcotest.(check int) "Number of statements" 1 num_statements;

    match program.statements with
    | [ExpressionStatement { expression = PrefixExpression { operator; right; _ }; _ }] ->
      Alcotest.(check string) "Operator" expected_operator operator;
      (match right with
      | IntegerLiteral { value; _ } when value = expected_value -> ()
      | _ -> Alcotest.fail "Right expression of PrefixExpression is not as expected")
    | _ -> Alcotest.fail "First statement is not an ExpressionStatement with PrefixExpression"
  ) prefix_tests

(* --- INFIX TEST --- *)
let test_parsing_infix_expressions () =
  let infix_tests = [
    ("4 + 4;", 4, "+", 4);
    ("4 - 4;", 4, "-", 4);
    ("4 * 4;", 4, "*", 4);
    ("4 / 4;", 4, "/", 4);
    ("4 > 4;", 4, ">", 4);
    ("4 < 4;", 4, "<", 4);
    ("4 == 4;", 4, "==", 4);
    ("4 != 4;", 4, "!=", 4);
  ] in

  List.iter (fun (input, expected_left_value, expected_operator, expected_right_value) ->
    let lexer = new_lexer input in
    let parser = new_parser lexer in
    let program = parse_program parser in

    let errors = get_errors parser in
    (match errors with
    | [] -> ()
    | _ -> Alcotest.fail ("Parser errors: " ^ String.concat ", " errors));

    let num_statements = List.length program.statements in
    Alcotest.(check int) "Number of statements" 1 num_statements;

    match program.statements with
    | [ExpressionStatement { expression = InfixExpression { operator; left; right; _ }; _ }] ->
      Alcotest.(check string) "Operator" expected_operator operator;
      (match left with
      | IntegerLiteral { value; _ } when value = expected_left_value -> ()
      | _ -> Alcotest.fail "Right expression of InfixExpression is not as expected");
      (match right with
      | IntegerLiteral { value; _ } when value = expected_right_value -> ()
      | _ -> Alcotest.fail "Right expression of InfixExpression is not as expected")
    | _ -> Alcotest.fail "First statement is not an ExpressionStatement with InfixExpression"
  ) infix_tests

let () =
  let open Alcotest in
  run "Parser Tests" [
    "BIND", [test_case "Bind statements" `Quick test_bind_statements];
    "RETURN", [test_case "Return statements" `Quick test_return_statements];
    "IDENT", [test_case "Identifier expression" `Quick test_identifier_expression];
    "INTEGER LITERAL", [test_case "IntegerLiteral expression" `Quick test_integer_literal_expression];
    "PREFIX", [test_case "Parsing prefix expressions" `Quick test_parsing_prefix_expressions];
    "INFIX", [test_case "Parsing infix expressions" `Quick test_parsing_infix_expressions];
  ]
