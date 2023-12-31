open Lexer
open Ast
open Parser

let check_parser_errors parser =
  if List.length parser.errors > 0 then begin
    print_endline "PARSER ERRORS as follows:";
    List.iter print_endline parser.errors;
    Alcotest.fail "Parser errors!"
  end

(* --- HELPERS --- *)
let test_identifier expression expected_value =
  match expression with
  | Identifier { value; _ } when value = expected_value -> ()
  | _ -> Alcotest.fail ("Identifier test failed!")

let test_integer_literal expression expected_value =
  match expression with
  | IntegerLiteral { value; _ } when value = expected_value -> ()
  | _ -> Alcotest.fail ("IntegerLiteral test failed with value " ^ string_of_int expected_value)

let test_boolean_literal expression expected_value =
  match expression with
  | Boolean { value; _ } when value = expected_value -> ()
  | _ -> Alcotest.fail ("BooleanLiteral test failed with value " ^ string_of_bool expected_value)

let test_literal_expression expression expected_value =
  match expected_value with
  | `Int value -> test_integer_literal expression value
  | `String value -> test_identifier expression value
  | `Bool value -> test_boolean_literal expression value

let test_infix_expression expression left_expected operator_expected right_expected =
  match expression with
  | InfixExpression { operator; left; right; _ } ->
    if operator <> operator_expected then
      Alcotest.fail ("Expected operator " ^ operator_expected ^ ", got " ^ operator)
    else begin
      test_literal_expression left left_expected;
      test_literal_expression right right_expected
    end
  | _ -> Alcotest.fail "Expression is not an InfixExpression"

(* --- BIND TEST --- *)
let test_bind_statements () =
  let bind_tests = [
    ("bind x # 4;", "x", (`Int 4));
    ("bind y # true;", "y", (`Bool true));
    ("bind foobar # y;", "foobar", (`String "y"));
  ] in

  List.iter (fun (input, expected_name, expected_value) ->
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
    | [BindStatement { name; value; _ }] ->
      Alcotest.(check string) "Name" expected_name name;
      test_literal_expression value expected_value;
    | _ -> Alcotest.fail "Statement is not a BindStatement"
  ) bind_tests

(* --- RETURN TEST --- *)
let test_return_statements () =
  let return_tests = [
    ("return 4;", (`Int 4));
    ("return true;", (`Bool true));
    ("return foobar;", (`String "foobar"));
  ] in

  List.iter (fun (input, expected_value) ->
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
    | [ReturnStatement { return_value; _ }] ->
      test_literal_expression return_value expected_value;
    | _ -> Alcotest.fail "Statement is not a ReturnStatement"
  ) return_tests

(* --- IDENT TEST --- *)
let test_identifier_expression () =
  let input = "foobar;" in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1 in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ExpressionStatement { expression; _ } ->
      begin match expression with
        | Identifier _ -> ()
        | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a Identifier")
      end
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not an ExpressionStatement")
  ) program.statements

(* --- INTEGER LITERAL TEST --- *)
let test_integer_literal_expression () =
  let input = "4;" in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1 in
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
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not an ExpressionStatement")
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
    ("4 = 4;", 4, "=", 4);
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

(* --- BOOLEAN TEST --- *)
let test_boolean_expression () =
  let input = "(true); false; true; false;" in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 4 in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ExpressionStatement { expression; _ } ->
      begin match expression with
        | Boolean _ -> ()
        | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not a Boolean")
      end
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not an ExpressionStatement")
  ) program.statements

(* --- IF TEST --- *)
let test_if_expression () =
  let input = "if ( x < y ) { x }" in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1 in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ExpressionStatement { expression; _ } ->
      (match expression with
        | IfExpression { condition; consequence; _ } ->
          (test_infix_expression condition (`String "x") "<" (`String "y");
          match consequence with
          | BlockStatement { statements; _ } when List.length statements = 1 ->
            (match List.hd statements with
             | ExpressionStatement { expression; _ } -> test_identifier expression "x";
             | _ -> Alcotest.fail ("Consequence cocked!"))
          | _ -> Alcotest.fail ("Consequence cocked!"))
        | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not an IfExpression"))
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not an ExpressionStatement")
  ) program.statements

(* --- IF ELSE TEST --- *)
let test_if_else_expression () =
  let input = "if ( x < y ) { x } else { y }" in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1 in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  List.iteri (fun i stmt ->
    match stmt with
    | ExpressionStatement { expression; _ } ->
      (match expression with
      | IfExpression { condition; consequence; alternative; _ } ->
        (test_infix_expression condition (`String "x") "<" (`String "y");
        (match consequence with
        | BlockStatement { statements; _ } when List.length statements = 1 ->
          (match List.hd statements with
          | ExpressionStatement { expression; _ } -> test_identifier expression "x";
          | _ -> Alcotest.fail ("Consequence cocked!"))
        | _ -> Alcotest.fail ("Consequence cocked!"));
        (match alternative with
        | BlockStatement { statements; _ } when List.length statements = 1 ->
          (match List.hd statements with
          | ExpressionStatement { expression; _ } -> test_identifier expression "y";
          | _ -> Alcotest.fail ("Alternative cocked!"))
        | _ -> Alcotest.fail ("Alternative cocked!")))
      | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not an IfExpression"))
    | _ -> Alcotest.fail ("Statement " ^ string_of_int i ^ " is not an ExpressionStatement")
  ) program.statements

(* --- FUNCTION LITERAL TEST --- *)
let test_function_literal_expression () =
  let input = "func(x, y) { x + y; }" in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1 in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  match program.statements with
  | [ExpressionStatement { expression; _ }] -> (
      match expression with
      | FunctionLiteral { parameters; body; _ } ->
        let param_names = List.map (function
          | Identifier { value; _ } -> value
          | _ -> Alcotest.fail "Parameter is not an Identifier"
        ) parameters in
        Alcotest.(check (list string)) "Function parameters" ["x"; "y"] param_names;

        (match body with
        | BlockStatement { statements; _ } when List.length statements = 1 ->
          (match List.hd statements with
          | ExpressionStatement { expression = InfixExpression { left; operator; right; _ }; _ } ->
            (* Test left operand *)
            (match left with
            | Identifier { value; _ } when value = "x" -> ()
            | _ -> Alcotest.fail "Left operand of infix expression is wrong");

            Alcotest.(check string) "Operator" "+" operator;

            (match right with
            | Identifier { value; _ } when value = "y" -> ()
            | _ -> Alcotest.fail "Right operand of infix expression is wrong")
          | _ -> Alcotest.fail "Body statement is not an ExpressionStatement")
        | _ -> Alcotest.fail "Body is not a BlockStatement")
      | _ -> Alcotest.fail "Expression is not a FunctionLiteral")
  | _ -> Alcotest.fail "Not a single ExpressionStatement"

(* --- CALL EXPRESSION TEST --- *)
let test_call_expression () =
  let input = "add(1, 2 * 3, 4 + 5);" in
  let lexer = new_lexer input in
  let parser = new_parser lexer in
  let program = parse_program parser in

  check_parser_errors parser;

  let num_expected = 1 in
  let num_statements = List.length program.statements in
  Alcotest.(check int) "Number of statements" num_expected num_statements;

  match program.statements with
  | [ExpressionStatement { expression; _ }] -> (
      match expression with
      | CallExpression { func; arguments; _ } ->
        Alcotest.(check int) "Number of arguments" 3 (List.length arguments);
        (match func with
        | Identifier { value; _ } -> Alcotest.(check string) "Function name" "add" value
        | _ -> Alcotest.fail "Function is not an Identifier");

        (match List.nth arguments 0 with
        | IntegerLiteral { value; _ } -> Alcotest.(check int) "First argument" 1 value
        | _ -> Alcotest.fail "First argument is not an IntegerLiteral");

        (match List.nth arguments 1 with
        | InfixExpression { left; operator; right; _ } ->
          test_literal_expression left (`Int 2);
          Alcotest.(check string) "Operator in first infix expression" "*" operator;
          test_literal_expression right (`Int 3);
        | _ -> Alcotest.fail "Second argument is not an InfixExpression");

        (match List.nth arguments 2 with
        | InfixExpression { left; operator; right; _ } ->
          test_literal_expression left (`Int 4);
          Alcotest.(check string) "Operator in second infix expression" "+" operator;
          test_literal_expression right (`Int 5);
        | _ -> Alcotest.fail "Third argument is not an InfixExpression");
      | _ -> Alcotest.fail "Expression is not a CallExpression")
  | _ -> Alcotest.fail "Not a single ExpressionStatement"

let () =
  let open Alcotest in
  run "Parser Tests" [
    "BIND", [test_case "Parsing bind statements" `Quick test_bind_statements];
    "RETURN", [test_case "Parsing return statements" `Quick test_return_statements];
    "IDENT", [test_case "Parsing identifier expression" `Quick test_identifier_expression];
    "INTEGER LITERAL", [test_case "Parsing integer literal expression" `Quick test_integer_literal_expression];
    "PREFIX", [test_case "Parsing prefix expressions" `Quick test_parsing_prefix_expressions];
    "INFIX", [test_case "Parsing infix expressions" `Quick test_parsing_infix_expressions];
    "BOOLEAN", [test_case "Parsing boolean expressions" `Quick test_boolean_expression];
    "IF", [test_case "Parsing if expressions" `Quick test_if_expression];
    "IF ELSE", [test_case "Parsing if else expressions" `Quick test_if_else_expression];
    "FUNCTION LITERAL", [test_case "Parsing function literal expressions" `Quick test_function_literal_expression];
    "CALL EXPRESSION", [test_case "Parsing call expressions" `Quick test_call_expression];
  ]
