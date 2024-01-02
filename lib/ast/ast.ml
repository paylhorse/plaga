(* PLAGA AST using ADT *)
open Token

type node =
  | StatementNode of statement
  | ExpressionNode of expression

and statement =
  | BindStatement of { token : token; name : string; value : expression }
  | ReturnStatement of { token : token; return_value : expression }
  | ExpressionStatement of { token : token; expression : expression }
  | EmptyStatement

and expression =
  | Identifier of { token : token; value : string }
  | IntegerLiteral of { token : token; value : int }
  | PrefixExpression of { token : token; operator : string; right: expression; }
  | InfixExpression of { token : token; operator : string; left: expression; right: expression; }
  | EmptyExpression

type program = {
  statements : statement list;
}

(* --- DEBUG UTILITY --- *)
let token_literal_of_statement = function
  | BindStatement { token; _ } -> token.literal
  | ReturnStatement { token; _ } -> token.literal
  | ExpressionStatement { token; _ } -> token.literal
  | EmptyStatement -> ""

let token_literal_of_expression = function
  | Identifier { token; _ } -> token.literal
  | IntegerLiteral { token; _ } -> token.literal
  | PrefixExpression { token; _ } -> token.literal
  | InfixExpression { token; _ } -> token.literal
  | EmptyExpression -> ""

let program_token_literal p =
  match p.statements with
  | [] -> ""
  | first_statement :: _ -> token_literal_of_statement first_statement

let rec string_of_statement = function
  | BindStatement { name; value; _ } ->
      Printf.sprintf "BindStatement(name: %s, value: %s)" name (string_of_expression value)
  | ReturnStatement { return_value; _ } ->
      Printf.sprintf "ReturnStatement(value: %s)" (string_of_expression return_value)
  | ExpressionStatement { expression; _ } ->
      Printf.sprintf "ExpressionStatement(expression: %s)" (string_of_expression expression)
  | EmptyStatement ->
      "EmptyStatement"

and string_of_expression = function
  | Identifier { value; _ } ->
      Printf.sprintf "Identifier(value: %s)" value
  | IntegerLiteral { value; _ } ->
      Printf.sprintf "IntegerLiteral(value: %d)" value
  | PrefixExpression { operator; _ } ->
      Printf.sprintf "PrefixExpression(value: %s)" operator
  | InfixExpression { operator; _ } ->
      Printf.sprintf "IntegerLiteral(value: %s)" operator
  | EmptyExpression ->
      "EmptyExpression"

let string_of_program program =
  let statement_strings = List.map string_of_statement program.statements in
  let str = String.concat "\n" statement_strings in
  str
