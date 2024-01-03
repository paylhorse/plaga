(* PLAGA AST using ADT *)
open Token

type node =
  | StatementNode of statement
  | ExpressionNode of expression

and statement =
  | BindStatement of { token : token; name : string; value : expression }
  | ReturnStatement of { token : token; return_value : expression }
  | ExpressionStatement of { token : token; expression : expression }
  | BlockStatement of { token : token; statements : statement list }
  | EmptyStatement

and expression =
  | Identifier of { token : token; value : string }
  | IntegerLiteral of { token : token; value : int }
  | PrefixExpression of { token : token; operator : string; right: expression; }
  | InfixExpression of { token : token; operator : string; left: expression; right: expression; }
  | Boolean of { token : token; value : bool; }
  | IfExpression of { token : token; condition : expression; consequence : statement; alternative : statement; }
  | FunctionLiteral of { token : token; parameters : expression list; body : statement }
  | EmptyExpression

type program = {
  statements : statement list;
}

(* --- DEBUG UTILITY --- *)
let token_literal_of_statement = function
  | BindStatement { token; _ } -> token.literal
  | ReturnStatement { token; _ } -> token.literal
  | ExpressionStatement { token; _ } -> token.literal
  | BlockStatement { token; _ } -> token.literal
  | EmptyStatement -> "o_o"

let token_literal_of_expression = function
  | Identifier { token; _ } -> token.literal
  | IntegerLiteral { token; _ } -> token.literal
  | PrefixExpression { token; _ } -> token.literal
  | InfixExpression { token; _ } -> token.literal
  | Boolean { token; _ } -> token.literal
  | IfExpression { token; _ } -> token.literal
  | FunctionLiteral { token; _ } -> token.literal
  | EmptyExpression -> "o_o"

let program_token_literal p =
  match p.statements with
  | [] -> ""
  | first_statement :: _ -> token_literal_of_statement first_statement

(* TODO: proper string representation ;( *)
let rec string_of_statement = function
  | BindStatement { name; value; _ } ->
    Printf.sprintf "🗞  Bind Statement: (name: %s, value: %s)" name (token_literal_of_expression value)
  | ReturnStatement { return_value; _ } ->
    Printf.sprintf "🗞  Return Statement: (value: %s)" (token_literal_of_expression return_value)
  | ExpressionStatement { expression; _ } ->
    Printf.sprintf "🗞  Expression Statement: (expression: %s)" (token_literal_of_expression expression)
  | BlockStatement { statements; _ } ->
    (let strings = List.map (fun stmt ->
      "🗞  Block Statement: " ^ string_of_statement stmt
    ) statements in
    String.concat "\n" strings)
  | EmptyStatement -> "🗞  Empty Statement o_o"

and string_of_expression = function
  | Identifier { value; _ } ->
    Printf.sprintf "-> Identifier: (value: %s)" value
  | IntegerLiteral { value; _ } ->
    Printf.sprintf "-> Integer Literal: (value: %d)" value
  | PrefixExpression { operator; _ } ->
    Printf.sprintf "-> Prefix Expression: (operator: %s)" operator
  | InfixExpression { left; operator; right; _ } ->
    Printf.sprintf "-> Infix Expression: (left: %s, operator: %s, right: %s)" (string_of_expression left) operator (string_of_expression right)
  | Boolean { value; _ } ->
    Printf.sprintf "->  Boolean: (value: %b)" value
  | IfExpression { condition; consequence; alternative; _ } ->
    Printf.sprintf "-> If Expression: (condition: %s, consequence: %s, alternative: %s)" (string_of_expression condition) (string_of_statement consequence) (string_of_statement alternative)
  | FunctionLiteral { parameters; body; _ } ->
    let param_strings = List.map (fun param -> string_of_expression param) parameters in
    let params = String.concat ", " param_strings in
    let body_str = string_of_statement body in
    "-> Function Literal: (parameters: [" ^ params ^ "], body: " ^ body_str ^ ")"
  | EmptyExpression ->
    "-> Empty Expression o_o"

let string_of_program program =
  let statement_strings = List.map string_of_statement program.statements in
  let str = String.concat "\n" statement_strings in
  str
