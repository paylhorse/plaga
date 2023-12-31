(* PLAGA PARSER *)
open Token
open Lexer
open Ast

type precedence =
  | Lowest
  | Equals      (* == *)
  | LessGreater (* > or < *)
  | Sum         (* + *)
  | Product     (* * *)
  | Prefix      (* -X or !X *)
  | Call        (* myFunction(X) *)

let precedence_value = function
  | Lowest -> 1
  | Equals -> 2
  | LessGreater -> 3
  | Sum -> 4
  | Product -> 5
  | Prefix -> 6
  | Call -> 7

type prefix_parse_fn = unit -> expression
type infix_parse_fn = expression -> expression

module TokenType = struct
  type t = token_type
  let compare = compare
end

module PrefixParseFnMap = Map.Make (TokenType)
module InfixParseFnMap = Map.Make (TokenType)

type parser = {
    lexer: lexer;
    mutable curr_token: token option;
    mutable peek_token: token option;
    mutable errors: string list;
    prefix_parse_fns: prefix_parse_fn PrefixParseFnMap.t ref;
    infix_parse_fns: infix_parse_fn InfixParseFnMap.t ref;
}

let next_token parser =
  parser.curr_token <- parser.peek_token;
  parser.peek_token <- Some (next_token parser.lexer)

let register_prefix parser token_type fn =
  parser.prefix_parse_fns := PrefixParseFnMap.add token_type fn !(parser.prefix_parse_fns)

let register_infix parser token_type fn =
  parser.infix_parse_fns := InfixParseFnMap.add token_type fn !(parser.infix_parse_fns)

let parse_identifier parser =
  Identifier { token = Option.get parser.curr_token; value = (Option.get parser.curr_token).literal}

let parse_integer_literal parser =
  (* TODO: in_to_string error handling *)
  IntegerLiteral { token = Option.get parser.curr_token; value = int_of_string (Option.get parser.curr_token).literal}

let new_parser lexer =
  let parser = {
    lexer;
    curr_token = None;
    peek_token = None;
    errors = [];
    prefix_parse_fns = ref PrefixParseFnMap.empty;
    infix_parse_fns = ref InfixParseFnMap.empty;
  } in
  next_token parser;
  next_token parser;
  register_prefix parser Ident (fun () -> parse_identifier parser);
  register_prefix parser Int (fun () -> parse_integer_literal parser);
  parser

let get_errors parser =
  parser.errors

(* -- HELPERS -- *)
let curr_token_is parser token_type =
  match parser.curr_token with
  | Some { typ = Some t; _ } -> t = token_type
  | _ -> false

let peek_token_is parser token_type =
  match parser.peek_token with
  | Some { typ = Some t; _ } -> t = token_type
  | _ -> false

let peek_error parser expected_type =
  let actual_type = match parser.peek_token with
    | Some { typ = Some t; _ } -> string_of_token_type (Some t)
    | _ -> "none"
  in
  let msg = Printf.sprintf "PEEK ERROR: Expected next token to be: %s, got: %s"
              (string_of_token_type expected_type) actual_type in
  parser.errors <- parser.errors @ [msg]

let expect_peek parser expected_type =
  if peek_token_is parser expected_type then begin
    next_token parser;
    true
  end else begin
    peek_error parser (Some expected_type);
    false
  end

(* --- PARSING --- *)
let parse_bind_statement parser =
  let token = Option.get parser.curr_token in
  if expect_peek parser Ident then begin
    match parser.curr_token with
    | Some { literal = name; _ } ->
      if expect_peek parser Assign then
      while not (curr_token_is parser Semicolon) do
        next_token parser;
      done;
      BindStatement { token; name; value = EmptyExpression }
    | _ -> EmptyStatement
  end else
    EmptyStatement

let parse_return_statement parser =
  let token = Option.get parser.curr_token in
  next_token parser;
  while not (curr_token_is parser Semicolon) do
    next_token parser;
  done;
  ReturnStatement { token; return_value = EmptyExpression }

let parse_expression parser =
  match parser.curr_token with
  | Some { typ = Some t; _ } ->
    (match PrefixParseFnMap.find_opt t !(parser.prefix_parse_fns) with
    | Some prefix_fn -> prefix_fn ()
    | None -> EmptyExpression)
  | _ -> EmptyExpression

let parse_expression_statement parser =
  let stmt = ExpressionStatement {
    token = Option.get parser.curr_token;
    expression = parse_expression parser
  } in
  if peek_token_is parser Semicolon then next_token parser;
  stmt

let parse_statement parser =
  match parser.curr_token with
  | Some { typ = Some Bind; _ } ->
    parse_bind_statement parser
  | Some { typ = Some Return; _ } ->
    parse_return_statement parser
  | _ ->
    parse_expression_statement parser

let parse_program parser =
  let rec helper statements =
    if curr_token_is parser EOF then
      statements
    else
      let stmt = parse_statement parser in
      print_endline (string_of_statement stmt);
      next_token parser;
      helper (statements @ [stmt])
  in
  { statements = helper [] }
