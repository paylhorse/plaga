(* PLAGA PARSER *)
open Token
open Lexer
open Ast

type precedence =
  | Lowest
  | Equals      (* == or != *)
  | LessGreater (* > or < *)
  | Sum         (* + or - *)
  | Product     (* * or / *)
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

let token_precedence = function
  | EQ | Not_EQ -> Equals
  | LT | GT -> LessGreater
  | Plus | Minus -> Sum
  | Asterisk | Slash -> Product
  | _ -> Lowest

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

let peek_precedence parser =
  match parser.peek_token with
  | Some { typ = Some t; _ } -> token_precedence t
  | _ -> Lowest

let curr_presedence parser =
  match parser.curr_token with
  | Some { typ = Some t; _ } -> token_precedence t
  | _ -> Lowest

let next_token parser =
  parser.curr_token <- parser.peek_token;
  parser.peek_token <- Some (next_token parser.lexer)

let register_prefix parser token_type fn =
  parser.prefix_parse_fns := PrefixParseFnMap.add token_type fn !(parser.prefix_parse_fns)

let register_infix parser token_type fn =
  parser.infix_parse_fns := InfixParseFnMap.add token_type fn !(parser.infix_parse_fns)

let get_errors parser =
  parser.errors

let no_prefix_parse_error parser token_type =
  let msg = Printf.sprintf "PARSE ERROR: No prefix parse function for %s found!" (string_of_token_type token_type) in
  parser.errors <- parser.errors @ [msg]

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

let expect_consume parser expected_type =
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
  if expect_consume parser Ident then begin
    match parser.curr_token with
    | Some { literal = name; _ } ->
      if expect_consume parser Assign then
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

(* Behold, Pratt Parsing *)
let parse_expression parser ?(precedence=Lowest) () =
  let left_exp =
    match parser.curr_token with
    | Some { typ = Some t; _ } -> (
        match PrefixParseFnMap.find_opt t !(parser.prefix_parse_fns) with
        | Some prefix_fn -> prefix_fn ()
        | None -> no_prefix_parse_error parser (Some t); EmptyExpression
      )
    | _ -> EmptyExpression
  in
  let rec parse_infix left_exp =
    if peek_token_is parser Semicolon || precedence_value precedence >= precedence_value (peek_precedence parser) then
      left_exp
    else
      match parser.peek_token with
      | Some { typ = Some t; _ } -> (
          match InfixParseFnMap.find_opt t !(parser.infix_parse_fns) with
          | Some infix_fn ->
              next_token parser;
              let new_left = infix_fn left_exp in
              parse_infix new_left
          | None -> left_exp
        )
      | _ -> left_exp
  in
  parse_infix left_exp

let parse_expression_statement parser =
  let stmt = ExpressionStatement {
    token = Option.get parser.curr_token;
    expression = parse_expression parser ()
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
  print_endline "🖨  ------ PROGRAM ------";
  let rec helper acc =
    if curr_token_is parser EOF then
      List.rev acc
    else
      let stmt = parse_statement parser in
      print_endline (string_of_statement stmt);
      (match stmt with
      | ExpressionStatement { expression; _ } -> print_endline (string_of_expression expression)
      | _ -> print_endline "----");
      next_token parser;
      helper (stmt :: acc)
  in
  { statements = helper [] }

let parse_identifier parser =
  Identifier { token = Option.get parser.curr_token; value = (Option.get parser.curr_token).literal }

let parse_integer_literal parser =
  (* TODO: int_of_string error handling *)
  IntegerLiteral { token = Option.get parser.curr_token; value = int_of_string (Option.get parser.curr_token).literal }

let parse_prefix_expression parser =
  let operator = (Option.get parser.curr_token).literal in
  next_token parser;
  PrefixExpression { token = Option.get parser.curr_token; operator; right = parse_expression parser ~precedence:Prefix () }

let parse_infix_expression parser left =
  let token = Option.get parser.curr_token in
  let operator = (Option.get parser.curr_token).literal in
  let precedence = curr_presedence parser in
  next_token parser;
  let right = parse_expression parser ~precedence () in
  InfixExpression { token; operator; left; right; }

let parse_boolean parser =
  Boolean { token = Option.get parser.curr_token; value = curr_token_is parser True }

let parse_grouped_expression parser =
  next_token parser;
  let exp = parse_expression parser () in
  if (expect_consume parser RParen) != true then EmptyExpression
  else exp

let parse_block_statement parser =
  let token = Option.get parser.curr_token in
  print_endline token.literal;
  next_token parser;
  print_endline (Option.get parser.curr_token).literal;
  let rec collect_statements acc =
    if curr_token_is parser RBrace || curr_token_is parser EOF then
      acc
    else begin
      let stmt = parse_statement parser in
      next_token parser;
      collect_statements (stmt :: acc)
    end
  in
  let statements = collect_statements [] in
  BlockStatement { token; statements = List.rev statements; }

let parse_if_expression parser =
  let token = Option.get parser.curr_token in
  if not (expect_consume parser LParen) then
    (parser.errors <- parser.errors @ ["Expected LParen"]; EmptyExpression)
  else begin
    next_token parser;
    let condition = parse_expression parser () in
    if not (peek_token_is parser RParen) then
      (parser.errors <- parser.errors @ ["Expected RParen"]; EmptyExpression)
    else begin
      next_token parser;
      if not (expect_consume parser LBrace) then
        (parser.errors <- parser.errors @ ["Expected LBrace"]; EmptyExpression)
      else begin
        let consequence = parse_block_statement parser in
        let alternative =
          if peek_token_is parser Else then begin
            next_token parser;
            if expect_consume parser LBrace then
              parse_block_statement parser
            else
              EmptyStatement  (* Error here! *)
          end else
            EmptyStatement  (* No Else *)
        in
        IfExpression { token; condition; consequence; alternative }
      end
    end
  end

let parse_function_parameters parser =
  let rec collect_params acc =
    if peek_token_is parser RParen then
      (next_token parser; acc)
    else begin
      if List.length acc > 0 then
        if not (expect_consume parser Comma) then
          (parser.errors <- parser.errors @ ["Expected Comma between parameters"]; ())
        else
          next_token parser;
      match parser.curr_token with
      | Some { literal = name; _ } -> collect_params (acc @ [Identifier { token = Option.get parser.curr_token; value = name }])
      | _ -> (parser.errors <- parser.errors @ ["Expected parameter identifier"]; acc)
    end
  in

  if peek_token_is parser RParen then begin
    next_token parser;
    []
  end else
    collect_params []

let parse_function_literal parser =
  let token = Option.get parser.curr_token in
  if not (expect_consume parser LParen) then
    (parser.errors <- parser.errors @ ["Expected LParen"]; EmptyExpression)
  else begin
    next_token parser;
    let parameters = parse_function_parameters parser in
    if not (expect_consume parser LBrace) then
      (parser.errors <- parser.errors @ ["Expected LBrace"]; EmptyExpression)
    else begin
      let body = parse_block_statement parser in
      FunctionLiteral { token; parameters; body }
    end
  end

(* --- CONSTRUCTION --- *)
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
  register_prefix parser Bang (fun () -> parse_prefix_expression parser);
  register_prefix parser Minus (fun () -> parse_prefix_expression parser);
  register_prefix parser True (fun () -> parse_boolean parser);
  register_prefix parser False (fun () -> parse_boolean parser);
  register_prefix parser LParen (fun () -> parse_grouped_expression parser);
  register_prefix parser If (fun () -> parse_if_expression parser);
  register_prefix parser Function (fun () -> parse_function_literal parser);
  register_infix parser Plus (fun left -> parse_infix_expression parser left);
  register_infix parser Minus (fun left -> parse_infix_expression parser left);
  register_infix parser Asterisk (fun left -> parse_infix_expression parser left);
  register_infix parser Slash (fun left -> parse_infix_expression parser left);
  register_infix parser EQ (fun left -> parse_infix_expression parser left);
  register_infix parser Not_EQ (fun left -> parse_infix_expression parser left);
  register_infix parser LT (fun left -> parse_infix_expression parser left);
  register_infix parser GT (fun left -> parse_infix_expression parser left);
  parser
