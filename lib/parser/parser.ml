(* PLAGA PARSER *)
open Token
open Lexer
open Ast

type parser = {
    lexer: lexer;
    mutable curr_token: token option;
    mutable peek_token: token option;
    mutable errors: string list;
}

let next_token parser =
  parser.curr_token <- parser.peek_token;
  parser.peek_token <- Some (next_token parser.lexer)

let new_parser lexer =
  let parser = { lexer; curr_token = None; peek_token = None; errors = []; } in
  next_token parser;
  next_token parser;
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

let parse_statement parser =
  match parser.curr_token with
  | Some { typ = Some Bind; _ } ->
    parse_bind_statement parser
  | Some { typ = Some Return; _ } ->
    parse_return_statement parser
  | _ -> EmptyStatement

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
