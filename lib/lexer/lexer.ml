open Core
open Token

let new_token typ literal = { typ; literal }

type lexer = {
  input: string;
  position: int ref;
  read_position: int ref;
  mutable ch: char;
}

(* TODO: unicode support *)
let read_char lexer =
  if !(lexer.read_position) >= String.length lexer.input
  then lexer.ch <- '\000'
  else lexer.ch <- String.get lexer.input !(lexer.read_position);
  lexer.position := !(lexer.read_position);
  lexer.read_position := !(lexer.read_position) + 1;;

let is_letter ch =
  (Char.(<=) 'a' ch && Char.(<=) ch 'z' || Char.(<=) 'A' ch && Char.(<=) ch 'Z' || Char.(=) ch '_')

let is_digit ch =
  (Char.(<=) '0' ch && Char.(<=) ch '9')

let read_identifier lexer =
  let position = !(lexer.position) in
  while is_letter lexer.ch do
    read_char lexer;
  done;
  let ident = String.sub lexer.input ~pos:position ~len:(!(lexer.position) - position) in
  print_endline "IDENTIFIER READ:";  print_endline ident;
  ident

let read_number lexer =
  let position = !(lexer.position) in
  while is_digit lexer.ch do
    read_char lexer;
  done;
  let ident = String.sub lexer.input ~pos:position ~len:(!(lexer.position) - position) in
  print_endline "NUMBER READ:";
  print_endline ident;
  ident

let eat_whitespace lexer =
  while Char.(=) lexer.ch ' ' || Char.(=) lexer.ch '\t' || Char.(=) lexer.ch '\n' || Char.(=) lexer.ch '\r' do
    read_char lexer;
  done;;

let peek_char lexer =
  if !(lexer.read_position) >= String.length lexer.input then '\000'
  else String.get lexer.input !(lexer.read_position)

let next_token lexer =
  eat_whitespace lexer;
  if is_letter lexer.ch then
    let literal = read_identifier lexer in
    new_token (Some (lookup_ident literal)) literal
  else if is_digit lexer.ch then
    new_token (Some Int) (read_number lexer)
  else
    let ch_to_string = Char.to_string lexer.ch in
    let tok = match lexer.ch with
      | '=' -> if Char.(=) (peek_char lexer) '=' then
                 let ch = lexer.ch in
                 read_char lexer;
                 let literal = Char.to_string ch ^ Char.to_string lexer.ch in
                 new_token (Some EQ) literal
               else new_token (Some Assign) ch_to_string
      | '+' -> new_token (Some Plus) ch_to_string
      | '-' -> new_token (Some Minus) ch_to_string
      | '!' -> if Char.(=) (peek_char lexer) '=' then
                 let ch = lexer.ch in
                 read_char lexer;
                 let literal = Char.to_string ch ^ Char.to_string lexer.ch in
                 new_token (Some Not_EQ) literal
               else new_token (Some Bang) ch_to_string
      | '/' -> new_token (Some Slash) ch_to_string
      | '*' -> new_token (Some Asterisk) ch_to_string
      | '<' -> new_token (Some LT) ch_to_string
      | '>' -> new_token (Some GT) ch_to_string
      | ';' -> new_token (Some Semicolon) ch_to_string
      | '(' -> new_token (Some LParen) ch_to_string
      | ')' -> new_token (Some RParen) ch_to_string
      | ',' -> new_token (Some Comma) ch_to_string
      | '{' -> new_token (Some LBrace) ch_to_string
      | '}' -> new_token (Some RBrace) ch_to_string
      | '\000' -> new_token (Some EOF) ""
      | _ -> new_token (Some Illegal) ch_to_string
    in
    read_char lexer;
    tok

let new_instance input =
  let lexer =
  {
    input = input;
    position = ref 0;
    read_position = ref 0;
    ch = if String.length input > 0 then input.[0] else '\000';
  } in
  read_char lexer;
  lexer
