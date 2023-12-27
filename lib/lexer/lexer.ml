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
  lexer.read_position := !(lexer.read_position) + 1;

let next_token lexer =
  let tok =
    let ch_as_string = Char.to_string lexer.ch in
    match lexer.ch with
    | '=' -> new_token Assign ch_as_string
    | ';' -> new_token Semicolon ch_as_string
    | '(' -> new_token LBrace ch_as_string
    | ')' -> new_token RBrace ch_as_string
    | ',' -> new_token Comma ch_as_string
    | '+' -> new_token Plus  ch_as_string
    | '{' -> new_token LBrace ch_as_string
    | '}' -> new_token RBrace ch_as_string
    | '\000' -> new_token EOF ""
    | _ -> new_token Illegal ch_as_string
  in
  read_char lexer;
  tok

let new_instance input =
  let lexer = {
    input = input;
    position = ref 0;
    read_position = ref 0;
    ch = if String.length input > 0 then input.[0] else '\000';
  } in
  read_char lexer;
  lexer
