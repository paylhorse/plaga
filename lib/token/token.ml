(* TOKEN TYPES *)
type token_type =
  | Illegal
  | EOF
  | Ident
  | Int
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let

(* integer or byte would give better performance, but string suffices *)
let string_of_token_type = function
  | Illegal -> "ILLEGAL"
  | EOF -> "EOF"
  | Ident -> "IDENT"
  | Int -> "INT"
  | Assign -> "="
  | Plus -> "+"
  | Comma -> ","
  | Semicolon -> ";"
  | LParen -> "("
  | RParen -> ")"
  | LBrace -> "{"
  | RBrace -> "}"
  | Function -> "FUNCTION"
  | Let -> "LET"

type token = {
  typ: token_type;
  literal: string;
}
