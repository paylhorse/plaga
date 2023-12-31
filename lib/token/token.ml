(* PLAGA TOKENS *)
(* --- TOKEN TYPES --- *)
type token_type =
  | Illegal
  | EOF
  | Ident
  | Int
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LT
  | GT
  | EQ
  | Not_EQ
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Bind
  | If
  | Else
  | True
  | False
  | Return

(* integer or byte would give better performance, but string suffices *)
let string_of_token_type = function
  | Some(Illegal) -> "ILLEGAL"
  | Some(EOF) -> "EOF"
  (* -- IDENTS + LITERALS -- *)
  | Some(Ident) -> "IDENT"
  | Some(Int) -> "INT"
  (* -- OPERATORS -- *)
  | Some(Assign) -> "="
  | Some(Plus) -> "+"
  | Some(Minus) -> "-"
  | Some(Bang) -> "!"
  | Some(Asterisk) -> "*"
  | Some(Slash) -> "/"
  | Some(LT) -> "<"
  | Some(GT) -> ">"
  | Some(EQ) -> "=="
  | Some(Not_EQ) -> "!="
  (* -- DELIMITERS -- *)
  | Some(Comma) -> ","
  | Some(Semicolon) -> ";"
  | Some(LParen) -> "("
  | Some(RParen) -> ")"
  | Some(LBrace) -> "{"
  | Some(RBrace) -> "}"
  (* -- KEYWORDS -- *)
  | Some(Function) -> "FUNCTION"
  | Some(Bind) -> "BIND"
  | Some(If) -> "IF"
  | Some(Else) -> "ELSE"
  | Some(True) -> "TRUE"
  | Some(False) -> "FALSE"
  | Some(Return) -> "RETURN"
  | None -> ""

type token = {
  typ: token_type option;
  literal: string;
}

module StringMap = Map.Make (String)

(* --- KEYWORDS --- *)
let keywords =
  List.fold_left (fun map (key, value) -> StringMap.add key value map)
  StringMap.empty
  [("bind", Bind);
   ("fn", Function);
   ("true", True);
   ("false", False);
   ("if", If);
   ("else", Else);
   ("return", Return);]

let lookup_ident ident =
  match StringMap.find_opt ident keywords with
  | Some(tok) -> tok
  | None -> Ident
