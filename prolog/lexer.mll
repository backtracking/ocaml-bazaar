
(* Analyseur lexical pour Mini-Python *)

{
  open Lexing
  open Ast
  open Parser

  exception Lexing_error of string

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter =  lower | upper
let digit = ['0'-'9']
let lident = lower (letter | digit | '_')*
let uident = upper (letter | digit | '_')*
let space = ' ' | '\t' | '\r'
let comment = "#" [^'\n']* '\n'

rule next_token = parse
  | '\n' | comment { new_line lexbuf; next_token lexbuf }
  | space+ { next_token lexbuf }
  | lident as id { SYM id }
  | uident as id { VAR id }
  | '.'     { DOT }
  | '('     { LPAR }
  | ','     { COMMA }
  | ')'     { RPAR }
  | ":-"    { IF }
  | "?-"    { QUERY }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }
