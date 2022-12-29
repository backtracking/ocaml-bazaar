
%{
  open Ast
%}

%token <string> SYM
%token <string> VAR
%token IF QUERY DOT LPAR RPAR COMMA
%token EOF

%start file
%type <Ast.file> file

%start command
%type <Ast.decl> command

%%

file:
| dl = decl* EOF { dl }
;

command:
| d = decl EOF { d }
;

decl:
| p=pred pl=premises DOT { Rule (p, pl) }
| QUERY p=pred       DOT { Query p      }
;

premises:
| IF pl=separated_nonempty_list(COMMA, pred) { pl }
| /* epsilon */                              { [] }
;

pred:
| id=SYM                    { id, [] }
| id=SYM LPAR tl=terms RPAR { id, tl }
;

term:
| id=SYM                    { App (id, []) }
| id=SYM LPAR tl=terms RPAR { App (id, tl) }
| id=VAR                    { Var id       }
;

terms:
| l=separated_nonempty_list(COMMA, term) { l }
;




