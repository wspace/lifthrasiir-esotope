/* This is a part of Esotope. See README for more information. */

%{
open LangMuriel_ast
%}

%token <int> NUM
%token <string> STR
%token <int> NUMVAR
%token <int> STRVAR
%token LPAREN
%token RPAREN
%token TILDE
%token PLUS
%token MINUS
%token STAR
%token EQUAL
%token GREATER
%token LESS
%token DOLLAR
%token HASH
%token AMPERSAND
%token PERCENT
%token COMMA
%token PIPE
%token COLON
%token DOT
%token AT
%token SEMICOLON
%token EOF

%nonassoc COMMA
%left EQUAL GREATER LESS
%left PLUS MINUS
%left STAR
%nonassoc UMINUS HASH AMPERSAND DOLLAR PERCENT PIPE

%start main
%type <LangMuriel_ast.t> main

%%

main: stmts EOF { $1 };

stmts:
| NUMVAR COLON numexpr more_stmts
  { let (stmts,exestmt) = $4 in (AssignNum ($1, $3) :: stmts, exestmt) }
| STRVAR COLON strexpr more_stmts
  { let (stmts,exestmt) = $4 in (AssignStr ($1, $3) :: stmts, exestmt) }
| DOT strexpr more_stmts
  { let (stmts,exestmt) = $3 in (Output $2 :: stmts, exestmt) }
| AT strexpr more_stmts
  { ([], Some $2) } /* ignores remaining statements */
| { ([], None) }
;

more_stmts:
| { ([], None) }
| SEMICOLON stmts { $2 }

numexpr:
| NUM { Num $1 }
| NUMVAR { NumVar $1 }
| LPAREN numexpr RPAREN { $2 }
| MINUS numexpr %prec UMINUS { Neg $2 }
| numexpr PLUS numexpr { Add ($1, $3) }
| numexpr MINUS numexpr { Sub ($1, $3) }
| numexpr STAR numexpr { Mult ($1, $3) }
| numexpr EQUAL numexpr { Equal ($1, $3) }
| numexpr GREATER numexpr { Greater ($1, $3) }
| numexpr LESS numexpr { Less ($1, $3) }
| HASH strexpr { ParseNum $2 }
| AMPERSAND strexpr { Len $2 }
;

strexpr:
| STR { Str $1 }
| STRVAR { StrVar $1 }
| TILDE { Input }
| LPAREN strexpr RPAREN { $2 }
| strexpr PLUS strexpr { Concat ($1, $3) }
| DOLLAR numexpr { Stringify $2 }
| PERCENT strexpr COMMA numexpr COMMA numexpr { Substr ($2, $4, $6) }
| PIPE strexpr { Quotify $2 }
;

