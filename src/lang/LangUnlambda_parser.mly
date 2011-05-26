/* This is a part of Esotope. See README for more information. */

%{
open LangUnlambda_ast
%}

%token APP
%token K
%token S
%token I
%token V
%token D
%token C
%token <char> DOT
%token R
%token E             /* Unlambda 2 additions from here. */
%token AT
%token PIPE
%token <char> QUO
%token EOF

%start main
%type <LangUnlambda_ast.t> main

%%

main:
| expr EOF { $1 }
;

expr:
| K { K }
| S { S }
| I { I }
| V { Void }
| D { Delay }
| C { Callcc }
| DOT { Print $1 }
| R { PrintNewline }
| E { Exit }
| AT { Read }
| QUO { CheckRead $1 }
| PIPE { Reprint }
| APP expr expr {
    (* these optimizations do not change the source code. the writer will
     * write them as originally read. *)
    match ($2,$3) with
    | (Delay, x) -> Delay1 x
    | (x, y) -> App (x,y)
  }
;

