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
%token EOL

%right K S I V D C DOT R E AT PIPE QUO
%right APP

%start main
%type <LangUnlambda_ast.t> main

%%

main:
| expr EOL { $1 }
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

/* these optimizations do not change the source code. the writer will write
 * them as originally read. */
| APP K expr { K1 $3 }
| APP S expr { S1 $3 }
| APP APP S expr expr { S2 ($4,$5) }
| APP D expr { Delay1 $3 }

| APP expr expr { App ($2,$3) }
;

