/* This is a part of Esotope. See README for more information. */

%{
open LangMinus_ast
%}

%token SUBTRACT
%token SEMICOLON
%token <int> NUM
%token <char> VAR
%token EOF

%start main
%type <LangMinus_ast.t> main

%%

main:
| EOF { [] }
| stmt main { $1 :: $2 }
;

stmt:
| lhs subtract rhs semicolon {
    match ($1, $3) with
    | ((Number _ | Breakpoint), _) as pair -> Exit pair
    | pair -> Subtract pair
  }
;

lhs:
| VAR {
    match $1 with
    | 'c' -> CodePtr
    | 'p' -> MemPtr
    | 'o' -> OutputChar
    | 'q' -> OutputNum
    | 'r' -> RandomRange
    | '_' -> Breakpoint   (* will generate Exit later *)
    | v ->
        if 'A' <= v && v <= 'Z' then
            MemRef ((int_of_char v) - (int_of_char 'A'))
        else
            Var ((int_of_char v) - (int_of_char 'a'))
  }
| NUM { Number $1 }
;

rhs:
| VAR {
    match $1 with
    | 'c' -> CodePtr
    | 'p' -> MemPtr
    | 'i' -> InputChar
    | 'j' -> InputNum
    | 'r' -> Random
    | '_' -> Breakpoint
    | v ->
        if 'A' <= v && v <= 'Z' then
            MemRef ((int_of_char v) - (int_of_char 'A'))
        else
            Var ((int_of_char v) - (int_of_char 'a'))
  }
| NUM { Number $1 }
;

subtract: SUBTRACT { () } | { () };
semicolon: SEMICOLON { () } | { () };

