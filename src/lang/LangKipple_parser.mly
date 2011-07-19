/* This is a part of Esotope. See README for more information. */

%{
open LangKipple_ast

let str_foldl f s tail =
    let nodes = ref tail in
    for i = 0 to String.length s - 1 do
        nodes := f (Int32.of_int (int_of_char s.[i])) !nodes
    done;
    !nodes

let str_foldr f s tail =
    let nodes = ref tail in
    for i = String.length s - 1 downto 0 do
        nodes := f (Int32.of_int (int_of_char s.[i])) !nodes
    done;
    !nodes

%}

%token LT
%token GT
%token PLUS
%token MINUS
%token QUO
%token LPAREN
%token RPAREN
%token <int32> NUM
%token <string> STR
%token <int> STACK
%token EOF

%start main
%type <LangKipple_ast.t> main

%%

main: stmts EOF { $1 };

stmts:
|                                               { [] }
| NUM num_stmt stmts                            { $2 $1 $3 }
| STR str_stmt stmts                            { $2 $1 $3 }
| STACK stack_stmt stmts                        { $2 $1 $3 }
| LPAREN STACK stack_partial stmts RPAREN stmts { While ($2, $3 $2 $4) :: $6 }
;

/* {num,str,stack}_stmt non-terminals return a function which adds a full
 * statement to the list of statements given the prior token. */

num_stmt:
| GT STACK stack_partial { fun v t -> Push ($2,v) :: $3 $2 t }
;

str_stmt:
| GT STACK stack_partial {
    fun s t -> str_foldl (fun c t -> Push ($2,c) :: t) s ($3 $2 t)
  }
;

stack_stmt:
| LT NUM num_partial        { fun s t -> Push (s,$2)   :: $3 $2 t }
| LT STR str_partial        {
    fun s t -> str_foldr (fun c t' -> Push (s,c) :: t') $2 ($3 $2 t)
  }
| LT STACK stack_partial    { fun s t -> Move (s,$2)   :: $3 $2 t }
| GT STACK stack_partial    { fun s t -> Move ($2,s)   :: $3 $2 t }
| PLUS NUM num_partial      { fun s t -> Add (s,$2)    :: $3 $2 t }
| PLUS STACK stack_partial  { fun s t -> AddPop (s,$2) :: $3 $2 t }
| MINUS NUM num_partial     { fun s t -> Sub (s,$2)    :: $3 $2 t }
| MINUS STACK stack_partial { fun s t -> SubPop (s,$2) :: $3 $2 t }
| QUO                       { fun s t -> ClearIfZero s :: t }
;

/* the first partial is mandatory, remaining ones are optional. */

num_partial:   { fun _ t -> t } | num_stmt   { $1 }
str_partial:   { fun _ t -> t } | str_stmt   { $1 }
stack_partial: { fun _ t -> t } | stack_stmt { $1 }

