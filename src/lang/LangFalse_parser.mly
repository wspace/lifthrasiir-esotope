/* This is a part of Esotope. See README for more information. */

%{
open LangFalse_ast
%}

%token <string> COMMENT
%token <char> VAR
%token <int32> INT
%token <string> STR
%token <char> CHAR
%token BACKQUOTE
%token LBRACKET
%token RBRACKET
%token COLON
%token SEMICOLON
%token BANG
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token UNDERSCORE
%token EQUAL
%token GREATER
%token AMPERSAND
%token PIPE
%token TILDE
%token DOLLAR
%token PERCENT
%token BACKSLASH
%token AT
%token ZEROSLASH
%token QUO
%token HASH
%token DOT
%token COMMA
%token CARAT
%token BETA
%token EOF

%start main
%type <LangFalse_ast.t> main

%%

main: stmts EOF { $1 };

stmts:
| { [] }
| stmt stmts { $1 :: $2 }
;

stmt:
| COMMENT { Comment $1 }
| LBRACKET stmts RBRACKET { PushCode $2 }
| VAR { PushVarRef (int_of_char $1 - int_of_char 'a') }
| INT BACKQUOTE { Intrinsic (Int32.to_int $1) }
| INT { Push $1 }
| STR BACKQUOTE { PushBuiltin $1 }
| STR { OutputStr $1 }
| CHAR { PushChar $1 }
| COLON { Store }
| SEMICOLON { Load }
| BANG { Apply }
| PLUS { Add }
| MINUS { Subtract }
| STAR { Multiply }
| SLASH { Divide }
| UNDERSCORE { Negate }
| EQUAL { Equals }
| GREATER { Greater }
| AMPERSAND { And }
| PIPE { Or }
| TILDE { Not }
| DOLLAR { Dup }
| PERCENT { Pop }
| BACKSLASH { Swap }
| AT { Rot }
| ZEROSLASH { Pick }
| QUO { If }
| HASH { While }
| DOT { OutputNum }
| COMMA { OutputChar }
| CARAT { InputChar }
| BETA { Flush }
;

