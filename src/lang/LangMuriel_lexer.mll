(* This is a part of Esotope. See README for more information. *)
{
open LangMuriel_parser
}

rule token = parse
    | [' ' '\t' '\r' '\n']+ { token lexbuf }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '~' { TILDE }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { STAR }
    | '=' { EQUAL }
    | '>' { GREATER }
    | '<' { LESS }
    | '$' { DOLLAR }
    | '#' { HASH }
    | '&' { AMPERSAND }
    | '%' { PERCENT }
    | ',' { COMMA }
    | '|' { PIPE }
    | ':' { COLON }
    | '.' { DOT }
    | '@' { AT }
    | ';' { SEMICOLON }
    | ['0'-'9']+ as v { NUM (int_of_string v) }
    | '"' { string (Buffer.create 16) lexbuf }
    | ['a'-'z'] as i { NUMVAR (int_of_char i - int_of_char 'a') }
    | ['A'-'Z'] as i { STRVAR (int_of_char i - int_of_char 'A') }
    | eof { EOF }

and string buf = parse
    | [^ '"' '\n' '\\'] as c { Buffer.add_char buf c; string buf lexbuf }
    | "\\\"" { Buffer.add_char buf '"'; string buf lexbuf }
    | "\\n" { Buffer.add_char buf '\n'; string buf lexbuf }
    | "\\\\" { Buffer.add_char buf '\\'; string buf lexbuf }
    | '"' { STR (Buffer.contents buf) }

