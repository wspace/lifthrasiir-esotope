(* This is a part of Esotope. See README for more information. *)
{
open LangKipple_parser
}

rule token = parse
    | [' ' '\t' '\r' '\n']+ { token lexbuf }
    | '#' [^ '#' '\r' '\n']* { token lexbuf }
    | '<' { LT }
    | '>' { GT }
    | '+' { PLUS }
    | '-' { MINUS }
    | '?' { QUO }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | ['0'-'9']+ as v { NUM (Int32.of_string v) }
    | '"' ([^ '"']+ as s) '"' { STR s }
    | ['A'-'Z' 'a'-'z' '@'] as i
        { STACK (int_of_char (Char.uppercase i) - int_of_char '@') }
    | eof { EOF }

