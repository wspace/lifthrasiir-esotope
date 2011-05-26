(* This is a part of Esotope. See README for more information. *)
{
open LangMinus_parser
}

rule token = parse
    | [' ' '\t' '\r' '\n']+ { token lexbuf }
    | '#' [^ '#' '\r' '\n']* { token lexbuf }
    | '{' { comments 0 lexbuf }
    | "-=" { SUBTRACT }
    | ';' { SEMICOLON }
    | ['0'-'9']+ as v { NUM (int_of_string v) }
    | ['a'-'z' 'A'-'Z' '_'] as v { VAR v }
    | eof { EOF }

and comments level = parse
    | '}' { if level = 0 then token lexbuf else comments (level-1) lexbuf }
    | '{' { comments (level+1) lexbuf }
    | _ { comments level lexbuf }
    | eof { EOF (* not an error *) }

