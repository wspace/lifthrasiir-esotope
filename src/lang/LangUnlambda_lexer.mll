(* This is a part of Esotope. See README for more information. *)
{
open LangUnlambda_parser
}

rule token = parse
    | [' ' '\t' '\r' '\n']+ { token lexbuf }
    | '#' [^ '#' '\r' '\n']* { token lexbuf }
    | '`' { APP }
    | ['K' 'k'] { K }
    | ['S' 's'] { S }
    | ['I' 'i'] { I }
    | ['V' 'v'] { V }
    | ['D' 'd'] { D }
    | ['C' 'c'] { C }
    | '.' (_ as ch) { DOT ch }
    | 'r' { R }
    | ['E' 'e'] { E }
    | '@' { AT }
    | '|' { PIPE }
    | '?' (_ as ch) { QUO ch }
    | eof { EOF }

