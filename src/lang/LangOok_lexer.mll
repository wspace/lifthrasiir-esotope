(* This is a part of Esotope. See README for more information. *)
{
type token = char option
}

rule token = parse
    | [' ' '\t' '\r' '\n']+ { token lexbuf }
    | "Ook." { Some '.' }
    | "Ook!" { Some '!' }
    | "Ook?" { Some '?' }
    | eof { None }

