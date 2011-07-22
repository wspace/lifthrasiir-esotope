(* This is a part of Esotope. See README for more information. *)
{
open LangFalse_parser
}

rule token = parse
    | [' ' '\t' '\r' '\n']+ { token lexbuf }
    | '{' ([^ '}']* as s) '}' { COMMENT s }
    | ['a'-'z'] as i { VAR i }
    | ['0'-'9']+ as v { INT (Int32.of_string v) }
    | '"' ([^ '"']* as s) '"' { STR s }
    | '\'' (_ as c) { CHAR c }
    | '`' { BACKQUOTE }
    | '[' { LBRACKET }
    | ']' { RBRACKET }
    | ':' { COLON }
    | ';' { SEMICOLON }
    | '!' { BANG }
    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { STAR }
    | '/' { SLASH }
    | '_' { UNDERSCORE }
    | '=' { EQUAL }
    | '>' { GREATER }
    | '&' { AMPERSAND }
    | '|' { PIPE }
    | '~' { TILDE }
    | '$' { DOLLAR }
    | '%' { PERCENT }
    | '\\' { BACKSLASH }
    | '@' { AT }
    | 'O' | '\248' | "\195\184" { ZEROSLASH } (* "ø" *)
    | '?' { QUO }
    | '#' { HASH }
    | '.' { DOT }
    | ',' { COMMA }
    | '^' { CARAT }
    | 'B' | '\223' | "\195\159" { BETA } (* "ß" *)
    | eof { EOF }

