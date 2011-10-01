(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangMuriel

 This module implements the Muriel programming language, designed by
 Matthew Westcott in 2001. It is one of a few programming language that uses
 quines (a program that prints itself) as a sole looping construct.

 The original specification is vague about operator precedences, and in fact
 the example "99 Bottles of Beer" program mixes two kinds of precedences:
 some assume the comparison bounds tighter than the arithmetic, others do
 not. Esotope uses the (arbitrary) decision due to the Perl interpreter by
 Chris Pressey, which assumes the latter. In order to run the original
 example in Esotope, change the last line from:
    @%Z,0,b>0*&Z
 to:
    @%Z,0,(b>0)*&Z
***************************************************************************)

open LangMuriel_ast

let quote_str s =
    let replace s =
        match Str.replace_matched "\\0" s with
        | "\"" -> "\\\""
        | "\n" -> "\\n"
        | "\\" -> "\\\\"
        | _ -> failwith "impossible" in
    Str.global_substitute (Str.regexp "[\"\n\\]") replace s

(**************************************************************************)
(* The kind. *)

let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "muriel"
    method aliases = [".mur"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.parsing_reader kind
    method process = LangMuriel_parser.main LangMuriel_lexer.token
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process code io =
        (* Muriel is incapable for interactive I/O. but we should read from
         * stdin until the program really requires it, as it would block
         * if we don't do so. *)
        let input = ref None in

        let rec run (stmts,exestmt) =
            let numvars = Array.make 26 0 in
            let strvars = Array.make 26 "" in

            let rec numeval = function
                | Num v -> v
                | NumVar i -> numvars.(i)
                | Add (a,b) -> numeval a + numeval b
                | Sub (a,b) -> numeval a - numeval b
                | Mult (a,b) -> numeval a * numeval b
                | Equal (a,b) -> if numeval a == numeval b then 1 else 0
                | Greater (a,b) -> if numeval a > numeval b then 1 else 0
                | Less (a,b) -> if numeval a < numeval b then 1 else 0
                | Neg a -> -(numeval a)
                | ParseNum a -> int_of_string (streval a) (* TODO base prefix *)
                | Len a -> String.length (streval a)
            and streval = function
                | Str v -> v
                | StrVar i -> strvars.(i)
                | Input ->
                    begin match !input with
                    | Some s -> s
                    | None -> let s = io#get_all () in input := Some s; s
                    end
                | Concat (a,b) -> streval a ^ streval b
                | Stringify a -> string_of_int (numeval a)
                | Substr (a,b,c) ->
                    let (a,b,c) = (streval a, numeval b, numeval c) in
                    String.sub a b (c-b)
                | Quotify a -> quote_str (streval a)
            in

            let exec = function
                | AssignNum (i,e) -> numvars.(i) <- numeval e
                | AssignStr (i,e) -> strvars.(i) <- streval e
                | Output e -> io#put_str (streval e); io#flush_out ()
            in List.iter exec stmts;

            match exestmt with
            | None -> ()
            | Some e ->
                let code = streval e in
                run (reader#process (Lexing.from_string code))

        in run code
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process code buf =
        let putc = Buffer.add_char buf in
        let paren cond f =
            if cond then begin putc '('; f (); putc ')' end else f () in

        let rec emit_num_expr prec = function
            | Num v -> Printf.bprintf buf "%d" v
            | NumVar i -> putc (char_of_int (int_of_char 'a' + i))
            | Add (a,b) ->
                paren (prec > 1) (fun () -> emit_num_expr 1 a; putc '+';
                                            emit_num_expr 2 b)
            | Sub (a,b) ->
                paren (prec > 1) (fun () -> emit_num_expr 1 a; putc '-';
                                            emit_num_expr 2 b)
            | Mult (a,b) ->
                paren (prec > 2) (fun () -> emit_num_expr 2 a; putc '*';
                                            emit_num_expr 3 b)
            | Equal (a,b) ->
                paren (prec > 0) (fun () -> emit_num_expr 1 a; putc '=';
                                            emit_num_expr 1 b)
            | Greater (a,b) ->
                paren (prec > 0) (fun () -> emit_num_expr 1 a; putc '>';
                                            emit_num_expr 1 b)
            | Less (a,b) ->
                paren (prec > 0) (fun () -> emit_num_expr 1 a; putc '<';
                                            emit_num_expr 1 b)
            | Neg a -> putc '-'; emit_num_expr 3 a
            | ParseNum a -> putc '#'; emit_str_expr 3 a
            | Len a -> putc '&'; emit_str_expr 3 a
        and emit_str_expr prec = function
            | Str v -> putc '"'; Buffer.add_string buf (quote_str v); putc '"'
            | StrVar i -> putc (char_of_int (int_of_char 'A' + i))
            | Input -> putc '~'
            | Concat (a,b) ->
                paren (prec > 1) (fun () -> emit_str_expr 1 a; putc '+';
                                            emit_str_expr 2 b)
            | Stringify a -> putc '$'; emit_num_expr 3 a
            | Substr (a,b,c) ->
                paren (prec > 0) (fun () -> putc '%'; emit_str_expr 3 a;
                                            putc ','; emit_num_expr 0 b;
                                            putc ','; emit_num_expr 0 c)
            | Quotify a -> putc '|'; emit_str_expr 3 a
        in

        let emit_stmt = function
            | AssignNum (i,e) ->
                putc (char_of_int (int_of_char 'a' + i));
                putc ':'; emit_num_expr 0 e
            | AssignStr (i,e) ->
                putc (char_of_int (int_of_char 'A' + i));
                putc ':'; emit_str_expr 0 e
            | Output e -> putc '.'; emit_str_expr 0 e
        in

        let rec emit = function
            | ([], Some e) -> putc '@'; emit_str_expr 0 e
            | ([], None) -> ()
            | ([stmt], Some e) ->
                emit_stmt stmt; Buffer.add_string buf ";\n";
                putc '@'; emit_str_expr 0 e
            | ([stmt], None) -> emit_stmt stmt
            | (stmt::t, exestmt) ->
                emit_stmt stmt; Buffer.add_string buf ";\n";
                emit (t, exestmt)
        in emit code; putc '\n'
end

