(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangOok

 This module implements the Ook! programming language, designed by
 David Morgan-Mar in 2001. It is a trivial modification to Brainfuck; all
 instructions are written in Orangutan words.
***************************************************************************)

let bf_to_ook ch =
    match ch with
    | '+' -> Some ('.', '.')
    | '-' -> Some ('!', '!')
    | '>' -> Some ('.', '?')
    | '<' -> Some ('?', '.')
    | '.' -> Some ('!', '.')
    | ',' -> Some ('.', '!')
    | '[' -> Some ('!', '?')
    | ']' -> Some ('?', '!')
    | '#' -> None
    | _ -> failwith "unexpected Brainfuck instruction."

let ook_to_bf chs =
    match chs with
    | ('.', '.') -> '+'
    | ('!', '!') -> '-'
    | ('.', '?') -> '>'
    | ('?', '.') -> '<'
    | ('!', '.') -> '.'
    | ('.', '!') -> ','
    | ('!', '?') -> '['
    | ('?', '!') -> ']'
    | _ -> failwith "unexpected Ook! instruction."

(**************************************************************************)
(* The kind. *)

type t = LangBrainfuck.t (* yeah, we don't need an another type *)
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "ook"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        (* TODO should we use a buffer or a stream? *)
        let buf = Buffer.create 1024 in
        Stream.iter (Buffer.add_char buf) stream;
        let lexbuf = Lexing.from_string (Buffer.contents buf) in
        let codebuf = Buffer.create 8 in

        let rec parse _ =
            let open LangOok_lexer in
            match token lexbuf with
            | Some first ->
                begin match token lexbuf with
                | Some second ->
                    Buffer.add_char codebuf (ook_to_bf (first,second));
                    parse ()
                | None ->
                    failwith "non-even number of instructions."
                end
            | None -> ()
        in

        (* reuse the existing Brainfuck parser. *)
        parse ();
        let code = Buffer.contents codebuf in
        LangBrainfuck.reader#process (Stream.of_string code)
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] EsotopeCommon.interpreter kind

    method process = LangBrainfuck.interpreter#process
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        (* again, reuse the existing Brainfuck writer. *)
        let buf' = Buffer.create 1024 in
        LangBrainfuck.writer#process nodes buf';

        let translate ch =
            match bf_to_ook ch with
            | Some (first, second) ->
                Printf.bprintf buf "Ook%c Ook%c " first second
            | None -> ()
        in String.iter translate (Buffer.contents buf')
end

(**************************************************************************)
(* The Brainfuck-to-Ook! and Ook!-to-Brainfuck transformer. (trivial!) *)

let to_brainfuck = object
    inherit [t, LangBrainfuck.t] EsotopeCommon.processor
        kind LangBrainfuck.kind

    method process nodes = nodes
end

let from_brainfuck = object
    inherit [LangBrainfuck.t, t] EsotopeCommon.processor
        LangBrainfuck.kind kind

    method process nodes = nodes
end

