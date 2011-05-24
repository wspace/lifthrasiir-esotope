(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangText

 This module implements the text kind, which can be (jokingly) regarded as
 a simple programming language where every code is a quine. This is useful
 for a source kind of code generators for printing that string.
***************************************************************************)

(**************************************************************************)
(* The kind. *)

type t = string
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "text"
end

(**************************************************************************)
(* The reader, interpreter and writer. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind
    method process stream =
        let buf = Buffer.create 1024 in
        Stream.iter (Buffer.add_char buf) stream;
        Buffer.contents buf
end

let interpreter = object
    inherit [t] EsotopeCommon.interpreter kind
    method process s =
        output_string stdout s;
        flush stdout
end

let writer = object
    inherit [t] EsotopeCommon.writer kind
    method process s buf = Buffer.add_string buf s
end

(**************************************************************************)
(* Brainfuck text generator. *)

let to_brainfuck = object
    inherit [t, LangBrainfuck.t] EsotopeCommon.processor
        kind LangBrainfuck.kind

    method process s =
        (* TODO this algorithm is too simple. *)
        let open LangBrainfuck in
        let last = ref 0 in
        let nodes = ref [] in (* reversed *)
        let nearest_to_zero x =
            if x < -128 then x + 256
            else if x > 128 then x - 256
            else x in
        for i = 0 to (String.length s) - 1 do
            let ch = int_of_char s.[i] in
            let delta = nearest_to_zero (ch - !last) in
            let deltazero = nearest_to_zero ch in
            if (abs deltazero) + 3 < abs delta then
                nodes := SetMemory (0, deltazero) :: !nodes
            else
                nodes := AdjustMemory (0, delta) :: !nodes;
            nodes := Output 0 :: !nodes;
            last := ch
        done;
        List.rev !nodes
end

