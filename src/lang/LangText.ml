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

