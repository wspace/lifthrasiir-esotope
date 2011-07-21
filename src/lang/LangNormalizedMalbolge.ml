(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangNormalizedMalbolge

 This module implements a normalized variant of Malbolge programming
 language. This variant is first introduced by Andrew Cooke in 2000, who
 also wrote (a program which generated) the first "Hello, world" program.

 Each character of normalized Malbolge program encodes an instruction to be
 executed instead of the actual memory value. It is very useful for
 analyzing permutation-invariant Malbolge codes, which are thought to be
 the only feasible way to create a reliable loop. The encoding follows the
 original interpreter's "xlat1" table (which has been eliminated in
 LangMalbolge).
***************************************************************************)

let to_op = function
    | 'i' -> LangMalbolge.Jump
    | '<' -> LangMalbolge.OutputChar
    | '/' -> LangMalbolge.InputChar
    | '*' -> LangMalbolge.Rotate
    | 'j' -> LangMalbolge.Deref
    | 'p' -> LangMalbolge.TritwiseOp
    | 'o' -> LangMalbolge.Nop
    | 'v' -> LangMalbolge.Stop
    | _ -> LangMalbolge.Unknown

let of_op = function
    | LangMalbolge.Jump -> 'i'
    | LangMalbolge.OutputChar -> '<'
    | LangMalbolge.InputChar -> '/'
    | LangMalbolge.Rotate -> '*'
    | LangMalbolge.Deref -> 'j'
    | LangMalbolge.TritwiseOp -> 'p'
    | LangMalbolge.Nop -> 'o'
    | LangMalbolge.Stop -> 'v'
    | LangMalbolge.Unknown -> failwith "undefined"

(**************************************************************************)
(* The kind. *)

type t = string
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "normalized-malbolge"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let buf = Buffer.create 64 in
        let rec read idx =
            match StreamUtil.try_next stream with
            | Some (' '|'\t'|'\r'|'\n') -> read idx
            | Some ('\033'..'\126' as c) ->
                if idx >= 59049 then
                    failwith "code too big";
                if to_op c = LangMalbolge.Unknown then
                    failwith "invalid instruction in the code";
                Buffer.add_char buf c;
                read (idx + 1)
            | Some _ -> failwith "invalid character in the code"
            | None ->
                if idx < 2 then (* see LangMalbolge for rationale *)
                    failwith "code too short";
                Buffer.contents buf
        in read 0
end

(**************************************************************************)
(* The Malbolge-to-normalized-Malbolge (and vice versa) transformers. *)

let from_malbolge = object
    inherit [LangMalbolge.t, t] EsotopeCommon.processor LangMalbolge.kind kind

    method process code =
        let len = String.length code in
        let code' = String.create len in
        for i = 0 to len - 1 do
            let op = LangMalbolge.to_op ((int_of_char code.[i] + i) mod 94) in
            code'.[i] <- of_op op
        done;
        code'
end

let to_malbolge = object
    inherit [t, LangMalbolge.t] EsotopeCommon.processor kind LangMalbolge.kind

    method process code =
        let len = String.length code in
        let code' = String.create len in
        for i = 0 to len - 1 do
            let op = to_op code.[i] in
            let memc = (LangMalbolge.of_op op + 94 - i mod 94) mod 94 in
            code'.[i] <- char_of_int (if memc < 33 then memc + 94 else memc)
        done;
        code'
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process code buf =
        Buffer.add_string buf code
end

