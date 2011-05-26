(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangUnlambda

 This module implements the Unlambda programming language, designed by
 David Madore in 1999. It is a classic esoteric programming language with
 (impure) functional features, featuring the SKI combinators, continuation
 and delayed computation. Some, however, regards Unlambda not fully
 functional language due to the presence of side effects. If you agree on
 this claim, it would be worthwhile to look at Lazy K instead.

 The current interpreter is straightforward, manually constructed with the
 continuation-passing style (CPS). "d" function is handled specially during
 the evaluation.
***************************************************************************)

open LangUnlambda_ast

(**************************************************************************)
(* The kind. *)

let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "unlambda"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.parsing_reader kind
    method process = LangUnlambda_parser.main LangUnlambda_lexer.token
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] EsotopeCommon.interpreter kind

    method process node =
        let inchan = stdin in
        let outchan = stdout in
        let currentch = ref None in
        let rec apply = function
            | K -> (fun x cont -> cont (K1 x))
            | K1 x -> (fun y cont -> cont x)
            | S -> (fun x cont -> cont (S1 x))
            | S1 x -> (fun y cont -> cont (S2 (x,y)))
            | S2 (x,y) ->
                fun z cont -> exec (App (App (x,z), App (y,z))) cont
            | I -> (fun x cont -> cont x)
            | Void -> (fun x cont -> cont Void)
            | Delay ->
                (* App (Delay x) y or equivalent is special form. *)
                failwith "d function cannot be directly applied."
            | Delay1 x -> (fun y cont -> exec (App (x,y)) cont)
            | Callcc ->
                (fun x cont -> exec (App (x, Extern (fun _ -> cont))) cont)
            | Print ch ->
                fun x cont ->
                    output_char outchan ch;
                    flush outchan;
                    cont x
            | PrintNewline ->
                fun x cont ->
                    output_char outchan '\n';
                    flush outchan;
                    cont x
            | Exit ->
                (* ignore the current continutation *)
                fun x cont -> x
            | Read ->
                fun x cont ->
                    begin try
                        currentch := Some (input_char inchan);
                        exec (App (x,I)) cont
                    with End_of_file ->
                        currentch := None;
                        exec (App (x,Void)) cont
                    end
            | CheckRead ch ->
                fun x cont ->
                    begin match !currentch with
                    | Some ch' when ch = ch' -> exec (App (x,I)) cont
                    | _ -> exec (App (x,Void)) cont
                    end
            | Reprint ->
                fun x cont ->
                    begin match !currentch with
                    | Some ch -> exec (App (x,Print ch)) cont
                    | _ -> exec (App (x,Void)) cont
                    end
            | Extern f -> (fun x cont -> f cont x)
            | App _ -> failwith "unexpected"
        and exec node cont =
            match node with
            | App (x,y) ->
                exec x (function Delay -> cont (Delay1 y)
                               | x' -> exec y (fun y' -> apply x' y' cont))
            | node -> cont node
        in ignore (exec node (fun x -> x))
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process node buf =
        let put = Buffer.add_char buf in
        let rec emit = function
            | K -> put 'k'
            | K1 x -> put '`'; put 'k'; emit x
            | S -> put 's'
            | S1 x -> put '`'; put 's'; emit x
            | S2 (x,y) -> put '`'; put '`'; put 's'; emit x; emit y
            | I -> put 'i'
            | Void -> put 'v'
            | Delay -> put 'd'
            | Delay1 x -> put '`'; put 'd'; emit x
            | Callcc -> put 'c'
            | Print ch -> put '.'; put ch
            | PrintNewline -> put 'r'
            | Exit -> put 'e'
            | Read -> put '@'
            | CheckRead ch -> put '?'; put ch
            | Reprint -> put '|'
            | Extern _ -> failwith "Extern node is for internal use only."
            | App (x, y) -> put '`'; emit x; emit y
        in emit node
end

(**************************************************************************)
(* The text generator. *)

let from_text = object
    inherit [LangText.t, t] EsotopeCommon.processor LangText.kind kind

    method process s =
        if s = "" then
            Void
        else
            let printnode ch =
                if ch = '\n' then PrintNewline else Print ch in
            let node = ref (printnode s.[0]) in
            for i = 1 to (String.length s) - 1 do
                node := App (!node, printnode s.[i])
            done;
            App (!node, Void)
end

