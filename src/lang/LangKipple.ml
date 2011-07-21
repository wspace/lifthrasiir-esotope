(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangKipple

 This module implements the Kipple programming language, designed by
 Rune Berge in 2003. It is a minimalistic stack-based programming language,
 where the only loop construct is a loop while the target stack is not
 empty and the only way to pop from the stack is to (conditionally) empty
 the stack using x? statement. The input and output is emulated as a stack
 "i" and "o", which precludes the very possibility of user interaction.

 Esotope implements a conservative extension to Kipple which allows
 a certain class of programs for the user interaction, as a separate
 module LangLazyKipple. (TODO)
***************************************************************************)

open Int32
open LangKipple_ast

(**************************************************************************)
(* The kind. *)

let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "kipple"
    method aliases = [".k"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.parsing_reader kind
    method process = LangKipple_parser.main LangKipple_lexer.token
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process nodes io =
        let rec all_input acc =
            match io#get_code None with
            | Some c -> all_input (of_int c :: acc)
            | None -> acc
        in

        let stacks =
            [| [];                                           (* @ *)
               []; []; []; []; []; []; []; []; all_input []; (* a -- i *)
               []; []; []; []; []; []; []; []; [];           (* j -- r *)
               []; []; []; []; []; []; []; [] |]             (* s -- z *)
        in

        let push s v =
            if s = 0 then begin
                let st = ref stacks.(s) in
                String.iter (fun c -> st := of_int (int_of_char c) :: !st)
                            (to_string v);
                stacks.(s) <- !st
            end else
                stacks.(s) <- v :: stacks.(s)
        in
        let peek s =
            match stacks.(s) with
            | h::t -> h
            | [] -> 0l
        in
        let pop s =
            match stacks.(s) with
            | h::t -> stacks.(s) <- t; h
            | [] -> 0l
        in

        let rec exec nodes =
            let exec_node = function
                | Nop -> ()
                | Push (s,v) -> push s v
                | Move (s1,s2) -> push s1 (pop s2)
                | Add (s,v) ->
                    let v0 = peek s in push s (add v0 v)
                | AddPop (s1,s2) ->
                    let v0 = peek s1 in push s1 (add v0 (pop s2))
                | Sub (s,v) ->
                    let v0 = peek s in push s (sub v0 v)
                | SubPop (s1,s2) ->
                    let v0 = peek s1 in push s1 (sub v0 (pop s2))
                | ClearIfZero s ->
                    if peek s = zero then stacks.(s) <- []
                | While (s,nodes) ->
                    while stacks.(s) <> [] do exec nodes done
            in List.iter exec_node nodes
        in exec nodes;

        List.iter (fun c -> io#put_code (to_int c)) stacks.(15)
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let stack s = Char.lowercase (char_of_int (int_of_char '@' + s)) in

        let rec emit = function
            | Nop -> ()
            | Push (s,v) ->
                Printf.bprintf buf "%c<%ld" (stack s) v
            | Move (s1,s2) ->
                Printf.bprintf buf "%c<%c" (stack s1) (stack s2)
            | Add (s,v) ->
                Printf.bprintf buf "%c+%ld" (stack s) v
            | AddPop (s1,s2) ->
                Printf.bprintf buf "%c+%c" (stack s1) (stack s2)
            | Sub (s,v) ->
                Printf.bprintf buf "%c-%ld" (stack s) v
            | SubPop (s1,s2) ->
                Printf.bprintf buf "%c-%c" (stack s1) (stack s2)
            | ClearIfZero s ->
                Printf.bprintf buf "%c?" (stack s)
            | While (s,nodes) ->
                Printf.bprintf buf "(%c" (stack s);
                List.iter sp_emit nodes;
                Printf.bprintf buf ")"
        and sp_emit node =
            Buffer.add_char buf ' '; emit node
        in

        match nodes with
        | [] -> ()
        | h::t -> emit h; List.iter sp_emit t
end

