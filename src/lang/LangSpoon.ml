(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangSpoon

 This module implements the Spoon programming language, designed by
 S. Goodwin in 1998. It is an almost trivial modification to Brainfuck,
 encoding every instructions in a binary prefix code using Huffman tree.

 One significant addition of Spoon to Brainfuck is an addition of "exit"
 instruction, encoded as 00101111. If this instruction is present in the
 middle of loop the translation is not trivial. (TODO)
***************************************************************************)

type ('memref, 'celltype) node =
    | Nop
    | AdjustMemory of 'memref * 'celltype
    | SetMemory of 'memref * 'celltype
    | MovePointer of 'memref
    | Input of 'memref
    | Output of 'memref
    | While of 'memref * ('memref, 'celltype) node list
    | Breakpoint
    | Exit
    | Comment of string

(**************************************************************************)
(* The kind. *)

type t = (int,int) node list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "spoon"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let huffman_tree =
            `B (`B (`B (`L '-',                          (* 000 *)
                        `B (`B (`L '[',                  (* 00100 *)
                                `B (`L '.',              (* 001010 *)
                                    `B (`L ',',          (* 0010110 *)
                                        `B (`L '#',      (* 00101110 *)
                                            `L '@')))),  (* 00101111 *)
                            `L ']')),                    (* 0011 *)
                    `B (`L '>',                          (* 010 *)
                        `L '<')),                        (* 011 *)
                `L '+')                                  (* 1 *)
        in

        let state = ref (`L '?') in (* to be immediately overwritten *)
        let update bit =
            begin match !state with
            | `B _ -> ()
            | `L _ -> state := huffman_tree
            end;
            begin match (bit, !state) with
            | (false, `B (x,_)) -> state := x
            | (true, `B (_,x)) -> state := x
            | _ -> failwith "unexpected"
            end;
            match !state with
            | `B _ -> None
            | `L t -> Some t
        in

        let rec parse acc =
            match StreamUtil.try_next stream with
            | Some (('0'|'1') as ch) ->
                begin match update (ch = '1') with
                | Some '+' -> parse (AdjustMemory (0, 1) :: acc)
                | Some '-' -> parse (AdjustMemory (0, -1) :: acc)
                | Some '>' -> parse (MovePointer 1 :: acc)
                | Some '<' -> parse (MovePointer (-1) :: acc)
                | Some '.' -> parse (Output 0 :: acc)
                | Some ',' -> parse (Input 0 :: acc)
                | Some '[' ->
                    let nodes, eof = parse [] in
                    if eof then failwith "no matching '0011'" else
                    parse (While (0, nodes) :: acc)
                | Some ']' -> (List.rev acc, false)
                | Some '#' -> parse (Breakpoint :: acc)
                | Some '@' -> parse (Exit :: acc)
                | Some _ | None -> parse acc
                end
            | Some (' '|'\t'|'\r'|'\n') -> parse acc
            | Some _ ->
                failwith "character other than '0' or '1' encountered."
            | None ->
                match !state with
                | `L _ -> (List.rev acc, true)
                | `B _ -> (* the tree is not completely consumed *)
                    failwith "the end of file in the middle of instruction"
        in

        let nodes, eof = parse [] in
        if not eof then
            failwith "no matching '00100'"
        else
            nodes
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let rec repeat n s =
            if n = 0 then ""
            else if n = 1 then s
            else
                let s' = repeat (n / 2) s in
                if n mod 2 = 0 then s' ^ s' else s' ^ s' ^ s
        in

        let emit_dir plus minus v =
            if v > 0 then
                Buffer.add_string buf (repeat v plus)
            else if v < 0 then
                Buffer.add_string buf (repeat (-v) minus)
            else
                ()
        in

        (* see LangBrainfuck.writer#process for notes. *)
        let rec emit nodes =
            let emit_node = function
                | Nop -> ()
                | AdjustMemory (ref, delta) ->
                    emit_dir "010" "011" ref;
                    emit_dir "1" "000" delta;
                    emit_dir "011" "010" ref
                | SetMemory (ref, value) ->
                    emit_dir "010" "011" ref;
                    Buffer.add_string buf "001000000011";
                    emit_dir "1" "000" value;
                    emit_dir "011" "010" ref
                | MovePointer offset ->
                    emit_dir "010" "011" offset
                | Input ref ->
                    emit_dir "010" "011" ref;
                    Buffer.add_string buf "0010110";
                    emit_dir "011" "010" ref
                | Output ref ->
                    emit_dir "010" "011" ref;
                    Buffer.add_string buf "001010";
                    emit_dir "011" "010" ref
                | While (ref, nodes) ->
                    emit_dir "010" "011" ref; Buffer.add_string buf "00100";
                    emit_dir "011" "010" ref; emit nodes; emit_dir "010" "011" ref;
                    Buffer.add_string buf "0011"; emit_dir "011" "010" ref
                | Breakpoint ->
                    Buffer.add_string buf "00101110"
                | Exit ->
                    Buffer.add_string buf "00101111"
                | Comment s -> ()
            in List.iter emit_node nodes
        in

        emit nodes
end

(**************************************************************************)
(* The Brainfuck-to-Spoon and Spoon-to-Brainfuck transformer. *)

let to_brainfuck = object
    inherit [t, LangBrainfuck.t] EsotopeCommon.processor
        kind LangBrainfuck.kind

    method weight = 5
    method process nodes =
        let rec has_unsafe_exit topmost nodes =
            let check = function
                | Exit -> true
                | While (_,nodes) -> has_unsafe_exit false nodes
                | _ -> false in
            let reduce (prior, prev) cur = (prior || prev, check cur) in
            let others, last = List.fold_left reduce (false,false) nodes in
            others || (last && not topmost)
        in

        if has_unsafe_exit true nodes then
            failwith "not yet implemented"
        else
            let rec process' = function
                | Nop -> LangBrainfuck.Nop
                | AdjustMemory (ref,delta) ->
                    LangBrainfuck.AdjustMemory (ref,delta)
                | SetMemory (ref,value) ->
                    LangBrainfuck.SetMemory (ref,value)
                | MovePointer ref -> LangBrainfuck.MovePointer ref
                | Input ref -> LangBrainfuck.Input ref
                | Output ref -> LangBrainfuck.Output ref
                | While (ref,nodes) ->
                    LangBrainfuck.While (ref, List.map process' nodes)
                | Breakpoint -> LangBrainfuck.Breakpoint
                | Exit ->
                    (* we are sure that this appears at the very end only. *)
                    LangBrainfuck.Nop
                | Comment s -> LangBrainfuck.Comment s
            in List.map process' nodes
end

let from_brainfuck = object
    inherit [LangBrainfuck.t, t] EsotopeCommon.processor
        LangBrainfuck.kind kind

    method weight = 5
    method process nodes =
        let rec process' = function
            | LangBrainfuck.Nop -> Nop
            | LangBrainfuck.AdjustMemory (ref,delta) ->
                AdjustMemory (ref,delta)
            | LangBrainfuck.SetMemory (ref,value) ->
                SetMemory (ref,value)
            | LangBrainfuck.MovePointer ref -> MovePointer ref
            | LangBrainfuck.Input ref -> Input ref
            | LangBrainfuck.Output ref -> Output ref
            | LangBrainfuck.While (ref,nodes) ->
                While (ref, List.map process' nodes)
            | LangBrainfuck.Breakpoint -> Breakpoint
            | LangBrainfuck.Comment s -> Comment s
        in List.map process' nodes
end

