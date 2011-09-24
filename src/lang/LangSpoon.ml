(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangSpoon

 This module implements the Spoon programming language, designed by
 S. Goodwin in 1998. It is an almost trivial modification to Brainfuck,
 encoding every instructions in a binary prefix code using Huffman tree.
 Only the true difference with Brainfuck is the immediate exit instruction.
***************************************************************************)

module BF = LangBrainfuckWithExit

(**************************************************************************)
(* The kind. *)

type t = BF.t
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "spoon"
    method aliases = [".sp"]
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
                | Some '+' -> parse (BF.AdjustMemory (0, 1) :: acc)
                | Some '-' -> parse (BF.AdjustMemory (0, -1) :: acc)
                | Some '>' -> parse (BF.MovePointer 1 :: acc)
                | Some '<' -> parse (BF.MovePointer (-1) :: acc)
                | Some '.' -> parse (BF.Output 0 :: acc)
                | Some ',' -> parse (BF.Input 0 :: acc)
                | Some '[' ->
                    let nodes, eof = parse [] in
                    if eof then failwith "no matching '0011'" else
                    parse (BF.While (0, nodes) :: acc)
                | Some ']' -> (List.rev acc, false)
                | Some '#' -> parse (BF.Breakpoint :: acc)
                | Some '@' -> parse (BF.Exit :: acc)
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
        in

        (* see LangBrainfuck.writer#process for notes. *)
        let rec emit nodes =
            let emit_node = function
                | BF.Nop -> ()
                | BF.AdjustMemory (ref, delta) ->
                    emit_dir "010" "011" ref;
                    emit_dir "1" "000" delta;
                    emit_dir "011" "010" ref
                | BF.SetMemory (ref, value) ->
                    emit_dir "010" "011" ref;
                    Buffer.add_string buf "001000000011";
                    emit_dir "1" "000" value;
                    emit_dir "011" "010" ref
                | BF.MovePointer offset ->
                    emit_dir "010" "011" offset
                | BF.Input ref ->
                    emit_dir "010" "011" ref;
                    Buffer.add_string buf "0010110";
                    emit_dir "011" "010" ref
                | BF.Output ref ->
                    emit_dir "010" "011" ref;
                    Buffer.add_string buf "001010";
                    emit_dir "011" "010" ref
                | BF.While (ref, nodes) ->
                    emit_dir "010" "011" ref; Buffer.add_string buf "00100";
                    emit_dir "011" "010" ref; emit nodes; emit_dir "010" "011" ref;
                    Buffer.add_string buf "0011"; emit_dir "011" "010" ref
                | BF.Breakpoint ->
                    Buffer.add_string buf "00101110"
                | BF.Exit ->
                    Buffer.add_string buf "00101111"
                | BF.Comment s -> ()
            in List.iter emit_node nodes
        in

        emit nodes
end

(**************************************************************************)
(* The Brainfuck-to-Spoon and Spoon-to-Brainfuck transformer. *)

let to_brainfuck_with_exit = object
    inherit [t,t] EsotopeCommon.processor kind BF.kind
    method weight = 1
    method process nodes = nodes
end

let from_brainfuck_with_exit = object
    inherit [t,t] EsotopeCommon.processor BF.kind kind
    method weight = 1
    method process nodes = nodes
end

