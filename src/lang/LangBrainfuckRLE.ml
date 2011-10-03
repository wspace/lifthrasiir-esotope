(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangBrainfuckRLE

 This module implements a particular run-length encoding for the Brainfuck
 programming language (implemented as LangBrainfuck in Esotope). There are
 many such encodings, but Esotope chose to implement an encoding used by
 gcc-bf because there is very low change that the typical Brainfuck code
 contains a run-length sequence used by this encoding, unlike others.

 The encoding uses +*<count>, -*<count>, <*<count>, >*<count> for <count>
 copies of +, -, < and > respectively. <count> should be non-negative and
 there should be no whitespaces around *. Other uses of * and subsequent
 <count> are ignored. There is one-to-one correspondence between Brainfuck
 and this encoding.
***************************************************************************)

module BF = LangBrainfuck

(**************************************************************************)
(* The kind. *)

type t = LangBrainfuck.t
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "brainfuck-rle"
    method aliases = ["bf-rle"; "bfrle"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let rec parse acc =
            (* see LangBrainfuck.reader for details. the only change in this
             * module is collect_run function. *)
            let rec collect_run target count =
                match Stream.npeek 2 stream with
                | '*'::('0'..'9')::[] ->
                    let rec collect_digits buf =
                        match Stream.peek stream with
                        | Some ('0'..'9' as ch) ->
                            Stream.junk stream; Buffer.add_char buf ch;
                            collect_digits buf
                        | _ -> Buffer.contents buf
                    in
                    Stream.junk stream;
                    let i = int_of_string (collect_digits (Buffer.create 1)) in
                    (* as the preceding command is already counted, we should
                     * not count it. *)
                    collect_run target (count+i-1)
                | ch::_ when ch == target ->
                    Stream.junk stream;
                    collect_run target (count+1)
                | _ -> count
            in

            match Stream.peek stream with
            | Some '+' -> parse (BF.AdjustMemory (0, collect_run '+' 0) :: acc)
            | Some '-' -> parse (BF.AdjustMemory (0, -(collect_run '-' 0)) :: acc)
            | Some '>' -> parse (BF.MovePointer (collect_run '>' 0) :: acc)
            | Some '<' -> parse (BF.MovePointer (-(collect_run '<' 0)) :: acc)

            | Some '.' -> Stream.junk stream; parse (BF.Output 0 :: acc)
            | Some ',' -> Stream.junk stream; parse (BF.Input 0 :: acc)

            | Some '[' ->
                Stream.junk stream;
                let nodes, eof = parse [] in
                if eof then failwith "no matching ']'" else
                parse (BF.While (0, nodes) :: acc)
            | Some ']' -> Stream.junk stream; (List.rev acc, false)
            | None -> (List.rev acc, true)

            | Some '#' -> Stream.junk stream; parse (BF.Breakpoint :: acc)

            | Some _ ->
                let buf = Buffer.create 16 in
                let rec collect () =
                    match Stream.peek stream with
                    | Some ch when not (String.contains "+-><.,[]#" ch) ->
                        Stream.junk stream;
                        Buffer.add_char buf ch;
                        collect ()
                    | _ -> ()
                in collect (); parse (BF.Comment (Buffer.contents buf) :: acc)
        in

        let nodes, eof = parse [] in
        if not eof then
            failwith "no matching '['"
        else
            nodes
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let emit_dir plus minus v =
            if v > 3 then
                Printf.bprintf buf "%c*%d" plus v
            else if v > 0 then
                Buffer.add_string buf (String.make v plus)
            else if v < -3 then
                Printf.bprintf buf "%c*%d" minus (-v)
            else if v < 0 then
                Buffer.add_string buf (String.make (-v) minus)
        in

        let rec emit nodes =
            let emit_node = function
                | BF.Nop -> ()
                | BF.AdjustMemory (ref,delta) ->
                    emit_dir '>' '<' ref; (* empty when ref=0 *)
                    emit_dir '+' '-' delta;
                    emit_dir '<' '>' ref (* back up *)
                | BF.MovePointer off ->
                    emit_dir '>' '<' off
                | BF.Input ref ->
                    emit_dir '>' '<' ref;
                    Buffer.add_char buf ',';
                    emit_dir '<' '>' ref
                | BF.Output ref ->
                    emit_dir '>' '<' ref;
                    Buffer.add_char buf '.';
                    emit_dir '<' '>' ref
                | BF.While (ref,body) ->
                    (* this is safe even when the pointer is updated. *)
                    emit_dir '>' '<' ref; Buffer.add_char buf '[';
                    emit_dir '<' '>' ref; emit body; emit_dir '>' '<' ref;
                    Buffer.add_char buf ']'; emit_dir '<' '>' ref
                | BF.Breakpoint ->
                    Buffer.add_char buf '#'
                | BF.Comment s ->
                    Buffer.add_string buf s
            in List.iter emit_node nodes
        in

        emit nodes
end

(**************************************************************************)
(* The Brainfuck-to-BFRLE and BFRLE-to-Brainfuck transformer. (trivial!) *)

let to_brainfuck = object
    inherit [t, LangBrainfuck.t] EsotopeCommon.processor
        kind LangBrainfuck.kind

    method weight = 5
    method process nodes = nodes
end

let from_brainfuck = object
    inherit [LangBrainfuck.t, t] EsotopeCommon.processor
        LangBrainfuck.kind kind

    method weight = 5
    method process nodes = nodes
end

