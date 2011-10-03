(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangBrainfuck

 This module implements the Brainfuck programming language, designed by
 Urban Müller in 1993. This is a starting point of many esolang enthusiasts,
 and also a crucial point in Esotope (as many other languages implement the
 reduction from/into Brainfuck).
 
 The actual language implemented here is trivially reducible, but not
 identical, to Brainfuck; every memory-referencing commands may refer the
 remote memory location using a relative address. This format makes an
 initial part of the optimization trivial, though the reader itself does
 not perform the optimization unless requested. Therefore the writer
 directly connected to the reader will produce a byte-identical result.
***************************************************************************)

type ('memref, 'celltype) node =
    | Nop
    | AdjustMemory of 'memref * 'celltype
    | MovePointer of 'memref
    | Input of 'memref
    | Output of 'memref
    | While of 'memref * ('memref, 'celltype) node list
    | Breakpoint                (* "#" *)
    | Comment of string

(**************************************************************************)
(* The kind. *)

type t = (int,int) node list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "brainfuck"
    method aliases = ["bf"; ".b"] (* ".bf" is used for Befunge-93. *)
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let rec parse acc =
            let rec collect_run target count =
                match Stream.peek stream with
                | Some ch when ch == target ->
                    Stream.junk stream;
                    collect_run target (count+1)
                | _ -> count
            in

            match Stream.peek stream with
            (* we do compress a run of identical commands, but we don't
             * compress a run of different commands even when we can do.
             * the compression is handled by the separate optimization. *)
            | Some '+' -> parse (AdjustMemory (0, collect_run '+' 0) :: acc)
            | Some '-' -> parse (AdjustMemory (0, -(collect_run '-' 0)) :: acc)
            | Some '>' -> parse (MovePointer (collect_run '>' 0) :: acc)
            | Some '<' -> parse (MovePointer (-(collect_run '<' 0)) :: acc)

            | Some '.' -> Stream.junk stream; parse (Output 0 :: acc)
            | Some ',' -> Stream.junk stream; parse (Input 0 :: acc)

            | Some '[' ->
                Stream.junk stream;
                let body, eof = parse [] in
                if eof then failwith "no matching ']'" else
                parse (While (0, body) :: acc)
            | Some ']' -> Stream.junk stream; (List.rev acc, false)
            | None -> (List.rev acc, true)

            (* no matter whether we recognize this command, we parse it anyway.
             * it is equivalent to Comment "#" when ignored. *)
            | Some '#' -> Stream.junk stream; parse (Breakpoint :: acc)

            | Some _ ->
                let buf = Buffer.create 16 in
                let rec collect () =
                    match Stream.peek stream with
                    | Some ch when not (String.contains "+-><.,[]#" ch) ->
                        Stream.junk stream;
                        Buffer.add_char buf ch;
                        collect ()
                    | _ -> ()
                in collect (); parse (Comment (Buffer.contents buf) :: acc)
        in

        let nodes, eof = parse [] in
        if not eof then
            failwith "no matching '['"
        else
            nodes
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process nodes io =
        let mem = Array.make 30000 0 in
        let ptr = ref 0 in
        let normalize x = x land 255 in
        let rec exec nodes =
            let exec_node = function
                | AdjustMemory (ref,delta) ->
                    mem.(!ptr + ref) <- normalize (mem.(!ptr + ref) + delta)
                | MovePointer off ->
                    ptr := !ptr + off
                | Input ref ->
                    begin match io#get_code None with
                    | Some x -> mem.(!ptr + ref) <- x
                    | None -> ()
                    end
                | Output ref ->
                    io#put_code mem.(!ptr + ref); io#flush_out ()
                | While (ref,body) ->
                    while mem.(!ptr + ref) <> 0 do exec body done
                | Breakpoint -> (* TODO *)
                    () (*prerr_endline "Breakpoint reached."*)
                | Nop | Comment _ -> ()
            in List.iter exec_node nodes
        in exec nodes
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let emit_dir plus minus v =
            if v > 0 then
                Buffer.add_string buf (String.make v plus)
            else if v < 0 then
                Buffer.add_string buf (String.make (-v) minus)
        in

        let rec emit nodes =
            let emit_node = function
                | Nop -> ()
                | AdjustMemory (ref,delta) ->
                    emit_dir '>' '<' ref; (* empty when ref=0 *)
                    emit_dir '+' '-' delta;
                    emit_dir '<' '>' ref (* back up *)
                | MovePointer off ->
                    emit_dir '>' '<' off
                | Input ref ->
                    emit_dir '>' '<' ref;
                    Buffer.add_char buf ',';
                    emit_dir '<' '>' ref
                | Output ref ->
                    emit_dir '>' '<' ref;
                    Buffer.add_char buf '.';
                    emit_dir '<' '>' ref
                | While (ref,body) ->
                    (* this is safe even when the pointer is updated. *)
                    emit_dir '>' '<' ref; Buffer.add_char buf '[';
                    emit_dir '<' '>' ref; emit body; emit_dir '>' '<' ref;
                    Buffer.add_char buf ']'; emit_dir '<' '>' ref
                | Breakpoint ->
                    Buffer.add_char buf '#'
                | Comment s ->
                    Buffer.add_string buf s
            in List.iter emit_node nodes
        in

        emit nodes
end

(**************************************************************************)
(* The text generator. *)

let from_text = object
    inherit [LangText.t, t] EsotopeCommon.processor LangText.kind kind

    method process s =
        (* TODO this algorithm is too simple. *)
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
                nodes := AdjustMemory (0, deltazero) ::
                         While (0, [AdjustMemory (0, -1)]) :: !nodes
            else
                nodes := AdjustMemory (0, delta) :: !nodes;
            nodes := Output 0 :: !nodes;
            last := ch
        done;
        List.rev !nodes
end

(**************************************************************************)
(* The basic Brainfuck-to-C translater. *)

(* TODO
let to_c = object
    inherit [t, LangC.t] EsotopeCommon.processor kind LangC.kind

    method process nodes buf =
        let rec emit' indent nodes =
            let rec emit_single_node indent node = match node with
                | Nop -> ()
                | AdjustMemory (target, delta) ->
                    Printf.bprintf buf "%sp[%d] += %d;\n" indent target delta
                | MovePointer offset ->
                    Printf.bprintf buf "%sp += %d;\n" indent offset
                | Input target ->
                    Printf.bprintf buf "%sp[%d] = getchar();\n" indent target
                | Output target ->
                    Printf.bprintf buf "%sputchar(p[%d]);\n" indent target
                | While (target, nodes) ->
                    Printf.bprintf buf "%swhile (p[%d]) {\n" indent target;
                    emit' (indent ^ "\t") nodes;
                    Printf.bprintf buf "%s}\n" indent
                | Breakpoint ->
                    Printf.bprintf buf "%s/* breakpoint */\n" indent
                | Comment s ->
                    Printf.bprintf buf "%s/* comment: %s */\n" indent s
            in
            List.iter (emit_single_node indent) nodes
        in

        Printf.bprintf buf "/* generated by esotope-bfc (ocaml ed.) */\n\
                            #include <stdio.h>\n\
                            #include <stdint.h>\n\
                            uint8_t m[30000], *p = m;\n\
                            int main(void) {\n";
        emit' "\t" nodes;
        Printf.bprintf buf "\treturn 0;\n\
                            }\n"
end
*)

