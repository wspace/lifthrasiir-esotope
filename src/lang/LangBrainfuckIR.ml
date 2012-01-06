(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangBrainfuckIR

 This module implements an internal representation inspired from the
 Brainfuck programming language.

 The IR is designed as a common kind for transformers, unlike other
 Brainfuck-related kinds. In fact, the IR contains all elements of other
 kinds and is not suitable for the direct optimization. Since other kinds
 can be trivially transformed into the IR, however, it should be easy to
 apply the varying degree of optimization to the source Brainfuck code
 before transforming it to the other kind.
***************************************************************************)

type ('memref, 'celltype) node =
    | Nop
    | AdjustMemory of 'memref * 'celltype               (* {x} += k *)
    | MovePointer of 'memref
    | SetMemory of 'memref * 'celltype                  (* {x} = k *)
    | AddMemory of 'memref * 'celltype * 'memref        (* {x} += k * {y} *)
    | CopyMemory of 'memref * 'celltype * 'memref       (* {x} = k * {y} *)
    | Input of 'memref
    | Output of 'memref
    | While of 'memref * ('memref, 'celltype) node list * 'memref option
                                                        (* None for exit *)
    | Breakpoint
    | Exit
    | Comment of string

(**************************************************************************)
(* The kind. *)

type t = (int,int) node list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "brainfuck-ir"
    method aliases = ["bf-ir"; "bfir"]
end

let inspector = object
    inherit [t] EsotopeCommon.inspector kind
    method process nodes ff =
        let rec emit nodes =
            let emit_node = function
                | Nop -> Format.fprintf ff "Nop[]"
                | AdjustMemory (ref,delta) ->
                    if delta < 0 then
                        Format.fprintf ff "{%d}-=%d" ref (-delta)
                    else
                        Format.fprintf ff "{%d}+=%d" ref delta
                | MovePointer ref ->
                    Format.fprintf ff "@@%d" ref
                | SetMemory (ref,value) ->
                    Format.fprintf ff "{%d}=%d" ref value
                | AddMemory (ref,1,y) ->
                    Format.fprintf ff "{%d}+={%d}" ref y
                | AddMemory (ref,-1,y) ->
                    Format.fprintf ff "{%d}-={%d}" ref y
                | AddMemory (ref,scale,y) ->
                    if scale < 0 then
                        Format.fprintf ff "{%d}-=%d*{%d}" ref (-scale) y
                    else
                        Format.fprintf ff "{%d}+=%d*{%d}" ref scale y
                | CopyMemory (ref,1,y) ->
                    Format.fprintf ff "{%d}={%d}" ref y
                | CopyMemory (ref,-1,y) ->
                    Format.fprintf ff "{%d}=-{%d}" ref y
                | CopyMemory (ref,scale,y) ->
                    Format.fprintf ff "{%d}=%d*{%d}" ref scale y
                | Input ref -> Format.fprintf ff "Input[{%d}]" ref
                | Output ref -> Format.fprintf ff "Output[{%d}]" ref
                | While (ref,body,Some 0) ->
                    Format.fprintf ff "While[{%d}; " ref; emit body;
                    Format.fprintf ff "]"
                | While (ref,[],Some stride) ->
                    Format.fprintf ff "While[{%d}; @@%d]" ref stride
                | While (ref,body,Some stride) ->
                    Format.fprintf ff "While[{%d}; " ref; emit body;
                    Format.fprintf ff "; @@%d]" stride
                | While (ref,[],None) ->
                    Format.fprintf ff "While[{%d}; @@!]" ref
                | While (ref,body,None) ->
                    Format.fprintf ff "While[{%d}; " ref; emit body;
                    Format.fprintf ff "; @@!]"
                | Exit -> Format.fprintf ff "Exit[]"
                | Breakpoint -> Format.fprintf ff "Breakpoint[]"
                | Comment s -> Format.fprintf ff "Comment[%S]" s
            in

            let rec emit_nodes = function
                | [] -> ()
                | [node] -> emit_node node
                | h::t -> emit_node h; Format.fprintf ff ",@ "; emit_nodes t
            in 

            Format.fprintf ff "@["; emit_nodes nodes; Format.fprintf ff "@]"
        in emit nodes
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process nodes io =
        let mem = Tape.create 4096 0 in
        let normalize x = x land 255 in
        let rec exec = function
            | [] -> ()
            | h::t -> match h with
                | AdjustMemory (ref,delta) ->
                    Tape.set ref (normalize (Tape.get ref mem + delta)) mem;
                    exec t
                | MovePointer ref ->
                    Tape.offset ref mem; exec t
                | SetMemory (ref,value) ->
                    Tape.set ref value mem; exec t
                | AddMemory (ref,scale,y) ->
                    let delta = scale * Tape.get y mem in
                    Tape.set ref (normalize (Tape.get ref mem + delta)) mem;
                    exec t
                | CopyMemory (ref,scale,y) ->
                    let value = scale * Tape.get y mem in
                    Tape.set ref (normalize value) mem; exec t
                | Input ref ->
                    begin match io#get_code None with
                    | Some x -> Tape.set ref (normalize x) mem
                    | None -> ()
                    end;
                    exec t
                | Output ref ->
                    io#put_code (Tape.get ref mem); io#flush_out (); exec t
                | While (ref,nodes,Some stride) ->
                    while Tape.get ref mem <> 0 do
                        exec nodes; Tape.offset stride mem
                    done;
                    exec t
                | While (ref,nodes,None) ->
                    if Tape.get ref mem <> 0 then exec nodes
                | Exit -> ()
                | Breakpoint -> (* TODO *)
                    (*prerr_endline "Breakpoint reached.";*) exec t
                | Comment _ | Nop -> exec t
        in exec nodes
end

