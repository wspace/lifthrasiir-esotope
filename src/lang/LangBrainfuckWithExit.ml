(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangBrainfuckWithExit

 This module implements a trivial extension to Brainfuck programming
 language: an immediate exit instruction. Having this intermediate language
 is essential for supporting various Brainfuck-like languages.
***************************************************************************)

module BF = LangBrainfuck

type ('memref, 'celltype) node =
    | Nop
    | AdjustMemory of 'memref * 'celltype
    | MovePointer of 'memref
    | Input of 'memref
    | Output of 'memref
    | While of 'memref * ('memref, 'celltype) node list
    | Breakpoint
    | Exit
    | Comment of string

let rec node_of_brainfuck = function
    | BF.Nop -> Nop
    | BF.AdjustMemory (ref,delta) -> AdjustMemory (ref,delta)
    | BF.MovePointer ref -> MovePointer ref
    | BF.Input ref -> Input ref
    | BF.Output ref -> Output ref
    | BF.While (ref,body) -> While (ref, List.map node_of_brainfuck body)
    | BF.Breakpoint -> Breakpoint
    | BF.Comment s -> Comment s

let rec brainfuck_of_node handle_exit = function
    | Nop -> BF.Nop
    | AdjustMemory (ref,delta) -> BF.AdjustMemory (ref,delta)
    | MovePointer ref -> BF.MovePointer ref
    | Input ref -> BF.Input ref
    | Output ref -> BF.Output ref
    | While (ref,body) ->
        BF.While (ref, List.map (brainfuck_of_node handle_exit) body)
    | Breakpoint -> BF.Breakpoint
    | Exit -> handle_exit ()
    | Comment s -> BF.Comment s

(**************************************************************************)
(* The kind. *)

type t = (int,int) node list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "brainfuck-with-exit"
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process nodes io =
        let mem = Array.make 30000 0 in
        let ptr = ref 0 in
        let normalize x = x land 255 in
        let rec exec = function
            | [] -> ()
            | h::t -> match h with
                | AdjustMemory (ref,delta) ->
                    mem.(!ptr + ref) <- normalize (mem.(!ptr + ref) + delta);
                    exec t
                | MovePointer off ->
                    ptr := !ptr + off; exec t
                | Input ref ->
                    begin match io#get_code None with
                    | Some x -> mem.(!ptr + ref) <- x
                    | None -> ()
                    end; exec t
                | Output ref ->
                    io#put_code mem.(!ptr + ref); io#flush_out (); exec t
                | While (ref,body) ->
                    while mem.(!ptr + ref) <> 0 do exec body done; exec t
                | Breakpoint -> (* TODO *)
                    () (*prerr_endline "Breakpoint reached."*); exec t
                | Exit -> ()
                | Nop | Comment _ -> exec t
        in exec nodes
end

(**************************************************************************)
(* The transformer between Brainfuck with Exit and plain Brainfuck *)

let to_brainfuck = object
    inherit [t, LangBrainfuck.t] EsotopeCommon.processor
        kind LangBrainfuck.kind

    method weight = 5
    method process nodes =
        let rec has_unsafe_exit topmost nodes =
            let check = function
                | Exit -> true
                | While (_,body) -> has_unsafe_exit false body
                | _ -> false in
            let reduce (prior, prev) cur = (prior || prev, check cur) in
            let others, last = List.fold_left reduce (false,false) nodes in
            others || (last && not topmost)
        in

        if has_unsafe_exit true nodes then
            failwith "not yet implemented"
        else
            (* we are sure that Exit appears at the very end only. *)
            let handle_exit () = BF.Nop in
            List.map (brainfuck_of_node handle_exit) nodes
end

let from_brainfuck = object
    inherit [LangBrainfuck.t, t] EsotopeCommon.processor
        LangBrainfuck.kind kind

    method weight = 5
    method process nodes = List.map node_of_brainfuck nodes
end

