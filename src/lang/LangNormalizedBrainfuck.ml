(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangNormalizedBrainfuck

 This module implements a normalized variant of the Brainfuck programming
 language (implemented as LangBrainfuck in Esotope). This language is very
 similar to Brainfuck, but has following differences:

 - There is no MovePointer node. Every non-local (i.e. relative to the
   current pointer) reference is propagated to individual nodes.
 - Only While node may move the pointer by fixed offset at the end of loop
   body; this is called the loop stride.
 - Likewise, there is no Exit node; there is a flag in While node which
   ends the execution if set.
 - Simple linear loops are replaced with dedicated nodes like SetMemory,
   AddMemory and CopyMemory. In particular, [-] maps to `SetMemory (0,0)`.
 - There is a limited amount of constant propagation (which can be done in
   a single scan).
***************************************************************************)

module BF = LangBrainfuckWithExit
module IR = LangBrainfuckIR

type ('memref, 'celltype) node =
    | Nop
    | AdjustMemory of 'memref * 'celltype               (* {x} += k *)
    | SetMemory of 'memref * 'celltype                  (* {x} = k *)
    | AddMemory of 'memref * 'celltype * 'memref        (* {x} += k * {y} *)
    | CopyMemory of 'memref * 'celltype * 'memref       (* {x} = k * {y} *)
    | Input of 'memref
    | Output of 'memref
    | While of 'memref * ('memref, 'celltype) node list * 'memref option
                                                        (* None for exit *)
    | Breakpoint

(**************************************************************************)
(* The kind. *)

type t = (int,int) node list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "normalized-brainfuck"
    method aliases = ["normalized-bf"]
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
                | Breakpoint -> Format.fprintf ff "Breakpoint[]"
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
(* The transformer to Brainfuck IR. *)

let to_ir = object
    inherit [t, IR.t] EsotopeCommon.processor kind IR.kind

    (* so BF -> BF w/ Exit -> normal BF -> IR has the same weight with
     * BF -> IR. *)
    method weight = 5

    method process nodes =
        let rec ir_of_node = function 
            | Nop -> IR.Nop
            | AdjustMemory (ref,delta) -> IR.AdjustMemory (ref,delta)
            | SetMemory (ref,value) -> IR.SetMemory (ref,value)
            | AddMemory (ref,scale,y) -> IR.AddMemory (ref,scale,y)
            | CopyMemory (ref,scale,y) -> IR.CopyMemory (ref,scale,y)
            | Input ref -> IR.Input ref
            | Output ref -> IR.Output ref
            | While (ref,body,stride) ->
                IR.While (ref, List.map ir_of_node body, stride)
            | Breakpoint -> IR.Breakpoint
        in List.map ir_of_node nodes
end

(**************************************************************************)
(* The normalizer. *)

module H = Hashtbl

let from_brainfuck = object
    inherit [BF.t, t] EsotopeCommon.processor BF.kind kind

    method weight = 4
    method optlevel = 1

    (* this process is quite straightforward except for one case: determining
     * the number of iterations in the linear loop. for example, given the
     * following C code (to which linear loops can be translated):
     *
     *     i = 0; for (j = 0; i != x; ++j) i += m;
     *
     * assuming that the overflow occurs when i >= w (256 for char, etc.),
     * will it eventually terminate and what is the final value of j (loop
     * count) if it does terminate? there are two cases possible:
     *
     * 1. the condition j * m = x (mod w) does not hold for any j. this case
     *    can happen when m = 0, or x is not a multiple of gcd(m,w).
     *    (e.g. given w = 256, x = 3, m = 4, i mod 4 is always 0 so i cannot
     *    equal to m.)
     * 2. the condition j * m = x (mod w) does hold for some j. in order to
     *    determine the actual j, first solve the following equation:
     *        u * m + v * w = gcd(m,w)
     *    this can be solved using the extended Euclidean algorithm. now we
     *    know that u * m = gcd(m,w) (mod w), so j = u * (x / gcd(m,w)).
     *
     * this particular pass computes this count only when x is statistically
     * determined, but it can be trivially extended to variable x with a
     * dynamic check for x being multiples of gcd(m,w). this is one example
     * of the scalar evolution (SCEV) of induction variables.
     *
     * TODO: scalar evolution on quadratic or cubic structures
     *)

    method process nodes =
        let flush_one state k acc =
            try
                let (set,v) = H.find state k in
                H.remove state k;
                (if set then SetMemory (k,v) else AdjustMemory (k,v)) :: acc
            with Not_found -> acc in

        let flush_all state acc =
            let f k (set,v) acc =
                (if set then SetMemory (k,v) else AdjustMemory (k,v)) :: acc in
            let acc = H.fold f state acc in
            H.clear state; acc in

        let rec propagate cur state acc = function
            | BF.AdjustMemory (ref,delta) :: nodes ->
                let ref' = ref + cur in
                let (set,value) =
                    try H.find state ref' with Not_found -> (false,0) in
                H.replace state ref' (set, value + delta);
                propagate cur state acc nodes
            | BF.MovePointer off :: nodes ->
                propagate (cur+off) state acc nodes
            | BF.Input ref :: nodes ->
                let ref' = ref + cur in
                let acc = Input ref' :: flush_one state ref' acc in
                propagate cur state acc nodes
            | BF.Output ref :: nodes ->
                let ref' = ref + cur in
                let acc = Output ref' :: flush_one state ref' acc in
                propagate cur state acc nodes

            | BF.While (ref,body) :: nodes ->
                let ref' = ref + cur in
                let state' = H.create 8 in
                let (cur',acc') = propagate cur state' [] body in

                let infinite_loop () =
                    let acc = While (ref',[],Some 0) :: acc in
                    propagate cur state acc nodes in
                begin match cur' with
                | Some cur' -> (* loop stride of cur' - cur *)
                    let stride = cur' - cur in
                    let prevpivot =
                        try H.find state ref' with Not_found -> (false,0) in
                    let pivot =
                        try H.find state' ref' with Not_found -> (false,0) in

                    begin match acc', stride, pivot, prevpivot with
                    | [], 0, (false,0), _ -> infinite_loop ()

                    | [], 0, (false,delta), (true,value) ->
                        (* linear loop with fixed loop count; all changes can
                         * be recorded to the state without touching nodes. *)
                        let normalize x = x land 255 (* TODO *) in
                        let (u,_,gcd) =
                            NumUtil.gcdex (normalize delta) 256 (* TODO *) in
                        let u' = normalize (u * (-value)) in
                        if u' mod gcd == 0 then begin
                            let count = u' / gcd in
                            let f k (set,v) =
                                let result =
                                    if k == ref' then
                                        (true,0)
                                    else
                                        let (set',v') =
                                            try H.find state k
                                            with Not_found -> (false,0) in
                                        if set then (set, normalize v)
                                        else (set', normalize (v'+v*count)) in
                                H.replace state k result in
                            H.iter f state';
                            propagate cur state acc nodes
                        end else infinite_loop ()

                    | [], 0, (false,-1), (false,delta') ->
                        (* linear loop with dynamic loop count, where that
                         * count equals to the value of current target cell.
                         *
                         * caveat: if there is a cell set (not just offset) by
                         * the body we cannot get rid of While node as it will
                         * affect the state once or never. we should check for
                         * it. *)
                        let contains_set =
                            H.fold (fun _ (set,_) flag -> set || flag)
                                   state' false in
                        if contains_set then
                            (* falls back to the normal while loop *)
                            let acc = flush_all state acc in
                            let body = List.rev (flush_all state' acc') in
                            let acc = While (ref',body,Some stride) :: acc in
                            propagate cur state acc nodes
                        else begin
                            let f k (_,v) acc =
                                if k == ref' then
                                    acc
                                else
                                    let prev =
                                        try H.find state k
                                        with Not_found -> (false,0) in
                                    if prev = (true,0) then begin
                                        (* "replace" the current state *)
                                        H.remove state k;
                                        CopyMemory (k,v,ref') :: acc
                                    end else begin
                                        let acc = flush_one state k acc in
                                        AddMemory (k,v,ref') :: acc
                                    end in
                            let acc = flush_one state ref' acc in
                            let acc = H.fold f state' acc in
                            H.replace state ref' (true,0);
                            propagate cur state acc nodes
                        end

                    | [], 0, (true,value), _ when value <> 0 -> infinite_loop ()

                    | _, _, _, _ ->
                        (* flush the state to the accumulator and add a While
                         * node. should flush the loop-body state to the
                         * corresponding accumulator too. *)
                        let acc = flush_all state acc in
                        let body = List.rev (flush_all state' acc') in
                        let acc = While (ref',body,Some stride) :: acc in
                        propagate cur state acc nodes
                    end

                | None ->
                    (* once entered the program terminates in the body. since
                     * the remaining loop-body state does not affect the output,
                     * we completely ignore the state. *)
                    let body = List.rev acc' in
                    let acc = While (ref',body,None) :: acc in
                    propagate cur state acc nodes
                end

            | BF.Breakpoint :: nodes ->
                let acc = Breakpoint :: flush_all state acc in
                propagate cur state acc nodes
            | BF.Exit :: _ -> (* remaining nodes are irrelevant *)
                (None, acc)
            | (BF.Nop | BF.Comment _) :: nodes ->
                propagate cur state acc nodes
            | [] ->
                (* we don't flush the state to the accumulator here, since the
                 * callee should analyze the state to process linear loops. *)
                (Some cur, acc)
        in

        let state = H.create 8 in
        let (_,nodes') = propagate 0 state [] nodes in
        List.rev nodes'
end

