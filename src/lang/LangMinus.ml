(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangMinus

 This module implements the Minus programming language, designed by
 Darren Smith in 2007. It has only one instruction, which subtracts a given
 variable with an other variable or integer. There are only 52 variables
 (A-Z and a-z), but 26 of them is mapped to a consecutive portion of the
 infinite memory (a la Brainfuck) and some others are mapped to the
 instruction pointer, I/O and random number generator.
***************************************************************************)

open LangMinus_ast

(**************************************************************************)
(* The kind. *)

let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "minus"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.parsing_reader kind
    method process = LangMinus_parser.main LangMinus_lexer.token
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] EsotopeCommon.interpreter kind

    method process nodes =
        let memsize = 30000 (* TODO *) in
        let vars = [| 0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
                      0; 0; 0; 0; 0; 0; 0; 0; 0; 0;
                      0; -memsize; -32; -10; -2; -1 |] in
        let memory = Array.make (memsize * 2 + 26) 0 in
        memory.(memsize * 2 + 25) <- -1;

        let inchan = stdin in
        let outchan = stdout in
        let code = Array.of_list nodes in
        let codeptr = ref 0 in
        let memptr = ref 0 in
        let randrange = ref 0 in

        let eval_rhs = function
            | Var v -> vars.(v)
            | MemRef ref -> memory.(memsize + ref + !memptr)
            | Number x -> x
            | CodePtr -> !codeptr
            | MemPtr -> !memptr
            | InputChar ->
                begin
                    try int_of_char (input_char inchan)
                    with End_of_file -> -1
                end
            | InputNum ->
                begin
                    try Scanf.fscanf inchan "%_[^-0-9]%d" (fun x -> x)
                    with End_of_file -> 0 (* as C interpreter does *)
                end
            | Random ->
                let range = !randrange in
                if range > 0 then
                    Random.int range
                else if range < 0 then
                    -(Random.int (-range))
                else
                    0
            | Breakpoint -> (* TODO proper output *)
                Printf.eprintf "Breakpoint encountered.\n";
                0
            | _ -> failwith "unexpected"
        in

        let eval_lhs rhs = function
            | Var v -> vars.(v) <- vars.(v) - rhs
            | MemRef ref ->
                let i = memsize + ref + !memptr in
                memory.(i) <- memory.(i) - rhs
            | CodePtr -> codeptr := !codeptr - rhs
            | MemPtr -> memptr := !memptr - rhs
            | OutputChar ->
                output_char outchan (char_of_int ((-rhs) land 255));
                flush outchan
            | OutputNum ->
                output_string outchan (string_of_int (-rhs));
                flush outchan
            | RandomRange -> randrange := !randrange - rhs
            | _ -> failwith "unexpected"
        in

        let rec exec _ =
            match code.(!codeptr) with
            | Subtract (lhs,rhs) ->
                eval_lhs (eval_rhs rhs) lhs;
                incr codeptr;
                if !codeptr < 0 || !codeptr >= Array.length code then
                    ()
                else
                    exec ()
            | Exit _ -> ()
        in Random.self_init (); exec ()
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let string_of_value = function
            | Var v ->
                if v < 0 || v >= 26 then
                    failwith "variable beyond a..z is not supported."
                else
                    String.make 1 (char_of_int (int_of_char 'a' + v))
            | MemRef ref ->
                if ref < 0 || ref >= 26 then
                    failwith "memory reference beyond A..Z is not supported."
                else
                    String.make 1 (char_of_int (int_of_char 'A' + ref))
            | Number x -> string_of_int x
            | CodePtr -> "c"
            | MemPtr -> "p"
            | InputChar -> "i"
            | InputNum -> "j"
            | OutputChar -> "o"
            | OutputNum -> "q"
            | RandomRange | Random -> "r"
            | Breakpoint -> "_"
        in

        let emit = function
            | Subtract (lhs,rhs) ->
                Printf.bprintf buf "%s -= %s;\n"
                    (string_of_value lhs) (string_of_value rhs)
            | Exit (lhs,rhs) ->
                Printf.bprintf buf "%s -= %s;\n"
                    (string_of_value lhs) (string_of_value rhs)
        in
        
        List.iter emit nodes
end

(**************************************************************************)
(* The Spoon-to-Minus (and in turn, Brainfuck-to-Minus) transformer. *)

let from_spoon = object
    inherit [LangSpoon.t, t] EsotopeCommon.processor LangSpoon.kind kind

    (* brief strategy: Minus allows a random accessible memory (by modifying
     * p). so we construct the memory with only one cell (p=0) being -k (k>0)
     * and others being 0; then c-=A will skip next k instructions. this
     * approach slightly differs from the original Turing-completeness proof,
     * but allows a negative cell to be compared. (since Minus explicitly
     * allows for the negative memory pointer, it is just fine.)
     *
     * in practice, we have to interleave this temporary memory with the
     * Brainfuck tape. thus we use odd-numbered cells for the tape, and
     * even-numbered cells for the temporary. *)

    method process nodes =
        let vA = MemRef 0 in (* current pointer *)
        let va = Var 0 in (* general temporary store *)
        let vb = Var 1 in (* for temporarily saving p *)
        let vy = Var 24 in (* constant -2 *)
        let vz = Var 25 in (* constant -1 *)

        let (^^) (n1,f1) (n2,f2) =
            (n1 + n2, fun code -> f1 (f2 code))
        in

        let adjust var x =
            if x = 1 then
                (1, fun code -> Subtract (var,vz) :: code)
            else if x = 2 then
                (1, fun code -> Subtract (var,vy) :: code)
            else if x > 0 then
                (3, fun code -> Subtract (va,va) ::
                                Subtract (va,Number x) ::
                                Subtract (var,va) :: code)
            else if x < 0 then
                (1, fun code -> Subtract (var,Number (-x)) :: code)
            else
                (0, fun code -> code)
        in

        let rec process' = function
            | LangSpoon.AdjustMemory (target, delta) ->
                (adjust MemPtr (2*target)) ^^
                (adjust vA delta) ^^
                (adjust MemPtr (-2*target))
            | LangSpoon.SetMemory (target, value) ->
                (adjust MemPtr (2*target)) ^^
                (1, fun code -> Subtract (vA,vA) :: code) ^^
                (adjust vA value) ^^
                (adjust MemPtr (-2*target))
            | LangSpoon.MovePointer offset ->
                (adjust MemPtr (2*offset))
            | LangSpoon.Input target ->
                (adjust MemPtr (2*target)) ^^
                (4, fun code -> Subtract (va,va) ::
                                Subtract (va,InputChar) ::
                                Subtract (vA,vA) ::
                                Subtract (vA,va) :: code) ^^
                (adjust MemPtr (-2*target))
            | LangSpoon.Output target ->
                (adjust MemPtr (2*target)) ^^
                (3, fun code -> Subtract (va,va) ::
                                Subtract (va,vA) ::
                                Subtract (OutputChar,va) :: code) ^^
                (adjust MemPtr (-2*target))
            | LangSpoon.While (target, nodes) ->
                let nodes' = List.map process' nodes in
                let n, f = List.fold_left (^^) (0, fun code -> code) nodes' in
                (12, fun code -> Subtract (va,va) ::
                                 Subtract (va,vA) ::
                                 Subtract (vb,vb) ::
                                 Subtract (vb,MemPtr) ::
                                 Subtract (MemPtr,MemPtr) ::
                                 Subtract (vA,vA) ::
                                 Subtract (vA,Number (n+3)) ::
                                 Subtract (MemPtr,va) ::
                                 Subtract (MemPtr,va) ::
                                 Subtract (CodePtr,vA) ::
                                 Subtract (MemPtr,MemPtr) ::
                                 Subtract (MemPtr,vb) :: code) ^^
                (n, f) ^^
                (3, fun code -> Subtract (CodePtr,Number (n+13)) ::
                                Subtract (MemPtr,MemPtr) ::
                                Subtract (MemPtr,vb) :: code)
            | LangSpoon.Exit ->
                (1, fun code -> Exit (Number 0, Number 0) :: code)
            | _ ->
                (0, fun code -> code)
        in

        let nodes' = List.map process' nodes in
        let n, f = List.fold_left (^^) (0, fun code -> code) nodes' in
        Subtract (MemPtr,vz) :: f []
end

