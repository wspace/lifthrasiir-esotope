(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangAheui

 This module implements the Aheui programming language, designed by Puzzlet
 Chung in 2005. It is one of the earliest esoteric programming languages
 explicitly designed for the CJK environment. The name "Aheui" refers to
 an archaic Korean word for "baby" (the modern form would be "Ai"), and also
 a short program in Aheui which does nothing.

 Aheui is a two-dimensional programming language similar to Befunge-98,
 except for the self-modification. Each command is represented as a Hangeul
 syllable and contains an actual operation (initial consonant), an optional
 argument (final consonant) and the next code flow (vowel). It has 26 stacks
 and one queue which can be switched using "ㅅ" Select operation.
***************************************************************************)

open Space
module S = Stack.ForcingOps
module Q = Queue.ForcingOps

type op =
    | Divide                    (* ㄴ *)
    | Add                       (* ㄷ *)
    | Multiply                  (* ㄸ *)
    | Modulo                    (* ㄹ *)
    | Pop                       (* ㅁ (also used for output) *)
    | Push                      (* ㅂ (also used for input) *)
    | Dup                       (* ㅃ *)
    | Select                    (* ㅅ *)
    | Move                      (* ㅆ *)
    | Nop                       (* ㅇ *)
    | GreaterThan               (* ㅈ *)
    | ReflectIfZero             (* ㅊ *)
    | Subtract                  (* ㅌ *)
    | Swap                      (* ㅍ *)
    | Exit                      (* ㅎ *)
    | ReservedOp of int         (* reserved (ㄱ, ㄲ, ㅉ, ㅊ) and does nothing *)

type dir =
    | Right                     (* ㅏ *)
    | Right2                    (* ㅑ *)
    | Left                      (* ㅓ *)
    | Left2                     (* ㅕ *)
    | Up                        (* ㅗ *)
    | Up2                       (* ㅛ *)
    | Down                      (* ㅜ *)
    | Down2                     (* ㅠ *)
    | ReflectV                  (* ㅡ *)
    | Reflect                   (* ㅢ *)
    | ReflectH                  (* ㅣ *)
    | NoDir of int              (* does not affect the direction *)

type command = (op * dir * int) option

let op_of_initial = function
    |  2 (*ㄴ*) -> Divide
    |  3 (*ㄷ*) -> Add
    |  4 (*ㄸ*) -> Multiply
    |  5 (*ㄹ*) -> Modulo
    |  6 (*ㅁ*) -> Pop
    |  7 (*ㅂ*) -> Push
    |  8 (*ㅃ*) -> Dup
    |  9 (*ㅅ*) -> Select
    | 10 (*ㅆ*) -> Move
    | 11 (*ㅇ*) -> Nop
    | 12 (*ㅈ*) -> GreaterThan
    | 14 (*ㅊ*) -> ReflectIfZero
    | 16 (*ㅌ*) -> Subtract
    | 17 (*ㅍ*) -> Swap
    | 18 (*ㅎ*) -> Exit
    | i         -> ReservedOp i

let num_pops_for_op = function
    | Divide        -> 2
    | Add           -> 2
    | Multiply      -> 2
    | Modulo        -> 2
    | Pop           -> 1
    | Push          -> 0
    | Dup           -> 1
    | Select        -> 0
    | Move          -> 1
    | Nop           -> 0
    | GreaterThan   -> 2
    | ReflectIfZero -> 1
    | Subtract      -> 2
    | Swap          -> 2
    | Exit          -> 0
    | ReservedOp i  -> 0

let initial_of_op = function
    | Divide        ->  2 (*ㄴ*)
    | Add           ->  3 (*ㄷ*)
    | Multiply      ->  4 (*ㄸ*)
    | Modulo        ->  5 (*ㄹ*)
    | Pop           ->  6 (*ㅁ*)
    | Push          ->  7 (*ㅂ*)
    | Dup           ->  8 (*ㅃ*)
    | Select        ->  9 (*ㅅ*)
    | Move          -> 10 (*ㅆ*)
    | Nop           -> 11 (*ㅇ*)
    | GreaterThan   -> 12 (*ㅈ*)
    | ReflectIfZero -> 14 (*ㅊ*)
    | Subtract      -> 16 (*ㅌ*)
    | Swap          -> 17 (*ㅍ*)
    | Exit          -> 18 (*ㅎ*)
    | ReservedOp i  -> i

let dir_of_medial = function
    |  0 (*ㅏ*) -> Right
    |  2 (*ㅑ*) -> Right2
    |  4 (*ㅓ*) -> Left
    |  6 (*ㅕ*) -> Left2
    |  8 (*ㅗ*) -> Up
    | 12 (*ㅛ*) -> Up2
    | 13 (*ㅜ*) -> Down
    | 17 (*ㅠ*) -> Down2
    | 18 (*ㅡ*) -> ReflectV
    | 19 (*ㅢ*) -> Reflect
    | 20 (*ㅣ*) -> ReflectH
    | m         -> NoDir m

let delta_of_dir (dx,dy) = function
    | Right    -> (1,0)
    | Right2   -> (2,0)
    | Left     -> (-1,0)
    | Left2    -> (-2,0)
    | Up       -> (0,-1)
    | Up2      -> (0,-2)
    | Down     -> (0,1)
    | Down2    -> (0,2)
    | ReflectV -> (dx,-dy)
    | Reflect  -> (-dx,-dy)
    | ReflectH -> (-dx,dy)
    | NoDir m  -> (dx,dy)

let medial_of_dir = function
    | Right    ->  0 (*ㅏ*)
    | Right2   ->  2 (*ㅑ*)
    | Left     ->  4 (*ㅓ*)
    | Left2    ->  6 (*ㅕ*)
    | Up       ->  8 (*ㅗ*)
    | Up2      -> 12 (*ㅛ*)
    | Down     -> 13 (*ㅜ*)
    | Down2    -> 17 (*ㅠ*)
    | ReflectV -> 18 (*ㅡ*)
    | Reflect  -> 19 (*ㅢ*)
    | ReflectH -> 20 (*ㅣ*)
    | NoDir m  -> m

let unicode_of_command = function
    | None -> 0x3000 (* ideographic space *)
    | Some (op,dir,arg) ->
        0xac00 + (initial_of_op op * 21 + medial_of_dir dir) * 28 + arg

let num_strokes_of_final = function
    |  0        -> 0 |  1 (*ㄱ*) -> 2 |  2 (*ㄲ*) -> 4 |  3 (*ㄳ*) -> 4
    |  4 (*ㄴ*) -> 2 |  5 (*ㄵ*) -> 5 |  6 (*ㄶ*) -> 5 |  7 (*ㄷ*) -> 3
    |  8 (*ㄹ*) -> 5 |  9 (*ㄺ*) -> 7 | 10 (*ㄻ*) -> 9 | 11 (*ㄼ*) -> 9
    | 12 (*ㄽ*) -> 7 | 13 (*ㄾ*) -> 9 | 14 (*ㄿ*) -> 9 | 15 (*ㅀ*) -> 8
    | 16 (*ㅁ*) -> 4 | 17 (*ㅂ*) -> 4 | 18 (*ㅄ*) -> 6 | 19 (*ㅅ*) -> 2
    | 20 (*ㅆ*) -> 4                  | 22 (*ㅈ*) -> 3 | 23 (*ㅊ*) -> 4
    | 24 (*ㅋ*) -> 3 | 25 (*ㅌ*) -> 4 | 26 (*ㅍ*) -> 4
    | _ -> failwith "unknown"

(**************************************************************************)
(* The kind. *)

type t = command Space2D.t
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "aheui"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.unicode_reader kind

    method process stream =
        let code = Space2D.create (8,8) None in

        let readcell stream =
            match StreamUtil.try_next stream with
            | Some ch ->
                if ch >= 0xac00 && ch < 0xd7a4 then
                    let ch = ch - 0xac00 in
                    let i, m, f = (ch / 588, (ch / 28) mod 21, ch mod 28) in
                    Some (op_of_initial i, dir_of_medial m, f)
                else
                    None
            | None -> raise End_of_file
        in Space2D.from_unicode_stream readcell stream code (0,0)
end

(**************************************************************************)
(* The interpreter. *)

type storage = {push: int -> unit;
                pop: unit -> int;
                pop2: unit -> int * int;
                dup: unit -> unit;
                swap: unit -> unit}

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process code io =
        let (_,(sx,sy)) = Space2D.bounds code in

        let create_stack () =
            let stack = ref [] in
            {push = (fun a -> S.push a stack);
             pop = (fun () -> S.pop stack);
             pop2 = (fun () -> S.pop2 stack);
             dup = (fun () -> S.dup stack);
             swap = (fun () -> S.swap stack)}
        in

        let create_queue () =
            let queue = ref ([], []) in
            {push = (fun a -> Q.enqueue a queue);
             pop = (fun () -> Q.dequeue queue);
             pop2 = (fun () -> Q.dequeue2 queue);
             dup = (fun () -> Q.front_dup queue);
             swap = (fun () -> Q.front_swap queue)}
        in

        let create_hole () =
            {push = (fun a -> failwith "extension not implemented");
             pop = (fun () -> failwith "extension not implemented");
             pop2 = (fun () -> failwith "extension not implemented");
             dup = (fun () -> failwith "extension not implemented");
             swap = (fun () -> failwith "extension not implemented")}
        in

        let create_storage i =
            match i with
            | 21 -> create_queue ()
            | 27 -> create_hole ()
            | _ -> create_stack ()
        in

        let storages = Array.init 28 create_storage in
        let storage = ref storages.(0) in

        let exec_op op arg =
            let binary f =
                let (a, b) = !storage.pop2 () in
                !storage.push (f a b); false
            in try begin
                match op, arg with
                | Nop, _ -> false
                | Divide, _ -> binary (fun a b -> a / b)
                | Add, _ -> binary (fun a b -> a + b)
                | Multiply, _ -> binary (fun a b -> a * b)
                | Modulo, _ -> binary (fun a b -> a mod b)
                | Pop, 21 ->
                    let v = !storage.pop () in
                    io#put_int v; io#flush_out (); false
                | Pop, 27 ->
                    let v = !storage.pop () in
                    io#put_code v; io#flush_out (); false
                | Pop, _ ->
                    ignore (!storage.pop ()); false
                | Push, 21 ->
                    begin match io#get_int None with
                    | Some v -> !storage.push v; false
                    | None -> true
                    end
                | Push, 27 ->
                    begin match io#get_code None with
                    | Some v -> !storage.push v; false
                    | None -> true
                    end
                | Push, arg ->
                    !storage.push (num_strokes_of_final arg); false
                | Dup, _ -> !storage.dup (); false
                | Select, arg -> storage := storages.(arg); false
                | Move, arg ->
                    let v = !storage.pop () in storages.(arg).push v; false
                | GreaterThan, _ ->
                    binary (fun a b -> if a >= b then 1 else 0)
                | ReflectIfZero, _ -> (!storage.pop () = 0)
                | Subtract, _ -> binary (fun a b -> a - b)
                | Swap, _ -> !storage.swap (); false
                | Exit, _ -> failwith "impossible"
                | ReservedOp i, _ -> false
            end with S.Stack_underflow | Q.Queue_underflow -> true
        in

        let rec exec (x,y) delta =
            match Space2D.get code (x,y) with
            | None -> continue (x,y) delta
            | Some (Exit,_,_) -> ()
            | Some (op,dir,arg) ->
                let (dx,dy) = delta_of_dir delta dir in
                if exec_op op arg then (* true on reflect *)
                    continue (x,y) (-dx,-dy)
                else
                    continue (x,y) (dx,dy)
        and continue (x,y) (dx,dy) =
            (* TODO this wrapping algorithm does conform to the specficiation
             * and yet does not match to the JS interpreter. we need per-row
             * bounds and exact number of lines to handle this correctly. *)
            exec ((x+dx+sx) mod sx, (y+dy+sy) mod sy) (dx,dy)
        in exec (0,0) (0,1)    (* the initial direction is downward *)
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process code buf =
        (* the starting offset is guaranteed to be in the 1st quadrant *)
        let (_,(sx,sy)) = Space2D.bounds code in

        let rec build_line y x acc =
            let x = x-1 in
            let cmd = Space2D.get code (x,y) in
            let acc' =
                if cmd = None && acc = "" then
                    acc
                else
                    UnicodeUtil.to_utf8 (unicode_of_command cmd) ^ acc
            in if x = 0 then acc' else build_line y x acc'
        in

        let rec build y acc =
            let y = y-1 in
            let line = build_line y sx "" in
            let acc' =
                if line = "" && acc = "" then
                    acc
                else
                    line ^ "\n" ^ acc
            in if y = 0 then acc' else build y acc'
        in

        Buffer.add_string buf (build sy "")
end

