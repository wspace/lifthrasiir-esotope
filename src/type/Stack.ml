(* This is a part of Esotope. See README for more information. *)

type 'a stack = 'a list
type 'a t = 'a stack

type ('a, 'b) stack_ret =
    | Success of 'a stack * 'b
    | Failure of ('a -> 'a stack * 'b)

exception Stack_underflow

let empty_stack = []
let create_stack () = ref empty_stack

(**************************************************************************)
(* Operations. *)

let pop stack =
    match stack with
    | a::t -> Success (t, a)
    | []   -> Failure (fun v -> ([], v))

let pop2 = function
    | a::b::t -> Success (t, (b,a))
    | a::[]   -> Failure (fun v -> ([], (v,a)))
    | []      -> Failure (fun v -> ([], (v,v)))

let pop3 = function
    | a::b::c::t -> Success (t, (c,b,a))
    | a::b::[]   -> Failure (fun v -> ([], (v,b,a)))
    | a::[]      -> Failure (fun v -> ([], (v,v,a)))
    | []         -> Failure (fun v -> ([], (v,v,v)))

let pop4 = function
    | a::b::c::d::t -> Success (t, (d,c,b,a))
    | a::b::c::[]   -> Failure (fun v -> ([], (v,c,b,a)))
    | a::b::[]      -> Failure (fun v -> ([], (v,v,b,a)))
    | a::[]         -> Failure (fun v -> ([], (v,v,v,a)))
    | []            -> Failure (fun v -> ([], (v,v,v,v)))

let pop5 = function
    | a::b::c::d::e::t -> Success (t, (e,d,c,b,a))
    | a::b::c::d::[]   -> Failure (fun v -> ([], (v,d,c,b,a)))
    | a::b::c::[]      -> Failure (fun v -> ([], (v,v,c,b,a)))
    | a::b::[]         -> Failure (fun v -> ([], (v,v,v,b,a)))
    | a::[]            -> Failure (fun v -> ([], (v,v,v,v,a)))
    | []               -> Failure (fun v -> ([], (v,v,v,v,v)))

let popn k stack =
    let rec recur k acc stack =
        if k = 0 then
            Success (stack, acc)
        else
            match stack with
            | h::t -> recur (k-1) (h::acc) t
            | [] -> Failure (recover k acc stack)
    and recover k acc stack v =
        if k = 0 then (stack, acc) else recover (k-1) (v::acc) stack v
    in recur k [] stack

let popupton k stack =
    let rec recur k acc stack =
        if k = 0 then
            Success (stack, acc)
        else
            match stack with
            | h::t -> recur (k-1) (h::acc) t
            | [] -> Success (stack, acc)
    in recur k [] stack

let push  a         stack = Success (            a::stack, ())
let push2 a b       stack = Success (         b::a::stack, ())
let push3 a b c     stack = Success (      c::b::a::stack, ())
let push4 a b c d   stack = Success (   d::c::b::a::stack, ())
let push5 a b c d e stack = Success (e::d::c::b::a::stack, ())

let rec pushn l stack =
    match l with
    | h::t -> pushn t (h::stack)
    | [] -> Success (stack, ())

let pop_push f = function
    | a::t -> Success ((f a)::t, ())
    | []   -> Failure (fun v -> ([f v], ()))

let pop2_push f = function
    | a::b::t -> Success ((f b a)::t, ())
    | a::[]   -> Failure (fun v -> ([f v a], ()))
    | []      -> Failure (fun v -> ([f v v], ()))

let pop3_push f = function
    | a::b::c::t -> Success ((f c b a)::t, ())
    | a::b::[]   -> Failure (fun v -> ([f v b a], ()))
    | a::[]      -> Failure (fun v -> ([f v v a], ()))
    | []         -> Failure (fun v -> ([f v v v], ()))

let peek = function
    | a::t -> Success (a::t, a)
    | []   -> Failure (fun v -> ([], v))

let get k stack =
    if k < 0 then invalid_arg "get";
    let rec loop k = function
        | h::t -> if k = 0 then Success (stack, h) else loop (k-1) t
        | []   -> Failure (fun v -> (stack, v))
    in loop k stack

let swap = function
    | a::b::t -> Success (b::a::t, ())
    | a::[]   -> Failure (fun v -> ([v;a], ()))
    | []      -> Failure (fun v -> ([v;v], ()))

let dup = function
    | a::t -> Success (a::a::t, ())
    | []   -> Failure (fun v -> ([v], ()))

let dup2 = function
    | a::t -> Success (a::a::t, ())
    | []   -> Failure (fun v -> ([v;v], ()))

let pick y x stack = failwith "TODO"
let pick2 y x stack = failwith "TODO"

let roll y x stack = failwith "TODO"

let discard y x stack = failwith "TODO"
let discard2 y x stack = failwith "TODO"

(**************************************************************************)
(* Operation wrappers. *)

let stack_op op stack =
    match op !stack with
    | Success (stack', ret) -> stack := stack'; Some ret
    | Failure cont -> None

(* variant of `stack_op` which translates `unit option` to `bool` *)
let stack_op_bool op stack =
    match stack_op op stack with
    | Some () -> true
    | None -> false

(* variant of `stack_op` which ignores `None` result *)
let stack_op_ignore op stack =
    match stack_op op stack with
    | Some ret -> ret
    | None -> failwith "unexpected"

module Ops = struct
    type 'a stack = 'a t
    let create = create_stack
    let pop stack = stack_op pop stack
    let pop2 stack = stack_op pop2 stack
    let pop3 stack = stack_op pop3 stack
    let pop4 stack = stack_op pop4 stack
    let pop5 stack = stack_op pop5 stack
    let popn k stack = stack_op (popn k) stack
    let popupton k stack = stack_op_ignore (popupton k) stack
    let push a stack = stack_op_ignore (push a) stack
    let push2 a b stack = stack_op_ignore (push2 a b) stack
    let push3 a b c stack = stack_op_ignore (push3 a b c) stack
    let push4 a b c d stack = stack_op_ignore (push4 a b c d) stack
    let push5 a b c d e stack = stack_op_ignore (push5 a b c d e) stack
    let pushn l stack = stack_op_ignore (pushn l) stack
    let pop_push f stack = stack_op_bool (pop_push f) stack
    let pop2_push f stack = stack_op_bool (pop2_push f) stack
    let pop3_push f stack = stack_op_bool (pop3_push f) stack
    let peek stack = stack_op peek stack
    let get k stack = stack_op (get k) stack
    let swap stack = stack_op_bool swap stack
    let dup stack = stack_op_bool dup stack
    let dup2 stack = stack_op_bool dup2 stack
    let pick y x stack = stack_op_bool (pick y x) stack
    let pick2 y x stack = stack_op_bool (pick2 y x) stack
    let roll y x stack = stack_op_bool (roll y x) stack
    let discard y x stack = stack_op_bool (discard y x) stack
    let discard2 y x stack = stack_op_bool (discard2 y x) stack
end

let forcing_stack_op op stack =
    match op !stack with
    | Success (stack', ret) -> stack := stack'; ret
    | Failure cont -> raise Stack_underflow

module ForcingOps = struct
    type 'a stack = 'a t
    exception Stack_underflow = Stack_underflow
    let create = create_stack
    let pop stack = forcing_stack_op pop stack
    let pop2 stack = forcing_stack_op pop2 stack
    let pop3 stack = forcing_stack_op pop3 stack
    let pop4 stack = forcing_stack_op pop4 stack
    let pop5 stack = forcing_stack_op pop5 stack
    let popn k stack = forcing_stack_op (popn k) stack
    let popupton k stack = forcing_stack_op (popupton k) stack
    let push a stack = forcing_stack_op (push a) stack
    let push2 a b stack = forcing_stack_op (push2 a b) stack
    let push3 a b c stack = forcing_stack_op (push3 a b c) stack
    let push4 a b c d stack = forcing_stack_op (push4 a b c d) stack
    let push5 a b c d e stack = forcing_stack_op (push5 a b c d e) stack
    let pushn l stack = forcing_stack_op (pushn l) stack
    let pop_push f stack = forcing_stack_op (pop_push f) stack
    let pop2_push f stack = forcing_stack_op (pop2_push f) stack
    let pop3_push f stack = forcing_stack_op (pop3_push f) stack
    let peek stack = forcing_stack_op peek stack
    let get k stack = forcing_stack_op (get k) stack
    let swap stack = forcing_stack_op swap stack
    let dup stack = forcing_stack_op dup stack
    let dup2 stack = forcing_stack_op dup2 stack
    let pick y x stack = forcing_stack_op (pick y x) stack
    let pick2 y x stack = forcing_stack_op (pick2 y x) stack
    let roll y x stack = forcing_stack_op (roll y x) stack
    let discard y x stack = forcing_stack_op (discard y x) stack
    let discard2 y x stack = forcing_stack_op (discard2 y x) stack
end

let assuming_stack_op v op stack =
    let (stack', ret) =
        match op !stack with
        | Success (stack',ret) -> (stack', ret)
        | Failure cont -> cont v
    in stack := stack'; ret

module AssumingOps = struct
    type 'a stack = 'a t
    let create = create_stack
    let pop v stack = assuming_stack_op v pop stack
    let pop2 v stack = assuming_stack_op v pop2 stack
    let pop3 v stack = assuming_stack_op v pop3 stack
    let pop4 v stack = assuming_stack_op v pop4 stack
    let pop5 v stack = assuming_stack_op v pop5 stack
    let popn v k stack = assuming_stack_op v (popn k) stack
    (* no default values are needed; `forcing_stack_op` is enough. *)
    let popupton k stack = forcing_stack_op (popupton k) stack
    let push a stack = forcing_stack_op (push a) stack
    let push2 a b stack = forcing_stack_op (push2 a b) stack
    let push3 a b c stack = forcing_stack_op (push3 a b c) stack
    let push4 a b c d stack = forcing_stack_op (push4 a b c d) stack
    let push5 a b c d e stack = forcing_stack_op (push5 a b c d e) stack
    let pushn l stack = forcing_stack_op (pushn l) stack
    let pop_push v f stack = assuming_stack_op v (pop_push f) stack
    let pop2_push v f stack = assuming_stack_op v (pop2_push f) stack
    let pop3_push v f stack = assuming_stack_op v (pop3_push f) stack
    let peek v stack = assuming_stack_op v peek stack
    let get v k stack = assuming_stack_op v (get k) stack
    let swap v stack = assuming_stack_op v swap stack
    let dup v stack = assuming_stack_op v dup stack
    let dup2 v stack = assuming_stack_op v dup2 stack
    let pick v y x stack = assuming_stack_op v (pick y x) stack
    let pick2 v y x stack = assuming_stack_op v (pick2 y x) stack
    let roll v y x stack = assuming_stack_op v (roll y x) stack
    let discard v y x stack = assuming_stack_op v (discard y x) stack
    let discard2 v y x stack = assuming_stack_op v (discard2 y x) stack
end

