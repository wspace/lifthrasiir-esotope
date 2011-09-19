(* This is a part of Esotope. See README for more information. *)

type 'a queue = 'a Stack.stack * 'a Stack.stack
type 'a t = 'a queue

type ('a, 'b) queue_ret =
    | Success of 'a queue * 'b
    | Failure of ('a -> 'a queue * 'b)

exception Queue_underflow

let empty_queue = ([], [])
let create_queue () = ref empty_queue

let queue_front (qf,qb) = qf
let queue_back (qf,qb) = qb

(**************************************************************************)
(* Operations. *)

let dequeue = function
    | (a::front', back) -> Success ((front',back), a)
    | (front, back) ->
        match front, List.rev back with
        | [], a::back' -> Success ((back',[]), a)
        | [], [] -> Failure (fun v -> (([],[]), v))
        | _, _ -> failwith "impossible"

let dequeue2 = function
    | (a::b::front', back) -> Success ((front',back), (b,a))
    | (front, back) ->
        match front, List.rev back with
        | a::[], b::back'
        | [], a::b::back' -> Success ((back',[]), (b,a))
        | a::[], []
        | [], a::[] -> Failure (fun v -> (([],[]), (v,a)))
        | [], [] -> Failure (fun v -> (([],[]), (v,v)))
        | _, _ -> failwith "impossible"

let dequeue3 = function
    | (a::b::c::front', back) -> Success ((front',back), (c,b,a))
    | (front, back) ->
        match front, List.rev back with
        | a::b::[], c::back'
        | a::[], b::c::back'
        | [], a::b::c::back' -> Success ((back',[]), (c,b,a))
        | a::b::[], []
        | a::[], b::[]
        | [], a::b::[] -> Failure (fun v -> (([],[]), (v,b,a)))
        | a::[], []
        | [], a::[] -> Failure (fun v -> (([],[]), (v,v,a)))
        | [], [] -> Failure (fun v -> (([],[]), (v,v,v)))
        | _, _ -> failwith "impossible"

let dequeue4 = function
    | (a::b::c::d::front', back) -> Success ((front',back), (d,c,b,a))
    | (front, back) ->
        match front, List.rev back with
        | a::b::c::[], d::back'
        | a::b::[], c::d::back'
        | a::[], b::c::d::back'
        | [], a::b::c::d::back' -> Success ((back',[]), (d,c,b,a))
        | a::b::c::[], []
        | a::b::[], c::[]
        | a::[], b::c::[]
        | [], a::b::c::[] -> Failure (fun v -> (([],[]), (v,c,b,a)))
        | a::b::[], []
        | a::[], b::[]
        | [], a::b::[] -> Failure (fun v -> (([],[]), (v,v,b,a)))
        | a::[], []
        | [], a::[] -> Failure (fun v -> (([],[]), (v,v,v,a)))
        | [], [] -> Failure (fun v -> (([],[]), (v,v,v,v)))
        | _, _ -> failwith "impossible"

let dequeue5 = function
    | (a::b::c::d::e::front', back) -> Success ((front',back), (e,d,c,b,a))
    | (front, back) ->
        match front, List.rev back with
        | a::b::c::d::[], e::back'
        | a::b::c::[], d::e::back'
        | a::b::[], c::d::e::back'
        | a::[], b::c::d::e::back'
        | [], a::b::c::d::e::back' -> Success ((back',[]), (e,d,c,b,a))
        | a::b::c::[], d::[]
        | a::b::[], c::d::[]
        | a::[], b::c::d::[]
        | [], a::b::c::d::[] -> Failure (fun v -> ([],[]), (v,d,c,b,a))
        | a::b::c::[], []
        | a::b::[], c::[]
        | a::[], b::c::[]
        | [], a::b::c::[] -> Failure (fun v -> (([],[]), (v,v,c,b,a)))
        | a::b::[], []
        | a::[], b::[]
        | [], a::b::[] -> Failure (fun v -> (([],[]), (v,v,v,b,a)))
        | a::[], []
        | [], a::[] -> Failure (fun v -> (([],[]), (v,v,v,v,a)))
        | [], [] -> Failure (fun v -> (([],[]), (v,v,v,v,v)))
        | _, _ -> failwith "impossible"

let enqueue  a         (front,back) = Success ((front,            a::back), ())
let enqueue2 a b       (front,back) = Success ((front,         b::a::back), ())
let enqueue3 a b c     (front,back) = Success ((front,      c::b::a::back), ())
let enqueue4 a b c d   (front,back) = Success ((front,   d::c::b::a::back), ())
let enqueue5 a b c d e (front,back) = Success ((front,e::d::c::b::a::back), ())

let front = function
    | (a::front', back) -> Success ((a::front',back), a)
    | ([], back) ->
        match List.rev back with
        | a::back' -> Success ((a::back',[]), a)
        | [] -> Failure (fun v -> (([],[]), v))

let front_get k (front,back) =
    if k < 0 then invalid_arg "front_get";
    let rec loop_front k = function
        | h::t -> if k = 0 then Success ((front,back), h) else loop_front (k-1) t
        | [] -> loop_back k (List.rev back)
    and loop_back k = function
        | h::t -> if k = 0 then Success ((front,back), h) else loop_back (k-1) t
        | [] -> Failure (fun v -> ((front,back), v))
    in loop_front k front

let front_swap = function
    | (a::b::front', back) -> Success ((b::a::front',back), ())
    | (front, back) ->
        match front, List.rev back with
        | a::[], b::back'
        | [], a::b::back' -> Success ((b::a::back',[]), ())
        | a::[], []
        | [], a::[] -> Failure (fun v -> (([v;a],[]), ()))
        | [], [] -> Failure (fun v -> (([v;v],[]), ()))
        | _, _ -> failwith "impossible"

let front_dup = function
    | (a::front', back) -> Success ((a::a::front',back), ())
    | (front, back) ->
        match front, List.rev back with
        | a::[], back'
        | [], a::back' -> Success ((a::a::back',[]), ())
        | [], [] -> Failure (fun v -> (([v;v],[]), ()))
        | _, _ -> failwith "impossible"

(**************************************************************************)
(* Operation wrappers. *)

let queue_op op queue =
    match op !queue with
    | Success (queue', ret) -> queue := queue'; Some ret
    | Failure cont -> None

(* variant of `queue_op` which translates `unit option` to `bool` *)
let queue_op_bool op queue =
    match queue_op op queue with
    | Some () -> true
    | None -> false

(* variant of `queue_op` which ignores `None` result *)
let queue_op_ignore op queue =
    ignore (queue_op op queue)

module Ops = struct
    type 'a queue = 'a t
    let create = create_queue
    let dequeue queue = queue_op dequeue queue
    let dequeue2 queue = queue_op dequeue2 queue
    let dequeue3 queue = queue_op dequeue3 queue
    let dequeue4 queue = queue_op dequeue4 queue
    let dequeue5 queue = queue_op dequeue5 queue
    let enqueue a queue = queue_op_ignore (enqueue a) queue
    let enqueue2 a b queue = queue_op_ignore (enqueue2 a b) queue
    let enqueue3 a b c queue = queue_op_ignore (enqueue3 a b c) queue
    let enqueue4 a b c d queue = queue_op_ignore (enqueue4 a b c d) queue
    let enqueue5 a b c d e queue = queue_op_ignore (enqueue5 a b c d e) queue
    let front queue = queue_op front queue
    let front_get k queue = queue_op (front_get k) queue
    let front_swap queue = queue_op_bool front_swap queue
    let front_dup queue = queue_op_bool front_dup queue
end

let forcing_queue_op op queue =
    match op !queue with
    | Success (queue', ret) -> queue := queue'; ret
    | Failure cont -> raise Queue_underflow

module ForcingOps = struct
    type 'a queue = 'a t
    exception Queue_underflow = Queue_underflow
    let create = create_queue
    let dequeue queue = forcing_queue_op dequeue queue
    let dequeue2 queue = forcing_queue_op dequeue2 queue
    let dequeue3 queue = forcing_queue_op dequeue3 queue
    let dequeue4 queue = forcing_queue_op dequeue4 queue
    let dequeue5 queue = forcing_queue_op dequeue5 queue
    let enqueue a queue = forcing_queue_op (enqueue a) queue
    let enqueue2 a b queue = forcing_queue_op (enqueue2 a b) queue
    let enqueue3 a b c queue = forcing_queue_op (enqueue3 a b c) queue
    let enqueue4 a b c d queue = forcing_queue_op (enqueue4 a b c d) queue
    let enqueue5 a b c d e queue = forcing_queue_op (enqueue5 a b c d e) queue
    let front queue = forcing_queue_op front queue
    let front_get k queue = forcing_queue_op (front_get k) queue
    let front_swap queue = forcing_queue_op front_swap queue
    let front_dup queue = forcing_queue_op front_dup queue
end

let assuming_queue_op v op queue =
    let (queue', ret) =
        match op !queue with
        | Success (queue',ret) -> (queue', ret)
        | Failure cont -> cont v
    in queue := queue'; ret

module AssumingOps = struct
    type 'a queue = 'a t
    let create = create_queue
    let dequeue v queue = assuming_queue_op v dequeue queue
    let dequeue2 v queue = assuming_queue_op v dequeue2 queue
    let dequeue3 v queue = assuming_queue_op v dequeue3 queue
    let dequeue4 v queue = assuming_queue_op v dequeue4 queue
    let dequeue5 v queue = assuming_queue_op v dequeue5 queue
    (* no default values are needed; `forcing_queue_op` is enough. *)
    let enqueue a queue = forcing_queue_op (enqueue a) queue
    let enqueue2 a b queue = forcing_queue_op (enqueue2 a b) queue
    let enqueue3 a b c queue = forcing_queue_op (enqueue3 a b c) queue
    let enqueue4 a b c d queue = forcing_queue_op (enqueue4 a b c d) queue
    let enqueue5 a b c d e queue = forcing_queue_op (enqueue5 a b c d e) queue
    let front v queue = assuming_queue_op v front queue
    let front_get v k queue = assuming_queue_op v (front_get k) queue
    let front_swap v queue = assuming_queue_op v front_swap queue
    let front_dup v queue = assuming_queue_op v front_dup queue
end

