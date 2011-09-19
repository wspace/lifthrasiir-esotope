(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 Queue

 This module implements utility functions for manipulating queues. Queues
 closely follow the convention used by stacks; see Stack module for details.
***************************************************************************)

(* The queue is a pair of two stacks; the "front" stack is exclusively used
 * for dequeue operations, the "back" stack is exclusively used for enqueue
 * operations. Whenever the front stack is empty the contents of back stack
 * is pushed into the front stack (i.e. in the reverse order). *)
type 'a queue = 'a Stack.stack * 'a Stack.stack
type 'a t = 'a queue

(* See `Stack.stack_ret` for explanation. *)
type ('a, 'b) queue_ret =
    | Success of 'a queue * 'b
    | Failure of ('a -> 'a queue * 'b)

(* This exception is used for the interface wrappers. *)
exception Queue_underflow

(* An empty queue. *)
val empty_queue : 'a queue

(* Creates an empty queue reference. *)
val create_queue : unit -> 'a queue ref

(* Extracts the "front" and "back" stack from the queue. *)
val queue_front : 'a queue -> 'a Stack.stack
val queue_back : 'a queue -> 'a Stack.stack

(**************************************************************************)
(* Operations. *)

(* Pops one element from the front and returns it. *)
val dequeue : 'a queue -> ('a, 'a) queue_ret

(* Pops multiple elements from the front and returns them. The return value
 * is ordered from the back of the queue to the front of the queue. (Thus if
 * there are enough elements, the return value of `dequeueK` is equivalent
 * to return values of K `dequeue`s concatenated in the *reverse* order.) *)
val dequeue2 : 'a queue -> ('a, 'a * 'a) queue_ret
val dequeue3 : 'a queue -> ('a, 'a * 'a * 'a) queue_ret
val dequeue4 : 'a queue -> ('a, 'a * 'a * 'a * 'a) queue_ret
val dequeue5 : 'a queue -> ('a, 'a * 'a * 'a * 'a * 'a) queue_ret

(* Pushes one element to the back. *)
val enqueue : 'a -> 'a queue -> ('a, unit) queue_ret

(* Pushes multiple elements to the back. The elements are pushed from
 * the left to the right, in that order. *)
val enqueue2 : 'a -> 'a -> 'a queue -> ('a, unit) queue_ret
val enqueue3 : 'a -> 'a -> 'a -> 'a queue -> ('a, unit) queue_ret
val enqueue4 : 'a -> 'a -> 'a -> 'a -> 'a queue -> ('a, unit) queue_ret
val enqueue5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a queue -> ('a, unit) queue_ret

(* Returns the element at the front (which will be popped next time).
 * Functionally equivalent to `front_get 0`. *)
val front : 'a queue -> ('a, 'a) queue_ret

(* Returns the i-th element from the front, where the element which will be
 * popped next time is zeroth. *)
val front_get : int -> 'a queue -> ('a, 'a) queue_ret

(* Swaps the first two elements at the front. This is NOT equivalent to
 * popping and pushing these two elements in the different order, since
 * those elements would be pushed to the back. *)
val front_swap : 'a queue -> ('a, unit) queue_ret

(* Duplicates the first element at the front. This is NOT equivalent to
 * pushing the frontmost element again, since that element would be pushed
 * to the back. *)
val front_dup : 'a queue -> ('a, unit) queue_ret

(**************************************************************************)
(* Operation wrappers. *)

(* Maps queue operations to the mutable version. Returns None on failure. *)
val queue_op : ('a queue -> ('a,'b) queue_ret) -> 'a queue ref -> 'b option

(* Version of mutable operations wrapped via `queue_op`. They return either
 * option types or boolean, which is isomorphic to `unit option`. *)
module Ops : sig
    type 'a queue = 'a t
    val create : unit -> 'a queue ref
    val dequeue : 'a queue ref -> 'a option
    val dequeue2 : 'a queue ref -> ('a * 'a) option
    val dequeue3 : 'a queue ref -> ('a * 'a * 'a) option
    val dequeue4 : 'a queue ref -> ('a * 'a * 'a * 'a) option
    val dequeue5 : 'a queue ref -> ('a * 'a * 'a * 'a * 'a) option
    val enqueue : 'a -> 'a queue ref -> unit
    val enqueue2 : 'a -> 'a -> 'a queue ref -> unit
    val enqueue3 : 'a -> 'a -> 'a -> 'a queue ref -> unit
    val enqueue4 : 'a -> 'a -> 'a -> 'a -> 'a queue ref -> unit
    val enqueue5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a queue ref -> unit
    val front : 'a queue ref -> 'a option
    val front_get : int -> 'a queue ref -> 'a option
    val front_swap : 'a queue ref -> bool
    val front_dup : 'a queue ref -> bool
end

(* Maps queue operations to the mutable version. Raises queue_underflow on
 * failure. *)
val forcing_queue_op : ('a queue -> ('a,'b) queue_ret) -> 'a queue ref -> 'b

(* Version of mutable operations wrapped via `forcing_queue_op`. They may
 * raise queue_underflow on failure; it does not affect the queue in that
 * case. *)
module ForcingOps : sig
    type 'a queue = 'a t
    exception Queue_underflow
    val create : unit -> 'a queue ref
    val dequeue : 'a queue ref -> 'a
    val dequeue2 : 'a queue ref -> 'a * 'a
    val dequeue3 : 'a queue ref -> 'a * 'a * 'a
    val dequeue4 : 'a queue ref -> 'a * 'a * 'a * 'a
    val dequeue5 : 'a queue ref -> 'a * 'a * 'a * 'a * 'a
    val enqueue : 'a -> 'a queue ref -> unit
    val enqueue2 : 'a -> 'a -> 'a queue ref -> unit
    val enqueue3 : 'a -> 'a -> 'a -> 'a queue ref -> unit
    val enqueue4 : 'a -> 'a -> 'a -> 'a -> 'a queue ref -> unit
    val enqueue5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a queue ref -> unit
    val front : 'a queue ref -> 'a
    val front_get : int -> 'a queue ref -> 'a
    val front_swap : 'a queue ref -> unit
    val front_dup : 'a queue ref -> unit
end

(* Maps queue operations to the mutable version. If the queue does not have
 * enough elements, assumes that the queue contains infinitely many default
 * values below it. *)
val assuming_queue_op : 'a -> ('a queue -> ('a,'b) queue_ret) ->
                        'a queue ref -> 'b

(* Version of mutable operations wrapped via `assuming_queue_op`. They
 * receive the default value as the first argument. *)
module AssumingOps : sig
    type 'a queue = 'a t
    val create : unit -> 'a queue ref
    val dequeue : 'a -> 'a queue ref -> 'a
    val dequeue2 : 'a -> 'a queue ref -> 'a * 'a
    val dequeue3 : 'a -> 'a queue ref -> 'a * 'a * 'a
    val dequeue4 : 'a -> 'a queue ref -> 'a * 'a * 'a * 'a
    val dequeue5 : 'a -> 'a queue ref -> 'a * 'a * 'a * 'a * 'a
    val enqueue : 'a -> 'a queue ref -> unit
    val enqueue2 : 'a -> 'a -> 'a queue ref -> unit
    val enqueue3 : 'a -> 'a -> 'a -> 'a queue ref -> unit
    val enqueue4 : 'a -> 'a -> 'a -> 'a -> 'a queue ref -> unit
    val enqueue5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a queue ref -> unit
    val front : 'a -> 'a queue ref -> 'a
    val front_get : 'a -> int -> 'a queue ref -> 'a
    val front_swap : 'a -> 'a queue ref -> unit
    val front_dup : 'a -> 'a queue ref -> unit
end

