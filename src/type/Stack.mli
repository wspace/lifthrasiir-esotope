(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 Stack

 This module implements utility functions for manipulating stacks. The most
 general version returns stack_ret type which can be either a successful
 result or a handler for failure due to not enough stack.

 Stack.Ops, Stack.ForcingOps and Stack.AssumingOps provides an easy-to-use
 version of these general operations with different behaviors when the stack
 does not have enough elements. They are suitable for most cases.
***************************************************************************)

(* The stack is a linked list; the first element is the top of the stack. *)
type 'a stack = 'a list
type 'a t = 'a stack

(* Every operation on the stack returns either a result and modified stack,
 * or a function that indicates the failure due to the stack underflow.
 * For the latter case, the function can be used to get the result assuming
 * the stack has infinitely many default values under it. *)
type ('a, 'b) stack_ret =
    | Success of 'a stack * 'b
    | Failure of ('a -> 'a stack * 'b)

(* This exception is used for the interface wrappers. *)
exception Stack_underflow

(* An empty stack. *)
val empty_stack : 'a stack

(* Creates an empty stack reference. *)
val create_stack : unit -> 'a stack ref

(**************************************************************************)
(* Operations. *)

(* (a -- ) Pops one element and returns it. *)
val pop : 'a stack -> ('a, 'a) stack_ret

(* (a1...ak -- ) Pops multiple elements and returns them. The return value
 * is ordered from the bottom of the stack to the top of the stack. (Thus
 * if there are enough elements, the return value of `popK` is equivalent
 * to return values of K `pop`s concatenated in the *reverse* order.) *)
val pop2 : 'a stack -> ('a, 'a * 'a) stack_ret
val pop3 : 'a stack -> ('a, 'a * 'a * 'a) stack_ret
val pop4 : 'a stack -> ('a, 'a * 'a * 'a * 'a) stack_ret
val pop5 : 'a stack -> ('a, 'a * 'a * 'a * 'a * 'a) stack_ret

(* (a1...ak -- ) Pops given number of elements and returns them. The return
 * value is ordered from the bottom of the stack to the top of the stack.
 * The resulting list has always same number of elements. *)
val popn : int -> 'a stack -> ('a, 'a list) stack_ret

(* (a1...ak -- ) Pops at most given number of elements and returns them.
 * The return value is ordered from the bottom of the stack to the top of
 * the stack. The resulting list may have less number of elements if the
 * stack does not have enough elements; it always succeeds therefore. *)
val popupton : int -> 'a stack -> ('a, 'a list) stack_ret

(* ( -- a) Pushes one element. *)
val push : 'a -> 'a stack -> ('a, unit) stack_ret

(* ( -- a1...ak) Pushes multiple elements. The elements are pushed from
 * the left to the right, in that order. *)
val push2 : 'a -> 'a -> 'a stack -> ('a, unit) stack_ret
val push3 : 'a -> 'a -> 'a -> 'a stack -> ('a, unit) stack_ret
val push4 : 'a -> 'a -> 'a -> 'a -> 'a stack -> ('a, unit) stack_ret
val push5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a stack -> ('a, unit) stack_ret

(* ( -- a1...ak) Pushes multiple elements. The elements are pushed from
 * the left to the right, in that order. *)
val pushn : 'a list -> 'a stack -> ('a, unit) stack_ret

(* (a1...ak -- f(a1...ak)) Pops elements, applies them to given function
 * and pushes the result. The function arguments are passed in the push
 * order (just like `popK` operations.) *)
val pop_push : ('a -> 'a) -> 'a stack -> ('a, unit) stack_ret
val pop2_push : ('a -> 'a -> 'a) -> 'a stack -> ('a, unit) stack_ret
val pop3_push : ('a -> 'a -> 'a -> 'a) -> 'a stack -> ('a, unit) stack_ret

(* Returns the top of the stack (without updating it). Functionally
 * equivalent to `get 0`. *)
val peek : 'a stack -> ('a, 'a) stack_ret

(* Returns the i-th element of the stack, where the top of the stack is the
 * zeroth element. *)
val get : int -> 'a stack -> ('a, 'a) stack_ret

(* (a b -- b a) Swaps the top and second-to-top elements of the stack.
 * Functionally equivalent to `roll 1 1`. *)
val swap : 'a stack -> ('a, unit) stack_ret

(* (a -- a a) Pushes the top of the stack. When the stack is empty and you
 * assumed the default value the resulting stack has one element; if you
 * want two elements in this case you should use `dup2` instead.
 * Functionally equivalent to `pick 1 0`. *)
val dup : 'a stack -> ('a, unit) stack_ret

(* (a -- a a) Pushes the top of the stack. It is functionally equivalent to
 * pop from the stack and push it twice; it thus ensures that the resulting
 * stack has at least two elements on success. Functionally equivalent to
 * `pick2 1 0`. *)
val dup2 : 'a stack -> ('a, unit) stack_ret

(* (a...a b...b -- a...a b...b a...a) Given two integers y and x, pushes y
 * elements starting at the x-th element (that is, (x+y-1)-th to x-th) to
 * the stack preserving the order. (The top of the stack is zeroth.)
 * The resulting stack has at least y elements; if you need to simulate
 * the sequence of pushes and pops you should use `pick2` instead. *)
val pick : int -> int -> 'a stack -> ('a, unit) stack_ret

(* (a...a b...b -- a...a b...b a...a) Given two integers y and x, pushes y
 * elements starting at the x-th element (that is, (x+y-1)-th to x-th) to
 * the stack preserving the order. (The top of the stack is zeroth.)
 * It simulates the required pushes and pops; the resulting stack has
 * at least (x+2y) elements on success. *)
val pick2 : int -> int -> 'a stack -> ('a, unit) stack_ret

(* (a...a b...b -- b...b a...a) Given two integers y and x, moves y elements
 * starting at the x-th element (that is, (x+y-1)-th to x-th) to the top of
 * the stack preserving the order. (The top of the stack is zeroth.) *)
val roll : int -> int -> 'a stack -> ('a, unit) stack_ret

(* (a...a b...b -- b...b) Given two integers y and x, removes y elements
 * starting at the x-th element (that is, (x+y-1)-th to x-th) from the stack.
 * If you need to simulate the sequence of pushes and pops you should use
 * `discard2` instead. *)
val discard : int -> int -> 'a stack -> ('a, unit) stack_ret

(* (a...a b...b -- b...b) Given two integers y and x, removes y elements
 * starting at the x-th element (that is, (x+y-1)-th to x-th) from the stack.
 * It simulates the required pushes and pops; the resulting stack has
 * at least x elements on success. *)
val discard2 : int -> int -> 'a stack -> ('a, unit) stack_ret

(**************************************************************************)
(* Operation wrappers. *)

(* Maps stack operations to the mutable version. Returns None on failure. *)
val stack_op : ('a stack -> ('a,'b) stack_ret) -> 'a stack ref -> 'b option

(* Version of mutable operations wrapped via `stack_op`. They return either
 * option types or boolean, which is isomorphic to `unit option`. *)
module Ops : sig
    type 'a stack = 'a t
    val create : unit -> 'a stack ref
    val pop : 'a stack ref -> 'a option
    val pop2 : 'a stack ref -> ('a * 'a) option
    val pop3 : 'a stack ref -> ('a * 'a * 'a) option
    val pop4 : 'a stack ref -> ('a * 'a * 'a * 'a) option
    val pop5 : 'a stack ref -> ('a * 'a * 'a * 'a * 'a) option
    val popn : int -> 'a stack ref -> 'a list option
    val popupton : int -> 'a stack ref -> 'a list
    val push : 'a -> 'a stack ref -> unit
    val push2 : 'a -> 'a -> 'a stack ref -> unit
    val push3 : 'a -> 'a -> 'a -> 'a stack ref -> unit
    val push4 : 'a -> 'a -> 'a -> 'a -> 'a stack ref -> unit
    val push5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a stack ref -> unit
    val pushn : 'a list -> 'a stack ref -> unit
    val pop_push : ('a -> 'a) -> 'a stack ref -> bool
    val pop2_push : ('a -> 'a -> 'a) -> 'a stack ref -> bool
    val pop3_push : ('a -> 'a -> 'a -> 'a) -> 'a stack ref -> bool
    val peek : 'a stack ref -> 'a option
    val get : int -> 'a stack ref -> 'a option
    val swap : 'a stack ref -> bool
    val dup : 'a stack ref -> bool
    val dup2 : 'a stack ref -> bool
    val pick : int -> int -> 'a stack ref -> bool
    val pick2 : int -> int -> 'a stack ref -> bool
    val roll : int -> int -> 'a stack ref -> bool
    val discard : int -> int -> 'a stack ref -> bool
    val discard2 : int -> int -> 'a stack ref -> bool
end

(* Maps stack operations to the mutable version. Raises Stack_underflow on
 * failure. *)
val forcing_stack_op : ('a stack -> ('a,'b) stack_ret) -> 'a stack ref -> 'b

(* Version of mutable operations wrapped via `forcing_stack_op`. They may
 * raise Stack_underflow on failure; it does not affect the stack in that
 * case. *)
module ForcingOps : sig
    type 'a stack = 'a t
    exception Stack_underflow
    val create : unit -> 'a stack ref
    val pop : 'a stack ref -> 'a
    val pop2 : 'a stack ref -> 'a * 'a
    val pop3 : 'a stack ref -> 'a * 'a * 'a
    val pop4 : 'a stack ref -> 'a * 'a * 'a * 'a
    val pop5 : 'a stack ref -> 'a * 'a * 'a * 'a * 'a
    val popn : int -> 'a stack ref -> 'a list
    val popupton : int -> 'a stack ref -> 'a list
    val push : 'a -> 'a stack ref -> unit
    val push2 : 'a -> 'a -> 'a stack ref -> unit
    val push3 : 'a -> 'a -> 'a -> 'a stack ref -> unit
    val push4 : 'a -> 'a -> 'a -> 'a -> 'a stack ref -> unit
    val push5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a stack ref -> unit
    val pushn : 'a list -> 'a stack ref -> unit
    val pop_push : ('a -> 'a) -> 'a stack ref -> unit
    val pop2_push : ('a -> 'a -> 'a) -> 'a stack ref -> unit
    val pop3_push : ('a -> 'a -> 'a -> 'a) -> 'a stack ref -> unit
    val peek : 'a stack ref -> 'a
    val get : int -> 'a stack ref -> 'a
    val swap : 'a stack ref -> unit
    val dup : 'a stack ref -> unit
    val dup2 : 'a stack ref -> unit
    val pick : int -> int -> 'a stack ref -> unit
    val pick2 : int -> int -> 'a stack ref -> unit
    val roll : int -> int -> 'a stack ref -> unit
    val discard : int -> int -> 'a stack ref -> unit
    val discard2 : int -> int -> 'a stack ref -> unit
end

(* Maps stack operations to the mutable version. If the stack does not have
 * enough elements, assumes that the stack contains infinitely many default
 * values below it. *)
val assuming_stack_op : 'a -> ('a stack -> ('a,'b) stack_ret) ->
                        'a stack ref -> 'b

(* Version of mutable operations wrapped via `assuming_stack_op`. They
 * receive the default value as the first argument. *)
module AssumingOps : sig
    type 'a stack = 'a t
    val create : unit -> 'a stack ref
    val pop : 'a -> 'a stack ref -> 'a
    val pop2 : 'a -> 'a stack ref -> 'a * 'a
    val pop3 : 'a -> 'a stack ref -> 'a * 'a * 'a
    val pop4 : 'a -> 'a stack ref -> 'a * 'a * 'a * 'a
    val pop5 : 'a -> 'a stack ref -> 'a * 'a * 'a * 'a * 'a
    val popn : 'a -> int -> 'a stack ref -> 'a list
    val popupton : int -> 'a stack ref -> 'a list
    val push : 'a -> 'a stack ref -> unit
    val push2 : 'a -> 'a -> 'a stack ref -> unit
    val push3 : 'a -> 'a -> 'a -> 'a stack ref -> unit
    val push4 : 'a -> 'a -> 'a -> 'a -> 'a stack ref -> unit
    val push5 : 'a -> 'a -> 'a -> 'a -> 'a -> 'a stack ref -> unit
    val pushn : 'a list -> 'a stack ref -> unit
    val pop_push : 'a -> ('a -> 'a) -> 'a stack ref -> unit
    val pop2_push : 'a -> ('a -> 'a -> 'a) -> 'a stack ref -> unit
    val pop3_push : 'a -> ('a -> 'a -> 'a -> 'a) -> 'a stack ref -> unit
    val peek : 'a -> 'a stack ref -> 'a
    val get : 'a -> int -> 'a stack ref -> 'a
    val swap : 'a -> 'a stack ref -> unit
    val dup : 'a -> 'a stack ref -> unit
    val dup2 : 'a -> 'a stack ref -> unit
    val pick : 'a -> int -> int -> 'a stack ref -> unit
    val pick2 : 'a -> int -> int -> 'a stack ref -> unit
    val roll : 'a -> int -> int -> 'a stack ref -> unit
    val discard : 'a -> int -> int -> 'a stack ref -> unit
    val discard2 : 'a -> int -> int -> 'a stack ref -> unit
end

