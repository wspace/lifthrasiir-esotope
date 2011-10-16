(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 PriorityQueue

 This module implements a priority queue, which is identical to queues but
 the dequeue operation returns an item with the lowest priority. It is
 useful for implementing various algorithms like Dijkstra's algorithm.

 The current implementation is a binary min-heap, and currently mutable
 (unlike Queues). This design is subject to change.
***************************************************************************)

(* TODO should be able to use arbitrary weight type *)
type 'a t

val create : unit -> 'a t

val add : 'a t -> int -> 'a -> unit

val extract : 'a t -> (int * 'a) option

