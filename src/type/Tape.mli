(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 Tape

 This module implements an one-dimensional tape commonly found on many
 esolangs. The tape is a non-random-access memory that only allows local
 access around the current tape "pointer".

 The tape is conceptually simpler than normal random access memory because
 it does not allow the direct access to the (globally unique) address.
 Since the *true* Turing machine requires the infinitely large memory,
 the RAM with finite-sized pointer cannot be used for truly Turing-complete
 language (it would be only the bounded-storage machine); there is no such
 complication with the tape.
***************************************************************************)

type 'celltype t

(* Creates an empty tape with given default (empty) value and desired block
 * size. The actual block size may be larger than given value.
 *
 * The block size determines how much cells are located in consecutive memory
 * positions. Since the current block that contains the tape pointer is cached,
 * larger blocks may be beneficial for performance in expense of memory usage.
 * Note that this means that large relative offsets for `get` and `set` are
 * inefficient. *)
val create : int -> 'celltype -> 'celltype t

(* Moves the pointer by given amount. *)
val offset : int -> 'celltype t -> unit

(* Moves the pointer forward by one cell. Equivalent to `offset 1`. *)
val advance : 'celltype t -> unit

(* Moves the pointer backward by one cell. Equivalent to `offset (-1)`. *)
val retract : 'celltype t -> unit

(* Returns the cell pointed by given relative offset to the current pointer.
 * The relative offset can be arbitrary, but most efficient when it ranges from
 * -N to N where N is the block size. *)
val get : int -> 'celltype t -> 'celltype

(* Sets the cell pointed by given relative offset to the current pointer. *)
val set : int -> 'celltype -> 'celltype t -> unit

