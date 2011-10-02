(* This is a part of Esotope. See README for more information. *)

(* Computes the base-2 logarithm, given that the value is the power of two.
 * (e.g. `log2_pow2 64` is 6, `log2_pow2 1` is 0.) It is undefined for values
 * that are not the power of two. *)
val log2_pow2 : int -> int

(* Computes the smallest power of two which is greater than or equal to given
 * value. (e.g. `next_pow2 49` is 64 and `next_pow2 8` is 8.) The result is
 * undefined for non-positive values. *)
val next_pow2 : int -> int

