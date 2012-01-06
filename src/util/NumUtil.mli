(* This is a part of Esotope. See README for more information. *)

(* Performs the extended Euclidean algorithm: `gcdex x y` returns a triple
 * `(u, v, g)` such that g = gcd(x,y) and ux + vy = g. *)
val gcdex : int -> int -> int * int * int

