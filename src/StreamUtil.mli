(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 StreamUtil

 This module adds some additional utility functions to the standard Stream
 module. 
***************************************************************************)

(* Reads and returns the first element of the stream, or None if the stream
 * is empty. *)
val try_next : 'a Stream.t -> 'a option

