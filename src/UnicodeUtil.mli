(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 UnicodeUtil

 This module provides additional utility functions for handling Unicode
 code points and texts.
***************************************************************************)

exception Invalid_code_point
exception Invalid_utf8_sequence

(* Reads one Unicode code point using given input operation. It may return
 * None if it encountered EOF at the very first character. *)
val get_utf8 : (unit -> char option) -> int option

(* Converts a Unicode code point to the corresponding UTF-8 sequence. *)
val to_utf8 : int -> string

