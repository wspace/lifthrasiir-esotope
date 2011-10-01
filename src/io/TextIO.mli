(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 TextIO

 This module provides a base interface for text-based input/output. It also
 provides an "interp-textio" kind for parametrizing the interpreter using
 given text IO object.
***************************************************************************)

class virtual text_io : object
    (* Returns the number of bits per a byte. Printing bits in the multiple of
     * this number should print all bits. *)
    method virtual byte_size : int
    (* Returns the maximal charater code allowed for put_code. All character
     * codes from 0 to this number should be valid for put_code, even it prints
     * an error message (e.g. surrogate pair for Unicode-based IO). *)
    method virtual max_code : int

    (* Prints one character code, which may correspond to one or more bytes.
     * The meaning of the character code is up to the implementation except
     * that it should be non-negative. Printing an invalid character code is
     * undefined, so it's the caller's responsibility for checking the range. *)
    method virtual put_code : int -> unit
    (* Prints one byte. *)
    method virtual put_char : char -> unit
    (* Prints a string. *)
    method virtual put_str : string -> unit
    (* Prints a newline. It may or may not flush the output. *)
    method virtual put_newline : unit -> unit
    (* Prints an integer as a string. *)
    method virtual put_int : int -> unit
    (* Prints a big integer as a string. *)
    method virtual put_big_int : Big_int.big_int -> unit

    (* Reads one character code, which may correspond to one or more bytes,
     * optionally showing a prompt to the user. It may return -1 if bytes do
     * not form the valid character; the IO should synchronize to the next
     * valid character with its own algorithm. *)
    method virtual get_code : string option -> int option
    (* Reads one byte, optionally showing a prompt to the user. *)
    method virtual get_char : string option -> char option
    (* Reads given number of bytes, optionally showing a prompt to the user.
     * It may return less than given number of bytes if it reached EOF. *)
    method virtual get_chars : int -> string option -> string
    (* Reads one line as a string, optionally showing a prompt to the user. *)
    method virtual get_line : string option -> string option
    (* Reads one string, optionally showing a prompt to the user. This method
     * should be able to read any kind of string if possible, including one
     * that contains a newline. Since the actual interface depends on the
     * implementation, the caller should use the get_line method whenever
     * possible. *)
    method virtual get_str : string option -> string option
    (* Reads one natural number (i.e. integer without a sign), optionally
     * showing a prompt to the user. It should skip the non-numeric characters
     * before the input, and should stop at the first non-numeric character or
     * a point that reading an additional digit causes an overflow. For a form-
     * based interface, it should reject an invalid number and ask the user
     * again. *)
    method virtual get_nat : string option -> int option
    (* Reads one integer, optionally showing a prompt to the user. Other
     * details are same to get_nat. *)
    method virtual get_int : string option -> int option
    (* Reads one natural number as a big_int, optionally showing a prompt to
     * the user. Other details are same to get_nat, but it never overflows. *)
    method virtual get_big_nat : string option -> Big_int.big_int option
    (* Reads one integer as a big_int, optionally showing a prompt to the
     * user. Other details are same to get_big_nat. *)
    method virtual get_big_int : string option -> Big_int.big_int option
    (* Reads *all* remaining input as a string. *)
    method virtual get_all : unit -> string

    (* Ensures that the internal output state of the IO object is processed and
     * updated back to the initial state. *)
    method virtual flush_out : unit -> unit
    (* Ensures that the internal input state of the IO object is processed and
     * updated back to the initial state. *)
    method virtual flush_in : unit -> unit
end

(* A byte-based IO object with given input and output channels. *)
class byte_io : in_channel -> out_channel -> object
    inherit text_io

    method byte_size : int
    method max_code : int

    method put_code : int -> unit
    method put_char : char -> unit
    method put_str : string -> unit
    method put_newline : unit -> unit
    method put_int : int -> unit
    method put_big_int : Big_int.big_int -> unit

    method get_code : string option -> int option
    method get_char : string option -> char option
    method get_chars : int -> string option -> string
    method get_line : string option -> string option
    method get_str : string option -> string option
    method get_nat : string option -> int option
    method get_int : string option -> int option
    method get_big_nat : string option -> Big_int.big_int option
    method get_big_int : string option -> Big_int.big_int option
    method get_all : unit -> string

    method flush_out : unit -> unit
    method flush_in : unit -> unit
end

(* A shortcut for a byte-based IO object with stdin and stdout/stderr,
 * respectively. *)
val byte_stdio : text_io
val byte_errio : text_io

(* A Unicode-based IO object with given input and output channels. It expects
 * a UTF-8-encoded stream as an input. *)
class unicode_io : in_channel -> out_channel -> object
    inherit byte_io

    method max_code : int
    method put_code : int -> unit
    method get_code : string option -> int option
end

(* A shortcut for a Unicode-based IO object with stdin and stdout/stderr,
 * respectively. *)
val unicode_stdio : text_io
val unicode_errio : text_io

(**************************************************************************)
(* Interpreters parametric to TextIO. *)

(* A kind for interpreters parametric to TextIO. *)
type t = text_io -> unit
val kind : t EsotopeCommon.kind

(* Interpreter parametric to TextIO. It is similar to the interpreter but
 * should get the IO object in order to actually execute given code. *)
class virtual ['src] interpreter : 'src EsotopeCommon.kind -> object
    inherit ['src,t] EsotopeCommon.processor
end

(* The current IO object in use of an internal interp-textio-to-interp
 * processor. *)
val current_text_io : text_io ref

