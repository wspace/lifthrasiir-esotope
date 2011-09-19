(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 Space

 This module implements a general framework for multi-dimensional code or
 data space commonly found on many esolangs.
***************************************************************************)

(* Specifies how to manipulate various coordinate types. *)
module type IndexType = sig
    (* A type for coordinate offsets. This type is used both as a cell
     * coordinate and as a block coordinate. *)
    type offset

    (* A type for coordinate differences. *)
    type size

    (* A type for separators. *)
    type sep

    (* A type for an internal block size information. *)
    type block_size

    (* A coordinate offset that represents the origin. *)
    val origin : offset

    (* A zero coordinate difference. *)
    val zero : size

    (* Converts the desired block size to an internal block size information.
     * This is not strictly necessary; block_size can be unit if the block
     * size is fixed, for example. *)
    val to_block_size : size -> block_size

    (* Returns the number of cells per a block. *)
    val block_length : block_size -> int

    (* Splits a cell coordinate to a block coordinate and an index to a cell
     * in the corresponding block. The latter index should be between 0 and
     * (block_length blocksize - 1), and should map 0 to the lowermost cell
     * in the block. (There is no further restriction on this index.) *)
    val split : offset -> block_size -> offset * int

    (* Converts a block coordinate and an index to a cell in the corresponding
     * block to the cell coordinate. *)
    val combine : offset -> int -> block_size -> offset

    (* Calculates a lowermost (as defined by `minmax`) nearest neighbors of
     * given coordinate. *)
    val pred_offset : offset -> offset

    (* Calculates an uppermost (as defined by `minmax`) nearest neighbors of
     * given coordinate. *)
    val succ_offset : offset -> offset

    (* Offsets a coordinate to given amount. *)
    val add_offset : offset -> size -> offset

    (* Adds two coordinate difference. *)
    val add_size : size -> size -> size

    (* Returns a difference between two coordinates. *)
    val sub_offset : offset -> offset -> size

    (* Returns a difference between two coordinate differences. *)
    val sub_size : size -> size -> size

    (* Returns the lowermost and uppermost coordinates that contains two given
     * coordinates. Both of them are inclusive. *)
    val minmax : offset -> offset -> offset * offset

    (* Generates all coordinate differences within a block of given size. *)
    val iter : size -> (size -> unit) -> unit

    (* Advances a coordinate difference given the next separator. *)
    val advance : size -> sep option -> size

    (* Tries to read one seperator from byte-based stream. *)
    val sep_of_char_stream : char Stream.t -> sep option

    (* Tries to read one seperator from Unicode-based stream. *)
    val sep_of_unicode_stream : int Stream.t -> sep option
end

(* Output module signature for Make. *)
module type S = sig
    (* A type for coordinate offsets. *)
    type offset

    (* A type for coordinate differences. *)
    type size

    (* A type for separators. *)
    type sep

    (* A type for spaces with given coordinate types. *)
    type 'celltype t

    (* Creates a space with desired block size and default value for empty
     * cells. *)
    val create : size -> 'celltype -> 'celltype t

    (* Clears the entire space. *)
    val clear : 'celltype t -> unit

    (* Makes a copy of the entire space. *)
    val copy : 'celltype t -> 'celltype t

    (* Returns a cell in given coordinate. *)
    val get : 'celltype t -> offset -> 'celltype

    (* Sets a cell in given coordinate. *)
    val set : 'celltype t -> offset -> 'celltype -> unit

    (* Copies a specified region of a space to another space. *)
    val blit : 'celltype t -> offset -> 'celltype t -> offset -> size -> unit

    (* Computes a bounding region (i.e. known to contain all non-empty cells)
     * of a space. The bounds are not necessarily tight, and in fact may
     * increase monotonically. *)
    val bounds : 'celltype t -> offset * size

    (* Reads a space from given stream. The first function should read a
     * separator, or return None when there is no seperator. The second function
     * read one cell and return that cell; this function will be only called
     * when there is no seperator and not on the EOF. *)
    val from_stream : ('a Stream.t -> sep option) ->
                      ('a Stream.t -> 'celltype) -> 'a Stream.t ->
                      'celltype t -> offset -> 'celltype t

    (* Reads a space from given byte-based stream. Equivalent to `of_stream
     * I.sep_of_char_stream` for appropriate module I. *)
    val from_char_stream : (char Stream.t -> 'celltype) -> char Stream.t ->
                           'celltype t -> offset -> 'celltype t

    (* Reads a space from given Unicode-based stream. Equivalent to `of_stream
     * I.sep_of_unicode_stream` for appropriate module I. *)
    val from_unicode_stream : (int Stream.t -> 'celltype) -> int Stream.t ->
                              'celltype t -> offset -> 'celltype t
end

(* Makes a space module from given coordinate types. *)
module Make : functor (I : IndexType) ->
    S with type offset = I.offset and type size = I.size and type sep = I.sep

(**************************************************************************)
(* Common instances. *)

type sep2d = Newline2
type sep3d = Newline3 | Formfeed3

(* Two-dimensional Euclidean space. *)
module Space2D :
    S with type offset = int * int and type size = int * int
       and type sep = sep2d

