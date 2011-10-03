(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 EsotopeCommon

 This module provides the common infrastructure for code readers, writers
 and processors. There are three main classes here:

   - The "source" generates the data somehow. For example, it may read the
     standard input to generate the text stream.
   - The "sink" consumes the data somehow. For example, it may execute
     given code.
   - The "kind" holds the information required for connecting a source to
     a sink. The connection is type-safe, and you cannot connect a source
     and a sink with different kinds.

 In this model the processor is not primitive, as it is both a source and
 a generator. In practice the processor should contain the data
 temporarily, so the "processor" class is provided for the convenience.
 You just have to implement one method (processor#process) to get it work.

 Since Ocaml lacks the run-time type inspection (RTTI), this code relies
 on the unsafe cast (Obj.magic). It makes this module one of the messiest
 module in Esotope. Be careful on the modification.
***************************************************************************)

(**************************************************************************)
(* Base classes. *)

(* A base class for kind. *)
class virtual kind_base : object
    (* Kind name. Used when parsing the user arguments. *)
    method virtual name : string
    (* Other possible aliases for kind name. If the entry starts with a period
     * ('.') then it is also used as an associated extension for this kind. *)
    method aliases : string list
    (* Connects a source and sink. *)
    method virtual connect : source_base -> sink_base -> unit
end

(* A base class for sink. *)
and virtual sink_base : object
    (* A kind for input type. *)
    method virtual input_kind : kind_base
end

(* A base class for source. *)
and virtual source_base : object
    (* A kind for output type. *)
    method virtual output_kind : kind_base
end

(* A base class for processor. Analogous to sink_base and source_base. *)
class virtual processor_base : object
    inherit sink_base
    inherit source_base

    (* The weight used for automatic connection; the connection path with
     * minimal sum of total weights is used. (Therefore it cannot be
     * negative.) The default weight is 10. *)
    method weight : int
end

(**************************************************************************)
(* Registry for kinds and processors. *)

(* A mapping from a principal kind name to a kind. Since there are exactly one
 * entry for each kind, this is mainly intended for enumerating all kinds. *)
val kinds : (string, kind_base) Hashtbl.t
(* A mapping from a kind alias to a kind. This is what lookup_kind uses. *)
val kind_aliases : (string, kind_base) Hashtbl.t
(* A mapping from an associated file extension to a kind. This is what
 * lookup_extension uses. *)
val kind_extensions : (string, kind_base) Hashtbl.t

(* Registers or unregisters given kind. *)
val register_kind : kind_base -> unit
val unregister_kind : kind_base -> unit

(* A mapping from source and destination kinds to a processor. *)
val processors : (kind_base, (kind_base, processor_base) Hashtbl.t) Hashtbl.t

(* Registers or unregisters given processor. *)
val register_proc : processor_base -> unit
val unregister_proc : processor_base -> unit

(**************************************************************************)
(* Implementations for kind, sink and source. *)

(* Implementation for kind. *)
class virtual ['t] kind : object
    inherit kind_base
    method connect : source_base -> sink_base -> unit
end

(* Implementation for sink. *)
and virtual ['t] sink : 't kind -> object
    inherit sink_base
    method input_kind : kind_base

    (* Receives the given data. *)
    method virtual receive : 't -> unit
end

(* Implementation for source. *)
and virtual ['t] source : 't kind -> object
    inherit source_base
    method output_kind : kind_base

    (* Sends the data. *)
    method virtual send : 't
end

(**************************************************************************)
(* Built-in kinds. *)

(* A kind for 8-bit character stream. *)
type stream_type = char Stream.t
val stream_kind : stream_type kind

(* A kind for unicode character stream. *)
type unicode_stream_type = int Stream.t
val unicode_stream_kind : unicode_stream_type kind

(* A kind for character buffer. Since it is normally an output kind, its
 * type does not directly represent a buffer, but a function that writes to
 * given buffer. *)
type buffer_type = Buffer.t -> unit
val buffer_kind : buffer_type kind

(* A kind for formatter (as defined in Format standard module). Like
 * buffer_type it is not the formatter itself but a function that writes to
 * given formatter. *)
type formatter_type = Format.formatter -> unit
val formatter_kind : formatter_type kind

(* A (sort-of-a) kind for forcing the execution. *)
type interp_type = unit
val interp_kind : interp_type kind

(**************************************************************************)
(* Processors. *)

(* Implementation for processor. *)
class virtual ['src,'dest] processor : 'src kind -> 'dest kind -> object
    inherit processor_base
    inherit ['src] sink
    inherit ['dest] source
    method receive : 'src -> unit
    method send : 'dest

    (* Processes the given data to produce an another data. *)
    method virtual process : 'src -> 'dest
end

(* Stream reader. It is a special case of processor and should read the code
 * from given stream. *)
class virtual ['dest] reader : 'dest kind -> object
    inherit [stream_type,'dest] processor
end

(* Unicode stream reader. It is a variant of stream reader that internally
 * converts UTF-8-encoded bytes into Unicode code points. Note that there is
 * no support for other encodings; use iconv instead. *)
class virtual ['dest] unicode_reader : 'dest kind -> object
    inherit [unicode_stream_type,'dest] processor
end

(* Parsing reader. It is a variant of stream reader that uses an ocamllex and
 * ocamlyacc to parse the code. *)
class virtual ['dest] parsing_reader : 'dest kind -> object
    inherit [Lexing.lexbuf,'dest] processor
end

(* Stream writer. It is a special case of processor and should emit the data
 * into given buffer. Note that the process method returns an another
 * function, which receives the output buffer and writes to it. *)
class virtual ['src] writer : 'src kind -> object
    inherit ['src,buffer_type] processor
end

(* Inspector. It is similar to the stream writer but generally used for
 * inspecting internal representation of the kind, and enabled when using -I
 * switch. The process method returns a function which receives the output
 * formatter (instead of a buffer). *)
class virtual ['src] inspector : 'src kind -> object
    inherit ['src,formatter_type] processor
end

(* Interpreter. It is a special case of processor and should execute given
 * code. It entirely relies on the side effect, and its result is actually
 * a unit. *)
class virtual ['src] interpreter : 'src kind -> object
    inherit ['src,interp_type] processor
end

(**************************************************************************)
(* Lookup interface and driver. *)

(* Searches the kind for given name. Raises Not_found if there is no such
 * kind. *)
val lookup_kind : string -> kind_base

(* Searches the kind for given file extension (a form of ".ext"). Raises
 * Not_found if there is no such extension. *)
val lookup_extension : string -> kind_base

(* Searches the processor for given input and output kind. Raises Not_found
 * if there is no such processor. *)
val lookup_proc : kind_base -> kind_base -> processor_base

(* Automatically builds the connection of processors with minimal sum of
 * weights. Raises Not_found if there exists no path that connects two given
 * kinds. *)
val find_procs : kind_base -> kind_base -> processor_base list

(* The connection driver. *)
val run : 'src -> 'src kind -> processor_base list -> 'dest kind -> 'dest

