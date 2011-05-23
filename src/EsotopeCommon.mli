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
(* Processors. *)

(* A base class for processor. Analogous to sink_base and source_base. *)
class virtual processor_base : object
    inherit sink_base
    inherit source_base

    (* The weight used for automatic connection; the connection path with
     * minimal sum of total weights is used. (Therefore it cannot be
     * negative.) The default weight is 10. *)
    method weight : int
end

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

(**************************************************************************)
(* Lookup interface and driver. *)

(* Searches the kind for given name. *)
val lookup_kind : string -> kind_base

(* Searches the processor for given input and output kind. *)
val lookup_proc : kind_base -> kind_base -> processor_base

(* The connection driver. *)
val run : 'src -> 'src kind -> processor_base list -> 'dest kind -> 'dest

(**************************************************************************)
(* Built-in kinds. *)

(* A kind for character stream. *)
type stream_type = char Stream.t
val stream_kind : stream_type kind

(* A (sort-of-a) kind for forcing the execution. *)
type interp_type = unit
val interp_kind : interp_type kind

