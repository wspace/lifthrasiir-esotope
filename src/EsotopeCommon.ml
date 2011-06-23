(* This is a part of Esotope. See README for more information. *)

open Graph

(**************************************************************************)
(* Base classes. *)

class virtual kind_base = object
    method virtual name : string
    method virtual connect : source_base -> sink_base -> unit
end

and virtual sink_base = object
    method virtual input_kind : kind_base
end

and virtual source_base = object
    method virtual output_kind : kind_base
end

class virtual processor_base = object
    inherit sink_base
    inherit source_base

    method weight = 10 (* default weight *)
end

(**************************************************************************)
(* Lookup helpers. *)

let dummy_kind = object
    inherit kind_base
    method name = "<dummy>"
    method connect a b = failwith "should not be called."
end

let dummy_processor = object
    inherit processor_base
    method input_kind = dummy_kind
    method output_kind = dummy_kind
end

module V = struct
    type t = kind_base
    let equal x y = (x = y)
    let hash = Hashtbl.hash
    let compare = compare
end

module E = struct
    type t = processor_base
    let src e = e#input_kind
    let dst e = e#output_kind
    let default = dummy_processor
    let compare = compare
end

module G = Imperative.Digraph.ConcreteBidirectionalLabeled(V)(E)

module W = struct
    type label = G.E.label
    type t = int
    let weight proc = proc#weight
    let zero = 0
    let add = (+)
    let compare = compare
end

module Dij = Path.Dijkstra(G)(W)

let proc_graph = G.create ()
let kinds = Hashtbl.create 8

(**************************************************************************)
(* Implementations for kind, sink and source. *)

class virtual ['t] kind = object (self)
    inherit kind_base

    method connect a b =
        (* might segfault! *)
        if a#output_kind == b#input_kind
            then (Obj.magic b : 't sink)#receive (Obj.magic a : 't source)#send
            else failwith "kind mismatch"

    (* save itself for the later lookup. *)
    initializer Hashtbl.add kinds self#name (self :> kind_base)
    initializer G.add_vertex proc_graph (self :> kind_base)
end

and virtual ['t] sink inkind = object
    inherit sink_base
    method input_kind = inkind

    method virtual receive : 't -> unit
end

and virtual ['t] source outkind = object
    inherit source_base
    method output_kind = outkind

    method virtual send : 't
end

(**************************************************************************)
(* Built-in kinds. *)

type stream_type = char Stream.t
let stream_kind = object
    inherit [stream_type] kind
    method name = "stream"
end

type unicode_stream_type = int Stream.t
let unicode_stream_kind = object
    inherit [unicode_stream_type] kind
    method name = "unicode-stream"
end

(* internal use only; users should use parsing_reader instead. *)
type lexbuf_type = Lexing.lexbuf
let lexbuf_kind = object
    inherit [lexbuf_type] kind
    method name = "lexbuf"
end

type buffer_type = Buffer.t -> unit
let buffer_kind = object
    inherit [buffer_type] kind
    method name = "buffer"
end

type interp_type = unit
let interp_kind = object
    inherit [interp_type] kind
    method name = "interp"
end

(**************************************************************************)
(* Processors. *)

let processors = Hashtbl.create 8

class virtual ['src,'dest] processor inkind outkind = object (self)
    inherit processor_base
    inherit ['src] sink inkind
    inherit ['dest] source outkind

    (* the received data is unavailable once sent. *)
    val current = ref None

    (* with simplistic error check. *)
    method receive x = match !current with
        | Some _ -> failwith "overconnected processor"
        | None -> current := Some x
    method send = match !current with
        | Some x -> current := None; self#process x
        | None -> failwith "unconnected processor"

    method virtual process : 'src -> 'dest

    (* save itself for the later lookup. *)
    initializer G.add_edge_e proc_graph
        ((self#input_kind :> kind_base), (self :> processor_base),
         (self#output_kind :> kind_base))
end

class virtual ['dest] reader outkind = object
    inherit [stream_type,'dest] processor stream_kind outkind
end

class virtual ['dest] unicode_reader outkind = object
    inherit [unicode_stream_type,'dest] processor unicode_stream_kind outkind
    method weight = 9 (* does not penalize the parsing reader over others *)
end

class virtual ['dest] parsing_reader outkind = object
    inherit [lexbuf_type,'dest] processor lexbuf_kind outkind
    method weight = 9 (* see unicode_reader#weight *)
end

class virtual ['src] writer inkind = object
    inherit ['src,buffer_type] processor inkind buffer_kind
end

class virtual ['src] interpreter inkind = object
    inherit ['src,interp_type] processor inkind interp_kind
end

(**************************************************************************)
(* Built-in processors. *)

exception Invalid_utf8_sequence

let stream_to_unicode = object
    inherit [stream_type,unicode_stream_type]
        processor stream_kind unicode_stream_kind

    method weight = 1
    method process stream =
        let is_remainder c = '\128' <= c && c < '\192' in

        let read_remaining chk f v =
            match StreamUtil.try_next stream with
            | Some c when chk c ->
                f ((v lsl 6) lor (int_of_char c land 63))
            | _ -> raise Invalid_utf8_sequence in

        let read_remaining_one =
            read_remaining is_remainder (fun v -> Some v) in
        let read_remaining_two =
            read_remaining is_remainder read_remaining_one in
        let read_remaining_three =
            read_remaining is_remainder read_remaining_two in

        let read_one c = Some (int_of_char c) in
        let read_two c =
            read_remaining_one (int_of_char c land 31) in
        let read_three c =
            read_remaining_two (int_of_char c land 15) in
        let read_three_overlong c =
            read_remaining (fun c' -> '\160' <= c' && c' < '\192')
                read_remaining_one (int_of_char c land 15) in
        let read_four c =
            read_remaining_three (int_of_char c land 7) in
        let read_four_overlong c =
            read_remaining (fun c' -> '\144' <= c' && c' < '\192')
                read_remaining_two (int_of_char c land 7) in
        let read_four_out_of_range c =
            read_remaining (fun c' -> '\128' <= c' && c' < '\144')
                read_remaining_two (int_of_char c land 7) in

        let read _ =
            match StreamUtil.try_next stream with
            | Some c ->
                begin match c with
                | '\000'..'\127' -> read_one c
                | '\194'..'\223' -> read_two c
                | '\224'         -> read_three_overlong c
                | '\225'..'\239' -> read_three c
                | '\240'         -> read_four_overlong c
                | '\241'..'\243' -> read_four c
                | '\244'         -> read_four_out_of_range c
                | _              -> raise Invalid_utf8_sequence
                end
            | None -> None
        in Stream.from read
end

let stream_to_lexbuf = object
    inherit [stream_type,lexbuf_type] processor stream_kind lexbuf_kind

    method weight = 1
    method process stream =
        let buf = Buffer.create 1024 in
        Stream.iter (Buffer.add_char buf) stream;
        Lexing.from_string (Buffer.contents buf)
end

(**************************************************************************)
(* Lookup interface and driver. *)

let lookup_kind name = Hashtbl.find kinds name

let lookup_proc inp out =
    let _, proc, _ = G.find_edge proc_graph inp out in proc

let find_procs srckind destkind =
    let path, _ = Dij.shortest_path proc_graph srckind destkind in
    List.map (fun (_,proc,_) -> proc) path

let run data srckind procs destkind = 
    let result = ref None in
    let initial = object
        inherit ['src] source srckind
        method send = data
    end in
    let final = object
        inherit ['dest] sink destkind
        method receive x = result := Some x
    end in
    let rec connect first = function
        | next::others ->
            first#output_kind#connect first (next :> sink_base);
            connect (next :> source_base) others
        | [] ->
            destkind#connect first (final :> sink_base);
            match !result with Some x -> x | None -> failwith "unexpected"
    in connect (initial :> source_base) procs

