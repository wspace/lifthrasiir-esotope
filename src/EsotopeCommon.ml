(* This is a part of Esotope. See README for more information. *)

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

(**************************************************************************)
(* Implementations for kind, sink and source. *)

let kinds = Hashtbl.create 8

class virtual ['t] kind = object (self)
    inherit kind_base

    method connect a b =
        (* might segfault! *)
        if a#output_kind == b#input_kind
            then (Obj.magic b : 't sink)#receive (Obj.magic a : 't source)#send
            else failwith "kind mismatch"

    (* save itself for the later lookup. *)
    initializer Hashtbl.add kinds self#name (self :> kind_base)
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

class virtual processor_base = object
    inherit sink_base
    inherit source_base

    method weight = 10 (* default weight *)
end

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
    (* TODO should really be hashtbl of hashtbl. *)
    initializer Hashtbl.add processors
                            ((inkind :> kind_base), (outkind :> kind_base))
                            (self :> processor_base)
end

class virtual ['dest] reader outkind = object
    inherit [stream_type,'dest] processor stream_kind outkind
end

class virtual ['src] writer inkind = object
    inherit ['src,buffer_type] processor inkind buffer_kind
end

class virtual ['src] interpreter inkind = object
    inherit ['src,interp_type] processor inkind interp_kind
end

(**************************************************************************)
(* Lookup interface and driver. *)

let lookup_kind name = Hashtbl.find kinds name

let lookup_proc inp out = Hashtbl.find processors (inp,out)

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

