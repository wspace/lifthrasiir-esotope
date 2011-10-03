(* This is a part of Esotope. See README for more information. *)

(**************************************************************************)
(* Base classes. *)

class virtual kind_base = object
    method virtual name : string
    method aliases = ([] : string list)
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
(* Registry for kinds and processors. *)

let kinds = Hashtbl.create 8
let kind_aliases = Hashtbl.create 8
let kind_extensions = Hashtbl.create 8
let processors = Hashtbl.create 8

let register_kind kind =
    (* sanity checks *)
    if Hashtbl.mem kinds kind#name then
        failwith (Printf.sprintf "fatal: duplicate kind name %S" kind#name);
    List.iter
        (fun alias -> if Hashtbl.mem kind_aliases alias then
            failwith (Printf.sprintf "fatal: duplicate kind alias %S" alias))
        kind#aliases;

    Hashtbl.add kinds kind#name kind;
    let add_alias alias =
        Hashtbl.add kind_aliases alias kind;
        if alias.[0] = '.' then Hashtbl.add kind_extensions alias kind
    in List.iter add_alias (kind#name :: kind#aliases);
    Hashtbl.add processors kind (Hashtbl.create 2)

let unregister_kind kind =
    Hashtbl.remove kinds kind#name;
    let remove_alias alias =
        Hashtbl.remove kind_aliases alias;
        Hashtbl.remove kind_extensions alias
    in List.iter remove_alias (kind#name :: kind#aliases);
    Hashtbl.remove processors kind;
    Hashtbl.iter (fun _ x -> Hashtbl.remove x kind) processors

let register_proc proc =
    let h = Hashtbl.find processors proc#input_kind in
    Hashtbl.add h proc#output_kind proc

let unregister_proc proc =
    let h = Hashtbl.find processors proc#input_kind in
    Hashtbl.remove h proc#output_kind

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
    initializer register_kind (self :> kind_base)
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

type formatter_type = Format.formatter -> unit
let formatter_kind = object
    inherit [formatter_type] kind
    method name = "formatter"
end

type interp_type = unit
let interp_kind = object
    inherit [interp_type] kind
    method name = "interp"
end

(**************************************************************************)
(* Processors. *)

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
    initializer register_proc (self :> processor_base)
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

class virtual ['src] inspector inkind = object
    inherit ['src,formatter_type] processor inkind formatter_kind
end

class virtual ['src] interpreter inkind = object
    inherit ['src,interp_type] processor inkind interp_kind
end

(**************************************************************************)
(* Built-in processors. *)

let stream_to_unicode = object
    inherit [stream_type,unicode_stream_type]
        processor stream_kind unicode_stream_kind

    method weight = 1
    method process stream =
        let getc () = StreamUtil.try_next stream in
        Stream.from (fun _ -> UnicodeUtil.get_utf8 getc)
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

let lookup_kind name = Hashtbl.find kind_aliases name

let lookup_extension name = Hashtbl.find kind_extensions name

let lookup_proc inp out =
    let h = Hashtbl.find processors inp in Hashtbl.find h out

module Heap = struct
    (* simple binary heap *)
    type 'a t = {
        mutable arr : (int * 'a option) array;
        mutable next : int
    }

    let create () = { arr = [| (0,None) |]; next = 1 }

    let add h w v =
        (* resizing *)
        if h.next = Array.length h.arr then begin
            let newarr = Array.make (h.next lsl 1) (0,None) in
            Array.blit h.arr 0 newarr 0 h.next; h.arr <- newarr
        end;

        let rec liftup pos =
            let upward = h.arr.(pos lsr 1) in
            if pos > 1 && fst upward > w then begin
                h.arr.(pos) <- upward;
                liftup (pos lsr 1)
            end else
                h.arr.(pos) <- (w, Some v)
        in liftup h.next; h.next <- h.next + 1

    let extract h =
        if h.next < 2 then
            None
        else
            let min = h.arr.(1) in
            h.next <- h.next - 1;
            let w, v = h.arr.(h.next) in
            h.arr.(h.next) <- (0,None);
            let rec liftdown pos =
                let posl, posr = pos lsl 1, (pos lsl 1) + 1 in
                let upward0, w0 =
                    if posl < h.next && fst h.arr.(posl) < w then
                        (posl, fst h.arr.(posl))
                    else
                        (pos, w) in
                let upward =
                    if posr < h.next && fst h.arr.(posr) < w0 then
                        posr
                    else
                        upward0 in
                if upward != pos then begin (* swap required *)
                    h.arr.(pos) <- h.arr.(upward);
                    liftdown upward
                end else
                    h.arr.(pos) <- (w, v)
            in liftdown 1;
            match min with
            | w', Some v' -> Some (w', v')
            | _, None -> None
end

let find_procs srckind destkind =
    let visited = Hashtbl.create 8 in
    let dist = Hashtbl.create 8 in
    let queue = Heap.create () in
    Heap.add queue 0 (srckind, []);
    Hashtbl.add dist srckind 0;

    let rec loop () =
        match Heap.extract queue with
        | None -> raise Not_found
        | Some (_, (v, trace)) when v = destkind -> List.rev trace
        | Some (w, (v, trace)) ->
            if not (Hashtbl.mem visited v) then begin
                Hashtbl.add visited v ();
                let relax v' proc =
                    if not (Hashtbl.mem visited v') then begin
                        let d = w + proc#weight in
                        if
                            try (d < Hashtbl.find dist v')
                            with Not_found -> true
                        then begin
                            Hashtbl.replace dist v' d;
                            Heap.add queue d (v', proc :: trace)
                        end
                    end
                in Hashtbl.iter relax (Hashtbl.find processors v)
            end;
            loop ()
    in loop ()

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

