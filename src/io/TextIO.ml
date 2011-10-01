(* This is a part of Esotope. See README for more information. *)

(**************************************************************************)
(* Base classes. *)

class virtual text_io = object
    method virtual byte_size : int
    method virtual max_code : int

    method virtual put_code : int -> unit
    method virtual put_char : char -> unit
    method virtual put_str : string -> unit
    method virtual put_newline : unit -> unit
    method virtual put_int : int -> unit
    method virtual put_big_int : Big_int.big_int -> unit

    method virtual get_code : string option -> int option
    method virtual get_char : string option -> char option
    method virtual get_chars : int -> string option -> string
    method virtual get_line : string option -> string option
    method virtual get_str : string option -> string option
    method virtual get_nat : string option -> int option
    method virtual get_int : string option -> int option
    method virtual get_big_nat : string option -> Big_int.big_int option
    method virtual get_big_int : string option -> Big_int.big_int option
    method virtual get_all : unit -> string

    method virtual flush_out : unit -> unit
    method virtual flush_in : unit -> unit
end

(**************************************************************************)
(* Byte-based IO. *)

let zero_char = int_of_char '0'
let max_int_prefix = (max_int - 9) / 10
let min_int_prefix = (min_int + 9) / 10

class byte_io inchan outchan = object (self)
    inherit text_io

    method byte_size = 8
    method max_code = 255

    method put_code = output_byte outchan
    method put_char = output_char outchan
    method put_str = output_string outchan
    method put_newline () = self#put_char '\n'
    method put_int = Printf.fprintf outchan "%d"
    method put_big_int n = self#put_str (Big_int.string_of_big_int n)

    val lookahead = ref None
    method private getc () =
        match !lookahead with
        | Some c -> lookahead := None; Some c
        | None -> try Some (input_char inchan) with End_of_file -> None
    method private ungetc c =
        lookahead := Some c
    method private prompt = function
        | Some p -> output_string outchan p
        | None -> ()

    method get_code p =
        self#prompt p;
        match self#getc () with Some c -> Some (int_of_char c) | None -> None

    method get_char p = self#prompt p; self#getc ()

    method get_chars n p =
        self#prompt p;
        let buf = String.create n in
        let rec recur i =
            if i < n then
                match self#getc () with
                | Some c -> buf.[i] <- c; recur (i + 1)
                | None -> String.sub buf 0 i (* early EOF *)
            else buf
        in recur 0

    method get_line p =
        self#prompt p;
        let buf = Buffer.create 8 in
        let rec recur () =
            match self#getc () with
            | Some '\n' -> Some (Buffer.contents buf)
            | Some c -> Buffer.add_char buf c; recur ()
            | None -> None
        in recur ()

    (* TODO unimplemented, always reads one line *)
    method get_str = self#get_line

    method private parse_pos_int acc =
        match self#getc () with
        | Some ('0'..'9' as c) ->
            if acc <= max_int_prefix then
                self#parse_pos_int (acc * 10 + (int_of_char c - zero_char))
            else (* this can be the last character of this number *)
                begin try
                    int_of_string (string_of_int acc ^ String.make 1 c)
                with Failure _ -> self#ungetc c; acc
                end
        | Some c -> self#ungetc c; acc
        | None -> acc

    method private parse_neg_int acc =
        match self#getc () with
        | Some ('0'..'9' as c) ->
            if acc >= min_int_prefix then
                self#parse_neg_int (acc * 10 - (int_of_char c - zero_char))
            else (* this can be the last character of this number *)
                begin try
                    int_of_string (string_of_int acc ^ String.make 1 c)
                with Failure _ -> self#ungetc c; acc
                end
        | Some c -> self#ungetc c; acc
        | None -> acc

    method private collect_digits buf =
        match self#getc () with
        | Some ('0'..'9' as c) -> Buffer.add_char buf c; self#collect_digits buf
        | Some c -> self#ungetc c; Buffer.contents buf
        | None -> Buffer.contents buf

    method get_nat p =
        let rec skip () =
            match self#getc () with
            | Some ('0'..'9' as c) ->
                Some (self#parse_pos_int (int_of_char c - zero_char))
            | Some _ -> skip ()
            | None -> None
        in self#prompt p; skip ()

    method get_int p =
        let rec skip neg =
            match self#getc () with
            | Some ('0'..'9' as c) ->
                if neg then
                    Some (self#parse_neg_int (-(int_of_char c - zero_char)))
                else
                    Some (self#parse_pos_int (int_of_char c - zero_char))
            | Some '-' -> skip true
            | Some _ -> skip false
            | None -> None
        in self#prompt p; skip false

    method get_big_nat p =
        let rec skip () =
            match self#getc () with
            | Some ('0'..'9' as c) ->
                let buf = Buffer.create 8 in
                Buffer.add_char buf c;
                Some (Big_int.big_int_of_string (self#collect_digits buf))
            | Some _ -> skip ()
            | None -> None
        in self#prompt p; skip ()

    method get_big_int p =
        let rec skip neg =
            match self#getc () with
            | Some ('0'..'9' as c) ->
                let buf = Buffer.create 8 in
                Buffer.add_char buf (if neg then '-' else '+');
                Buffer.add_char buf c;
                Some (Big_int.big_int_of_string (self#collect_digits buf))
            | Some '-' -> skip true
            | Some _ -> skip false
            | None -> None
        in self#prompt p; skip false

    method get_all () =
        let rec loop s i bufsize =
            if i == bufsize then begin
                let bufsize' = bufsize * 2 in
                let s' = String.create bufsize' in
                String.blit s 0 s' 0 bufsize; loop s' i bufsize'
            end else begin
                let ret = input inchan s i (bufsize-i) in
                if ret == 0 then String.sub s 0 i else loop s (i+ret) bufsize
            end in
        let bufsize = 4096 in
        let s = String.create bufsize in
        match !lookahead with
        | Some c -> lookahead := None; s.[0] <- c; loop s 1 bufsize
        | None -> loop s 0 bufsize

    method flush_out () = flush outchan

    method flush_in () =
        (* XXX this is the best we can do. *)
        lookahead := None
end

let byte_stdio = new byte_io stdin stdout
let byte_errio = new byte_io stdin stderr

(**************************************************************************)
(* Unicode-based IO. *)

class unicode_io inchan outchan = object (self)
    inherit byte_io inchan outchan

    method max_code = 0x10ffff

    method put_code v =
        try
            self#put_str (UnicodeUtil.to_utf8 v)
        with UnicodeUtil.Invalid_code_point ->
            self#put_str "\239\191\189" (* U+FFFD *)

    method get_code p =
        self#prompt p; UnicodeUtil.get_utf8 self#getc
end

let unicode_stdio = new unicode_io stdin stdout
let unicode_errio = new unicode_io stdin stderr

(**************************************************************************)
(* Interpreters parametric to TextIO. *)

type t = text_io -> unit
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "interp-textio"
end

class virtual ['src] interpreter inkind = object
    inherit ['src,t] EsotopeCommon.processor inkind kind
    method weight = 9 (* do not penalize TextIO-based interpreters *)
end

let current_text_io = ref (byte_stdio :> text_io)

let interp_textio_to_interp = object
    inherit [t,EsotopeCommon.interp_type] EsotopeCommon.processor
        kind EsotopeCommon.interp_kind

    method weight = 1
    method process f = f !current_text_io
end

