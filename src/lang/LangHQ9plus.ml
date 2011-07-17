(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangHQ9plus

 This module implements the HQ9+ programming language, designed by
 Cliff L. Biffle in 2001. It is a good example of joke programming language,
 where you can only print "Hello, world", print its source code, print
 lyrics of 99 Bottles of Beer and increment the (invisible) accumulator.

 This module mostly provides a transformer *from* HQ9+ to other languages.
 Surprisingly, there still *exists* a set of languages that can be
 translated into HQ9+, like the Hello language.
***************************************************************************)

(* bool indicates the uppercased (true) and lowercased (false) version of
 * each commands. it matters when printing its own source code. *)
type node =
    | PrintHello of bool
    | PrintItself of bool  (* so-called "quine" instruction, but no. *)
    | PrintBeer
    | Increment
    | Comment of string    (* since the quine still requires it. *)

let stringify = function
    | PrintHello true -> "H"
    | PrintHello false -> "h"
    | PrintItself true -> "Q"
    | PrintItself false -> "q"
    | PrintBeer -> "9"
    | Increment -> "+"
    | Comment s -> s

let beer_song =
    let buf = Buffer.create 11354 (* the exact length *) in
    let bottles n =
        if n = 0 then "No bottles"
        else if n = 1 then "1 bottle"
        else (string_of_int n) ^ " bottles" in
    for n = 99 downto 1 do
        Printf.bprintf buf
            "%s of beer on the wall\n\
             %s of beer\n\
             Take one down and pass it around\n\
             %s of beer on the wall\n"
            (bottles n) (bottles n) (bottles (n-1));
        if n > 1 then Buffer.add_char buf '\n' else ()
    done;
    Buffer.contents buf

(**************************************************************************)
(* The kind. *)

type t = node list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "hq9+"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let rec parse acc =
            match Stream.peek stream with
            | Some 'H' -> Stream.junk stream; parse (PrintHello true :: acc)
            | Some 'h' -> Stream.junk stream; parse (PrintHello false :: acc)
            | Some 'Q' -> Stream.junk stream; parse (PrintItself true :: acc)
            | Some 'q' -> Stream.junk stream; parse (PrintItself false :: acc)
            | Some '9' -> Stream.junk stream; parse (PrintBeer :: acc)
            | Some '+' -> Stream.junk stream; parse (Increment :: acc)
            | Some _ ->
                let buf = Buffer.create 1 in
                let rec collect _ =
                    match Stream.peek stream with
                    | Some ch when not (String.contains "HhQq9+" ch) ->
                        Stream.junk stream;
                        Buffer.add_char buf ch;
                        collect ()
                    | _ -> () in
                collect ();
                parse (Comment (Buffer.contents buf) :: acc)
            | None -> List.rev acc
        in parse []
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process nodes io =
        let accum = ref 0 in (* actually useless *)
        let exec_node = function
            | PrintHello _ -> io#put_str "Hello, world!\n"
            | PrintItself _ ->
                let print_node n = io#put_str (stringify n) in
                List.iter print_node nodes
            | PrintBeer -> io#put_str beer_song
            | Increment -> incr accum
            | Comment _ -> ()
        in List.iter exec_node nodes
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let emit_node n = Buffer.add_string buf (stringify n) in
        List.iter emit_node nodes
end

(**************************************************************************)
(* The text-to-HQ9+ (TODO) and HQ9+-to-text transformer. *)

let to_text = object
    inherit [t, LangText.t] EsotopeCommon.processor kind LangText.kind

    method process nodes =
        let buf = Buffer.create 1024 in
        let exec_node = function
            | PrintHello _ -> Buffer.add_string buf "Hello, world!\n"
            | PrintItself _ ->
                let print_node n = Buffer.add_string buf (stringify n) in
                List.iter print_node nodes
            | PrintBeer -> Buffer.add_string buf beer_song
            | Increment | Comment _ -> ()
        in List.iter exec_node nodes; Buffer.contents buf
end

