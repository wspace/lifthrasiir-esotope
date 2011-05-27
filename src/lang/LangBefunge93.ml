(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangBefunge93

 This module implements the Befunge programming language, designed by
 Chris Pressey in 1993. (It is now retroactively called Befunge-93 too.)
 It is one of the earliest two-dimensional programming languages, which
 also include Biota (1991) and Orthagonal/Orthogonal (1994), and the most
 successful one among them.

 Befunge-93 code is laid on an 80x25 space of instructions, which can be
 read or written during the execution (self-modification). The instruction
 pointer (IP) is a 2D vector and the direction (delta) is added to the IP
 after the execution of each instruction. Besides from the code space,
 Befunge program operates on a stack, so every non-self-modifying Befunge
 code can be translated to the simple stack machine language. In practice,
 however, using only stack is quite hard so many Befunge programs also use
 a portion of the code as a scratchpad (especially left-top 10x10 region);
 therefore they are self-modifying and yet still very static except for
 the scratchpad region. See LangBefunge93Static for the static form of
 the language. (TODO)
***************************************************************************)

(**************************************************************************)
(* The kind. *)

type t = char array array
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "befunge93"
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let nrows = 25 in
        let ncolumns = 80 in
        let code = Array.init nrows (fun _ -> Array.make ncolumns ' ') in

        let rec parse_line x row =
            match Stream.peek stream with
            (* TODO should eventually be moved to IO module... *)
            | Some '\n' -> Stream.junk stream; true
            | Some '\r' ->
                Stream.junk stream;
                begin match Stream.peek stream with
                | Some '\n' -> Stream.junk stream; true
                | Some _ -> true
                | None -> false
                end
            | Some ch ->
                Stream.junk stream;
                if x < ncolumns then row.(x) <- ch else ();
                parse_line (x + 1) row
            | None -> false
        in

        let rec parse y =
            if parse_line 0 code.(y) && y + 1 < nrows then
                parse (y + 1)
            else
                code
        in

        parse 0
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] EsotopeCommon.interpreter kind

    method process code =
        let code = Array.map Array.copy code in
        let nrows = Array.length code in
        let ncolumns = Array.length code.(0) in

        let inchan = stdin in
        let outchan = stdout in
        let delta = ref (1,0) in
        let stack = ref [] in

        let get _ = match !stack with [] -> 0 | h::t -> stack := t; h in
        let get2 _ = let a = get () in let b = get () in (a, b) in
        let advance (x,y) =
            let dx, dy = !delta in
            let x, y = (x + dx, y + dy) in
            let x = if x < 0 then x + ncolumns
                    else if x >= ncolumns then x - ncolumns
                    else x in
            let y = if y < 0 then y + nrows
                    else if y >= nrows then y - nrows
                    else y in
            (x, y)
        in

        let rec exec (x,y) =
            match code.(y).(x) with
            | '@' -> ()
            | '#' -> exec (advance (advance (x,y)))
            | '"' -> exec_stringmode (advance (x,y))
            | ch ->
                begin match ch with
                | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                    stack := (int_of_char ch - int_of_char '0') :: !stack
                | '+' -> let b, a = get2 () in stack := (a+b) :: !stack
                | '-' -> let b, a = get2 () in stack := (a-b) :: !stack
                | '*' -> let b, a = get2 () in stack := (a*b) :: !stack
                | '/' ->
                    let b, a = get2 () in
                    if b = 0 then
                        stack := 0 :: !stack (* TODO proper message *)
                    else
                        stack := (a/b) :: !stack
                | '%' ->
                    let b, a = get2 () in
                    if b = 0 then
                        stack := 0 :: !stack (* TODO proper message *)
                    else
                        stack := (a mod b) :: !stack
                | '!' ->
                    let a = get () in
                    stack := (if a = 0 then 1 else 0) :: !stack
                | '`' ->
                    let b, a = get2 () in
                    stack := (if a > b then 1 else 0) :: !stack
                | '>' -> delta := (1,0)
                | 'v' -> delta := (0,1)
                | '<' -> delta := (-1,0)
                | '^' -> delta := (0,-1)
                | '?' ->
                    delta := [| (1,0); (0,1); (-1,0); (0,-1) |].(Random.int 4)
                | '_' ->
                    let a = get () in delta := if a = 0 then (1,0) else (-1,0)
                | '|' ->
                    let a = get () in delta := if a = 0 then (0,1) else (0,-1)
                | ':' -> let a = get () in stack := a :: a :: !stack
                | '\\' -> let b, a = get2 () in stack := a :: b :: !stack
                | '$' -> ignore (get ())
                | '.' ->
                    let a = get () in
                    Printf.fprintf outchan "%d " a;
                    flush outchan
                | ',' ->
                    let a = get () in
                    output_char outchan (char_of_int (a land 255));
                    flush outchan
                | 'p' ->
                    let ty, tx = get2 () in
                    let v = get () in
                    if 0 <= ty && ty < nrows && 0 <= tx && tx < ncolumns then
                        code.(ty).(tx) <- char_of_int (v land 255)
                    else
                        () (* out-of-boundary is not an error, but a nop *)
                | 'g' ->
                    let ty, tx = get2 () in
                    if 0 <= ty && ty < nrows && 0 <= tx && tx < ncolumns then
                        stack := int_of_char code.(ty).(tx) :: !stack
                    else
                        stack := 0 :: !stack
                | '&' ->
                    begin try
                        Scanf.fscanf inchan "%_[^0-9]%d"
                            (fun x -> stack := x :: !stack)
                    with End_of_file ->
                        stack := 0 :: !stack
                    end
                | '~' ->
                    begin try
                        stack := int_of_char (input_char inchan) :: !stack
                    with End_of_file ->
                        stack := -1 :: !stack
                    end
                | _ -> ()
                end;
                exec (advance (x,y))

        and exec_stringmode (x,y) =
            let ch = code.(y).(x) in
            if ch = '"' then
                exec (advance (x,y))
            else begin
                stack := int_of_char ch :: !stack;
                exec_stringmode (advance (x,y))
            end
        in

        Random.self_init ();
        exec (0,0)
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process code buf =
        let build_line row =
            Array.fold_right
                (fun x y ->
                    if x = ' ' && y = "" then y else String.make 1 x ^ y)
                row "" in
        let codestr =
            Array.fold_right
                (fun x y -> if x = "" && y = "" then y else x ^ "\n" ^ y)
                (Array.map build_line code) "" in
        Buffer.add_string buf codestr
end

