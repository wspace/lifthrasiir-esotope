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

open Space

(**************************************************************************)
(* The kind. *)

type t = char Space2D.t
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "befunge93"
    method aliases = [".bf"; ".b93"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        (*
        let nrows = 25 in
        let ncolumns = 80 in
        *)
        let code = Space2D.create (8,8) ' ' in

        let readcell stream =
            match StreamUtil.try_next stream with
            | Some ch -> ch
            | None -> raise End_of_file
        in Space2D.from_char_stream readcell stream code (0,0)
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process code io =
        let code = Space2D.copy code in
        let nrows = 25 in
        let ncolumns = 80 in

        let delta = ref (1,0) in
        let stack = ref [] in

        let get () = match !stack with [] -> 0 | h::t -> stack := t; h in
        let get2 () = let a = get () in let b = get () in (a, b) in
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
            match Space2D.get code (x,y) with
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
                | '.' -> io#put_int (get ()); io#put_char ' '; io#flush_out ()
                | ',' -> io#put_code (get () land 255); io#flush_out ()
                | 'p' ->
                    let ty, tx = get2 () in
                    let v = get () in
                    (* out-of-boundary is not an error, but a nop *)
                    if 0 <= ty && ty < nrows && 0 <= tx && tx < ncolumns then
                        Space2D.set code (tx,ty) (char_of_int (v land 255))
                | 'g' ->
                    let ty, tx = get2 () in
                    if 0 <= ty && ty < nrows && 0 <= tx && tx < ncolumns then
                        stack := int_of_char (Space2D.get code (tx,ty)) :: !stack
                    else
                        stack := 0 :: !stack
                | '&' ->
                    let x =
                        match io#get_nat None with Some x -> x | None -> 0
                    in stack := x :: !stack
                | '~' ->
                    let c =
                        match io#get_code None with Some x -> x | None -> -1
                    in stack := c :: !stack
                | _ -> ()
                end;
                exec (advance (x,y))

        and exec_stringmode (x,y) =
            let ch = Space2D.get code (x,y) in
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
        (* the starting offset is guaranteed to be in the 1st quadrant *)
        let (_,(sx,sy)) = Space2D.bounds code in

        let rec build_line y x acc =
            let x = x-1 in
            let ch = Space2D.get code (x,y) in
            let acc' =
                if ch = ' ' && acc = "" then
                    acc
                else
                    String.make 1 ch ^ acc
            in if x = 0 then acc' else build_line y x acc'
        in

        let rec build y acc =
            let y = y-1 in
            let line = build_line y sx "" in
            let acc' =
                if line = "" && acc = "" then
                    acc
                else
                    line ^ "\n" ^ acc
            in if y = 0 then acc' else build y acc'
        in

        Buffer.add_string buf (build sy "")
end

