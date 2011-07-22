(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangFalse

 This module implements the FALSE programming language, designed by
 Wouter van Oortmerssen in 1993. It is arguably the first "modern" esoteric
 programming language, in such that it generated a stream of other esoteric
 languages like Brainfuck and Befunge.

 It is a stack-based programming language which strikingly similar to Forth,
 though it differs from Forth due to its support of anonymous functions
 (instead of compile-time word rewriting in Forth). It was also highly
 useful due to its ability to embed a 68000 assembly (number`).

 Some compatibility note:
 - Esotope supports three forms of the flush (ß/B) and pick (ø/O) commands:
   ISO-8859-1 version, UTF-8 version and an ASCII version (which is used
   in the portable interpreter).
 - The character input (^) pushes -1 on EOF.
 - Intrinsic (number`) is unsupported; Esotope instead introduces new
   built-in functions ("name"`) for the future extension. They can be called
   or stored like other functions (e.g. 4 5"plus"`!).

 Supported built-in functions:
 - "allocmem"` (size -- mem) returns an array. The array can be compared to
   0 for checking the success or failure.
 - "freemem"` (mem size -- ) unallocates the array. In the current
   implementation this is a no-op except for the sanity check.
***************************************************************************)

open LangFalse_ast

type applicable =
    | Code of node list
    | Builtin of string

type value =
    | Num of int32
    | Ref of value ref
    | ArrayRef of int32 * value array
    | Applicable of applicable

(**************************************************************************)
(* The kind. *)

let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "false"
    method aliases = [".f"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.parsing_reader kind
    method process = LangFalse_parser.main LangFalse_lexer.token
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process nodes io =
        let vars = Array.init 26 (fun _ -> ref (Num 0l)) in

        let rec exec a stack =
            let exec_node = function
                | Nop, st -> st
                | Comment _, st -> st
                | PushCode nodes, st -> Applicable (Code nodes) :: st
                | PushVarRef i, st -> Ref vars.(i) :: st
                | Push v, st -> Num v :: st
                | PushChar c, st -> Num (Int32.of_int (int_of_char c)) :: st
                | PushBuiltin s, st -> Applicable (Builtin s) :: st
                | Intrinsic _, _ -> failwith "intrinsics (num`) not supported"
                | Store, Ref r :: v :: st -> r := v; st
                | Store, ArrayRef (off,arr) :: v :: st ->
                    arr.(Int32.to_int off) <- v; st
                | Store, _ :: _ :: _ -> failwith "non-reference store"
                | Load, Ref r :: st -> !r :: st
                | Load, ArrayRef (off,arr) :: st ->
                    arr.(Int32.to_int off) :: st
                | Load, _ :: _ -> failwith "non-reference load"
                | Apply, Applicable a :: st -> exec a st
                | Apply, _ :: _ -> failwith "non-function apply"
                | Add, Num b :: Num a :: st -> Num (Int32.add a b) :: st
                | Add, ArrayRef (off,arr) :: Num a :: st ->
                    ArrayRef (Int32.add a off, arr) :: st
                | Add, Num b :: ArrayRef (off,arr) :: st ->
                    ArrayRef (Int32.add off b, arr) :: st
                | Add, _ :: _ :: _ -> failwith "non-number addition"
                | Subtract, Num b :: Num a :: st -> Num (Int32.sub a b) :: st
                | Subtract, Num b :: ArrayRef (off,arr) :: st ->
                    ArrayRef (Int32.sub off b, arr) :: st
                | Subtract, ArrayRef (off',arr') :: ArrayRef (off,arr) :: st ->
                    if arr != arr' then
                        failwith "different array base for subtraction";
                    Num (Int32.sub off off') :: st
                | Subtract, _ :: _ :: _ -> failwith "non-number subtraction"
                | Multiply, Num b :: Num a :: st -> Num (Int32.mul a b) :: st
                | Multiply, _ :: _ :: _ -> failwith "non-number multiplication"
                | Divide, Num b :: Num a :: st -> Num (Int32.div a b) :: st
                | Divide, _ :: _ :: _ -> failwith "non-number division"
                | Negate, Num a :: st -> Num (Int32.neg a) :: st
                | Negate, _ :: _ -> failwith "non-number negation"
                | Equals, b :: a :: st ->
                    let eq = match a, b with
                        | Num a', Num b' -> a' = b'
                        | Ref a', Ref b' -> a' == b'
                        | ArrayRef (off1,arr1), ArrayRef (off2,arr2) ->
                            arr1 == arr2 && off1 = off2
                        | Applicable (Code a'), Applicable (Code b') ->
                            a' == b'
                        | Applicable (Builtin a'), Applicable (Builtin b') ->
                            a' = b'
                        | _, _ -> false
                    in Num (if eq then -1l else 0l) :: st
                | Greater, Num b :: Num a :: st ->
                    Num (if a > b then -1l else 0l) :: st
                | Greater, _ :: _ :: _ -> failwith "non-number comparison"
                | And, Num b :: Num a :: st -> Num (Int32.logand a b) :: st
                | And, _ :: _ :: _ -> failwith "non-number bitwise AND"
                | Or, Num b :: Num a :: st -> Num (Int32.logor a b) :: st
                | Or, _ :: _ :: _ -> failwith "non-number bitwise OR"
                | Not, Num a :: st -> Num (Int32.lognot a) :: st
                | Not, _ :: _ -> failwith "non-number bitwise NOT"
                | Dup, a :: st -> a :: a :: st
                | Pop, _ :: st -> st
                | Swap, b :: a :: st -> a :: b :: st
                | Rot, c :: b :: a :: st -> a :: c :: b :: st
                | Pick, Num k :: st ->
                    if k < 0l then failwith "negative offset in pick";
                    if k > Int32.of_int max_int then failwith "stack underflow";
                    begin match ListUtil.drop (Int32.to_int k) st with
                    | h :: _ -> h :: st
                    | [] -> failwith "stack underflow"
                    end
                | If, Applicable a :: Num c :: st ->
                    if c <> 0l then exec a st else st
                | If, _ :: Num _ :: _ ->
                    failwith "non-lambda for conditional"
                | If, Applicable _ :: _ :: _ ->
                    failwith "non-number for conditional"
                | While, Applicable a :: Applicable a' :: st ->
                    let rec loop st =
                        match exec a' st with
                        | Num c :: st' ->
                            if c <> 0l then loop (exec a st') else st'
                        | _ :: st' -> failwith "non-number for loop"
                        | [] -> failwith "stack underflow"
                    in loop st
                | While, _ :: _ :: _ -> failwith "non-lambda for loop"
                | OutputNum, Num n :: st -> io#put_int (Int32.to_int n); st
                    (* TODO above overflows when n exceeds max_int *)
                | OutputNum, _ :: _ -> failwith "non-number for output"
                | OutputStr s, st -> io#put_str s; st
                | OutputChar, Num n :: st -> io#put_code (Int32.to_int n); st
                | OutputChar, _ :: _ -> failwith "non-number for output"
                | InputChar, st ->
                    let n = match io#get_code None with Some n -> n | _ -> -1
                    in Num (Int32.of_int n) :: st
                | Flush, st -> io#flush_out (); st
                | _, _ -> failwith "stack underflow"
            in

            match a with
            | Code nodes ->
                List.fold_left (fun st n -> exec_node (n,st)) stack nodes
            | Builtin "allocmem" ->
                begin match stack with
                | Num size :: st ->
                    begin try
                        let arr = Array.make (Int32.to_int size) (Num 0l) in
                        ArrayRef (0l,arr) :: st
                    with Invalid_argument _ ->
                        Num 0l :: st (* so that comparison to 0 is possible *)
                    end
                | _ :: _ -> failwith "non-number for allocmem"
                | _ -> failwith "stack underflow"
                end
            | Builtin "freemem" ->
                begin match stack with
                | Num size :: ArrayRef (off,arr) :: st ->
                    if off <> 0l then
                        failwith "invalid array reference for freemem";
                    if Int32.of_int (Array.length arr) <> size then
                        failwith "array size mismatch in freemem";
                    st
                | _ :: ArrayRef _ :: _ -> failwith "non-number for freemem"
                | Num _ :: _ :: _ -> failwith "non-array reference for freemem"
                | _ -> failwith "stack underflow"
                end
            | Builtin _ -> failwith "unrecognized builtin name"

        in ignore (exec (Code nodes) [])
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let putc = Buffer.add_char buf in
        let prev = ref Nop in

        let rec emit node =
            begin match !prev, node with
            | Push _, (Push _ | Intrinsic _) ->
                putc ' '; prev := node (* whitespace needed! *)
            | _, Nop -> ()
            | _, _ -> prev := node
            end;

            match node with
            | Nop -> ()
            | Comment s ->
                if String.contains s '}' then
                    failwith "cannot write a comment containing '}'";
                Printf.bprintf buf "{%s}" s
            | PushCode nodes -> putc '['; List.iter emit nodes; putc ']'
            | PushVarRef i -> putc (char_of_int (int_of_char 'a' + i))
            | Push v -> Printf.bprintf buf "%ld" v
            | PushChar c -> putc '\''; putc c
            | PushBuiltin s ->
                if String.contains s '"' then
                    failwith "cannot write a built-in function containing '\"'";
                Printf.bprintf buf "\"%s\"`" s
            | Intrinsic n -> Printf.bprintf buf "%d`" n
            | Store -> putc ':'
            | Load -> putc ';'
            | Apply -> putc '!'
            | Add -> putc '+'
            | Subtract -> putc '-'
            | Multiply -> putc '*'
            | Divide -> putc '/'
            | Negate -> putc '_'
            | Equals -> putc '='
            | Greater -> putc '>'
            | And -> putc '&'
            | Or -> putc '|'
            | Not -> putc '~'
            | Dup -> putc '$'
            | Pop -> putc '%'
            | Swap -> putc '\\'
            | Rot -> putc '@'
            | Pick -> Buffer.add_string buf "\195\184" (* "ø" *)
            | If -> putc '?'
            | While -> putc '#'
            | OutputNum -> putc '.'
            | OutputStr s ->
                let pattern = Str.regexp_string in
                let s' = Str.global_replace (pattern "\"") "\"'\",\"" s in
                let s'' = Str.global_replace (pattern "\"\"") "" s' in
                Printf.bprintf buf "\"%s\"" s''
            | OutputChar -> putc ','
            | InputChar -> putc '^'
            | Flush -> Buffer.add_string buf "\195\159" (* "ß" *)
        in List.iter emit nodes
end

