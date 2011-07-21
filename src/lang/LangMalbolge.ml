(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangMalbolge

 This module implements the Malbolge programming language, designed by
 Ben Olmstead in 1998. It is designed to be as difficult to program in as
 possible, and still considered so. It is a virtual machine with 3^10 (=
 59,409) 10-trit-long memory with built-in encryption.
 
 Many serious efforts have been made to analyze the language so far and its
 computational complexity is yet to be clearly determined. As an exercise,
 try to write any program which does slightly more thing than printing the
 predetermined output; if you figured out how to do that then it is a major
 feat. No, I'm not joking here: the most complex program written to date is,
 in fact, the 99 Bottles of Beer program appeared in 2005. 

 Any suggestion for transformers from languages other than Text to Malbolge
 is welcomed.
***************************************************************************)

type op =
    | Jump                      (* "i", (C+[C])%94 == 4 *)
    | OutputChar                (* "<", (C+[C])%94 == 5 *)
    | InputChar                 (* "/", (C+[C])%94 == 23 *)
                                (* (follows the official interpreter) *)
    | Rotate                    (* "*", (C+[C])%94 == 39 *)
    | Deref                     (* "j", (C+[C])%94 == 40 *)
    | TritwiseOp                (* "p", (C+[C])%94 == 62 *)
                                (* (often called "crazy" operation) *)
    | Nop                       (* "o", (C+[C])%94 == 68 *)
    | Stop                      (* "v", (C+[C])%94 == 81 *)
    | Unknown                   (* otherwise *)

let to_op = function
    | 4 -> Jump
    | 5 -> OutputChar
    | 23 -> InputChar
    | 39 -> Rotate
    | 40 -> Deref
    | 62 -> TritwiseOp
    | 68 -> Nop
    | 81 -> Stop
    | _ -> Unknown

let of_op = function
    | Jump -> 4
    | OutputChar -> 5
    | InputChar -> 23
    | Rotate -> 39
    | Deref -> 40
    | TritwiseOp -> 62
    | Nop -> 68
    | Stop -> 81
    | Unknown -> failwith "undefined"

let permute = function                     |  33 ->  53 |  34 -> 122
    |  35 ->  93 |  36 ->  38 |  37 -> 103 |  38 -> 113 |  39 -> 116
    |  40 -> 121 |  41 -> 102 |  42 -> 114 |  43 ->  36 |  44 ->  40
    |  45 -> 119 |  46 -> 101 |  47 ->  52 |  48 -> 123 |  49 ->  87
    |  50 ->  80 |  51 ->  41 |  52 ->  72 |  53 ->  45 |  54 ->  90
    |  55 -> 110 |  56 ->  44 |  57 ->  91 |  58 ->  37 |  59 ->  92
    |  60 ->  51 |  61 -> 100 |  62 ->  76 |  63 ->  43 |  64 ->  81
    |  65 ->  59 |  66 ->  62 |  67 ->  85 |  68 ->  33 |  69 -> 112
    |  70 ->  74 |  71 ->  83 |  72 ->  55 |  73 ->  50 |  74 ->  70
    |  75 -> 104 |  76 ->  79 |  77 ->  65 |  78 ->  49 |  79 ->  67
    |  80 ->  66 |  81 ->  54 |  82 -> 118 |  83 ->  94 |  84 ->  61
    |  85 ->  73 |  86 ->  95 |  87 ->  48 |  88 ->  47 |  89 ->  56
    |  90 -> 124 |  91 -> 106 |  92 -> 115 |  93 ->  98 |  94 ->  57
    |  95 -> 109 |  96 ->  60 |  97 ->  46 |  98 ->  84 |  99 ->  86
    | 100 ->  97 | 101 ->  99 | 102 ->  96 | 103 -> 117 | 104 ->  89
    | 105 ->  42 | 106 ->  77 | 107 ->  75 | 108 ->  39 | 109 ->  88
    | 110 -> 126 | 111 -> 120 | 112 ->  68 | 113 -> 108 | 114 -> 125
    | 115 ->  82 | 116 ->  69 | 117 -> 111 | 118 -> 107 | 119 ->  78
    | 120 ->  58 | 121 ->  35 | 122 ->  63 | 123 ->  71 | 124 ->  34
    | 125 -> 105 | 126 ->  64
    | _ -> failwith "undefined instruction"

let tritwise0 =
    [|[| 4; 3; 3; 1; 0; 0; 1; 0; 0 |];
      [| 4; 3; 5; 1; 0; 2; 1; 0; 2 |];
      [| 5; 5; 4; 2; 2; 1; 2; 2; 1 |];
      [| 4; 3; 3; 1; 0; 0; 7; 6; 6 |];
      [| 4; 3; 5; 1; 0; 2; 7; 6; 8 |];
      [| 5; 5; 4; 2; 2; 1; 8; 8; 7 |];
      [| 7; 6; 6; 7; 6; 6; 4; 3; 3 |];
      [| 7; 6; 8; 7; 6; 8; 4; 3; 5 |];
      [| 8; 8; 7; 8; 8; 7; 5; 5; 4 |]|]

let tritwise1 = Array.map (fun a -> Array.map (fun v -> v * 9) a) tritwise0
let tritwise2 = Array.map (fun a -> Array.map (fun v -> v * 9) a) tritwise1
let tritwise3 = Array.map (fun a -> Array.map (fun v -> v * 9) a) tritwise2
let tritwise4 = Array.map (fun a -> Array.map (fun v -> v * 9) a) tritwise3

let tritwise_op a b =
    let a, b, c = a / 9, b / 9,     tritwise0.(b mod 9).(a mod 9) in
    let a, b, c = a / 9, b / 9, c + tritwise1.(b mod 9).(a mod 9) in
    let a, b, c = a / 9, b / 9, c + tritwise2.(b mod 9).(a mod 9) in
    let a, b, c = a / 9, b / 9, c + tritwise3.(b mod 9).(a mod 9) in
                                c + tritwise4.(b      ).(a      )

(**************************************************************************)
(* The kind. *)

type t = string
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "malbolge"
    method aliases = [".mal"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let buf = Buffer.create 64 in
        let rec read idx =
            match StreamUtil.try_next stream with
            | Some (' '|'\t'|'\r'|'\n') -> read idx
            | Some ('\033'..'\126' as c) ->
                if idx >= 59049 then
                    failwith "code too big";
                if to_op ((int_of_char c + idx) mod 94) = Unknown then
                    failwith "invalid instruction in the code";
                Buffer.add_char buf c;
                read (idx + 1)
            | Some _ -> failwith "invalid character in the code"
            | None ->
                if idx < 2 then
                    (* the remainder of memory space is undefined if the
                     * program is shorter than two words; Esotope rejects
                     * such program for compatibility. *)
                    failwith "code too short";
                Buffer.contents buf
        in read 0
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process code io =
        let mem = Array.make 59049 0 in

        let rec fill_mem i prev pprev =
            if i < 59049 then begin
                let cur = tritwise_op prev pprev in
                mem.(i) <- cur;
                fill_mem (i + 1) cur prev
            end
        and init_mem i =
            if i < String.length code then begin
                mem.(i) <- int_of_char code.[i];
                init_mem (i + 1)
            end else
                fill_mem i mem.(i-1) mem.(i-2)
        in init_mem 0;

        let rec exec a c d =
            let memc = mem.(c) in
            if memc < 33 || memc > 126 then
                failwith "invalid instruction in the memory"
            else
                match to_op ((c + memc) mod 94) with
                | Jump ->
                    advance_and_exec a mem.(d) d
                | OutputChar ->
                    io#put_char (char_of_int (a land 255));
                    io#flush_out ();
                    advance_and_exec a c d
                | InputChar ->
                    let a' = match io#get_char None with
                             | Some c -> int_of_char c
                             | None -> 59048 (* 2222222222t *) in
                    advance_and_exec a' c d
                | Rotate ->
                    let memd = mem.(d) in
                    let memd' = memd / 3 + (memd mod 3) * 19683 in
                    mem.(d) <- memd';
                    advance_and_exec memd' c d
                | Deref ->
                    advance_and_exec a c mem.(d)
                | TritwiseOp ->
                    let memd' = tritwise_op a mem.(d) in
                    mem.(d) <- memd';
                    advance_and_exec memd' c d
                | Stop -> ()
                | Nop | Unknown ->
                    advance_and_exec a c d
        and advance_and_exec a c d =
            mem.(c) <- permute mem.(c);
            exec a (if c < 59048 then c + 1 else 0)
                   (if d < 59048 then d + 1 else 0)
        in exec 0 0 0
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process code buf =
        Buffer.add_string buf code
end

