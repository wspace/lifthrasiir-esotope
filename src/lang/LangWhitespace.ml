(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangWhitespace

 This module implements the Whitespace programming language, designed by
 Edwin Brady and Chris Morris in 2003. It is a simple stack-based imperative
 language, but it is notable for using only whitespaces and ignore
 everything else. Despite of its release on April 1, April Fools' Day,
 it is still a full-featured language which is Turing-complete.
***************************************************************************)

type 'label node =
    | Push of Big_int.big_int  (* -- n *)
    | Pick of int * int        (* a...a b...b -- a...a b...b a...a *)
    | Roll of int * int        (* a...a b...b -- b...b a...a *)
    | Discard of int * int     (* a...a b...b -- b...b *)
    | Add                      (* a b -- a+b *)
    | Subtract                 (* a b -- a-b *)
    | Multiply                 (* a b -- a*b *)
    | Quotient                 (* a b -- a//b *)
    | Modulo                   (* a b -- a%b *)
    | Store                    (* k v -- (and heap[k]<-v) *)
    | Retrieve                 (* k -- v *)
    | Label of 'label          (* essentially nop during the execution *)
    | Call of 'label
    | Jump of 'label
    | JumpIfZero of 'label
    | JumpIfNeg of 'label
    | Return
    | Exit
    | OutputChar               (* c -- *)
    | OutputNum                (* v -- *)
    | InputChar                (* k -- (and heap[k]<-input) *)
    | InputNum                 (* k -- (and heap[k]<-input) *)

let string_of_node string_of_label node =
    match node with
    | Push n -> "push " ^ Big_int.string_of_big_int n
    | Pick (1,0) -> "dup"
    | Pick (1,m) -> Printf.sprintf "copy %d" m
    | Pick (n,m) -> Printf.sprintf "pick %d %d" n m
    | Roll (1,1) -> "swap"
    | Roll (n,m) -> Printf.sprintf "roll %d %d" n m
    | Discard (1,0) -> "pop"
    | Discard (n,1) -> Printf.sprintf "slide %d" n
    | Discard (n,m) -> Printf.sprintf "discard %d %d" n m
    | Add -> "add"
    | Subtract -> "sub"
    | Multiply -> "mul"
    | Quotient -> "div"
    | Modulo -> "mod"
    | Store -> "store"
    | Retrieve -> "retrieve"
    | Label l -> string_of_label l ^ ":"
    | Call l -> "call " ^ string_of_label l
    | Jump l -> "jmp " ^ string_of_label l
    | JumpIfZero l -> "jz " ^ string_of_label l
    | JumpIfNeg l -> "jn " ^ string_of_label l
    | Return -> "ret"
    | Exit -> "halt"
    | OutputChar -> "putchar"
    | OutputNum -> "putint"
    | InputChar -> "getchar"
    | InputNum -> "getint"

(**************************************************************************)
(* The kind. *)

type t = string node list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "whitespace"
    method aliases = ["ws"; ".ws"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let handle_copy v =
            let v' = Big_int.int_of_big_int v in
            if v' >= 0 then Pick (1,v')
            else failwith "invalid COPY parameter" in
        let handle_slide v =
            let v' = Big_int.int_of_big_int v in
            if v' >= 0 then Discard (v',1)
            else failwith "invalid SLIDE parameter" in

        let huffman_tree =
            `B (`B (`LNum (fun v -> Push v),              (* SP SP *)
                    `B (`LNum handle_copy,                (* SP HT SP *)
                        `X,
                        `LNum handle_slide),              (* SP HT LF *)
                    `B (`L (Pick (1,0)),                  (* SP LF SP *)
                        `L (Roll (1,1)),                  (* SP LF HT *)
                        `L (Discard (1,0)))),             (* SP LF LF *)
                `B (`B (`B (`L Add,                       (* HT SP SP SP *)
                            `L Subtract,                  (* HT SP SP HT *)
                            `L Multiply),                 (* HT SP SP LF *)
                        `B (`L Quotient,                  (* HT SP HT SP *)
                            `L Modulo,                    (* HT SP HT HT *)
                            `X),
                        `X),
                    `B (`L Store,                         (* HT HT SP *)
                        `L Retrieve,                      (* HT HT HT *)
                        `X),
                    `B (`B (`L OutputChar,                (* HT LF SP SP *)
                            `L OutputNum,                 (* HT LF SP HT *)
                            `X),
                        `B (`L InputChar,                 (* HT LF HT SP *)
                            `L InputNum,                  (* HT LF HT HT *)
                            `X),
                        `X)),
                `B (`B (`LLabel (fun l -> Label l),       (* LF SP SP *)
                        `LLabel (fun l -> Call l),        (* LF SP HT *)
                        `LLabel (fun l -> Jump l)),       (* LF SP LF *)
                    `B (`LLabel (fun l -> JumpIfZero l),  (* LF HT SP *)
                        `LLabel (fun l -> JumpIfNeg l),   (* LF HT HT *)
                        `L Return),                       (* LF HT LF *)
                    `B (`X,
                        `X,
                        `L Exit)))                        (* LF LF LF *)
        in

        let rec read () =
            match StreamUtil.try_next stream with
            | Some ((' '|'\t'|'\n') as ch) -> Some ch
            | Some _ -> read ()
            | None -> None
        in

        let state = ref (`L Exit) in (* to be immediately overwritten *)
        let rec parse_label f label acc =
            match read () with
            | Some ((' '|'\t') as ch) ->
                parse_label f (label ^ String.make 1 ch) acc
            | Some '\n' -> parse (f label :: acc)
            | Some _ -> failwith "unexpected"
            | None -> failwith "EOF during a label"
        and parse_number f sign num acc =
            match read () with
            | Some ' ' ->
                let num2 = Big_int.mult_int_big_int 2 num in
                parse_number f sign num2 acc
            | Some '\t' ->
                let num2 = Big_int.mult_int_big_int 2 num in
                parse_number f sign (Big_int.add_int_big_int 1 num2) acc
            | Some '\n' ->
                if sign then
                    parse (f (Big_int.minus_big_int num) :: acc)
                else
                    parse (f num :: acc)
            | Some _ -> failwith "unexpected"
            | None -> failwith "EOF within a number"
        and parse_number_sign f acc =
            match read () with
            | Some ' ' -> parse_number f false Big_int.zero_big_int acc
            | Some '\t' -> parse_number f true Big_int.zero_big_int acc
            | Some '\n' -> parse (f Big_int.zero_big_int :: acc)
            | Some _ -> failwith "unexpected"
            | None -> failwith "EOF within a number"
        and parse acc =
            match read () with
            | Some ch ->
                begin match !state with
                | `B _ -> ()
                | _ -> state := huffman_tree
                end;
                begin match (ch, !state) with
                | (' ', `B (x,_,_)) -> state := x
                | ('\t', `B (_,x,_)) -> state := x
                | ('\n', `B (_,_,x)) -> state := x
                | _ -> failwith "unexpected"
                end;
                begin match !state with
                | `B _ -> parse acc
                | `X -> failwith "unsupported instruction"
                | `L v -> parse (v :: acc)
                | `LLabel f -> parse_label f "" acc
                | `LNum f -> parse_number_sign f acc
                end
            | None ->
                match !state with
                | `L _ | `LLabel _ | `LNum _ -> List.rev acc
                | `X -> failwith "unexpected"
                | `B _ -> (* the tree is not completely consumed *)
                    failwith "the end of file in the middle of instruction"
        in

        parse []
end

(**************************************************************************)
(* The interpreter. *)

exception Stack_underflow
exception Call_stack_underflow
exception Invalid_heap_address
exception Invalid_label

module BigHashtbl = Hashtbl.Make(struct
    type t = Big_int.big_int
    let equal = Big_int.eq_big_int
    let hash v = Hashtbl.hash (Big_int.string_of_big_int v)
end)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process code io =
        (* pre-scanning *)
        let labels = Hashtbl.create 64 in
        let rec scan = function
            | [] -> ()
            | Label l :: t ->
                if Hashtbl.mem labels l then
                    failwith "duplicate label"
                else
                    Hashtbl.add labels l t; scan t
            | _ :: t -> scan t
        in scan code;

        let heap = BigHashtbl.create 8 in
        let subs = ref [] in

        let rec exec code stack =
            let try1 f =
                match stack with
                | a::stack' -> f a stack'
                | _ -> raise Stack_underflow
            in
            let try2 f =
                match stack with
                | b::a::stack' -> f a b stack'
                | _ -> raise Stack_underflow
            in
            let label l =
                try Hashtbl.find labels l with Not_found -> raise Invalid_label
            in

            match code with
            | Push v :: t -> exec t (v::stack)
            | Pick (n,m) :: t ->
                exec t (ListUtil.take n (ListUtil.drop m stack) @ stack)
            | Roll (n,m) :: t ->
                let head, tail = ListUtil.partition m stack in
                let head2, tail2 = ListUtil.partition n tail in
                exec t (head2 @ head @ tail2)
            | Discard (n,m) :: t ->
                let head, tail = ListUtil.partition m stack in
                exec t (head @ ListUtil.drop n tail)
            | Add :: t ->
                try2 (fun a b s -> exec t (Big_int.add_big_int a b :: s))
            | Subtract :: t ->
                try2 (fun a b s -> exec t (Big_int.sub_big_int a b :: s))
            | Multiply :: t ->
                try2 (fun a b s -> exec t (Big_int.mult_big_int a b :: s))
            | Quotient :: t ->
                try2 (fun a b s -> exec t (Big_int.div_big_int a b :: s))
            | Modulo :: t ->
                try2 (fun a b s -> exec t (Big_int.mod_big_int a b :: s))
            | Store :: t ->
                try2 (fun k v s -> BigHashtbl.replace heap k v; exec t s)
            | Retrieve :: t ->
                let run k s =
                    if BigHashtbl.mem heap k then
                        exec t (BigHashtbl.find heap k :: s)
                    else
                        raise Invalid_heap_address
                in try1 run
            | Label _ :: t -> exec t stack
            | Call l :: t -> subs := t :: !subs; exec (label l) stack
            | Jump l :: t -> exec (label l) stack
            | JumpIfZero l :: t ->
                let t' = label l in
                let run k s =
                    let sign = Big_int.sign_big_int k in
                    exec (if sign == 0 then t' else t) s
                in try1 run
            | JumpIfNeg l :: t ->
                let t' = label l in
                let run k s =
                    let sign = Big_int.sign_big_int k in
                    exec (if sign < 0 then t' else t) s
                in try1 run
            | Return :: t ->
                begin match !subs with
                | [] -> raise Call_stack_underflow
                | callee :: subs' -> subs := subs'; exec callee stack
                end
            | Exit :: _ -> ()
            | OutputChar :: t ->
                let run c s =
                    io#put_code (Big_int.int_of_big_int c);
                    io#flush_out ();
                    exec t s
                in try1 run
            | OutputNum :: t ->
                let run v s =
                    io#put_big_int v;
                    io#flush_out ();
                    exec t s
                in try1 run
            | InputChar :: t ->
                let run k s =
                    begin match io#get_code None with
                    | Some v -> 
                        BigHashtbl.replace heap k (Big_int.big_int_of_int v)
                    | None -> ()
                    end;
                    exec t s
                in try1 run
            | InputNum :: t ->
                let run k s =
                    begin match io#get_big_int None with
                    | Some v -> BigHashtbl.replace heap k v
                    | None -> ()
                    end;
                    exec t s
                in try1 run
            | [] -> failwith "unexpected end of program"
        in

        exec code []
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process nodes buf =
        let number n =
            let rec recur v acc =
                if Big_int.sign_big_int v > 0 then
                    let v', lsb =
                        Big_int.quomod_big_int v (Big_int.big_int_of_int 2) in
                    if Big_int.sign_big_int lsb > 0 then
                        recur v' ("\t" ^ acc)
                    else
                        recur v' (" " ^ acc)
                else
                    acc in
            let sign = Big_int.sign_big_int n in
            if sign > 0 then " " ^ recur n "" ^ "\n"
            else if sign < 0 then "\t" ^ recur (Big_int.minus_big_int n) "" ^ "\n"
            else "\n"
        in

        (* TODO filter out non-whitespace labels *)
        let label l = l ^ "\n" in

        let rec emit = function
            | Push v -> Buffer.add_string buf ("  " ^ number v)
            | Pick (0,_) -> ()
            | Pick (1,0) -> Buffer.add_string buf " \n "
            | Pick (n,m) -> (* n copies of COPY *)
                let code = " \t " ^ number (Big_int.big_int_of_int (n+m-1)) in
                for i = 1 to n do Buffer.add_string buf code done
            | Roll (0,_) -> ()
            | Roll (_,0) -> ()
            | Roll (1,1) -> Buffer.add_string buf " \n\t"
            | Roll (n,m) -> failwith "not directly representable"
            | Discard (0,0) -> ()
            | Discard (1,0) -> Buffer.add_string buf " \n\n"
            | Discard (2,0) -> Buffer.add_string buf " \n\n \n\n"
            | Discard (n,0) -> (* SLIDE followed by DISCARD *)
                let code = " \t\n" ^ number (Big_int.big_int_of_int (n-1)) in
                Buffer.add_string buf (code ^ " \n\n")
            | Discard (n,1) ->
                let code = " \t\n" ^ number (Big_int.big_int_of_int n) in
                Buffer.add_string buf code
            | Discard (n,m) -> failwith "not directly representable"
            | Add -> Buffer.add_string buf "\t   "
            | Subtract -> Buffer.add_string buf "\t  \t"
            | Multiply -> Buffer.add_string buf "\t  \n"
            | Quotient -> Buffer.add_string buf "\t \t "
            | Modulo -> Buffer.add_string buf "\t \t\t"
            | Store -> Buffer.add_string buf "\t\t "
            | Retrieve -> Buffer.add_string buf "\t\t\t"
            | Label l -> Buffer.add_string buf ("\n  " ^ label l)
            | Call l -> Buffer.add_string buf ("\n \t" ^ label l)
            | Jump l -> Buffer.add_string buf ("\n \n" ^ label l)
            | JumpIfZero l -> Buffer.add_string buf ("\n\t " ^ label l)
            | JumpIfNeg l -> Buffer.add_string buf ("\n\t\t" ^ label l)
            | Return -> Buffer.add_string buf "\n\t\n"
            | Exit -> Buffer.add_string buf "\n\n\n"
            | OutputChar -> Buffer.add_string buf "\t\n  "
            | OutputNum -> Buffer.add_string buf "\t\n \t"
            | InputChar -> Buffer.add_string buf "\t\n\t "
            | InputNum -> Buffer.add_string buf "\t\n\t\t"
        in List.iter emit nodes
end

