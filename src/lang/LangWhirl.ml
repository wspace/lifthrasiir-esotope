(* This is a part of Esotope. See README for more information. *)

(***************************************************************************
 LangWhirl

 This module implements the Whirl programming language, designed by
 Sean Heber in 2004. It uses only two letters (0 and 1) which select and
 execute one of 24 actual commands arranged in the two "ring"s. There are
 jump and branch commands, but they refer to the letter offset (in contrast
 to the command offset) so making loops and complex program is a non-trivial
 task.
***************************************************************************)

(**************************************************************************)
(* The kind. *)

(* An array of a pair of the numbers of successive runs of ones and zeroes,
 * in that order. *)
type t = (int * int) list
let kind = object
    inherit [t] EsotopeCommon.kind
    method name = "whirl"
    method aliases = [".wrl"]
end

(**************************************************************************)
(* The code reader. *)

let reader = object
    inherit [t] EsotopeCommon.reader kind

    method process stream =
        let rec parse_ones x acc =
            match StreamUtil.try_next stream with
            | Some '0' -> parse_zeroes x 1 acc
            | Some '1' -> parse_ones (x+1) acc
            | Some _ -> parse_ones x acc
            | None -> List.rev ((x,0) :: acc)
        and parse_zeroes x y acc =
            match StreamUtil.try_next stream with
            | Some '0' -> parse_zeroes x (y+1) acc
            | Some '1' -> parse_ones 1 ((x,y) :: acc)
            | Some _ -> parse_zeroes x y acc
            | None -> List.rev ((x,y) :: acc)
        in parse_ones 0 []
end

(**************************************************************************)
(* The interpreter. *)

let interpreter = object
    inherit [t] TextIO.interpreter kind

    method process code io =
        let noffsets = List.length code in
        let offsets = Array.make (noffsets+1) (-1, []) in
        let rec init_offsets i off code =
            offsets.(i) <- (off, code);
            match code with
            | (x,y)::t -> init_offsets (i+1) (off+x+y) t
            | [] -> ()
        in init_offsets 0 0 code;

        let seek off =
            let seek_to_entry i =
                (* precondition: offsets[i] <= off < offsets[i+1] *)
                match offsets.(i) with
                | (base, (x,y)::t) ->
                    let delta = off - base in
                    (if delta < x then (x-delta,y) else (0,x+y-delta))::t
                | (_, []) -> []
            in
            let rec recur lo hi =
                (* invariant: offsets[lo] <= off < offsets[hi] *)
                let mid = (lo + hi) lsr 1 in
                if lo == mid then (* i.e. hi - lo is 0 or 1 *)
                    seek_to_entry mid
                else if fst offsets.(mid) > off then
                    recur lo mid
                else
                    recur mid hi
            in recur 0 noffsets
        in

        let opsv = ref 0 in
        let mathv = ref 0 in
        let mem = Tape.create 4096 0 in

        let rec exec_zeroes side (v,w) (vr,wr) off next = function
            | 0 -> exec side (v,w) (vr,wr) off next
            | 1 -> exec side (v,w) (12-vr,wr) (off+1) next
            | y ->
                let continue () =
                    exec_zeroes (not side) (w,v) (wr,vr) (off+2) next (y-2) in
                if side then begin (* math ring *)
                    match v with
                    |  0 -> continue ()
                    |  1 -> mathv := Tape.get 0 mem; continue ()
                    |  2 -> Tape.set 0 !mathv mem; continue ()
                    |  3 -> mathv := !mathv + Tape.get 0 mem; continue ()
                    |  4 -> mathv := !mathv * Tape.get 0 mem; continue ()
                    |  5 -> mathv := !mathv / Tape.get 0 mem; continue ()
                    |  6 -> mathv := 0; continue ()
                    |  7 ->
                        mathv := if !mathv < Tape.get 0 mem then 1 else 0;
                        continue ()
                    |  8 ->
                        mathv := if !mathv > Tape.get 0 mem then 1 else 0;
                        continue ()
                    |  9 ->
                        mathv := if !mathv == Tape.get 0 mem then 1 else 0;
                        continue ()
                    | 10 -> mathv := if !mathv == 0 then 1 else 0; continue ()
                    | 11 -> mathv := -(!mathv); continue ()
                    |  _ -> failwith "impossible"
                end else begin (* ops ring *)
                    match v with
                    |  0 -> continue ()
                    |  1 -> ()
                    |  2 -> opsv := 1; continue ()
                    |  3 -> opsv := 0; continue ()
                    |  4 -> opsv := Tape.get 0 mem; continue ()
                    |  5 -> Tape.set 0 !opsv mem; continue ()
                    |  6 ->
                        (* when the 0 instruction triggers an operation, that
                         * operation takes place at *that* position and the
                         * program pointer advances (except for the jump). *)
                        let off' = off + !opsv + 1 in
                        exec (not side) (w,v) (wr,vr) off' (seek off')
                    |  7 -> Tape.offset !opsv mem; continue ()
                    |  8 ->
                        opsv :=
                            if !mathv != 0 && Tape.get 0 mem != 0 then 1 else 0;
                        continue ()
                    |  9 -> 
                        if Tape.get 0 mem == 0 then
                            continue ()
                        else
                            let off' = off + !opsv + 1 in
                            exec (not side) (w,v) (wr,vr) off' (seek off')
                    | 10 ->
                        if !opsv == 0 then begin
                            match io#get_int None with
                            | Some v -> Tape.set 0 v mem
                            | None -> ()
                        end else begin
                            io#put_int (Tape.get 0 mem); io#flush_out ()
                        end;
                        continue ()
                    | 11 ->
                        if !opsv == 0 then begin
                            match io#get_code None with
                            | Some v -> Tape.set 0 v mem
                            | None -> ()
                        end else begin
                            io#put_code (Tape.get 0 mem); io#flush_out ()
                        end;
                        continue ()
                    |  _ -> failwith "impossible"
                end
        and exec side (v,w) (vr,wr) off = function
            | (x,y)::t ->
                let v' = (v + vr * x) mod 12 in
                exec_zeroes side (v',w) (vr,wr) (off+x) t y
            | [] -> ()
        in exec false (0,0) (1,1) 0 code
end

(**************************************************************************)
(* The code writer. *)

let writer = object
    inherit [t] EsotopeCommon.writer kind

    method process code buf =
        let emit (x,y) =
            Buffer.add_string buf (String.make x '1' ^ String.make y '0') in
        List.iter emit code
end

