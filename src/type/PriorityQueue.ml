(* This is a part of Esotope. See README for more information. *)

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

