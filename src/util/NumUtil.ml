(* This is a part of Esotope. See README for more information. *)

let gcdex x y =
    let rec loop x y a b c d =
        if x == 0 then
            (a, b, y)
        else
            let (q,r) = (y / x, y mod x) in
            loop r x c d (a-c*q) (b-d*q)
    in loop x y 0 1 1 0

