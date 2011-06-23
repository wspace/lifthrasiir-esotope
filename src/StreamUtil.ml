(* This is a part of Esotope. See README for more information. *)

let try_next stream =
    match Stream.peek stream with
    | Some v -> Stream.junk stream; Some v
    | None -> None

