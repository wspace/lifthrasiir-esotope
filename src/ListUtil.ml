(* This is a part of Esotope. See README for more information. *)

let drop k l =
    let rec recur = function
        | (0,l) -> l
        | (k,[]) -> invalid_arg "drop: list too short"
        | (k,h::t) -> recur (k-1,t)
    in recur (k,l)

let partition k l =
    let rec recur = function
        | (0,l,acc) -> (List.rev acc, l)
        | (k,[],acc) -> invalid_arg "partition: list too short"
        | (k,h::t,acc) -> recur (k-1,t,h::acc)
    in recur (k,l,[])

let take k l =
    let l', _ = partition k l in l'

