(* This is a part of Esotope. See README for more information. *)

type t =
    | K                   (* k *)
    | K1 of t             (* `kx *)
    | S                   (* s *)
    | S1 of t             (* `sx *)
    | S2 of t * t         (* ``sxy *)
    | I                   (* i *)
    | Void                (* v *)
    | Delay               (* d *)
    | Delay1 of t         (* `dx *)
    | Callcc              (* c *)
    | Print of char       (* .x *)
    | PrintNewline        (* r *)
    | Exit                (* e *)
    | Read                (* @ *)
    | CheckRead of char   (* ?x *)
    | Reprint             (* | *)
    | Extern of ((t -> t) -> t -> t)
                          (* external function: called with the current
                           * continuation and an argument. *)
    | App of t * t        (* `xy *)

