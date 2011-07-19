(* This is a part of Esotope. See README for more information. *)

type 'sid node =
    | Nop
    | Push of 'sid * int32              (* a<42 or 42>a *)
    | Move of 'sid * 'sid               (* a<b or b>a *)
    | Add of 'sid * int32               (* a+42 *)
    | AddPop of 'sid * 'sid             (* a+b *)
    | Sub of 'sid * int32               (* a-42 *)
    | SubPop of 'sid * 'sid             (* a-b *)
    | ClearIfZero of 'sid               (* a? *)
    | While of 'sid * 'sid node list    (* (a ...) *)

type t = int node list

