(* This is a part of Esotope. See README for more information. *)

type 'a value =
    | Var of 'a
    | MemRef of 'a
    | Number of 'a
    | CodePtr           (* c *)
    | MemPtr            (* p *)
    | InputChar         (* i (rhs only) *)
    | InputNum          (* j (rhs only) *)
    | OutputChar        (* o (lhs only) *)
    | OutputNum         (* q (lhs only) *)
    | RandomRange       (* r (lhs only) *)
    | Random            (* r (rhs only) *)
    | Breakpoint        (* _ (rhs only) *)

(* Exit arguments are only used for the source reconstruction. *)
type 'a node =
    | Subtract of ('a value * 'a value)
    | Exit of ('a value * 'a value)

type t = int node list  (* TODO int32? *)

