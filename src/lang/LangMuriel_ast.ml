(* This is a part of Esotope. See README for more information. *)

type num_expr =
    | Num of int                                (* 123 *)
    | NumVar of int                             (* x *)
    | Add of num_expr * num_expr                (* x + y *)
    | Sub of num_expr * num_expr                (* x - y *)
    | Mult of num_expr * num_expr               (* x * y *)
    | Equal of num_expr * num_expr              (* x = y *)
    | Greater of num_expr * num_expr            (* x > y *)
    | Less of num_expr * num_expr               (* x < y *)
    | Neg of num_expr                           (* -x *)
    | ParseNum of str_expr                      (* #X *)
    | Len of str_expr                           (* &X *)
and str_expr =
    | Str of string                             (* "string" *)
    | StrVar of int                             (* X *)
    | Input                                     (* ~ *)
    | Concat of str_expr * str_expr             (* X + Y *)
    | Stringify of num_expr                     (* $x *)
    | Substr of str_expr * num_expr * num_expr  (* %X,y,z *)
    | Quotify of str_expr                       (* |X *)

type stmt =
    | AssignNum of int * num_expr               (* a:42 *)
    | AssignStr of int * str_expr               (* A:"foo" *)
    | Output of str_expr                        (* ."bar" *)

(* `str_expr option` for the final `@...` statement; may be missing. *)
type t = stmt list * str_expr option

