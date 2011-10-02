(* This is a part of Esotope. See README for more information. *)

(**************************************************************************)
(* The base-2 logarithm for the powers of two. *)

let log2_pow2 =
    if max_int lsr 31 == 0 then
        let de_bruijn_seq = 0x04653adf (* B(2,5) *) in
        let inv_de_bruijn_seq =
            [| -1;  0;  1;  5;  2; 10;  6; 15;  3; 13;
               11; 20;  7; 22; 16; 25; 30;  4;  9; 14;
               12; 19; 21; 24; 29;  8; 18; 23; 28; 17;
               27; 26 |] in
        fun v -> inv_de_bruijn_seq.((v * de_bruijn_seq) lsr 26)
    else if max_int lsr 63 == 0 then
        let de_bruijn_seq = 0x0218a392cd3d5dbf (* B(2,6) *) in
        let inv_de_bruijn_seq =
            [| -1;  0;  1;  6;  2; 12;  7; 18;  3; 24;
               13; 27;  8; 33; 19; 39;  4; 16; 25; 37;
               14; 45; 28; 47;  9; 30; 34; 53; 20; 49;
               40; 56; 62;  5; 11; 17; 23; 26; 32; 38;
               15; 36; 44; 46; 29; 52; 48; 55; 61; 10;
               22; 31; 35; 43; 51; 54; 60; 21; 42; 50;
               59; 41; 58; 57 |] in
        fun v -> inv_de_bruijn_seq.((v * de_bruijn_seq) lsr 57)
    else
        failwith "no appropriate implementation for log2_pow2"

(**************************************************************************)
(* The next power of two. *)

let next_pow2 =
    if max_int lsr 31 == 0 then
        fun v -> let v = pred v in
                 let v = v lor (v lsr 1) in
                 let v = v lor (v lsr 2) in
                 let v = v lor (v lsr 4) in
                 let v = v lor (v lsr 8) in
                 let v = v lor (v lsr 16) in
                 succ v
    else if max_int lsr 63 == 0 then
        fun v -> let v = pred v in
                 let v = v lor (v lsr 1) in
                 let v = v lor (v lsr 2) in
                 let v = v lor (v lsr 4) in
                 let v = v lor (v lsr 8) in
                 let v = v lor (v lsr 16) in
                 let v = v lor (v lsr 32) in
                 succ v
    else
        failwith "no appropriate implementation for next_pow2"

