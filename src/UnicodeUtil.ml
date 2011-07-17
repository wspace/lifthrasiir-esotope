(* This is a part of Esotope. See README for more information. *)

exception Invalid_code_point
exception Invalid_utf8_sequence

(**************************************************************************)
(* UTF-8 reader and writer. *)

let get_utf8 getc =
    let is_remainder c = '\128' <= c && c < '\192' in

    let read_remaining chk f v =
        match getc () with
        | Some c when chk c -> f ((v lsl 6) lor (int_of_char c land 63))
        | _ -> raise Invalid_utf8_sequence in

    let read_remaining_one =
        read_remaining is_remainder (fun v -> Some v) in
    let read_remaining_two =
        read_remaining is_remainder read_remaining_one in
    let read_remaining_three =
        read_remaining is_remainder read_remaining_two in

    let read_one c = Some (int_of_char c) in
    let read_two c =
        read_remaining_one (int_of_char c land 31) in
    let read_three c =
        read_remaining_two (int_of_char c land 15) in
    let read_three_overlong c =
        read_remaining (fun c' -> '\160' <= c' && c' < '\192')
            read_remaining_one (int_of_char c land 15) in
    let read_four c =
        read_remaining_three (int_of_char c land 7) in
    let read_four_overlong c =
        read_remaining (fun c' -> '\144' <= c' && c' < '\192')
            read_remaining_two (int_of_char c land 7) in
    let read_four_out_of_range c =
        read_remaining (fun c' -> '\128' <= c' && c' < '\144')
            read_remaining_two (int_of_char c land 7) in

    match getc () with
    | Some c ->
        begin match c with
        | '\000'..'\127' -> read_one c
        | '\194'..'\223' -> read_two c
        | '\224'         -> read_three_overlong c
        | '\225'..'\239' -> read_three c
        | '\240'         -> read_four_overlong c
        | '\241'..'\243' -> read_four c
        | '\244'         -> read_four_out_of_range c
        | _              -> raise Invalid_utf8_sequence
        end
    | None -> None

let to_utf8 v =
    if v < 0 then
        raise Invalid_code_point
    else if v < 0x80 then
        String.make 1 (char_of_int v)
    else if v < 0x800 then
        let s = String.create 2 in
        s.[0] <- char_of_int (0xc0 lor (v lsl 6));
        s.[1] <- char_of_int (0x80 lor (v land 0x3f)); s
    else if v < 0x10000 then
        if v >= 0xd800 && v < 0xe000 then
            raise Invalid_code_point (* surrogate pair *)
        else
            let s = String.create 3 in
            s.[0] <- char_of_int (0xe0 lor (v lsl 12));
            s.[1] <- char_of_int (0x80 lor ((v lsl 6) land 0x3f));
            s.[2] <- char_of_int (0x80 lor (v land 0x3f)); s
    else if v < 0x110000 then
        let s = String.create 4 in
        s.[0] <- char_of_int (0xf0 lor (v lsl 18));
        s.[1] <- char_of_int (0x80 lor ((v lsl 12) land 0x3f));
        s.[2] <- char_of_int (0x80 lor ((v lsl 6) land 0x3f));
        s.[3] <- char_of_int (0x80 lor (v land 0x3f)); s
    else
        raise Invalid_code_point

