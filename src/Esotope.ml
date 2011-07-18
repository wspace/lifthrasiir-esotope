(* This is a part of Esotope. See README for more information. *)

open EsotopeCommon

type cmdflags =
    {mutable fromkind : string;
     mutable tokind : string;
     mutable fromfile : string;
     mutable tofile : string option;
     mutable verbose : bool}

let parse_args =
    let result = {fromkind = "auto";
                  tokind = "interp";
                  fromfile = "-";
                  tofile = None;
                  verbose = false} in
    Arg.parse [("-f", Arg.String (fun x -> result.fromkind <- x),
                "Language translated from.");
               ("-t", Arg.String (fun x -> result.tokind <- x),
                "Language translated into.");
               ("-r", Arg.Unit (fun _ -> result.tokind <- "interp"),
                "Run the program immediately.");
               ("-v", Arg.Unit (fun _ -> result.verbose <- true),
                "Shows the detailed information about how the code is \
                 processed.");
               ("-o", Arg.String (fun x -> result.tofile <- Some x),
                "Output file.")]
              (fun x -> result.fromfile <- x)
              "Esotope: a growing collection of esoteric language \
               implementation.";
    result

let display_procs procs =
    let num_proc = List.length procs in
    let sum_weights = List.fold_left (fun w p -> w + p#weight) 0 procs in
    let first_kind = (List.hd procs)#input_kind#name in
    let each_proc proc =
        Printf.sprintf " --(%d)--> %s" proc#weight proc#output_kind#name in
    let remaining_kinds = String.concat "" (List.map each_proc procs) in
    Printf.eprintf "Found a path with %d processors (weight=%d): %s%s\n"
        num_proc sum_weights first_kind remaining_kinds;
    flush stderr

let build_procs fromkind tokind =
    let procs = find_procs stream_kind fromkind @
                find_procs fromkind tokind in
    if tokind = interp_kind then
        procs
    else
        procs @ find_procs tokind buffer_kind

let _ =
    let result = parse_args in
    let chan =
        if result.fromfile = "-" then stdin
        else open_in_bin result.fromfile in
    let stream = Stream.of_channel chan in
    let fromkind = lookup_kind result.fromkind in
    let tokind = lookup_kind result.tokind in
    let procs = build_procs fromkind tokind in
    if result.verbose then display_procs procs;
    if tokind = interp_kind then
        (run stream stream_kind procs interp_kind : unit)
    else
        let outchan =
            match result.tofile with
            | Some s -> open_out_bin s
            | None -> stdout in
        let buf = Buffer.create 1024 in
        (run stream stream_kind procs buffer_kind : buffer_type) buf;
        Buffer.output_buffer outchan buf

