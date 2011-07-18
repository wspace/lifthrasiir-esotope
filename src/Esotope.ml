(* This is a part of Esotope. See README for more information. *)

open EsotopeCommon

type op = Process | ListKinds

type cmdflags =
    {mutable oper : op;
     mutable fromkind : string;
     mutable tokind : string;
     mutable fromfile : string;
     mutable tofile : string option;
     mutable verbose : bool}

let parse_args =
    let result = {oper = Process;
                  fromkind = "auto";
                  tokind = "interp";
                  fromfile = "-";
                  tofile = None;
                  verbose = false} in
    Arg.parse [("-f", Arg.String (fun x -> result.fromkind <- x),
                "Language translated from.");
               ("-t", Arg.String (fun x -> result.tokind <- x),
                "Language translated into.");
               ("-r", Arg.Unit (fun () -> result.tokind <- "interp"),
                "Run the program immediately.");
               ("-v", Arg.Unit (fun () -> result.verbose <- true),
                "Shows the detailed information about how the code is \
                 processed.");
               ("-o", Arg.String (fun x -> result.tofile <- Some x),
                "Output file.");
               ("--list-kinds", Arg.Unit (fun () -> result.oper <- ListKinds),
                "Shows all supported kinds.")]
              (fun x -> result.fromfile <- x)
              "Esotope: a growing collection of esoteric language \
               implementation.";
    result

let list_kinds () =
    let kinds' =
        List.fast_sort (fun (x,_) (y,_) -> compare x y)
            (Hashtbl.fold (fun k v t -> (k,v)::t) kinds []) in
    Printf.printf "%-20s%s\n\n" "KIND NAME" "ALIASES";
    let show (k,v) =
        Printf.printf "%-20s" k;
        List.iter (fun alias -> Printf.printf "%s " alias)
            (List.fast_sort compare v#aliases);
        Printf.printf "\n"
    in List.iter show kinds'

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

let guess_kind fn verbose =
    try
        let extpos = String.length (Filename.chop_extension fn) in
        let ext = String.sub fn extpos (String.length fn - extpos) in
        begin try
            let guessed = lookup_extension ext in
            if verbose then
                Printf.eprintf "Automatically guessed a kind %S for the \
                                extension %S.\n" guessed#name ext;
            guessed
        with Not_found ->
            Printf.eprintf "Error: cannot automatically guess a kind for \
                            the extension %S.\n" ext;
            exit 1
        end
    with Invalid_argument _ ->
        Printf.eprintf "Error: cannot automatically guess a kind.\n";
        exit 1

let process result =
    let chan =
        if result.fromfile = "-" then stdin
        else open_in_bin result.fromfile in
    let stream = Stream.of_channel chan in
    let fromkind =
        if result.fromkind = "auto" then
            guess_kind result.fromfile result.verbose
        else
            lookup_kind result.fromkind in
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

let _ =
    let result = parse_args in
    match result.oper with
    | Process -> process result
    | ListKinds -> list_kinds ()

