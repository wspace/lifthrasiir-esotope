(* This is a part of Esotope. See README for more information. *)

open EsotopeCommon

type cmdflags =
    {mutable fromkind: string;
     mutable tokind: string;
     mutable fromfile: string;
     mutable tofile: string option}

let parse_args =
    let result = {fromkind = "auto";
                  tokind = "interp";
                  fromfile = "-";
                  tofile = None} in
    Arg.parse [("-f", Arg.String (fun x -> result.fromkind <- x),
                "Language translated from.");
               ("-t", Arg.String (fun x -> result.tokind <- x),
                "Language translated into.");
               ("-r", Arg.Unit (fun _ -> result.tokind <- "interp"),
                "Run the program immediately.");
               ("-o", Arg.String (fun x -> result.tofile <- Some x),
                "Output file.")]
              (fun x -> result.fromfile <- x)
              "Esotope: a growing collection of esoteric language \
               implementation.";
    result

let _ =
    let result = parse_args in
    let chan =
        if result.fromfile = "-" then stdin
        else open_in_bin result.fromfile in
    let stream = Stream.of_channel chan in
    let fromkind = lookup_kind result.fromkind in
    let tokind = lookup_kind result.tokind in
    if tokind = interp_kind then
        let procs = [lookup_proc stream_kind fromkind;
                     lookup_proc fromkind interp_kind] in
        (run stream stream_kind procs interp_kind : unit)
    else
        let outchan =
            match result.tofile with
            | Some s -> open_out_bin s
            | None -> stdout in
        let procs =
            if fromkind = tokind then
                [lookup_proc stream_kind fromkind;
                 lookup_proc fromkind buffer_kind]
            else
                [lookup_proc stream_kind fromkind;
                 lookup_proc fromkind tokind;
                 lookup_proc tokind buffer_kind] in
        let buf = Buffer.create 1024 in
        (run stream stream_kind procs buffer_kind : buffer_type) buf;
        Buffer.output_buffer outchan buf

