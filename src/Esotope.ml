open EsotopeCommon

type cmdflags =
    {mutable fromlang: string;
     mutable tolang: string;
     mutable fromfile: string;
     mutable tofile: string option}

let parse_args =
    let result = {fromlang = "auto";
                  tolang = "interp";
                  fromfile = "-";
                  tofile = None} in
    Arg.parse [("-f", Arg.String (fun x -> result.fromlang <- x),
                "Language translated from.");
               ("-t", Arg.String (fun x -> result.tolang <- x),
                "Language translated into.");
               ("-o", Arg.String (fun x -> result.tofile <- Some x),
                "Output file.")]
              (fun x -> result.fromfile <- x)
              "Esotope: a growing collection of esoteric language
              implementation.";
    result

let _ =
    let result = parse_args in
    let chan = if result.fromfile = "-" then stdin else open_in result.fromfile in
    let procs = [lookup_proc stream_kind LangBrainfuck.code_kind;
                 lookup_proc LangBrainfuck.code_kind interp_kind] in
    run (Stream.of_channel chan) stream_kind procs interp_kind

