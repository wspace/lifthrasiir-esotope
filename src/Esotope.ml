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
    let stream = Stream.of_channel chan in
    let procs = [lookup_proc stream_kind LangBrainfuck.code_kind;
                 lookup_proc LangBrainfuck.code_kind buffer_kind] in
    let buf = Buffer.create 1024 in
    (run stream stream_kind procs buffer_kind : buffer_type) buf;
    Buffer.output_buffer stdout buf

