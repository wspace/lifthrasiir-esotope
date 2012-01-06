(* This is a part of Esotope. See README for more information. *)

(* basically, the tape is a mapping from the block index to the block "triple".
 * the triple contains the current block, the previous block (if any) and the
 * next block (if any). note that the presence of the triple means that there
 * is an actual block allocated to that index.
 *
 * the tape caches the triple that contains the current tape pointer. this
 * means that the relative offset within plus or minus block size should hit
 * the cache. *)

type 'celltype triple =
    'celltype array option * 'celltype array * 'celltype array option

type 'celltype t = {
    blocks : (int, 'celltype triple) Hashtbl.t;
    blockshift : int;
    blockmask : int;
    mutable curblock : int;
    mutable curoffset : int;
    mutable cached : 'celltype triple option;
    default : 'celltype
}

let create blocksize default =
    if blocksize <= 1 then invalid_arg "too small block size";
    let blocksize = BitUtil.next_pow2 blocksize in
    let blockshift = BitUtil.log2_pow2 blocksize in
    {blocks = Hashtbl.create 8;
     blockshift = blockshift;
     blockmask = pred blocksize;
     curblock = 0;
     curoffset = 0;
     cached = None;
     default = default}

let get_block idx tape =
    try Some (Hashtbl.find tape.blocks idx)
    with Not_found -> None

let update_and_get_prev idx block tape =
    try
        let idx = pred idx in
        let (prev, cur, _) = Hashtbl.find tape.blocks idx in
        Hashtbl.replace tape.blocks idx (prev, cur, Some block);
        Some cur
    with Not_found -> None

let update_and_get_next idx block tape =
    try
        let idx = succ idx in
        let (_, cur, next) = Hashtbl.find tape.blocks idx in
        Hashtbl.replace tape.blocks idx (Some block, cur, next);
        Some cur
    with Not_found -> None

let ensure_triple fprev fnext idx tape =
    try
        Hashtbl.find tape.blocks idx
    with Not_found ->
        let block = Array.create (1 lsl tape.blockshift) tape.default in
        let triple = (fprev idx block tape, block, fnext idx block tape) in
        Hashtbl.replace tape.blocks idx triple; triple

let ensure_block idx tape =
    let (_, block, _) =
        ensure_triple update_and_get_prev update_and_get_next idx tape in
    block

let offset delta tape =
    let off = tape.curoffset + delta in
    let (idx, off) = (off asr tape.blockshift, off land tape.blockmask) in
    if idx <> 0 then begin (* invalidate cache *)
        let idx' = tape.curblock + idx in
        tape.curblock <- idx';
        tape.cached <- get_block idx' tape
    end;
    tape.curoffset <- off

let advance tape =
    if tape.curoffset == tape.blockmask then begin
        let idx' = succ tape.curblock in
        tape.curblock <- idx'; tape.curoffset <- 0;
        tape.cached <- get_block idx' tape
    end else
        tape.curoffset <- succ tape.curoffset

let retract tape =
    if tape.curoffset == 0 then begin
        let idx' = pred tape.curblock in
        tape.curblock <- idx'; tape.curoffset <- tape.blockmask;
        tape.cached <- get_block idx' tape
    end else
        tape.curoffset <- pred tape.curoffset

let get_slow idx off tape =
    try
        let (_, block, _) = Hashtbl.find tape.blocks idx in block.(off)
    with Not_found -> tape.default

let get delta tape =
    let off = tape.curoffset + delta in
    match off asr tape.blockshift with
    | -1 ->
        begin match tape.cached with
        | Some (Some block, _, _) -> block.(off land tape.blockmask)
        | Some (None, _, _) -> tape.default
        | None -> get_slow (pred tape.curblock) (off land tape.blockmask) tape
        end
    | 0 ->
        begin match tape.cached with
        | Some (_, block, _) -> block.(off land tape.blockmask)
        | None -> tape.default
        end
    | 1 ->
        begin match tape.cached with
        | Some (_, _, Some block) -> block.(off land tape.blockmask)
        | Some (_, _, None) -> tape.default
        | None -> get_slow (succ tape.curblock) (off land tape.blockmask) tape
        end
    | idx -> get_slow (tape.curblock + idx) (off land tape.blockmask) tape

let set delta v tape =
    let off = tape.curoffset + delta in
    let block =
        match off asr tape.blockshift with
        | -1 ->
            begin match tape.cached with
            | Some (Some block, _, _) -> block
            | Some (None, cur, next) ->
                let fnext _ block _ =
                    let triple = (Some block, cur, next) in
                    Hashtbl.replace tape.blocks tape.curblock triple;
                    tape.cached <- Some triple;
                    Some cur in
                let (_, block, _) =
                    ensure_triple update_and_get_prev fnext
                                  (pred tape.curblock) tape in
                block
            | None -> ensure_block (pred tape.curblock) tape
            end
        | 0 ->
            begin match tape.cached with
            | Some (_, block, _) -> block
            | None ->
                begin
                    let triple =
                        ensure_triple update_and_get_prev update_and_get_next
                                      tape.curblock tape in
                    tape.cached <- Some triple;
                    let (_, block, _) = triple in block
                end
            end
        | 1 ->
            begin match tape.cached with
            | Some (_, _, Some block) -> block
            | Some (prev, cur, None) ->
                let fprev _ block _ =
                    let triple = (prev, cur, Some block) in
                    Hashtbl.replace tape.blocks tape.curblock triple;
                    tape.cached <- Some triple;
                    Some cur in
                let (_, block, _) =
                    ensure_triple fprev update_and_get_next
                                  (succ tape.curblock) tape in
                block
            | None -> ensure_block (succ tape.curblock) tape
            end
        | idx -> ensure_block (tape.curblock + idx) tape
    in block.(off land tape.blockmask) <- v

