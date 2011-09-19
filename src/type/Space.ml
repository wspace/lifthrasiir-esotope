(* This is a part of Esotope. See README for more information. *)

open EsotopeCommon

(**************************************************************************)
(* Signatures. *)

module type IndexType = sig
    type offset
    type size
    type sep
    type block_size
    val origin : offset
    val zero : size
    val to_block_size : size -> block_size
    val block_length : block_size -> int
    val split : offset -> block_size -> offset * int
    val combine : offset -> int -> block_size -> offset
    val pred_offset : offset -> offset
    val succ_offset : offset -> offset
    val add_offset : offset -> size -> offset
    val add_size : size -> size -> size
    val sub_offset : offset -> offset -> size
    val sub_size : size -> size -> size
    val minmax : offset -> offset -> offset * offset
    val iter : size -> (size -> unit) -> unit
    val advance : size -> sep option -> size
    val sep_of_char_stream : char Stream.t -> sep option
    val sep_of_unicode_stream : int Stream.t -> sep option
end

module type S = sig
    type offset
    type size
    type sep
    type 'celltype t
    val create : size -> 'celltype -> 'celltype t
    val clear : 'celltype t -> unit
    val copy : 'celltype t -> 'celltype t
    val get : 'celltype t -> offset -> 'celltype
    val set : 'celltype t -> offset -> 'celltype -> unit
    val blit : 'celltype t -> offset -> 'celltype t -> offset -> size -> unit
    val bounds : 'celltype t -> offset * size
    val from_stream : ('a Stream.t -> sep option) ->
                      ('a Stream.t -> 'celltype) -> 'a Stream.t ->
                      'celltype t -> offset -> 'celltype t
    val from_char_stream : (char Stream.t -> 'celltype) -> char Stream.t ->
                           'celltype t -> offset -> 'celltype t
    val from_unicode_stream : (int Stream.t -> 'celltype) -> int Stream.t ->
                              'celltype t -> offset -> 'celltype t
end

(**************************************************************************)
(* Functor for spaces. *)

module Make = functor (I : IndexType) -> struct
    type offset = I.offset
    type size = I.size
    type sep = I.sep

    type 'celltype t = {
        blocks : (offset, 'celltype array) Hashtbl.t;
        blocksize : I.block_size;
        mutable minoffset : offset;
        mutable maxoffset : offset;
        default : 'celltype
    }

    let create blocksize default =
        {blocks = Hashtbl.create 8;
         blocksize = I.to_block_size blocksize;
         minoffset = I.origin;
         maxoffset = I.origin;
         default = default}

    let clear space =
        space.minoffset <- I.origin;
        space.maxoffset <- I.origin;
        Hashtbl.clear space.blocks

    let copy space =
        let h = Hashtbl.create 8 in
        Hashtbl.iter (fun k v -> Hashtbl.add h k (Array.copy v)) space.blocks;
        {blocks = h;
         blocksize = space.blocksize;
         minoffset = space.minoffset;
         maxoffset = space.maxoffset;
         default = space.default}

    let block space idx =
        try Some (Hashtbl.find space.blocks idx)
        with Not_found -> None

    let ensure_block space idx =
        try Hashtbl.find space.blocks idx
        with Not_found ->
            let length = I.block_length space.blocksize in
            let block = Array.make length space.default in
            let (minoffset,tmp) = I.minmax space.minoffset idx in
            let (_,maxoffset) = I.minmax tmp space.maxoffset in
            space.minoffset <- minoffset;
            space.maxoffset <- maxoffset;
            Hashtbl.add space.blocks idx block; block

    let get space idx =
        let (blockidx, cellidx) = I.split idx space.blocksize in
        match block space blockidx with
        | Some block -> block.(cellidx)
        | None -> space.default

    let set space idx cell =
        let (blockidx, cellidx) = I.split idx space.blocksize in
        let block = ensure_block space blockidx in
        block.(cellidx) <- cell

    let blit src srcpos dest destpos size =
        (* TODO overlap testing *)
        let iter idx =
            let srcidx = I.add_offset srcpos idx in
            let destidx = I.add_offset destpos idx in
            set dest destidx (get src srcidx) in
        I.iter size iter

    let bounds space =
        let min = I.combine space.minoffset 0 space.blocksize in
        let max = I.combine (I.succ_offset space.maxoffset) 0 space.blocksize in
        (min, I.sub_offset max min)

    let from_stream readsep readcell stream space offset =
        let offset_of_size = I.add_offset offset in
        let rec loop pos =
            try
                let sep = readsep stream in
                if sep = None then
                    set space (offset_of_size pos) (readcell stream);
                loop (I.advance pos sep)
            with End_of_file -> space
        in loop I.zero

    let from_char_stream readcell stream space offset =
        from_stream I.sep_of_char_stream readcell stream space offset
    let from_unicode_stream readcell stream space offset =
        from_stream I.sep_of_unicode_stream readcell stream space offset
end

(**************************************************************************)
(* Two-dimensional Euclidean space. *)

type sep2d = Newline2

module Space2D = Make(struct
    type offset = int * int
    type size = int * int
    type sep = sep2d
    type block_size = (int * int) * (int * int)   (* shift, cellidx mask *)

    let origin = (0, 0)
    let zero = (0, 0)

    let to_block_size (x,y) =
        let log2up v =
            let rec loop i v = if v > 0 then loop (succ i) (v lsr 1) else i in
            loop 0 (pred v) in
        let (xshift,yshift) = (log2up x, log2up y) in
        if xshift < 1 || yshift < 1 then invalid_arg "too small block size";
        ((xshift, yshift), ((1 lsl xshift) - 1, (1 lsl yshift) - 1))

    let block_length ((xshift,yshift),_) = 1 lsl (xshift + yshift)

    let split (x,y) ((xshift,yshift),(xmask,ymask)) =
        let blockidx = (x asr xshift, y asr yshift) in
        let (cellx,celly) = (x land xmask, y land ymask) in
        (blockidx, cellx lor (celly lsl yshift))

    let combine (blockx,blocky) cellidx ((xshift,yshift),(xmask,ymask)) =
        let (cellx,celly) = (cellidx land xmask, cellidx lsr yshift) in
        ((blockx lsl xshift) + cellx, (blocky lsl yshift) + celly)

    let pred_offset (x,y) = (x-1, y-1)
    let succ_offset (x,y) = (x+1, y+1)

    let add_offset (x,y) (dx,dy) = (x+dx, y+dy)
    let add_size (dx1,dy1) (dx2,dy2) = (dx1+dx2, dy1+dy2)
    let sub_offset (x1,y1) (x2,y2) = (x1-x2, y1-y2)
    let sub_size (dx1,dy1) (dx2,dy2) = (dx1-dx2, dy1-dy2)

    let minmax (x1,y1) (x2,y2) =
        let (minx,maxx) = if x1 < x2 then (x1,x2) else (x2,x1) in
        let (miny,maxy) = if y1 < y2 then (y1,y2) else (y2,y1) in
        ((minx,miny), (maxx,maxy))

    let iter (dx,dy) f =
        for y = 0 to (dy - 1) do
            for x = 0 to (dx - 1) do f (x,y) done
        done

    let advance (x,y) = function
        | Some Newline2 -> (0,y+1)
        | None -> (x+1,y)

    let sep_of_char_stream stream =
        match Stream.peek stream with
        | Some '\n' -> Stream.junk stream; Some Newline2
        | Some '\r' ->
            Stream.junk stream;
            begin match Stream.peek stream with
            | Some '\n' -> Stream.junk stream; Some Newline2
            | Some _ -> None
            | None -> raise End_of_file
            end
        | Some _ -> None
        | None -> raise End_of_file

    let sep_of_unicode_stream stream =
        match Stream.peek stream with
        | Some 10 -> Stream.junk stream; Some Newline2
        | Some 13 ->
            Stream.junk stream;
            begin match Stream.peek stream with
            | Some 10 -> Stream.junk stream; Some Newline2
            | Some _ -> None
            | None -> raise End_of_file
            end
        | Some _ -> None
        | None -> raise End_of_file
end)

type sep3d = Newline3 | Formfeed3

(* TODO *)

