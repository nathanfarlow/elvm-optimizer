module Data = struct
  type type_ = Chunk of Eir.Data.t list | Heap [@@deriving sexp, equal, hash]
  type t = { label : string; type_ : type_ } [@@deriving sexp, equal, hash]
end

type t = { blocks : (string, Block.t) Hashtbl.t; data : Data.t list }
[@@deriving fields]

let create blocks data = { blocks; data }
