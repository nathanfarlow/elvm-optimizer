open Core

type data_type = Chunk of Program.data_entry list | Heap
[@@deriving sexp, equal]

type data_block = { label : string; data : data_type } [@@deriving sexp, equal]

type t = { blocks : Block.M.t list; data : data_block list }
[@@deriving sexp, equal]

let create ~blocks ~data = { blocks; data }
let blocks t = t.blocks
let data t = t.data