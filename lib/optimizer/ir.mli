type data_type = Chunk of Program.data_entry list | Heap
[@@deriving sexp, equal]

type data_block = { label : string; data : data_type } [@@deriving sexp, equal]

type t = { blocks : Block.t list; data : data_block list }
[@@deriving sexp, equal]

val optimize : t -> t
val of_program : Program.t -> t
val to_program : Program.t -> t