type t [@@deriving sexp, equal]

type data_type = Chunk of Program.data_entry list | Heap
[@@deriving sexp, equal]

type data_block = { label : string; data : data_type } [@@deriving sexp, equal]

val create : blocks:Block.t list -> data:data_block list -> t
val blocks : t -> Block.t list
val data : t -> data_block list