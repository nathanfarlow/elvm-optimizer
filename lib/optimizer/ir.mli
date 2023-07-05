type data_chunk = { label : string; data : Program.data_entry list }
[@@deriving sexp, equal]

type t = { blocks : Block.t list; data : data_chunk list }
[@@deriving sexp, equal]

val optimize : t -> t
val of_program : Program.t -> t
val to_program : Program.t -> t