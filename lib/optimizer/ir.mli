type t = Block.t list [@@deriving sexp, equal]

val optimize : t -> t
val of_program : Program.t -> t
val to_program : Program.t -> t