type t

module Data : sig
  type type_ = Chunk of Eir.Data.t list | Heap [@@deriving sexp, equal, hash]
  type t = { label : string; type_ : type_ } [@@deriving sexp, equal, hash]
end

val create : (string, Block.t) Hashtbl.t -> Data.t list -> t
val blocks : t -> (string, Block.t) Hashtbl.t
val data : t -> Data.t list