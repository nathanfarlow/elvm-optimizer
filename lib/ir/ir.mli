open! Core

type t [@@deriving sexp, equal]

module Data : sig
  type t [@@deriving sexp, equal]

  type type_ = Chunk of Program.data_entry list | Heap
  [@@deriving sexp, equal]

  val create : label:string -> type_:type_ -> t
  val label : t -> string
  val type_ : t -> type_
end

val create : blocks:Block.M.t list -> data:Data.t list -> t
val blocks : t -> Block.M.t list
val data : t -> Data.t list