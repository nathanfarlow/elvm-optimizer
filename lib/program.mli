open! Core

module Data : sig
  type type_ =
    | Chunk of Eir.Data.t list
    | Heap
  [@@deriving sexp_of, equal, hash]

  type t =
    { label : string
    ; type_ : type_
    }
  [@@deriving sexp_of, equal, hash]
end

type t

val create : graph:Ast.Statement.t Graph.t -> data:Data.t list -> t
val graph : t -> Ast.Statement.t Graph.t
val data : t -> Data.t list

module For_tests : sig
  val to_string : t -> string
end
