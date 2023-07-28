module Data : sig
  type type_ = Chunk of Eir.Data.t list | Heap [@@deriving sexp, equal, hash]
  type t = { label : string; type_ : type_ } [@@deriving sexp, equal, hash]
end

type 'a t

val create : graph:'a Graph.t -> data:Data.t list -> 'a t
val graph : 'a t -> 'a Graph.t
val data : 'a t -> Data.t list

module For_tests (Element : Sexpable) : sig
  val to_string : Element.t t -> string
end