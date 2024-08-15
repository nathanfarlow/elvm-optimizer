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

type t =
  { graph : Ast.Statement.t list Graph.t
  ; data : Data.t list
  }
[@@deriving sexp_of, fields]
