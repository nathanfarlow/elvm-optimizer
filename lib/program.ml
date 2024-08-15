open Core

module Data = struct
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

module Graph = Graph.Make (Ast.Statement)

type t =
  { graph : Graph.t
  ; data : Data.t list
  }
[@@deriving sexp_of, fields]
