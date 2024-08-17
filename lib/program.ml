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

type t =
  { graph : (Ast.Statement.t list Graph.t[@sexp.opaque])
  ; data : Data.t list
  }
