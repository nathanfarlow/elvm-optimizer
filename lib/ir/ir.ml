open Core

module Data = struct
  type type_ = Chunk of Program.data_entry list | Heap
  [@@deriving sexp, equal]

  type t = { label : string; type_ : type_ } [@@deriving sexp, equal, fields]

  let create ~label ~type_ = { label; type_ }
end

type t = { blocks : Block.M.t list; data : Data.t list }
[@@deriving sexp, equal, fields]

let create ~blocks ~data = { blocks; data }