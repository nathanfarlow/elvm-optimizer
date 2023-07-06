open Core

type segment = Data | Text [@@deriving sexp, equal]
type address = { segment : segment; offset : int } [@@deriving sexp, equal]
type data_entry = Const of int | Label of string [@@deriving sexp, equal]

type t = {
  data : data_entry list;
  instructions : Instruction.t list;
  labels : (string, address) Hashtbl.t;
}

let heap_label = "__reserved_heap_base"
let create ~data ~instructions ~labels = { data; instructions; labels }
let data t = t.data
let instructions t = t.instructions
let labels t = t.labels
let resolve_label t label = Hashtbl.find t.labels label
