open Core

type t
type segment = Data | Text [@@deriving sexp, equal]
type address = { segment : segment; offset : int } [@@deriving sexp, equal]
type data_entry = Const of int | Label of string [@@deriving sexp, equal]

val heap_label : string

val create :
  data:data_entry list ->
  instructions:Instruction.t list ->
  labels:(string, address) Hashtbl.t ->
  t

val data : t -> data_entry list
val instructions : t -> Instruction.t list
val labels : t -> (string, address) Hashtbl.t
val resolve_label : t -> string -> address option