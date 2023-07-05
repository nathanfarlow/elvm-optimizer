open Core

type segment = Data | Text [@@deriving sexp, equal]
type address = { segment : segment; offset : int } [@@deriving sexp, equal]
type data_entry = Const of int | Label of string [@@deriving sexp, equal]

type t = {
  data : data_entry list;
  instructions : Instruction.t list;
  labels : (string, address) Hashtbl.t;
}

exception Parse_error of string

val parse_exn : string -> t