open Core

type segment = Data | Text [@@deriving sexp, equal]
type address = { segment : segment; offset : int } [@@deriving sexp, equal]
type data_entry = Const of int | Address of address [@@deriving sexp, equal]

type t = {
  data : data_entry list;
  instructions : Elvm_instruction.t list;
  labels : (string, address) Hashtbl.t;
}

exception Parse_error of string

val parse_exn : string -> t