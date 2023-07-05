open! Core

type t = {
  label : string;
  statements : Statement.t list;
  branch : branch option;
}
[@@deriving sexp, equal]

and branch = { primary : t; secondary : t option }

let optimize _t = failwith "not implemented"
let find_living _ = failwith "find_living not implemented"
let references _ = failwith "not implemented"