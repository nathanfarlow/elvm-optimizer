open! Core

type t = {
  label : string;
  statements : Statement.t list;
  branch : branch option;
}

and branch = { primary : t; secondary : t option }

let get_statements t = t.statements
let find_living _ = failwith "find_living not implemented"