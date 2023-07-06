open! Core

type t = {
  label : string;
  statements : Statement.t list;
  branch : branch option;
}
[@@deriving sexp, equal]

and branch = { primary : t; secondary : t option }

(* todo: test when there are 2 cfg *)

(* optimization ideas:
    If this block has unconditional branch and is the only in edge to
    target block, delete the jump instruction if one exists and
    glue this block to the target, returning one block
*)

let optimize _t = failwith "not implemented"
let find_living _ = failwith "find_living not implemented"

let references t =
  List.concat_map t.statements ~f:Statement.references
  |> List.dedup_and_sort ~compare:String.compare
