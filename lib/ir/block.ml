open! Core

type t = {
  label : string;
  statements : Statement.t list;
  statements_rev : Statement.t list;
  in_edges : string list;
  mutable branch : branch option;
}
[@@deriving sexp, equal]

and branch = Conditional of { true_ : t; false_ : t } | Unconditional of t
[@@deriving sexp, equal]

let create ~label ~statements ~in_edges ~branch =
  { label; statements; statements_rev = List.rev statements; in_edges; branch }

let label t = t.label
let statements t = t.statements
let statements_rev t = t.statements_rev
let in_edges t = t.in_edges
let branch t = t.branch
let set_branch t branch = t.branch <- branch

let is_top_level t =
  match t.in_edges with
  | [] -> true
  (* check for self reference *)
  | [ in_label ] -> String.equal in_label t.label
  | _ -> false

let dependencies _ = failwith "dependencies not implemented"

let references t =
  List.concat_map t.statements ~f:Statement.references
  |> List.dedup_and_sort ~compare:String.compare
