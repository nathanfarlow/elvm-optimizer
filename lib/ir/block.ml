open Core

module Edge = struct
  type type_ = Jump | Fallthrough [@@deriving sexp, equal]
  type t = { target : string; type_ : type_ } [@@deriving sexp, equal]
end

type t = {
  label : string;
  statements : Statement.t list;
  in_edges : Edge.t list;
  mutable branch : branch option;
}
[@@deriving sexp, equal, fields]

and branch =
  | Conditional_jump of { true_ : t; false_ : t }
  | Unconditional_jump of t
  | Fallthrough of t
[@@deriving sexp, equal]

let create ~label ~statements ~in_edges ~branch =
  { label; statements; in_edges; branch }

let set_branch t branch = t.branch <- branch

let is_top_level t =
  match t.in_edges with
  | [] -> true
  (* check for self reference *)
  | [ { target; type_ = Jump } ] -> String.equal target t.label
  | _ -> false

let dependencies _ = failwith "dependencies not implemented"

let references t =
  List.concat_map t.statements ~f:Statement.references
  |> List.dedup_and_sort ~compare:String.compare
