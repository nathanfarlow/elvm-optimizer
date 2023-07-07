open Core

module Edge = struct
  type type_ = Jump | Fallthrough [@@deriving sexp, equal]
  type t = { target : string; type_ : type_ } [@@deriving sexp, equal]
end

module rec M : sig
  type t = {
    label : string;
    mutable statements : Statement.t list;
    mutable in_edges : Edge.t list;
    mutable branch : Branch.t option;
  }
  [@@deriving sexp, equal]
end = struct
  type t = {
    label : string;
    mutable statements : Statement.t list;
    mutable in_edges : Edge.t list;
    mutable branch : Branch.t option;
  }
  [@@deriving sexp, equal]
end

and Branch : sig
  type t =
    | Conditional_jump of { true_ : M.t; false_ : M.t }
    | Unconditional_jump of M.t
    | Fallthrough of M.t
  [@@deriving sexp, equal]
end = struct
  type t =
    | Conditional_jump of { true_ : M.t; false_ : M.t }
    | Unconditional_jump of M.t
    | Fallthrough of M.t
  [@@deriving sexp, equal]
end

open M

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
