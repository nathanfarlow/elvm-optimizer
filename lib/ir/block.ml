open Core

module rec M : sig
  type t = {
    label : string;
    mutable statements : Statement.t array;
    mutable in_edges : M.Edge.t list;
    mutable branch : M.Branch.t option;
  }
  [@@deriving sexp, equal]

  module Edge : sig
    type type_ = Jump | Fallthrough [@@deriving sexp, equal]
    type t = { target : string; type_ : type_ } [@@deriving sexp, equal]
  end

  module Branch : sig
    type t =
      | Conditional_jump of { true_ : M.t; false_ : M.t }
      | Unconditional_jump of M.t
      | Fallthrough of M.t
    [@@deriving sexp, equal]
  end

  val is_top_level : t -> bool
  val dependencies : t -> Statement.variable list
  val references : t -> string Hash_set.t
end = struct
  module Edge = struct
    type type_ = Jump | Fallthrough [@@deriving sexp, equal]
    type t = { target : string; type_ : type_ } [@@deriving sexp, equal]
  end

  type t = {
    label : string;
    mutable statements : Statement.t array;
    mutable in_edges : Edge.t list;
    mutable branch : M.Branch.t option;
  }
  [@@deriving sexp, equal]

  module Branch = struct
    type t =
      | Conditional_jump of { true_ : M.t; false_ : M.t }
      | Unconditional_jump of M.t
      | Fallthrough of M.t
    [@@deriving sexp, equal]
  end

  let is_top_level t =
    match t.in_edges with
    | [] -> true
    (* check for self reference *)
    | [ { target; type_ = Jump } ] -> String.equal target t.label
    | _ -> false

  let dependencies _ = failwith "dependencies not implemented"

  let references t =
    let set = Hash_set.create (module String) in
    Array.iter t.statements ~f:(fun s ->
        Hash_set.iter ~f:(Hash_set.add set) (Statement.references s));
    set
end
