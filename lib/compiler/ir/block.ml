module rec M : sig
  type t = {
    label : string;
    mutable statements : Statement.t array;
    mutable in_edges : M.Edge.t list;
    mutable branch : M.Branch.t option;
  }
  [@@deriving sexp, equal]

  module Edge : sig
    type type_ = Jump | Fallthrough | Call [@@deriving sexp, equal, hash]
    type t = { label : string; type_ : type_ } [@@deriving sexp, equal, hash]
  end

  module Branch : sig
    type t =
      | Conditional_jump of { true_ : M.t; false_ : M.t }
      | Unconditional_jump of M.t
      | Fallthrough of M.t
      | Call of M.t
    [@@deriving sexp, equal]
  end

  val is_top_level : t -> bool
  val references : t -> string Hash_set.t
end = struct
  type t = {
    label : string;
    mutable statements : Statement.t array;
    mutable in_edges : M.Edge.t list;
    mutable branch : M.Branch.t option;
  }
  [@@deriving sexp, equal]

  module Edge = struct
    type type_ = Jump | Fallthrough | Call [@@deriving sexp, equal, hash]
    type t = { label : string; type_ : type_ } [@@deriving sexp, equal, hash]
  end

  module Branch = struct
    type t =
      | Conditional_jump of { true_ : M.t; false_ : M.t }
      | Unconditional_jump of M.t
      | Fallthrough of M.t
      | Call of M.t
    [@@deriving sexp, equal]
  end

  let is_top_level t =
    match t.in_edges with
    | [] -> true
    | [ { label; type_ = Jump } ] -> String.equal label t.label
    | _ -> false

  let references t =
    let set = Hash_set.create (module String) in
    Array.iter t.statements ~f:(fun s ->
        Hash_set.iter ~f:(Hash_set.add set) (Statement.references s));
    set
end

include M