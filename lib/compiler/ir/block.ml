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
    type t = { label : string; type_ : type_ } [@@deriving sexp, equal]
  end

  module Branch : sig
    type t =
      | Conditional_jump of { true_ : M.t; false_ : M.t }
      | Unconditional_jump of M.t
      | Fallthrough of M.t
    [@@deriving sexp, equal]
  end
end = struct
  type t = {
    label : string;
    mutable statements : Statement.t array;
    mutable in_edges : M.Edge.t list;
    mutable branch : M.Branch.t option;
  }
  [@@deriving sexp, equal]

  module Edge = struct
    type type_ = Jump | Fallthrough [@@deriving sexp, equal]
    type t = { label : string; type_ : type_ } [@@deriving sexp, equal]
  end

  module Branch = struct
    type t =
      | Conditional_jump of { true_ : M.t; false_ : M.t }
      | Unconditional_jump of M.t
      | Fallthrough of M.t
    [@@deriving sexp, equal]
  end
end
