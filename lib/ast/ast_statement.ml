module T : sig
  module Assignment : sig
    type t = { dst : Ast.Variable.t; src : Ast.Expression.t }
    [@@deriving sexp, equal, compare, hash]
  end

  module Jump : sig
    type t = { target : Ast.Expression.t; cond : Ast.Condition.t option }
    [@@deriving sexp, equal, compare, hash]
  end

  type t =
    | Assign of Assignment.t
    | Putc of Ast.Expression.t
    | Jump of Jump.t
    | Exit
    | Nop
  [@@deriving sexp, equal, compare, hash]

  include
    Propagator_statement_intf.S
      with type t := t
       and type lhs := Ast.Variable.t
       and type rhs := Ast.Expression.t
end = struct
  module Assignment = struct
    type t = { dst : Ast.Variable.t; src : Ast.Expression.t }
    [@@deriving sexp, equal, compare, hash]
  end

  module Jump = struct
    type t = { target : Ast.Expression.t; cond : Ast.Condition.t option }
    [@@deriving sexp, equal, compare, hash]
  end

  type t =
    | Assign of Assignment.t
    | Putc of Ast.Expression.t
    | Jump of Jump.t
    | Exit
    | Nop
  [@@deriving sexp, equal, compare, hash]

  type mapping = { from : Ast.Variable.t; to_ : Ast.Expression.t }

  let nop = Nop
  let from_mapping { from; to_ } = Assign { dst = from; src = to_ }

  let substitute t { from; to_ } =
    match t with
    | Assign { dst; src } ->
        let dst, dst_changed = Ast.Variable.substitute dst ~from ~to_ in
        let src, src_changed = Ast.Expression.substitute src ~from ~to_ in
        (Assign { dst; src }, dst_changed || src_changed)
    | _ -> failwith "guh"

  let get_mapping_from_assignment = function
    | Assign { dst; src } -> Some { from = dst; to_ = src }
    | _ -> None
end

include T