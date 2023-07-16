module T : sig
  module Assignment : sig
    type t = { dst : Ast_expression.Variable.t; src : Ast_expression.t }
    [@@deriving sexp, equal, compare, hash]
  end

  module Jump : sig
    type t = {
      target : Ast_expression.t;
      cond : Ast_expression.Condition.t option;
    }
    [@@deriving sexp, equal, compare, hash]
  end

  type t =
    | Assign of Assignment.t
    | Putc of Ast_expression.t
    | Jump of Jump.t
    | Exit
    | Nop
  [@@deriving sexp, equal, compare, hash]

  include
    Propagator_statement_intf.S
      with type t := t
       and type lhs := Ast_expression.Variable.t
       and type rhs := Ast_expression.t
end = struct
  module Assignment = struct
    type t = { dst : Ast_expression.Variable.t; src : Ast_expression.t }
    [@@deriving sexp, equal, compare, hash]
  end

  module Jump = struct
    type t = {
      target : Ast_expression.t;
      cond : Ast_expression.Condition.t option;
    }
    [@@deriving sexp, equal, compare, hash]
  end

  type t =
    | Assign of Assignment.t
    | Putc of Ast_expression.t
    | Jump of Jump.t
    | Exit
    | Nop
  [@@deriving sexp, equal, compare, hash]

  type mapping = { from : Ast_expression.Variable.t; to_ : Ast_expression.t }

  let nop = Nop
  let from_mapping { from; to_ } = Assign { dst = from; src = to_ }

  let substitute t { from; to_ } =
    match t with
    | Assign { dst; src } ->
        let dst, dst_changed =
          Ast_expression.Variable.substitute dst ~from ~to_
        in
        let src, src_changed = Ast_expression.substitute src ~from ~to_ in
        (Assign { dst; src }, dst_changed || src_changed)
    | _ -> failwith "guh"

  let get_mapping_from_assignment = function
    | Assign { dst; src } -> Some { from = dst; to_ = src }
    | _ -> None
end

include T