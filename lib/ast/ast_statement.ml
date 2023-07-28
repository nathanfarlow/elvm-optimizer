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

  include Statement_intf.S with type t := t

  include
    Propagator_statement_intf.S
      with type t := t
       and type var := Ast.Variable.t
       and type exp := Ast.Expression.t

  include
    Eliminator_statement_intf.S
      with type t := t
       and type var := Ast.Variable.t
       and type exp := Ast.Expression.t
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
  [@@deriving sexp]

  let is_nop = equal Nop
  let nop = Nop

  let branch_type = function
    | Exit -> None
    | Assign _ | Putc _ | Nop -> Some Statement_intf.Branch_type.Fallthrough
    | Jump { cond = None; _ } -> Some Unconditional_jump
    | Jump { cond = Some _; _ } -> Some Conditional_jump

  let from_mapping { from; to_ } = Assign { dst = from; src = to_ }

  let substitute ~from ~to_ = function
    | Assign { dst; src } ->
        let dst, dst_changed = Ast.Variable.substitute dst ~from ~to_ in
        let src, src_changed = Ast.Expression.substitute src ~from ~to_ in
        (Assign { dst; src }, dst_changed || src_changed)
    | Putc e ->
        let e, changed = Ast.Expression.substitute e ~from ~to_ in
        (Putc e, changed)
    | Jump { target; cond } ->
        let target, target_changed =
          Ast.Expression.substitute target ~from ~to_
        in
        let cond, cond_changed =
          Option.value_map cond ~default:(None, false) ~f:(fun cond ->
              let cond, changed = Ast.Condition.substitute cond ~from ~to_ in
              (Some cond, changed))
        in
        (Jump { target; cond }, target_changed || cond_changed)
    | Exit -> (Exit, false)
    | Nop -> (Nop, false)

  let substitute_var_to_exp t ~from ~to_ =
    let from = Ast.Expression.Var from in
    substitute t ~from ~to_

  let substitute_exp_to_var t ~from ~to_ =
    let to_ = Ast.Expression.Var to_ in
    substitute t ~from ~to_

  let get_mapping_from_assignment = function
    | Assign { dst; src } -> Some { from = dst; to_ = src }
    | _ -> None
end

include T