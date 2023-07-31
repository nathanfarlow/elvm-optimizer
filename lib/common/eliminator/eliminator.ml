module Make
    (Statement : Eliminator_statement_intf.S)
    (Var : Propagator_var_intf.S with type t = Statement.var)
    (Exp : Propagator_exp_intf.S
             with type t = Statement.exp
              and type var := Var.t) =
struct
  module Statement_wrapper : sig
    include
      Propagator_statement_intf.S
        with type t = Statement.t
         and type var = Statement.var
         and type exp = Statement.exp
  end = struct
    include Statement

    let substitute_var_to_exp t ~from ~to_ =
      Statement.substitute_exp_to_var t ~from:to_ ~to_:from
  end

  include Propagator.Make (Statement_wrapper) (Var) (Exp)
end
