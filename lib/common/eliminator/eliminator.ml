module Make
    (Statement : Eliminator_statement_intf.S)
    (Mapping : Propagator_mapping_intf.S
                 with type key := Statement.var
                  and type value := Statement.exp) =
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

  include Propagator.Make (Statement_wrapper) (Mapping)
end
