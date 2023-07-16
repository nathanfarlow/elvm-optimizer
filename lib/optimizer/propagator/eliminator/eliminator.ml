module Make
    (Statement : Eliminator_statement_intf.S)
    (Lhs : Propagator_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Propagator_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Lhs.t) : sig end = struct
  module Statement_wrapper : sig
    include
      Propagator_statement_intf.S
        with type t = Statement.t
         and type lhs = Lhs.t
         and type rhs = Rhs.t
  end = struct
    include Statement

    let substitute t { from; to_ } =
      Statement.substitute_reverse t ~from:to_ ~to_:from
  end

  include Propagator.Make (Statement_wrapper) (Lhs) (Rhs)
end
