module Make
    (Statement : Eliminator_stmt_intf.S)
    (Lhs : Lhs_intf.S with type t = Statement.lhs)
    (Rhs : Rhs_intf.S with type t = Statement.rhs and type lhs := Lhs.t) : sig end =
struct
  module Statement_wrapper : sig
    include
      Prop_statement_intf.S
        with type t = Statement.t
         and type lhs = Lhs.t
         and type rhs = Rhs.t
  end = struct
    include Statement

    let substitute t { from; to_ } =
      Statement.substitute_rev t ~from:to_ ~to_:from
  end

  include Propagator.Make (Statement_wrapper) (Lhs) (Rhs)
end
