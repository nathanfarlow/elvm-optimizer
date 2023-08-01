module Make
    (Statement : Eliminator_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Environment_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Lhs.t) =
struct
  module Delegate = struct
    type stmt = Statement.t
    type lhs = Lhs.t
    type rhs = Rhs.t

    let substitute stmt ~lhs ~rhs =
      Statement.substitute_rhs_to_lhs stmt ~from:rhs ~to_:lhs
  end

  include Substituter.Make (Statement) (Lhs) (Rhs) (Delegate)
end
