module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (_ : Environment_rhs_intf.S
           with type t = Statement.rhs
            and type lhs := Lhs.t) =
struct
  type t = unit

  let create () = ()
  let optimize _t _ = false
end
