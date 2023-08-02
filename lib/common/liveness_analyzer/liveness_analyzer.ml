module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Liveness_analyzer_lhs_intf.S with type t = Statement.lhs)
    (_ : Liveness_analyzer_rhs_intf.S
           with type t = Statement.rhs
            and type lhs := Lhs.t) =
struct
  module Lhs_set = Set.Make (Lhs)

  type t = { get_living_after : Statement.t Node.t -> Lhs_set.t }

  let create () =
    let get_living_after =
      Graph_util.memoize
        ~f:(fun _get_living_after _node -> Lhs_set.empty)
        ~on_cycle:(fun _ -> Lhs_set.empty)
    in
    { get_living_after }

  let get_living_after t = t.get_living_after
end