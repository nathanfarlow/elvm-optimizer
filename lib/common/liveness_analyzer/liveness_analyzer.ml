module Make
    (Statement : Assignable_statement_intf.S)
    (Lhs : Liveness_analyzer_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Liveness_analyzer_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Lhs.t) =
struct
  module Lhs_set = Rhs.Lhs_set

  type t = { get_living_after : Statement.t Node.t -> Lhs_set.t }

  let create () =
    let get_living_before get_living_after node =
      let living = get_living_after node in
      match Statement.get_assignment (Node.stmt node) with
      | None -> living
      | Some { from; to_ } ->
          Lhs_set.remove living from
          |> Lhs_set.union (Rhs.get_all_lhs_dependencies to_)
    in
    let get_living_after =
      Graph_util.memoize
        ~f:(fun node get_living_after ->
          Graph_util.get_all_branch_targets node
          |> List.map ~f:(get_living_before get_living_after)
          |> Lhs_set.union_list)
        ~on_cycle:(fun _ -> Lhs_set.empty)
    in
    { get_living_after }

  let get_living_after t = t.get_living_after
end