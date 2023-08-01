module Make
    (Statement : Substituter_statement_intf.S)
    (Lhs : Environment_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Environment_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Lhs.t)
    (Substitute_delegate : Substitute_delegate_intf.S
                             with type stmt = Statement.t
                              and type lhs = Lhs.t
                              and type rhs = Rhs.t) =
struct
  module Simulator =
    Simulator.Make (Statement) (Lhs) (Rhs) (Substitute_delegate)

  module Environment = Simulator.Environment

  module Simulator_util =
    Substitute_util.Make (Substitute_delegate) (Environment)

  type t = unit

  let create = Fn.id

  let prepend_assignment graph node left right =
    let stmt = Statement.from_assignment { from = left; to_ = right } in
    let label = Graph.fresh_label graph in
    let new_node = Node.create ~label ~stmt in
    Node.prepend_node node new_node;
    Graph.register_node graph new_node

  let get_branch_dependencies node simulator =
    Graph_util.get_all_branch_targets node
    |> List.map ~f:(Simulator.get_initial_env simulator)
    |> List.reduce ~f:Environment.intersection
    |> Option.value ~default:Environment.empty

  let optimize_node graph node simulator =
    let end_mappings = Simulator.get_final_env simulator node in
    let obligations = get_branch_dependencies node simulator in
    let valid = Environment.intersection obligations end_mappings in
    let invalid = Environment.diff end_mappings valid |> Environment.to_alist in
    let invalid' =
      match Statement.get_assignment (Node.stmt node) with
      | Some { from; _ } ->
          (* nop out assignment, since assignment is known in mappings *)
          Node.set_stmt node Statement.nop;
          (* don't write down the previous mapping of this variable,
             since the mappings already reflect the change*)
          List.filter invalid ~f:(fun (left, _) -> not (Lhs.equal left from))
      | _ -> invalid
    in
    (* write down invalidated mappings *)
    List.iter invalid' ~f:(fun (left, right) ->
        prepend_assignment graph node left right);
    (* update statement according to valid mappings *)
    let updated_stmt, did_update =
      Simulator_util.substitute_all (Node.stmt node) valid
    in
    Node.set_stmt node updated_stmt;
    did_update

  let optimize _ graph =
    let simulator = Simulator.create () in
    (* shallow copy graph nodes map so we can mutate graph as we fold *)
    Hashtbl.copy (Graph.nodes graph)
    |> Hashtbl.fold ~init:false ~f:(fun ~key:_ ~data:node acc ->
           optimize_node graph node simulator || acc)
end
