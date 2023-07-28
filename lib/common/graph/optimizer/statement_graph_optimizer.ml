module Make
    (Statement : Statement_intf.S)
    (Optimizer : Optimizer_intf.S with type target := Statement.t) =
struct
  type t = Optimizer.t

  let create = Fn.id

  let update_branch node =
    match Node.branch node with
    | Some (Node.Branch.Conditional_jump { true_; false_ }) -> (
        match Statement.branch_type (Node.stmt node) with
        (* if a conditional jump statement was simplified to an unconditional
           jump, then the condition was determined to always be true *)
        | Some Unconditional_jump ->
            (* delete this node from the false branch's references *)
            Node.set_references false_
              (List.filter (Node.references false_) ~f:(fun ref ->
                   not (String.equal (Node.label ref.from) (Node.label node))));
            (* correct this node's branch to be unconditional to the true branch *)
            Node.set_branch node (Some (Unconditional_jump true_));
            true
        (* if a conditional jump statement was simplified to a fallthrough,
            (definitely a nop), then the condition was determined to always be false *)
        | Some Fallthrough ->
            (* assert (Statement.is_nop node.stmt); *)
            assert (Statement.is_nop @@ Node.stmt node);
            (* delete this node from the true branch's references *)
            Node.set_references true_
              (List.filter (Node.references true_) ~f:(fun ref ->
                   not (String.equal (Node.label ref.from) (Node.label node))));
            (* correct this node's branch to be fallthrough *)
            Node.set_branch node (Some (Fallthrough false_));
            true
        | _ -> false)
    | _ -> false

  let rec optimize optimizer graph =
    Inplace_optimizer_util.optimize_until_unchanging (optimize' optimizer) graph

  and optimize' optimizer graph =
    Hashtbl.fold (Graph.nodes graph) ~init:false
      ~f:(fun ~key:_ ~data:node changed_in_past ->
        let stmt, did_optimize =
          Optimizer.optimize optimizer (Node.stmt node)
        in
        Node.set_stmt node stmt;
        let did_update = update_branch node in
        changed_in_past || did_update || did_optimize)
end
