module Make
    (Statement : Statement_intf.S)
    (Optimizer : Optimizer_intf.S with type target := Statement.t) : sig
  type t

  include
    Inplace_optimizer_intf.S
      with type t := t
       and type target := Statement.t Graph.t

  val create : Optimizer.t -> t
end = struct
  type t = Optimizer.t

  let create = Fn.id

  let update_branch (node : Statement.t Node.t) =
    match node.branch with
    | Some (Node.Branch.Conditional_jump { true_; false_ }) -> (
        match Statement.branch_type node.stmt with
        (* if a conditional jump statement was simplified to an unconditional
           jump, then the condition was determined to always be true *)
        | Some Unconditional_jump ->
            (* delete this node from the false branch's references *)
            false_.references <-
              List.filter false_.references ~f:(fun ref ->
                  not (String.equal ref.from.label node.label));
            (* correct this node's branch to be unconditional to the true branch *)
            node.branch <- Some (Unconditional_jump true_);
            true
        (* if a conditional jump statement was simplified to a fallthrough,
            (definitely a nop), then the condition was determined to always be false *)
        | Some Fallthrough ->
            assert (Statement.is_nop node.stmt);
            (* delete this node from the true branch's references *)
            true_.references <-
              List.filter true_.references ~f:(fun ref ->
                  not (String.equal ref.from.label node.label));
            (* correct this node's branch to be fallthrough *)
            node.branch <- Some (Fallthrough false_);
            true
        | _ -> false)
    | _ -> false

  let rec optimize optimizer graph =
    Inplace_optimizer_util.optimize_until_unchanging (optimize' optimizer) graph

  and optimize' optimizer graph =
    Hashtbl.fold (Graph.nodes graph) ~init:false
      ~f:(fun ~key:_ ~data:node changed_in_past ->
        let stmt, did_optimize = Optimizer.optimize optimizer node.stmt in
        node.stmt <- stmt;
        let did_update = update_branch node in
        changed_in_past || did_update || did_optimize)
end
