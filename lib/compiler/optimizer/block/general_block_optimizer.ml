(* This optimizer simplifies each statement and fixes the branch when
   the conditional jump statement was simplified to an unconditional jump. *)
module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) =
struct
  type t = Statement_optimizer.t

  let optimize_statements stmt_optimizer block =
    let open Block.M in
    Array.foldi block.statements ~init:false ~f:(fun i changed stmt ->
        let stmt', changed' =
          Statement_optimizer.optimize stmt_optimizer stmt
        in
        block.statements.(i) <- stmt';
        changed || changed')

  let correct_branch block =
    let open Block.M in
    let maybe_jump =
      if Array.is_empty block.statements then None
      else Some (Array.last block.statements)
    in
    match (maybe_jump, block.branch) with
    (* if a conditional jump statement was simplified to an unconditional
       jump: *)
    | ( Some (Statement.Jump { cond = None; _ }),
        Some (Conditional_jump { true_; false_ }) ) ->
        (* delete this block from the false branch's in edges *)
        (* print false_.in_edges *)
        false_.in_edges <-
          List.filter false_.in_edges ~f:(fun edge ->
              not (String.equal edge.label block.label));
        (* correct this block's branch to be unconditional to the true branch *)
        block.branch <- Some (Branch.Unconditional_jump true_);
        true
    | _ -> false

  let rec optimize stmt_optimizer block =
    Inplace_optimizer_util.optimize_until_unchanging (optimize' stmt_optimizer)
      block

  and optimize' stmt_optimizer block =
    let stmts_changed = optimize_statements stmt_optimizer block in
    let branch_corrected = correct_branch block in
    let branch_optimized =
      Block_optimizer_util.optimize_branch (optimize stmt_optimizer)
        block.branch
    in
    stmts_changed || branch_corrected || branch_optimized

  let create = Fn.id
end