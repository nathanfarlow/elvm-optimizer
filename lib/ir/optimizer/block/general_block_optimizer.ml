open Core

(* This optimizer simplifies each statement and fixes the branch when
   the conditional jump statement was simplified to an unconditional jump. *)
module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) =
struct
  type t = Statement_optimizer.t

  let optimize_statements stmt_optimizer block =
    let statements, statement_changes =
      Block.statements block
      |> List.map ~f:(Statement_optimizer.optimize stmt_optimizer)
      |> List.unzip
    in
    let did_change = List.exists statement_changes ~f:Fn.id in
    (statements, did_change)

  let correct_branch block =
    let maybe_jump = List.last (Block.statements block) in
    let branch = Block.branch block in
    match (maybe_jump, branch) with
    (* if a conditional jump statement was simplified to an unconditional
       jump, correct the branch to the unconditionally true branch *)
    | ( Some (Statement.Jump { condition = None; _ }),
        Some (Conditional_jump { true_; _ }) ) ->
        (Some (Block.Unconditional_jump true_), true)
    | _ -> (branch, false)

  let optimize' stmt_optimizer block =
    let label = Block.label block in
    let in_edges = Block.in_edges block in
    let statements, stmts_changed = optimize_statements stmt_optimizer block in
    let branch, branch_changed = correct_branch block in
    let block = Block.create ~label ~statements ~in_edges ~branch in
    (block, stmts_changed || branch_changed)

  let optimize stmt_optimizer block =
    Optimizer_util.optimize_until_unchanging (optimize' stmt_optimizer) block

  let create = Fn.id
end
