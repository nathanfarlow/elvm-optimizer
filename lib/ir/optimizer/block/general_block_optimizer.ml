open Core

(* This optimizer simplifies each statement and fixes the branch when
   the conditional jump statement was simplified to an unconditional jump. *)
module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) =
struct
  type t = Statement_optimizer.t

  let optimize_statements stmt_optimizer (block : Block.M.t) =
    let statements, statement_changes =
      block.statements
      |> List.map ~f:(Statement_optimizer.optimize stmt_optimizer)
      |> List.unzip
    in
    block.statements <- statements;
    List.exists statement_changes ~f:Fn.id

  let correct_branch (block : Block.M.t) =
    let maybe_jump = List.last block.statements in
    match (maybe_jump, block.branch) with
    (* if a conditional jump statement was simplified to an unconditional
       jump, correct the branch to the unconditionally true branch *)
    | ( Some (Statement.Jump { condition = None; _ }),
        Some (Conditional_jump { true_; _ }) ) ->
        block.branch <- Some (Block.Branch.Unconditional_jump true_);
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
