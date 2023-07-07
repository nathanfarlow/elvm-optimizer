(* optimization pipeline:
    general:

    glue:

    copy prop

    common subexpr elim

    dead code:
      remove nops
      remove everything after exit
      remove dead assignments

    advanced:
      swap 2 isntructions if they are same value and
      one is more efficient to compute
*)

open Core

let optimize_branch optimize_block = function
  | None -> (None, false)
  | Some (Block.Unconditional_jump target) ->
      let target, target_changed = optimize_block target in
      (Some (Block.Unconditional_jump target), target_changed)
  | Some (Fallthrough target) ->
      let target, target_changed = optimize_block target in
      (Some (Fallthrough target), target_changed)
  | Some (Conditional_jump { true_; false_ }) ->
      let true_, true_changed = optimize_block true_ in
      let false_, false_changed = optimize_block false_ in
      (Some (Conditional_jump { true_; false_ }), true_changed || false_changed)

let block_with_optimized_branch optimize_block block =
  let branch, branch_changed =
    optimize_branch optimize_block (Block.branch block)
  in
  ( Block.create ~label:(Block.label block) ~statements:(Block.statements block)
      ~in_edges:(Block.in_edges block) ~branch,
    branch_changed )
