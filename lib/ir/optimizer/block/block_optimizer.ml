open Core

module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) =
struct
  module General_optimizer = General_block_optimizer.Make (Statement_optimizer)

  type t = { general_optimizer : General_optimizer.t }

  (* optimization pipeline:
      glue:
        if this block has unconditional branch and is the only in edge to
        target block, delete the jump instruction if one exists and
        glue this block to the target, returning one block

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

  let rec optimize' t block =
    let block, block_changed =
      List.fold
        [ General_optimizer.optimize t.general_optimizer ]
        ~init:(block, false)
        ~f:(fun (block, changed) f ->
          let block, changed' = f block in
          (block, changed || changed'))
    in
    let branch, branch_changed = optimize_branch t block in
    let block =
      Block.create ~label:(Block.label block)
        ~statements:(Block.statements block) ~in_edges:(Block.in_edges block)
        ~branch
    in
    (block, block_changed || branch_changed)

  and optimize_branch _t _block = assert false
  (* let open Block in
     match Block.branch block with
     | None -> (None, false)
     | Some (Unconditional target) ->
         let target, target_changed = optimize' t target in
         (Some (Unconditional target), target_changed)
     | Some (Conditional { true_; false_ }) ->
         let true_, true_changed = optimize' t true_ in
         let false_, false_changed = optimize' t false_ in
         (Some (Conditional { true_; false_ }), true_changed || false_changed) *)

  let optimize stmt_optimizer block =
    Optimizer_util.optimize_until_unchanging (optimize' stmt_optimizer) block

  let create stmt_optimizer =
    { general_optimizer = General_optimizer.create stmt_optimizer }
end
