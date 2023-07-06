open Core

module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) =
struct
  type t = Statement_optimizer.t

  (* optimization ideas:
      recursive simplify branch targets

      simplify every statement

      if this block has unconditional branch and is the only in edge to
      target block, delete the jump instruction if one exists and
      glue this block to the target, returning one block

      dead code elimination:
        remove nops
        kill everything after exit
        copy propagation
        common subexpression elimination

      swap 2 isntructions if they are same value and
      one is more efficient to compute
  *)

  let optimize _ _ = failwith "TODO"
  let create = Fn.id
end
