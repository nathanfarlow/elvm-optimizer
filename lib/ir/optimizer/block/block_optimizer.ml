open Core

module Make
    (Statement_optimizer : Optimizer_intf.S with type target := Statement.t) =
struct
  type t = Statement_optimizer.t

  (* optimization pipeline:
      general:
        recursive simplify branch targets
        simplify every statement

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

  let optimize _ _ = failwith "unimplemented"
  let create = Fn.id
end
