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
  | None -> false
  | Some (Block.Branch.Unconditional_jump target) -> optimize_block target
  | Some (Fallthrough target) -> optimize_block target
  | Some (Conditional_jump { true_; false_ }) ->
      optimize_block true_ || optimize_block false_
