let optimize_branch optimize_block = function
  | None -> false
  | Some (Block.Branch.Unconditional_jump target) -> optimize_block target
  | Some (Fallthrough target) -> optimize_block target
  | Some (Call target) -> optimize_block target
  | Some (Conditional_jump { true_; false_ }) ->
      optimize_block true_ || optimize_block false_