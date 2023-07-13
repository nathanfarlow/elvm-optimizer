module Branch_type = struct
  type t = Unconditional_jump | Conditional_jump | Fallthrough
end

module type S = sig
  type t

  val branch_type : t -> Branch_type.t option
  val is_nop : t -> bool
end