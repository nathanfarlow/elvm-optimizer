open! Core

module Branch_type = struct
  type t =
    | Unconditional_jump
    | Conditional_jump
    | Fallthrough
  [@@deriving sexp]
end

module type S = sig
  type t

  val is_nop : t -> bool
  val branch_type : t -> Branch_type.t option
end
