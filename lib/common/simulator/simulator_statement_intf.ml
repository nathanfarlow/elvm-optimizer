module type S = sig
  type t
  type lhs
  type rhs
  type assignment = { from : lhs; to_ : rhs } [@@deriving sexp]

  val get_assignment : t -> assignment option
end