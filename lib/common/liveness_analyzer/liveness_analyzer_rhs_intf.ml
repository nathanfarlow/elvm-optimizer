module type S = sig
  type t
  type lhs

  val get_all_lhs_dependencies : t -> lhs list
end