module type S = sig
  type t
  type lhs

  module Lhs : Liveness_analyzer_lhs_intf.S with type t = lhs
  module Lhs_set : module type of Set.Make (Lhs)

  val get_all_lhs_dependencies : t -> Lhs_set.t
end