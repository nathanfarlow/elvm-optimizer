module type S = sig
  type t [@@deriving sexp]
  type lhs
  type rhs
  type update_result = { valid : t; invalid : t }

  val empty : t
  val update : t -> from:lhs -> to_:rhs -> update_result
  val intersection : t -> t -> t
  val diff : t -> t -> t
  val get : t -> lhs -> rhs option
  val to_alist : t -> (lhs * rhs) list
end