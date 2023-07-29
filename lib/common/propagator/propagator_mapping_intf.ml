module type S = sig
  type t [@@deriving sexp]
  type key
  type value
  type update_result = { valid : t; invalid : t }

  val empty : t
  val update : t -> from:key -> to_:value -> update_result
  val merge : t -> t -> t
  val get : t -> key -> value option
  val to_alist : t -> (key * value) list
end