module type S = sig
  type t
  type key
  type value
  type update_result = { valid : t; invalid : t }

  val empty : t
  val update : t -> key -> value -> update_result
  val merge : t -> t -> t
  val get : t -> key -> value option
end