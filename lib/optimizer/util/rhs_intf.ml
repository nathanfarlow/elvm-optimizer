module type S = sig
  type t
  type lhs

  val contains : t -> lhs -> bool
  val substitute : t -> lhs -> t -> t

  include Equal.S with type t := t
end