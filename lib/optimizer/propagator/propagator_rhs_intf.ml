module type S = sig
  type t
  type lhs

  val contains : t -> lhs -> bool

  include Equal.S with type t := t
end