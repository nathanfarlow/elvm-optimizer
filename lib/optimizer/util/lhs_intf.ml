module type S = sig
  type t
  type rhs

  val compare : t -> t -> int
  val contains : t -> t -> bool
  val substitute : t -> t -> rhs -> t

  include Sexpable.S with type t := t
end