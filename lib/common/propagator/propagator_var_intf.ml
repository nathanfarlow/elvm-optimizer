module type S = sig
  type t

  val compare : t -> t -> int
  val contains : t -> t -> bool

  include Sexpable.S with type t := t
end