module type S = sig
  type t

  val contains_var : t -> t -> bool
  val compare : t -> t -> int

  include Sexpable.S with type t := t
end