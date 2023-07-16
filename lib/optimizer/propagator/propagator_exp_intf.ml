module type S = sig
  type t
  type var

  val contains : t -> var -> bool

  include Equal.S with type t := t
end