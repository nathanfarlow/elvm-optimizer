module type S = sig
  type t
  type variable

  val contains_var : t -> variable -> bool

  include Equal.S with type t := t
end