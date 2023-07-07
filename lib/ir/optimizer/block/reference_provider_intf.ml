open Core

module type S = sig
  type t

  val references : t -> string Hash_set.t
end