open Core

module type S = sig
  type t

  val has_reference : t -> string -> bool
  val all_references : t -> string Hash_set.t
end