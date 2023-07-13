module type S = sig
  type t
  type target

  val optimize : t -> target -> bool
end