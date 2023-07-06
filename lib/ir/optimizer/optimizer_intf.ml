module type S = sig
  type t
  type target

  val optimize : t -> target -> target * bool
end