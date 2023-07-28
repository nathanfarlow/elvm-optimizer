module type S = sig
  type t [@@deriving sexp, compare]

  val contains : t -> t -> bool
end