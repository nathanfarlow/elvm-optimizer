module type S = sig
  type t [@@deriving sexp, equal, compare]

  val contains : t -> t -> bool
end