module type S = sig
  type t [@@deriving sexp, equal]
  type var

  val contains : t -> var -> bool
end