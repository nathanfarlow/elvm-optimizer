open! Core

module type S = sig
  type t [@@deriving sexp_of, equal, compare]

  val contains : t -> t -> bool
end
