open! Core

module type S = sig
  type t [@@deriving sexp_of, equal]
  type lhs

  val contains : t -> lhs -> bool
end
