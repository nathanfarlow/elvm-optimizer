open! Core

module type S = sig
  type t [@@deriving sexp, equal]
  type lhs

  val contains : t -> lhs -> bool
end
