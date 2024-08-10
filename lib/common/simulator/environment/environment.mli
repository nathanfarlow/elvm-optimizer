open! Core

module Make
    (Lhs : Environment_lhs_intf.S)
    (Rhs : Environment_rhs_intf.S with type lhs := Lhs.t) : sig
  type t [@@deriving sexp]

  include
    Environment_intf.S
      with type t := t
       and type lhs := Lhs.t
       and type rhs := Rhs.t
end
