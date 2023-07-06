open! Core

module Make (Optimizer : Optimizer_intf.S) : sig
  type t

  include Optimizer_intf.S with type t := t and type target := Optimizer.target

  val create : Optimizer.t list -> t
end
