open! Core

module Make (Reference_provider : Reference_provider_intf.S) : sig
  type t

  include Optimizer_intf.S with type t := t and type target := Block.t

  val create : Reference_provider.t -> t
end