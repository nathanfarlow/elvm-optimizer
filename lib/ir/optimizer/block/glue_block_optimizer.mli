open! Core

module Make (Reference_provider : Reference_provider_intf.S) : sig
  type t

  include Inplace_optimizer_intf.S with type t := t and type target := Block.M.t

  val create : Reference_provider.t -> t
end