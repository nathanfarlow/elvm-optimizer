open! Core

module Make (Block_optimizer : Optimizer_intf.S with type target := Block.t) =
struct
  type t = Block_optimizer.t

  (* optimization ideas:
      delete blocks which have no references. keep children.
      delete data chunks which have no references *)

  let optimize _ = failwith "unimplemented"
  let create block_opt = block_opt
end
