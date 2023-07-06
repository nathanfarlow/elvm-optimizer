module Make (Block_optimizer : Optimizer_intf.S with type target := Block.t) =
struct
  type t = Block_optimizer.t

  let optimize _ = failwith "TODO"
  let create block_opt = block_opt
end
