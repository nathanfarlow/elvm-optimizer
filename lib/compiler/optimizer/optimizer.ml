module Optimizer_intf = Optimizer_intf
module Expression_optimizer = Expression_optimizer
module General_block_optimizer = General_block_optimizer
module Concat_block_optimizer = Concat_block_optimizer
module Statement_optimizer = Statement_optimizer
module Inplace_optimizer_intf = Inplace_optimizer_intf
module Reference_provider_intf = Reference_provider_intf

let optimize (_ir : Ir.t) : Ir.t = failwith "unimplemented"