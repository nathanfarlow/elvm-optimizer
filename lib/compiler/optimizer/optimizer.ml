module Optimizer_intf = Optimizer_intf
module Expression_optimizer = Expression_optimizer
module Statement_optimizer = Statement_optimizer
module Inplace_optimizer_intf = Inplace_optimizer_intf

let optimize (_ir : Ir.t) : Ir.t = failwith "unimplemented"