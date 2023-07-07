open Core

(* gotta look for topmost nodes repeatedly
   delete the topmost nodes which don't have references *)

(* this optimizer will concatenate a and b if block a 1. a falls through to b,
    and 2. b's label is not referenced anywhere (implying b has only the one in-edge)

   this optimizer will delete the jump instruction from a and change branch to
   fallthrough if 1. a has an unconditional branch to b and 2. b does not have a fallthrough
   branch yet *)
module Make (Reference_provider : Reference_provider_intf.S) = struct
  type t = Reference_provider.t

  let optimize' _t _block = assert false

  let optimize t block =
    Optimizer_util.optimize_until_unchanging (optimize' t) block

  let create = Fn.id
end
