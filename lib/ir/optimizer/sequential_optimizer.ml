open! Core

module Make (Optimizer : Optimizer_intf.S) = struct
  type t = Optimizer.t list

  let create = Fn.id

  let optimize optimizers target =
    let optimize' target =
      List.fold optimizers ~init:(target, false)
        ~f:(fun (target, has_changed_before) opt ->
          let target, just_changed =
            Optimizer_util.optimize_until_unchanging (Optimizer.optimize opt)
              target
          in
          (target, has_changed_before || just_changed))
    in
    Optimizer_util.optimize_until_unchanging optimize' target
end
