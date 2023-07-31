module Make
    (Var : Propagator_var_intf.S)
    (Exp : Propagator_exp_intf.S with type var := Var.t) =
struct
  module Var_map = Map.Make (Var)

  type t = Exp.t Var_map.t [@@deriving sexp]
  type update_result = { valid : t; invalid : t }

  let empty = Var_map.empty

  let update t ~from ~to_ =
    (* a mapping is invalidated if either its var or exp contains the
        new mapping's var *)
    let invalid, valid =
      Var_map.partitioni_tf t ~f:(fun ~key ~data ->
          Var.contains key from || Exp.contains data from)
    in
    let valid = Var_map.add_exn valid ~key:from ~data:to_ in
    { valid; invalid }

  let intersection t1 t2 =
    (* only keep entries from t1 and t2 which are identical *)
    Var_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Both (e1, e2) when Exp.equal e1 e2 -> Some e1 | _ -> None)

  let diff t1 t2 =
    (* only keep entries from t1 which are not identical in t2 *)
    Var_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Both (e1, e2) when not @@ Exp.equal e1 e2 -> Some e1
      | `Left e -> Some e
      | _ -> None)

  let get t var = Var_map.find t var
  let to_alist t = Var_map.to_alist t ~key_order:`Decreasing
end
