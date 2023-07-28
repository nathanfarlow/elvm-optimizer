module Make
    (Var : Propagator_var_intf.S)
    (Exp : Propagator_exp_intf.S with type var := Var.t) =
struct
  module Var_map = Map.Make (Var)

  type t = Exp.t Var_map.t [@@deriving sexp]
  type update_result = { valid : t; invalid : t }

  let empty = Var_map.empty

  let update t ~from ~to_ =
    let invalid, valid =
      Var_map.partitioni_tf t ~f:(fun ~key ~data ->
          Var.contains from key || Exp.contains data from)
    in
    let valid = Var_map.add_exn valid ~key:from ~data:to_ in
    { valid; invalid }

  let merge t1 t2 =
    (* only keep entries for which t1 and t2 which are exactly the same. *)
    Var_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Left _ | `Right _ -> None
      | `Both (rhs1, rhs2) -> if Exp.equal rhs1 rhs2 then Some rhs1 else None)

  let get t var = Var_map.find t var
  let to_alist t = Var_map.to_alist t ~key_order:`Decreasing
end
