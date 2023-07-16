module Make (Lhs : Lhs_intf.S) (Rhs : Rhs_intf.S with type lhs := Lhs.t) =
struct
  module Lhs_map = Map.Make (Lhs)

  type t = Rhs.t Lhs_map.t
  type update_result = { valid : t; invalid : t }

  let empty = Lhs_map.empty

  let update t ~from ~to_ =
    let invalid, valid =
      Lhs_map.partitioni_tf t ~f:(fun ~key ~data ->
          Lhs.contains from key || Rhs.contains data from)
    in
    let valid = Lhs_map.add_exn valid ~key:from ~data:to_ in
    { valid; invalid }

  let merge t1 t2 =
    (* only keep entries for which t1 and t2 which are exactly the same. *)
    Lhs_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Left _ | `Right _ -> None
      | `Both (rhs1, rhs2) -> if Rhs.equal rhs1 rhs2 then Some rhs1 else None)

  let get t var = Lhs_map.find t var
  let to_alist t = Lhs_map.to_alist t ~key_order:`Decreasing
end
