module Make
    (Statement : Statement_mapping_intf.S)
    (Lhs : Lhs_intf.S with type t = Statement.lhs and type rhs := Statement.rhs)
    (Rhs : Rhs_intf.S with type t = Statement.rhs and type lhs := Statement.lhs) =
struct
  module Lhs_map = Map.Make (Lhs)

  type t = Rhs.t Lhs_map.t
  type update_result = { valid : t; invalid : t }

  let empty = Lhs_map.empty

  let update mappings var expr =
    let invalid, valid =
      Lhs_map.partitioni_tf mappings ~f:(fun ~key ~data ->
          Lhs.contains var key || Rhs.contains data var)
    in
    let valid = Lhs_map.add_exn valid ~key:var ~data:expr in
    { valid; invalid }

  let merge t1 t2 =
    (* only keep entries for which t1 and t2 which are exactly the same. *)
    Lhs_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Left _ | `Right _ -> None
      | `Both (e1, e2) -> if Rhs.equal e1 e2 then Some e1 else None)

  let get t var = Lhs_map.find t var
  let to_alist t = Lhs_map.to_alist t ~key_order:`Decreasing
end
