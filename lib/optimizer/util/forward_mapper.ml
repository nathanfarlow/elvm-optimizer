module Make
    (Variable : Forward_variable_intf.S)
    (Expression : Forward_expression_intf.S with type variable := Variable.t) =
struct
  module Variable_map = Map.Make (Variable)

  type t = Expression.t Variable_map.t
  type update_result = { valid : t; invalid : t }

  let empty = Variable_map.empty

  let update mappings var expr =
    let invalid, valid =
      Variable_map.partitioni_tf mappings ~f:(fun ~key ~data ->
          Variable.contains_var var key || Expression.contains_var data var)
    in
    let valid = Variable_map.add_exn valid ~key:var ~data:expr in
    { valid; invalid }

  let merge t1 t2 =
    (* only keep entries for which t1 and t2 which are exactly the same. *)
    Variable_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Left _ | `Right _ -> None
      | `Both (e1, e2) -> if Expression.equal e1 e2 then Some e1 else None)

  let get t var = Variable_map.find t var
end
