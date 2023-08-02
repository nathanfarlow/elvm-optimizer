module Make
    (Lhs : Environment_lhs_intf.S)
    (Rhs : Environment_rhs_intf.S with type lhs := Lhs.t) =
struct
  module Lhs_map = Map.Make (Lhs)

  type t = Rhs.t Lhs_map.t [@@deriving sexp]
  type update_result = { valid : t; invalid : t }

  let empty = Lhs_map.empty

  let update t ~from ~to_ =
    (* a mapping is invalidated if either its rhs or lhs contains the
        new mapping's rhs *)
    let invalid, valid =
      Lhs_map.partitioni_tf t ~f:(fun ~key ~data ->
          Lhs.contains key from || Rhs.contains data from)
    in
    let valid = Lhs_map.add_exn valid ~key:from ~data:to_ in
    { valid; invalid }

  let intersection t1 t2 =
    (* only keep entries from t1 and t2 which are identical *)
    Lhs_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Both (e1, e2) when Rhs.equal e1 e2 -> Some e1 | _ -> None)

  let union t1 t2 =
    (* keep entries from t1 and t2, unless they are not identical *)
    Lhs_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Both (e1, e2) when Rhs.equal e1 e2 -> Some e1
      | `Left e | `Right e -> Some e
      | _ -> None)

  let diff t1 t2 =
    (* only keep entries from t1 which are not identical in t2 *)
    Lhs_map.merge t1 t2 ~f:(fun ~key:_ -> function
      | `Both (e1, e2) when not @@ Rhs.equal e1 e2 -> Some e1
      | `Left e -> Some e
      | _ -> None)

  let get t rhs = Lhs_map.find t rhs
  let to_alist t = Lhs_map.to_alist t ~key_order:`Decreasing
end
