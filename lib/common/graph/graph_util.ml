let memoize ~f ~on_cycle =
  let memo = Hashtbl.create (module String) in
  let rec g node =
    let label = Node.label node in
    match Hashtbl.find memo label with
    | Some res -> res
    | None ->
        Hashtbl.set memo ~key:label ~data:(on_cycle node);
        let data = f node g in
        Hashtbl.set memo ~key:label ~data;
        data
  in
  g

let get_all_branch_targets node =
  match Node.branch node with
  | None -> []
  | Some (Fallthrough target) | Some (Unconditional_jump target) -> [ target ]
  | Some (Conditional_jump { true_; false_ }) -> [ true_; false_ ]
