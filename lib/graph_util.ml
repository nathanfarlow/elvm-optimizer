open Core

let memoize ~f ~on_cycle =
  let memo = Hashtbl.create (module String) in
  let evaluating = Hash_set.create (module String) in
  let rec g node =
    let label = Node.label node in
    match Hashtbl.find memo label with
    | Some res -> res
    | None ->
      if Hash_set.mem evaluating label
      then on_cycle node
      else (
        Hash_set.add evaluating label;
        let data = f node g in
        Hash_set.remove evaluating label;
        (* don't cache computations if we are still evaluating another node.
           this ensures consistency across the cached results *)
        if Hash_set.is_empty evaluating then Hashtbl.set memo ~key:label ~data;
        data)
  in
  g
;;

let get_all_branch_targets node =
  match Node.branch node with
  | None -> []
  | Some (Fallthrough target) | Some (Unconditional_jump target) -> [ target ]
  | Some (Conditional_jump { true_; false_ }) -> [ true_; false_ ]
;;
