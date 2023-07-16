let memoize ~f ~on_cycle =
  let memo = Hashtbl.create (module String) in
  let rec g node =
    match Hashtbl.find memo (Node.label node) with
    | Some res -> res
    | None ->
        Hashtbl.set memo ~key:(Node.label node) ~data:(on_cycle node);
        f node g
  in
  g