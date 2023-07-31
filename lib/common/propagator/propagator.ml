module Make
    (Statement : Propagator_statement_intf.S)
    (Var : Propagator_var_intf.S with type t = Statement.var)
    (Exp : Propagator_exp_intf.S
             with type t = Statement.exp
              and type var := Var.t) =
struct
  module Mapping = Propagator_mapping.Make (Var) (Exp)

  type t = unit

  let create = Fn.id

  let substitute_all stmt mappings =
    List.fold (Mapping.to_alist mappings) ~init:(stmt, false)
      ~f:(fun (stmt, did_substitute) (left, right) ->
        let stmt', did_substitute' =
          Statement.substitute_var_to_exp stmt ~from:left ~to_:right
        in
        (stmt', did_substitute || did_substitute'))

  let get_end_mappings get_prelim_mappings node =
    let prelim = get_prelim_mappings node in
    let stmt, _ = substitute_all (Node.stmt node) prelim in
    match Statement.get_mapping_from_assignment stmt with
    | Some { from; to_ } -> (Mapping.update prelim ~from ~to_).valid
    | None -> prelim

  let make_get_prelim_mappings () =
    Graph_util.memoize
      ~f:(fun node get_prelim_mappings ->
        let mappings =
          List.map (Node.references node) ~f:(fun parent ->
              get_end_mappings get_prelim_mappings parent.from)
          |> List.reduce ~f:Mapping.merge
          |> Option.value ~default:Mapping.empty
        in
        mappings)
      ~on_cycle:(fun _ -> Mapping.empty)

  let prepend_assignment graph node left right =
    let stmt = Statement.from_mapping { from = left; to_ = right } in
    let label = Graph.fresh_label graph in
    let new_node = Node.create ~label ~stmt in
    Node.prepend_node node new_node;
    Graph.register_node graph new_node

  let invalidate_mappings graph node prelim =
    match Statement.get_mapping_from_assignment (Node.stmt node) with
    | Some { from; to_ } ->
        (* delete the assignment, since we have it in our mappings now. *)
        Node.set_stmt node Statement.nop;
        (* write down the mappings that were invalidated by this assignment,
           unless said mapping is writing to the same variable. *)
        let Mapping.{ invalid; _ } = Mapping.update prelim ~from ~to_ in
        Mapping.to_alist invalid
        |> List.filter ~f:(fun (left, _) -> not (Var.equal left from))
        |> List.iter ~f:(fun (left, right) ->
               prepend_assignment graph node left right)
    | None -> ()

  let optimize_node graph node get_prelim_mappings =
    let prelim = get_prelim_mappings node in
    let stmt = Node.stmt node in
    match Node.branch node with
    | None ->
        (* write down all mappings when we reach a terminal node, like a jump register *)
        Mapping.to_alist prelim
        |> List.iter ~f:(fun (left, right) ->
               prepend_assignment graph node left right);
        false
    | Some _ ->
        (* update the statement in this node to reflect the mappings *)
        let updated_stmt, did_update = substitute_all stmt prelim in
        Node.set_stmt node updated_stmt;
        (* write down mappings which were invalidated from an assignment of this statement *)
        invalidate_mappings graph node prelim;
        did_update

  let optimize _ graph =
    let get_prelim_mappings = make_get_prelim_mappings () in
    let copy = Hashtbl.copy (Graph.nodes graph) in
    (* precompute preliminary mappings of each node so we can mutate the graph *)
    Hashtbl.iter copy ~f:(fun node -> ignore @@ get_prelim_mappings node);
    Hashtbl.fold copy ~init:false ~f:(fun ~key:_ ~data:node acc ->
        optimize_node graph node get_prelim_mappings || acc)
end
