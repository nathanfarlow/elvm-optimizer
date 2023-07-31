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
          |> List.reduce ~f:Mapping.intersection
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

  let get_branch_dependencies node get_prelim_mappings =
    Graph_util.get_all_branch_targets node
    |> List.map ~f:get_prelim_mappings
    |> List.reduce ~f:Mapping.intersection
    |> Option.value ~default:Mapping.empty

  let optimize_node graph node get_prelim_mappings =
    let end_mappings = get_end_mappings get_prelim_mappings node in
    let obligations = get_branch_dependencies node get_prelim_mappings in
    let valid = Mapping.intersection obligations end_mappings in
    let invalid = Mapping.diff end_mappings valid |> Mapping.to_alist in
    let invalid' =
      match Statement.get_mapping_from_assignment (Node.stmt node) with
      | Some { from; _ } ->
          (* nop out assignment while we're here *)
          Node.set_stmt node Statement.nop;
          (* don't include the current assignment variable in the invalid list *)
          List.filter invalid ~f:(fun (left, _) -> not (Var.equal left from))
      | _ -> invalid
    in
    (* prepend invalidated mappings *)
    List.iter invalid' ~f:(fun (left, right) ->
        prepend_assignment graph node left right);
    (* update statement according to existing preliminary mappings *)
    let updated_stmt, did_update = substitute_all (Node.stmt node) valid in
    Node.set_stmt node updated_stmt;
    did_update

  let optimize _ graph =
    let get_prelim_mappings = make_get_prelim_mappings () in
    let copy = Hashtbl.copy (Graph.nodes graph) in
    (* precompute preliminary mappings of each node so we can mutate the graph *)
    Hashtbl.iter copy ~f:(fun node -> ignore @@ get_prelim_mappings node);
    Hashtbl.fold copy ~init:false ~f:(fun ~key:_ ~data:node acc ->
        optimize_node graph node get_prelim_mappings || acc)
end
