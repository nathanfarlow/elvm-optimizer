module Make
    (Statement : Propagator_statement_intf.S)
    (Mapping : Propagator_mapping_intf.S
                 with type key := Statement.var
                  and type value := Statement.exp) =
struct
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

  let optimize_node node get_prelim_mappings =
    let prelim = get_prelim_mappings node in
    let updated_stmt, did_update = substitute_all (Node.stmt node) prelim in
    Node.set_stmt node updated_stmt;
    did_update

  let optimize _ graph =
    let get_prelim_mappings = make_get_prelim_mappings () in
    Hashtbl.fold
      (Hashtbl.copy (Graph.nodes graph))
      ~init:false
      ~f:(fun ~key:_ ~data:node did_optimize ->
        optimize_node node get_prelim_mappings || did_optimize)
end
