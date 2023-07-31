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

  (* type op = Merge of (op * op) | Intersection of (op * op) | Empty | Mapping of Mapping.t *)

  let make_mappings () =
    (* gets the mappings which are living at the end of this node *)
    let get_end_mappings get_prelim_mappings node =
      let prelim_mappings = get_prelim_mappings node in
      let local_mapping =
        substitute_all (Node.stmt node)
          (Option.value prelim_mappings ~default:Mapping.empty)
        |> fun (stmt, _) -> Statement.get_mapping_from_assignment stmt
      in
      match (prelim_mappings, local_mapping) with
      | Some prelim_mappings, Some { from; to_ } ->
          Some Mapping.(update prelim_mappings ~from ~to_).valid
      | Some prelim_mappings, None -> Some prelim_mappings
      | None, Some { from; to_ } -> Some Mapping.(update empty ~from ~to_).valid
      | None, None -> None
    in
    (* gets the mappings which are living at the start of this node *)
    let get_prelim_mappings =
      Graph_util.memoize
        ~f:(fun node get_prelim_mappings ->
          Node.references node
          |> List.map ~f:(fun Node.Reference.{ from; _ } ->
                 get_end_mappings get_prelim_mappings from)
          |> List.filter_opt
          |> List.reduce ~f:Mapping.intersection)
        ~on_cycle:(fun _ -> None)
    in
    ( (fun node ->
        get_prelim_mappings node |> Option.value ~default:Mapping.empty),
      fun node ->
        get_end_mappings get_prelim_mappings node
        |> Option.value ~default:Mapping.empty )

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

  let optimize_node graph node get_prelim_mappings get_end_mappings =
    let end_mappings = get_end_mappings node in
    let obligations = get_branch_dependencies node get_prelim_mappings in
    let valid = Mapping.intersection obligations end_mappings in
    let invalid = Mapping.diff end_mappings valid |> Mapping.to_alist in
    let invalid' =
      match Statement.get_mapping_from_assignment (Node.stmt node) with
      | Some { from; _ } ->
          (* nop out assignment, since assignment is known in mappings *)
          Node.set_stmt node Statement.nop;
          (* don't write down the previous mapping of this variable,
             since the mappings already reflect the change*)
          List.filter invalid ~f:(fun (left, _) -> not (Var.equal left from))
      | _ -> invalid
    in
    (* write down invalidated mappings *)
    List.iter invalid' ~f:(fun (left, right) ->
        prepend_assignment graph node left right);
    (* update statement according to valid mappings *)
    let updated_stmt, did_update = substitute_all (Node.stmt node) valid in
    Node.set_stmt node updated_stmt;
    did_update

  let optimize _ graph =
    let get_prelim_mappings, get_end_mappings = make_mappings () in
    (* shallow copy graph nodes map so we can mutate graph as we fold *)
    Hashtbl.copy (Graph.nodes graph)
    |> Hashtbl.fold ~init:false ~f:(fun ~key:_ ~data:node acc ->
           optimize_node graph node get_prelim_mappings get_end_mappings || acc)
end
