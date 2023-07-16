module Make
    (Statement : Propagator_statement_intf.S)
    (Lhs : Propagator_lhs_intf.S with type t = Statement.lhs)
    (Rhs : Propagator_rhs_intf.S
             with type t = Statement.rhs
              and type lhs := Statement.lhs) : sig
  type t

  val create : unit -> t

  include
    Inplace_optimizer_intf.S
      with type t := t
       and type target := Statement.t Graph.t
end = struct
  module Mapping = Propagator_mapping.Make (Lhs) (Rhs)

  type t = unit

  let create = Fn.id

  let memoize ~f ~on_cycle =
    let memo = Hashtbl.create (module String) in
    let rec g node =
      match Hashtbl.find memo (Node.label node) with
      | Some res -> res
      | None ->
          Hashtbl.set memo ~key:(Node.label node) ~data:on_cycle;
          f g node
    in
    g

  let substitute_all stmt mappings =
    let mappings = Mapping.to_alist mappings in
    List.fold mappings ~init:(stmt, false) ~f:(fun acc (left, right) ->
        let stmt, has_substituted_already = acc in
        let stmt, just_substituted =
          Statement.substitute stmt { from = left; to_ = right }
        in
        (stmt, has_substituted_already || just_substituted))

  let get_end_mappings get_prelim_mappings node =
    let prelim = get_prelim_mappings node in
    let stmt, _ = substitute_all (Node.stmt node) prelim in
    match Statement.get_mapping_from_assignment stmt with
    | Some { from; to_ } -> (Mapping.update prelim ~from ~to_).valid
    | None -> prelim

  let get_prelim_mappings =
    memoize
      ~f:(fun get_prelim_mappings node ->
        List.fold (Node.references node) ~init:Mapping.empty
          ~f:(fun acc parent ->
            Mapping.merge acc (get_end_mappings get_prelim_mappings parent.from)))
      ~on_cycle:Mapping.empty

  let prepend_assignment graph node left right =
    let stmt = Statement.from_mapping { from = left; to_ = right } in
    let label = Graph.fresh_label graph in
    let new_node = Node.create ~label ~stmt in
    Node.prepend_node node new_node;
    Graph.register_node graph new_node

  let invalidate_mappings graph node prelim =
    match Statement.get_mapping_from_assignment (Node.stmt node) with
    | Some { from; to_ } ->
        let Mapping.{ valid; invalid } = Mapping.update prelim ~from ~to_ in
        let invalid_as_list = Mapping.to_alist invalid in
        List.iter invalid_as_list ~f:(fun (left, right) ->
            prepend_assignment graph node left right);
        (valid, List.length invalid_as_list > 0)
    | None -> (prelim, false)

  let update_statement node mappings =
    let updated_stmt, did_update = substitute_all (Node.stmt node) mappings in
    Node.set_stmt node updated_stmt;
    did_update

  let optimize_node graph (node : Statement.t Node.t) =
    let prelim = get_prelim_mappings node in
    let did_update = update_statement node prelim in
    let _, did_prepend = invalidate_mappings graph node prelim in
    did_prepend || did_update

  let optimize _ graph =
    Hashtbl.fold (Graph.nodes graph) ~init:false
      ~f:(fun ~key:_ ~data:node acc -> optimize_node graph node || acc)
end