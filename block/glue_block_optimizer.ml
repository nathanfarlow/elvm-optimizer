open Core

module Make (Reference_provider : Reference_provider_intf.S) = struct
  type t = Reference_provider.t

  let has_fallthrough_in_edge block =
    List.exists (Block.in_edges block) ~f:(function
      | { type_ = Fallthrough; _ } -> true
      | _ -> false)

  let map_block_in_edges block ~f =
    let in_edges = List.map (Block.in_edges block) ~f in
    Block.create ~label:(Block.label block) ~statements:(Block.statements block)
      ~in_edges ~branch:(Block.branch block)

  let rec optimize ref_provider block =
    Optimizer_util.optimize_until_unchanging (optimize' ref_provider) block

  and optimize' ref_provider block =
    match Block.branch block with
    | Some (Unconditional_jump target)
    (* if this block jumps unconditionally to target and target
       does not have any fallthrough in edges, then we can change
       the jump to fallthrough to target, saving a jump instruction *)
      when not @@ has_fallthrough_in_edge target ->
        let target, _ = optimize ref_provider target in
        (change_jump_to_fallthrough block target, true)
    | Some (Fallthrough target)
    (* if there are no references to target, it implies that target has only
       one in-edge, which is this block. As a result, we can concatenate
       these blocks *)
      when not
           @@ Reference_provider.has_reference ref_provider (Block.label target)
      ->
        let target, _ = optimize ref_provider target in
        (concatenate block target, true)
    | _ ->
        Block_optimizer_util.block_with_optimized_branch (optimize ref_provider)
          block

  and change_jump_to_fallthrough block target =
    let label = Block.label block in
    let target =
      (* update the target's in-edge for this block to be a fallthrough edge *)
      map_block_in_edges target ~f:(function
        | { target; _ } when String.equal target label ->
            { target; type_ = Fallthrough }
        | _ as e -> e)
    in
    (* update this block's branch to be fallthrough to target *)
    let branch = Some (Block.Fallthrough target) in
    (* drop the jump instruction *)
    let statements = Block.statements block |> List.drop_last_exn in
    let in_edges = Block.in_edges block in
    Block.create ~label ~statements ~in_edges ~branch

  and concatenate block target =
    let label = Block.label block in
    let statements = Block.statements block @ Block.statements target in
    let in_edges = Block.in_edges block in
    (* update this block's branch to be target's branch, but with
       target's target's in-edges updated to this block *)
    let branch =
      let update_in_edges targets_target =
        map_block_in_edges targets_target ~f:(fun edge ->
            match edge with
            | { target = target_label; type_ }
              when String.equal target_label (Block.label target) ->
                { target = label; type_ }
            | _ as e -> e)
      in
      match Block.branch target with
      | Some (Unconditional_jump target) ->
          Some (Block.Unconditional_jump (update_in_edges target))
      | Some (Conditional_jump { true_; false_ }) ->
          let true_ = update_in_edges true_ in
          let false_ = update_in_edges false_ in
          Some (Conditional_jump { true_; false_ })
      | Some (Fallthrough target) -> Some (Fallthrough (update_in_edges target))
      | None -> None
    in
    Block.create ~label ~statements ~in_edges ~branch

  let create = Fn.id
end
