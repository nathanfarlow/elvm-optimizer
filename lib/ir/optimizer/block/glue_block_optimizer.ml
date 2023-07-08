open Core

module Make (Reference_provider : Reference_provider_intf.S) = struct
  type t = Reference_provider.t

  let has_fallthrough_in_edge block =
    let open Block.M in
    List.exists block.in_edges ~f:(function
      | { type_ = Fallthrough; _ } -> true
      | _ -> false)

  let rec optimize ref_provider block =
    Inplace_optimizer_util.optimize_until_unchanging (optimize' ref_provider)
      block

  and optimize' ref_provider block =
    let open Block.M in
    match block.branch with
    | Some (Unconditional_jump target)
    (* if this block jumps unconditionally to target and target
       does not have any fallthrough in edges, then we can change
       the jump to fallthrough to target, saving a jump instruction *)
      when not @@ has_fallthrough_in_edge target ->
        ignore @@ optimize ref_provider target;
        change_jump_to_fallthrough block target;
        true
    | Some (Fallthrough target)
    (* if there are no references to target, it implies that target has only
       one in-edge, which is this block. As a result, we can concatenate
       these blocks *)
      when not @@ Reference_provider.has_reference ref_provider target.label ->
        ignore @@ optimize ref_provider target;
        concatenate block target;
        true
    | _ -> false
  (* Block_optimizer_util.optimize_branch (optimize ref_provider)
     block.branch *)

  and change_jump_to_fallthrough block target =
    (* update the target's in-edge for this block to be a fallthrough edge *)
    let module B = Block.M in
    target.in_edges <-
      List.map target.in_edges ~f:(function
        | { label; _ } as e when String.equal label block.label ->
            B.Edge.{ e with type_ = Fallthrough }
        | _ as e -> e);
    (* update this block's branch to be fallthrough to target *)
    block.branch <- Some (B.Branch.Fallthrough target);
    (* drop the jump instruction *)
    block.statements <- Array.slice block.statements 0 (-1)

  and concatenate block target =
    let open Block.M in
    block.statements <- Array.append block.statements target.statements;
    (* update target's target's in-edges to this block *)
    let update_in_edges targets_target =
      targets_target.in_edges <-
        List.map targets_target.in_edges ~f:(function
          | { label; _ } as e when String.equal label target.label ->
              Edge.{ e with label = block.label }
          | _ as e -> e)
    in
    (* update this block's branch to be target's branch *)
    block.branch <-
      (match target.branch with
      | Some (Unconditional_jump target) ->
          update_in_edges target;
          Some (Branch.Unconditional_jump target)
      | Some (Conditional_jump { true_; false_ }) ->
          update_in_edges true_;
          update_in_edges false_;
          Some (Conditional_jump { true_; false_ })
      | Some (Fallthrough target) ->
          update_in_edges target;
          Some (Fallthrough target)
      | None -> None)

  let create = Fn.id
end
