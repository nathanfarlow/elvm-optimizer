open Core

module rec Node : sig
  type 'a t

  val create : label:string -> stmt:'a -> 'a t
  val label : 'a t -> string
  val stmt : 'a t -> 'a
  val references : 'a t -> 'a Reference.t list
  val branch : 'a t -> 'a Branch.t option
  val set_stmt : 'a t -> 'a -> unit
  val set_references : 'a t -> 'a Reference.t list -> unit
  val add_reference : 'a t -> 'a Reference.t -> unit
  val set_branch : 'a t -> 'a Branch.t option -> unit
  val prepend_node : 'a t -> 'a t -> unit
  val detach : 'a t -> unit
  val is_top_level : 'a t -> bool
end = struct
  type 'a t =
    { label : string
    ; mutable stmt : 'a
    ; mutable references : 'a Reference.t list
    ; mutable branch : 'a Branch.t option
    }
  [@@deriving fields]

  let create ~label ~stmt = { label; stmt; references = []; branch = None }
  let label t = t.label
  let stmt t = t.stmt
  let references t = t.references
  let branch t = t.branch
  let set_stmt t stmt = t.stmt <- stmt
  let set_references t references = t.references <- references
  let add_reference t reference = t.references <- reference :: t.references
  let set_branch t branch = t.branch <- branch

  let update_branch_target t ~old ~new_ =
    t.branch
    <- (match t.branch with
        | Some (Unconditional_jump target) when String.equal target.label old.label ->
          Some (Unconditional_jump new_)
        | Some (Conditional_jump { true_; false_ })
          when String.equal true_.label old.label && String.equal false_.label old.label
          -> Some (Conditional_jump { true_ = new_; false_ = new_ })
        | Some (Conditional_jump { true_; false_ })
          when String.equal true_.label old.label ->
          Some (Conditional_jump { true_ = new_; false_ })
        | Some (Conditional_jump { true_; false_ })
          when String.equal false_.label old.label ->
          Some (Conditional_jump { true_; false_ = new_ })
        | Some (Fallthrough target) when String.equal target.label old.label ->
          Some (Fallthrough new_)
        | _ -> t.branch)
  ;;

  let prepend_node t other =
    List.map t.references ~f:(fun r -> r.from)
    |> List.iter ~f:(update_branch_target ~old:t ~new_:other);
    other.references <- t.references;
    other.branch <- Some (Fallthrough t);
    t.references <- [ { from = other; type_ = Fallthrough } ]
  ;;

  let detach t =
    match t.branch with
    | Some (Fallthrough target) ->
      (* update t's parents to jump to target instead *)
      List.map t.references ~f:(fun r -> r.from)
      |> List.iter ~f:(update_branch_target ~old:t ~new_:target);
      (* update target's references to include t's parents *)
      target.references
      <- target.references @ t.references
         (* delete the fallthrough reference from t *)
         |> List.filter ~f:(fun Reference.{ from; _ } ->
           not (String.equal from.label t.label))
         (* deduplicate in case of conditional jump to both t and target *)
         |> List.dedup_and_sort ~compare:Reference.compare;
      (* cleanup t *)
      t.references <- [];
      t.branch <- None
    | _ -> failwith "Cannot detach a node that doesn't have a fallthrough"
  ;;

  let is_top_level t =
    match t.references with
    | [] -> true
    | [ { from; type_ = Jump } ] -> String.equal from.label t.label
    | _ -> false
  ;;
end

and Reference : sig
  type type_ =
    | Jump
    | Fallthrough
  [@@deriving sexp, equal, compare]

  type 'a t =
    { from : 'a Node.t
    ; type_ : type_
    }

  val compare : 'a t -> 'a t -> int
end = struct
  type type_ =
    | Jump
    | Fallthrough
  [@@deriving sexp, equal, compare]

  type 'a t =
    { from : 'a Node.t
    ; type_ : type_
    }

  let compare t other =
    [%compare: string * type_]
      (Node.label t.from, t.type_)
      (Node.label other.from, other.type_)
  ;;
end

and Branch : sig
  type 'a t =
    | Unconditional_jump of 'a Node.t
    | Conditional_jump of
        { true_ : 'a Node.t
        ; false_ : 'a Node.t
        }
    | Fallthrough of 'a Node.t
end = struct
  type 'a t =
    | Unconditional_jump of 'a Node.t
    | Conditional_jump of
        { true_ : 'a Node.t
        ; false_ : 'a Node.t
        }
    | Fallthrough of 'a Node.t
end

include Node

module For_tests (Element : Sexpable.S) = struct
  let ref_to_string (ref : Element.t Reference.t) =
    sprintf
      "%s from %s"
      (Sexp.to_string_hum ([%sexp_of: Reference.type_] ref.type_))
      (Node.label ref.from)
  ;;

  let branch_to_string (branch : Element.t Branch.t) =
    match branch with
    | Unconditional_jump node -> "unconditional jump to " ^ Node.label node
    | Conditional_jump { true_; false_ } ->
      sprintf
        "conditional jump to true: %s false: %s"
        (Node.label true_)
        (Node.label false_)
    | Fallthrough node -> "fallthrough to " ^ Node.label node
  ;;

  let to_string node =
    (sprintf
       "%s: %s"
       (Node.label node)
       (Sexp.to_string_hum ([%sexp_of: Element.t] (Node.stmt node)))
     ::
     (if not @@ List.is_empty (Node.references node)
      then [ String_util.indent_string "references:" ~indent:1 ]
      else []))
    @ (Node.references node
       |> List.map ~f:ref_to_string
       |> List.map ~f:(String_util.indent_string ~indent:2))
    @ (if Option.is_some (Node.branch node)
       then [ String_util.indent_string "branch:" ~indent:1 ]
       else [])
    @ Option.value_map (Node.branch node) ~default:[] ~f:(fun branch ->
      [ branch_to_string branch |> String_util.indent_string ~indent:2 ])
    |> String.concat ~sep:"\n"
  ;;
end
