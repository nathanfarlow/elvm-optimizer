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
  val is_top_level : 'a t -> bool

  module For_tests (Element : Sexpable.S) : sig
    val to_string : Element.t t -> string
  end
end = struct
  type 'a t = {
    label : string;
    mutable stmt : 'a;
    mutable references : 'a Reference.t list;
    mutable branch : 'a Branch.t option;
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
  let prepend_node _t _other = failwith "todo"

  let is_top_level t =
    match t.references with
    | [] -> true
    | [ { from; type_ = Jump } ] -> String.equal from.label t.label
    | _ -> false

  module For_tests (Element : Sexpable.S) = struct
    module Reference_tests = Reference.For_tests (Element)
    module Branch_tests = Branch.For_tests (Element)

    let to_string t =
      (t.label ^ ": " ^ ([%sexp_of: Element.t] t.stmt |> Sexp.to_string_hum))
      :: (if not @@ List.is_empty t.references then [ "references:" ] else [])
      @ (t.references
        |> List.map
             ~f:(Reference_tests.to_string ~node_to_string:(fun n -> n.label))
        |> List.map ~f:(String_util.indent_string ~indent:1))
      @ (if Option.is_some t.branch then [ "branch:" ] else [])
      @ Option.value_map t.branch ~default:[] ~f:(fun node ->
            [
              Branch_tests.to_string node ~node_to_string:(fun n -> n.label)
              |> String_util.indent_string ~indent:1;
            ])
      |> String.concat ~sep:"\n"
  end
end

and Reference : sig
  type type_ = Jump | Fallthrough [@@deriving sexp]
  type 'a t = { from : 'a Node.t; type_ : type_ }

  module For_tests (Element : Sexpable.S) : sig
    val to_string :
      Element.t t -> node_to_string:(Element.t Node.t -> string) -> string
  end
end = struct
  type type_ = Jump | Fallthrough [@@deriving sexp]
  type 'a t = { from : 'a Node.t; type_ : type_ }

  module For_tests (Element : Sexpable.S) = struct
    let to_string t ~node_to_string =
      sprintf "%s from %s"
        (Sexp.to_string_hum ([%sexp_of: type_] t.type_))
        (node_to_string t.from)
  end
end

and Branch : sig
  type 'a t =
    | Unconditional_jump of 'a Node.t
    | Conditional_jump of { true_ : 'a Node.t; false_ : 'a Node.t }
    | Fallthrough of 'a Node.t

  module For_tests (Element : Sexpable.S) : sig
    val to_string :
      Element.t t -> node_to_string:(Element.t Node.t -> string) -> string
  end
end = struct
  type 'a t =
    | Unconditional_jump of 'a Node.t
    | Conditional_jump of { true_ : 'a Node.t; false_ : 'a Node.t }
    | Fallthrough of 'a Node.t

  module For_tests (Element : Sexpable.S) = struct
    let to_string t ~node_to_string =
      match t with
      | Unconditional_jump node ->
          "unconditional jump to " ^ node_to_string node
      | Conditional_jump { true_; false_ } ->
          sprintf "conditional jump to true: %s false: %s"
            (node_to_string true_) (node_to_string false_)
      | Fallthrough node -> "fallthrough to " ^ node_to_string node
  end
end

include Node