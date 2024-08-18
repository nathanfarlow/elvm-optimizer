open Core

module Node = struct
  type 'a t =
    { id : string
    ; mutable v : 'a
    ; mutable in_ : 'a t list
    ; mutable out : 'a out option
    }
  [@@deriving fields ~getters ~setters]

  and 'a jmp =
    | Unconditional of 'a t
    | Conditional of
        { true_ : 'a t
        ; false_ : 'a t
        }

  and 'a out =
    | Fallthrough of 'a t
    | Jump of 'a jmp

  let equal_id t1 t2 = String.equal t1.id t2.id

  let out_as_list t =
    match t.out with
    | None -> []
    | Some (Fallthrough t') -> [ t' ]
    | Some (Jump (Unconditional t')) -> [ t' ]
    | Some (Jump (Conditional { true_; false_ })) -> [ true_; false_ ]
  ;;

  (** Does t have this parent in its in edges? *)
  let has_parent t ~parent = List.mem t.in_ parent ~equal:equal_id

  (** Does t have this child in its out edges? *)
  let has_child t ~child = List.mem (out_as_list t) child ~equal:equal_id

  let set_in = set_in_
end

type 'a t = { mutable nodes : 'a Node.t Map.M(String).t }

let nodes t = t.nodes
let create () = { nodes = Map.empty (module String) }

let add t id v =
  let node = { Node.id; v; in_ = []; out = None } in
  t.nodes <- Map.set t.nodes ~key:id ~data:node;
  node
;;

let remove t node =
  t.nodes <- Map.remove t.nodes node.Node.id;
  assert (not @@ List.exists node.in_ ~f:(Node.has_child ~child:node));
  assert (not @@ List.exists (Node.out_as_list node) ~f:(Node.has_parent ~parent:node))
;;

let find t = Map.find t.nodes
let find_exn t = Map.find_exn t.nodes

let to_dot t to_string =
  let buf = Buffer.create 100 in
  let add_line s = Buffer.add_string buf (s ^ "\n") in
  add_line "digraph G {";
  Map.iteri t.nodes ~f:(fun ~key:id ~data:node ->
    let node_str = to_string node.v |> String.split_lines |> String.concat ~sep:"\\l" in
    add_line (sprintf "  %s [label=<\\l%s:\\l\\l%s>];" id id node_str);
    match node.out with
    | None -> ()
    | Some (Fallthrough child | Jump (Unconditional child)) ->
      add_line (sprintf "  %s -> %s;" id child.id)
    | Some (Jump (Conditional { true_; false_ })) ->
      add_line (sprintf "  %s -> %s [label=\"true\"];" id true_.id);
      add_line (sprintf "  %s -> %s [label=\"false\"];" id false_.id));
  add_line "}";
  Buffer.contents buf
;;
