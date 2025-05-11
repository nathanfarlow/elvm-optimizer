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

type 'a t =
  { mutable i : int
  ; mutable nodes : 'a Node.t Map.M(String).t
  }

let nodes t = t.nodes
let create () = { i = 0; nodes = Map.empty (module String) }

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

let rec fresh_label t =
  let label = [%string "__L%{t.i#Int}"] in
  t.i <- t.i + 1;
  if Map.mem t.nodes label then fresh_label t else label
;;

let mapi t ~f =
  (* TODO: we should change this representation *)
  let t' = create () in
  let rec insert node =
    if Map.mem t'.nodes node.Node.id
    then ()
    else (
      let node' = add t' node.Node.id (f node.Node.id node.Node.v) in
      List.iter node.Node.in_ ~f:(fun parent -> insert parent);
      Node.set_in
        node'
        (List.map node.Node.in_ ~f:(fun parent -> Map.find_exn t'.nodes parent.Node.id));
      match node.Node.out with
      | None -> ()
      | Some (Fallthrough child) ->
        insert child;
        Node.set_out node' (Some (Fallthrough (Map.find_exn t'.nodes child.Node.id)))
      | Some (Jump (Unconditional child)) ->
        insert child;
        Node.set_out
          node'
          (Some (Jump (Unconditional (Map.find_exn t'.nodes child.Node.id))))
      | Some (Jump (Conditional { true_; false_ })) ->
        insert true_;
        insert false_;
        Node.set_out
          node'
          (Some
             (Jump
                (Conditional
                   { true_ = Map.find_exn t'.nodes true_.Node.id
                   ; false_ = Map.find_exn t'.nodes false_.Node.id
                   }))))
  in
  Map.iteri t.nodes ~f:(fun ~key:_ ~data:node -> insert node);
  t'
;;

let map t ~f = mapi t ~f:(fun _ v -> f v)
let iter t ~f = Map.iteri t.nodes ~f:(fun ~key:_ ~data:node -> f node)

let fallthrough t init_id (insns : 'a list) =
  (* TODO: switch to a nonempty list *)
  assert (not (List.is_empty insns));
  let hd, tl = List.hd_exn insns, List.drop insns 1 in
  let nodes =
    add t init_id hd
    :: List.map tl ~f:(fun insn ->
      let label = fresh_label t in
      add t label insn)
  in
  let rec add_edges = function
    | [] | [ _ ] -> ()
    | a :: b :: rest ->
      Node.set_in b [ a ];
      Node.set_out a (Some (Fallthrough b));
      add_edges (b :: rest)
  in
  add_edges nodes;
  List.last_exn nodes
;;

let flatten (t : 'a list t) : 'a t =
  (* TODO: this is bad, jank code. Can we unify with [map]?*)
  let new_graph = create () in
  let last_nodes = mapi t ~f:(fun id node -> fallthrough new_graph id node) in
  iter t ~f:(fun node ->
    let id = Node.id node in
    let parents = Node.in_ node in
    let corresponding_node = Map.find_exn new_graph.nodes id in
    Node.set_in
      corresponding_node
      (List.map parents ~f:(fun parent -> Map.find_exn new_graph.nodes parent.Node.id));
    let out = Node.out node in
    let corresponding_node = find_exn last_nodes id |> Node.v in
    let out =
      match out with
      | None -> None
      | Some (Fallthrough child) ->
        Some (Node.Fallthrough (Map.find_exn new_graph.nodes child.Node.id))
      | Some (Jump (Unconditional child)) ->
        Some (Jump (Unconditional (Map.find_exn new_graph.nodes child.Node.id)))
      | Some (Jump (Conditional { true_; false_ })) ->
        Some
          (Jump
             (Conditional
                { true_ = Map.find_exn new_graph.nodes true_.Node.id
                ; false_ = Map.find_exn new_graph.nodes false_.Node.id
                }))
    in
    Node.set_out corresponding_node out);
  new_graph
;;
