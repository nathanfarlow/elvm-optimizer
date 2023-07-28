type 'a t = { nodes : (string, 'a Node.t) Hashtbl.t }

let create nodes = { nodes }
let nodes t = t.nodes
let find_blocks t = Hashtbl.filter t.nodes ~f:Node.is_top_level
let fresh_label _t = failwith "todo"
let register_node _t _node = failwith "todo"

module For_tests (Element : Sexpable.S) = struct
  module Node_test = Node.For_tests (Element)

  let to_string (t : Element.t t) =
    t.nodes |> Hashtbl.to_alist
    (* sort for deterministic output *)
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (_, node) -> Node_test.to_string node)
    |> String.concat ~sep:"\n"
end