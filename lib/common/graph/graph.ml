type 'a t = { nodes : (string, 'a Node.t) Hashtbl.t }

let create nodes = { nodes }
let nodes t = t.nodes
let find_blocks _t = failwith "todo"
let find_terminals _t = failwith "todo"
let fresh_label _t = failwith "todo"
let register_node _t _node = failwith "todo"