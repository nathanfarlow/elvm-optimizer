type 'a t = { nodes : (string, 'a Node.t) Hashtbl.t }

let create nodes = { nodes }
let nodes t = t.nodes
let find_blocks _t = failwith "todo"
let find_terminals _t = failwith "todo"
