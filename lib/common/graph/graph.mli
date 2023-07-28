type 'a t = { nodes : (string, 'a Node.t) Base.Hashtbl.t }

val create : (string, 'a Node.t) Base.Hashtbl.t -> 'a t
val nodes : 'a t -> (string, 'a Node.t) Base.Hashtbl.t
val find_blocks : 'a t -> (string, 'a Node.t) Base.Hashtbl.t
val fresh_label : 'a -> 'b
val register_node : 'a -> 'b -> 'c

module For_tests : functor (Element : Sexpable) -> sig
  val to_string : Element.t t
  val find_terminals : 'a -> 'b -> string
end
