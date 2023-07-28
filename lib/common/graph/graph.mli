type 'a t = { nodes : (string, 'a Node.t) Base.Hashtbl.t }

val create : (string, 'a Node.t) Base.Hashtbl.t -> 'a t
val nodes : 'a t -> (string, 'a Node.t) Base.Hashtbl.t
val find_blocks : 'a t -> (string, 'a Node.t) Base.Hashtbl.t
val fresh_label : 'a t -> string
val register_node : 'a t -> 'a Node.t -> unit

module For_tests : functor (Element : Sexpable) -> sig
  val to_string : Element.t t -> string
end
