open! Core

type 'a t

val create : (string, 'a Node.t) Base.Hashtbl.t -> 'a t
val nodes : 'a t -> (string, 'a Node.t) Base.Hashtbl.t
val find_blocks : 'a t -> (string, 'a Node.t) Base.Hashtbl.t
val fresh_label : 'a t -> string
val register_node : 'a t -> 'a Node.t -> unit
val unregister_node : 'a t -> string -> unit

val memo
  :  f:('a Node.t -> ('a Node.t -> 'b) -> 'b)
  -> on_cycle:('a Node.t -> 'b)
  -> 'a Node.t
  -> 'b

module For_tests : functor
    (E : sig
       type t [@@deriving sexp_of]
     end)
    -> sig
  val to_string : E.t t -> string
end
