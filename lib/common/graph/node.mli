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
end

and Reference : sig
  type type_ =
    | Jump
    | Fallthrough
  [@@deriving sexp]

  type 'a t =
    { from : 'a Node.t
    ; type_ : type_
    }
end

and Branch : sig
  type 'a t =
    | Unconditional_jump of 'a Node.t
    | Conditional_jump of
        { true_ : 'a Node.t
        ; false_ : 'a Node.t
        }
    | Fallthrough of 'a Node.t
end

include module type of Node with type 'a t = 'a Node.t

module For_tests : functor (Element : Sexpable) -> sig
  val to_string : Element.t Node.t -> string
end
