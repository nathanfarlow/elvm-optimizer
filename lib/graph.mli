open Core

module Node : sig
  type 'a t

  type 'a jmp =
    | Unconditional of 'a t
    | Conditional of
        { true_ : 'a t
        ; false_ : 'a t
        }

  type 'a out =
    | Fallthrough of 'a t
    | Jump of 'a jmp

  val id : 'a t -> string
  val v : 'a t -> 'a
  val set_v : 'a t -> 'a -> unit
  val in_ : 'a t -> 'a t list
  val set_in : 'a t -> 'a t list -> unit
  val out : 'a t -> 'a out option
  val set_out : 'a t -> 'a out option -> unit
  val out_as_list : 'a t -> 'a t list
end

type 'a t

val create : unit -> 'a t
val nodes : 'a t -> 'a Node.t Map.M(String).t
val add : 'a t -> string -> 'a -> 'a Node.t
val remove : 'a t -> 'a Node.t -> unit
val find : 'a t -> string -> 'a Node.t option
val find_exn : 'a t -> string -> 'a Node.t
val to_dot : 'a t -> ('a -> string) -> string
val fresh_label : 'a t -> string
val map : 'a t -> f:('a -> 'b) -> 'b t
val iter : 'a t -> f:('a Node.t -> unit) -> unit
