open Core

module type Sexp_of_m = sig
  type t [@@deriving sexp_of]
end

module Node : sig
  type 'a t

  type 'a out =
    | Unconditional of 'a t
    | Conditional of
        { true_ : 'a t
        ; false_ : 'a t
        }

  val in_ : 'a t -> 'a t list
  val add_in : 'a t -> 'a t -> unit
  val out : 'a t -> 'a out option
  val set_out : 'a t -> 'a out option -> unit
end

type 'a t [@@deriving sexp_of]

val create : unit -> 'a t
val nodes : 'a t -> 'a Node.t Map.M(String).t
val add : 'a t -> string -> 'a -> 'a Node.t
val remove : 'a t -> 'a Node.t -> unit
val find : 'a t -> string -> 'a Node.t option
val find_exn : 'a t -> string -> 'a Node.t
