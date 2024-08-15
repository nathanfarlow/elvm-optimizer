open Core

module Make (V : sig
    type t [@@deriving sexp_of]
  end) : sig
  module Node : sig
    type t [@@deriving sexp_of]

    type out =
      | Unconditional of t
      | Conditional of
          { true_ : t
          ; false_ : t
          }

    val in_ : t -> t list
    val add_in : t -> t -> unit
    val out : t -> out option
    val set_out : t -> out option -> unit
  end

  type t [@@deriving sexp_of]

  val create : unit -> t
  val nodes : t -> Node.t Map.M(String).t
  val add : t -> string -> V.t list -> Node.t
  val remove : t -> Node.t -> unit
  val find : t -> string -> Node.t option
  val find_exn : t -> string -> Node.t
end
