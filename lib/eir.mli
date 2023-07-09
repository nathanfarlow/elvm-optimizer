type t

module Register : sig
  type t = A | B | C | D | SP | BP [@@deriving sexp, equal]
end

module Instruction : sig
  module Imm_or_reg : sig
    type t = Int of int | Label of string | Register of Register.t
    [@@deriving sexp, equal]
  end

  module Operands : sig
    type t = { src : Imm_or_reg.t; dst : Register.t } [@@deriving sexp, equal]
  end

  module Comparison : sig
    type t = Eq | Ne | Lt | Le | Gt | Ge [@@deriving sexp, equal]
  end

  module Condition : sig
    type t = { cmp : Comparison.t; args : Operands.t } [@@deriving sexp, equal]
  end

  type t =
    | Mov of Operands.t
    | Add of Operands.t
    | Sub of Operands.t
    | Load of Operands.t
    | Store of { src : Register.t; dst : Imm_or_reg.t }
    | Putc of Imm_or_reg.t
    | Getc of Register.t
    | Exit
    | Jump of { target : Imm_or_reg.t; cond : Condition.t option }
    | Set of Condition.t
    | Dump
  [@@deriving sexp, equal]
end

module Segment : sig
  type t = Data | Text [@@deriving sexp, equal]
end

module Address : sig
  type t = { segment : Segment.t; offset : int } [@@deriving sexp, equal]
end

module Data : sig
  type t = Const of int | Label of string [@@deriving sexp, equal]
end

val const_heap_start_label : string

val create :
  data:Data.t list ->
  insns:Instruction.t list ->
  labels:(string, Address.t) Hashtbl.t ->
  t

val data : t -> Data.t list
val insns : t -> Instruction.t list
val labels : t -> (string, Address.t) Hashtbl.t
val resolve_label : t -> string -> Address.t option
val parse_exn : string -> t