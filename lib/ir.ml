open! Core

[@@@warning "-37-69-34"]

module type Var = sig
  type t
end

module Make (Var : Var) = struct
  module Imm_or_var = struct
    type t =
      | Int of int
      | Var of Var.t
  end

  module Operands = struct
    type t =
      { src : Imm_or_var.t
      ; dst : Var.t
      }
  end

  module Comparison = struct
    type t =
      | Eq
      | Ne
      | Lt
      | Le
  end

  type t =
    | Mov of Operands.t
    | Add of Operands.t
    | Sub of Operands.t
    | Load of Operands.t
    | Store of
        { src : Var.t
        ; dst : Imm_or_var.t
        }
    | Putc of Imm_or_var.t
    | Getc of Var.t
    | Exit
    | Jump of
        { target : Imm_or_var.t
        ; cond : Condition.t option
        }
    | Set of Condition.t
end

(* let register_allocate prog = *)
