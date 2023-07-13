module Operand = struct
  type t = Variable of string | Constant of int | Label of string
end

module Operands = struct
  type t = { dst : Operand.t; src : Operand.t }
end

module Comparison = struct
  type t = Eq | Ne | Lt | Le
end

module Condition = struct
  type t = { cmp : Comparison.t; args : Operands.t }
end

module Jump = struct
  type t = { target : Operand.t; cond : Condition.t option }
end

type t =
  | Mov of Operands.t
  | Add of Operands.t
  | Sub of Operands.t
  | Load of Operands.t
  | Store of Operands.t
  | Putc of Operand.t
  | Getc of Operand.t
  | Exit
  | Jump of Jump.t
  | Set of Condition.t
  | Dump
