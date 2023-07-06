open Core

type immediate_or_register =
  | Int of int
  | Label of string
  | Register of Register.t
[@@deriving sexp, equal]

type src_dst = { src : immediate_or_register; dst : Register.t }
[@@deriving sexp, equal]

type comparison = Eq | Ne | Lt | Le | Gt | Ge [@@deriving sexp, equal]

type condition = { comparison : comparison; args : src_dst }
[@@deriving sexp, equal]

type t =
  | Mov of src_dst
  | Add of src_dst
  | Sub of src_dst
  | Load of src_dst
  | Store of { src : Register.t; dst : immediate_or_register }
  | Putc of immediate_or_register
  | Getc of Register.t
  | Exit
  | Jump of { target : immediate_or_register; condition : condition option }
  | Set of condition
  | Dump
[@@deriving sexp, equal]
