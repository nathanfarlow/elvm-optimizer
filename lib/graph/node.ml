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
end = struct
  type 'a t = {
    label : string;
    mutable stmt : 'a;
    mutable references : 'a Reference.t list;
    mutable branch : 'a Branch.t option;
  }
  [@@deriving fields]

  let create ~label ~stmt = { label; stmt; references = []; branch = None }
  let label t = t.label
  let stmt t = t.stmt
  let references t = t.references
  let branch t = t.branch
  let set_stmt t stmt = t.stmt <- stmt
  let set_references t references = t.references <- references
  let add_reference t reference = t.references <- reference :: t.references
  let set_branch t branch = t.branch <- branch
  let prepend_node _t _other = failwith "todo"
end

and Reference : sig
  type type_ = Jump | Fallthrough
  type 'a t = { from : 'a Node.t; type_ : type_ }
end =
  Reference

and Branch : sig
  type 'a t =
    | Unconditional_jump of 'a Node.t
    | Conditional_jump of { true_ : 'a Node.t; false_ : 'a Node.t }
    | Fallthrough of 'a Node.t
end =
  Branch

include Node