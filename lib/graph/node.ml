module rec T : sig
  type 'a t = {
    label : string;
    mutable stmt : 'a;
    mutable references : 'a T.Reference.t list;
    mutable branch : 'a T.Branch.t option;
  }

  module Reference : sig
    type type_ = Jump | Fallthrough
    type 'a t = { from : 'a T.t; type_ : type_ }
  end

  module Branch : sig
    type 'a t =
      | Unconditional_jump of 'a T.t
      | Conditional_jump of { true_ : 'a T.t; false_ : 'a T.t }
      | Fallthrough of 'a T.t
  end
end =
  T

include T