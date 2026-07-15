module Nonempty_list = struct
  (** List with at least one element. Defined to use existing list syntax, e.g.:
      {v
        let a = [ 1; 2; 3 ]
        let b = 1 :: 2 :: 3 :: []
      v}
      Functions supporting this type can be found in [Base.Nonempty_list]. *)
  type ('a : value_or_null) t = ( :: ) of 'a * 'a list
end
