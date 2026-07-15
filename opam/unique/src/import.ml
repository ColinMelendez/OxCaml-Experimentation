include Basement.Or_null_shim.Export
include Basement.Modes

let failwith = Basement.Stdlib_shim.failwith

type nothing = |

external magic_many
  : ('a : value_or_null).
  'a @ contended once portable -> 'a @ contended portable
  @@ portable
  = "%identity"

external magic_unique
  : ('a : value_or_null).
  ('a[@local_opt]) -> ('a[@local_opt]) @ unique
  @@ portable
  = "%identity"

external magic_unique__portended
  : ('a : value_or_null).
  ('a[@local_opt]) @ contended portable -> ('a[@local_opt]) @ contended portable unique
  @@ portable
  = "%identity"

external phys_equal
  : ('a : value_or_null).
  'a @ contended local -> 'a @ contended local -> bool
  @@ portable
  = "%eq"
