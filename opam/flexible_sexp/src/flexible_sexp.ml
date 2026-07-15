module Record = Record
module Variant = Variant

let within_inflexible_context ~f = Deep_inflexible_context.run_within ~f

module Private = struct
  let am_i_within_inflexible_context = Deep_inflexible_context.am_i_within
end

module Stable = struct
  module Record = Record.Stable
  module Variant = Variant.Stable
end
