module Either_u = Either_u
module Fake_tuple = Fake_tuple

module Kind_tag = struct
  include Kind_tag

  module Templated = Templated
  [@@alert
    template_specialization
      "Using templated kind tags often involves template specialization that won't be \
       subsumed by layout polymorphism. Consider how your code will be updated in the \
       future."]
end

module Kind_witness = struct
  include Kind_witness

  module Templated = Templated
  [@@alert
    template_specialization
      "Using templated kind witnesses often involves template specialization that won't \
       be subsumed by layout polymorphism. Consider how your code will be updated in the \
       future."]
end

module Result_u = Result_u
module Or_error_u = Or_error_u
module Option_u = Option_u
