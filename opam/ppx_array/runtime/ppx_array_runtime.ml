type nonrec ('a : any mod separable) array = 'a array
type nonrec ('a : any mod separable) iarray = 'a iarray

external length
  : ('a : any mod separable).
  ('a array[@local_opt]) @ contended -> int
  @@ portable
  = "%array_length"
[@@layout_poly]

external unsafe_set
  : ('a : any mod separable).
  ('a array[@local_opt]) -> (int[@local_opt]) -> 'a -> unit
  @@ portable
  = "%array_unsafe_set"
[@@layout_poly]

external%template unsafe_get
  : ('a : any mod separable).
  ('a array[@local_opt]) @ m -> (int[@local_opt]) -> 'a @ m
  @@ portable
  = "%array_unsafe_get"
[@@mode m = (uncontended, shared)] [@@layout_poly]

[%%template
  external unsafe_to_array__promise_no_mutation
    : ('a : any mod separable).
    ('a iarray[@local_opt]) @ c -> ('a array[@local_opt]) @ c
    @@ portable
    = "%array_of_iarray"
  [@@mode c = (uncontended, shared, contended)] [@@layout_poly]]

[%%template
  external unsafe_of_array__promise_no_mutation
    : ('a : any mod separable).
    ('a array[@local_opt]) @ c -> ('a iarray[@local_opt]) @ c
    @@ portable
    = "%array_to_iarray"
  [@@mode c = (uncontended, shared, contended)] [@@layout_poly]]

[%%template
  external create
    : ('a : any mod separable).
    len:int -> 'a -> 'a array @ m
    @@ portable
    = "%makearray_dynamic"
  [@@alloc __ @ m = (heap_global, stack_local)] [@@layout_poly]]

external magic_create_uninitialized
  : ('a : any mod separable).
  len:int -> ('a array[@local_opt])
  @@ portable
  = "%makearray_dynamic_uninit"
[@@layout_poly]
