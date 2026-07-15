ppx_fuelproof
=============

```ocaml
open Base
```

TL;DR: Write `type%fuelproof` to get better error messages around mode-crossing, and
even to get mode-crossing in places where the compiler can't currently
infer it, such as complicated GADTs:

```ocaml
type%fuelproof _ t : value mod portable =
  | A : int * _ u -> int t
  | B : bool -> bool t

and _ u : value mod portable =
  | A : 'a t -> 'a u
  | B : bool -> bool u
```

`ppx_fuelproof` helps work around several issues when trying to make your
type mode-cross in the way you want:

  1. It generates better error messages when your type doesn't mode-cross.
  2. It helps you avoid running out of fuel. (The mode-crossing check in
     the compiler limits how hard it will try to prove mode-crossing.)
  3. It (safely) permits more mode-crossing GADTs than the native compiler.

All of these are things that can be fixed, but the OCaml Language team is
busy with lots of things, so a ppx stopgap seems fine for now.

# Examples

## Example of better error message

```ocaml
module type Not_crossing = sig
  type t
end

module type P = sig
  type t : value mod portable
end

module Field1 : P = Int
module Field2 : P = Int
module Field3 : P = Int
module Field4 : Not_crossing = Int
module Field5 : P = Int
module Field6 : P = Int
module Field7 : P = Int
```

Confusing message without `fuelproof`:

```ocaml
module Confusing = struct
  type t : value mod portable =
    { field1 : Field1.t;
      field2 : Field2.t;
      field3 : Field3.t;
      field4 : Field4.t;
      field5 : Field5.t;
      field6 : Field6.t;
      field7 : Field7.t;
    }
end
```
```mdx-error
Lines 2-10, characters 5-8:
Error: The kind of type t is
           immutable_data
             with Field1.t
             with Field2.t
             with Field3.t
             with Field4.t
             with Field5.t
             with Field6.t
             with Field7.t
         because it's a boxed record type.
       But the kind of type t must be a subkind of value mod portable
         because of the annotation on the declaration of the type t.
```

Good error message with `fuelproof`:

```ocaml
module Fuelproof = struct
  type%fuelproof t : value mod portable =
    { field1 : Field1.t;
      field2 : Field2.t;
      field3 : Field3.t;
      field4 : Field4.t;
      field5 : Field5.t;
      field6 : Field6.t;
      field7 : Field7.t;
    }
end
```
```mdx-error
Line 6, characters 18-26:
Error: Bad layout annotation:
         The kind of Field4.t is value
           because of the definition of t at line 2, characters 5-11.
         But the kind of Field4.t must be a subkind of any mod portable
           because of the annotation on the wildcard _ at line 6, characters 18-26.
```

## Example of fuel

`fuelproof` helps the check not run out of fuel by checking each field for mode-crossing
in isolation.

```ocaml
module type Example_setup = sig
  module type P := sig
    type 'a t : value mod portable with 'a
  end

  type t0 : value mod portable
  type 'a t1 = { a : 'a }
  type 'a t2 = 'a t1 t1
  type 'a t3 = 'a t2 t2
  type 'a t4 = 'a t3 t3
end
```

Running out of fuel (NOTE: a recent compiler roll made this better; we still need to update docs):
```ocaml
module type Confusing = sig
  include Example_setup

  type t : value mod portable =
    { x1 : [ `a1 ] t2 t2;
      x2 : [ `a2 ] t2 t2;
      x3 : [ `a3 ] t2 t2;
      x4 : [ `a4 ] t2 t2;
      x5 : [ `a5 ] t2 t2;
      x6 : [ `a6 ] t2 t2;
    }
end
```

Refueling:
```ocaml
module type Fuelproof = sig
  include Example_setup

  type%fuelproof t : value mod portable =
    { x1 : [ `a1 ] t2 t2;
      x2 : [ `a2 ] t2 t2;
      x3 : [ `a3 ] t2 t2;
      x4 : [ `a4 ] t2 t2;
      x5 : [ `a5 ] t2 t2;
      x6 : [ `a6 ] t2 t2;
    }
end
```

## Example of complicated GADTs

This one's really just a case of running out of fuel. Currently, mode-crossing inference
for GADTs is fuel hungry. In practice, many reasonable-looking GADTs are in fact rejected.
It is hard to predict in advance which GADTs the typechecker is able to infer mode
crossing for. For now, `fuelproof` lets you make a wider (but admittedly still limited)
subset of GADTs cross modes.

Without `fuelproof` (NOTE: a recent compiler roll made this better; we still need to update docs):

```ocaml
type _ t : value mod portable =
  | Zero : [ `zero ] t
  | Succ : 'a t -> [ `succ of 'a ] t
```

With `fuelproof`:

```ocaml
type%fuelproof _ t : value mod portable =
  | Zero : [ `zero ] t
  | Succ : 'a t -> [ `succ of 'a ] t
```
