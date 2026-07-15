# Trampoline

`Trampoline` is a monad meant to help with "stack overflow" issues in `js_of_ocaml` programs.

JSOO does not have tail call optimization, so if you write a deeply recursive
function, you may run into stack overflow issues.

If a function like:

```ocaml
let some_function x =
  let rec f x =
    let a = f (x - 1) in
    let b = f (x - 2) in
    some_reduce a b
  in
  f x
```

stack overflows, you can re-write it with trampoline with:

```ocaml
let some_function x =
  let rec f x =
    let%bind.Trampoline a = f (x - 1) in
    let%bind.Trampoline b = f (x - 2) in
    Trampoline.return (some_reduce a b)
  in
  Trampoline.run (f x)
```
