# Patching: recovering from OxCaml / vendored-package skew

This monorepo vendors Jane Street and OxCaml-adapted packages under `opam/`. Dune builds those copies, **not** whatever happens to be installed in your opam switch. When you upgrade the OxCaml compiler (or `opam upgrade` a bunch of JS packages) without refreshing `opam/`, builds fail with confusing errors in vendored code rather than in `avsm/`.

## Symptoms

Typical signs that the switch is ahead of `opam/`:

| Error | Likely cause |
|---|---|
| `Unknown builtin primitive %runstack` | Compiler moved to `%with_stack` / `%with_stack_bind`; vendored `basement` / `handled_effect` still use the old API |
| `Format_doc.formatter` vs `Format.formatter` | Compiler `Printtyp` / `Location` use `Format_doc`; vendored `base`, `ppxlib`, or `mdx` still call the old `Format` printers |
| `Unbound type constructor Asttypes.index_kind` | Vendored `ppxlib` AST expects OxCaml AST bits that don’t match this compiler |
| `Too many incompatible ppx drivers were found: ppxlib and ppxlib` | A PPX (e.g. `ppx_builtin`) is coming from the switch while `ppx_jane` uses vendored `ppxlib` |
| `simde/...h file not found` or arm64 `emit.ml` assert in SIMD code | Newer `ocaml_simd` builds on arm64; older monorepo gated SSE/AVX with `enabled_if amd64` |

`dune exec -- oxmono …` fails the same way: it has to compile against vendored packages first. Even a working `oxmono sync` only refreshes `sources/`; you still merge into `opam/` by hand.

## Recovery outline

### 1. Confirm the skew

```bash
opam list oxcaml-compiler
ocamlopt -vnum
# Compare a known hotspot, e.g. basement:
rg '%runstack|%with_stack' opam/basement/src/dynamic.ml
rg '%runstack|%with_stack' ~/.opam/$OPAM_SWITCH_PREFIX/../.opam-switch/sources/basement.*/src/dynamic.ml 2>/dev/null
```

Your switch should be something like `ocaml-variants.5.2.0+ox` with a matching `oxcaml-compiler` (see README). The vendored tree must speak the same primitive / AST dialect.

### 2. Sync Jane Street packages from the switch

Prefer copies that already built in your switch:

```bash
SWITCH=~/.opam/$(opam switch show)/.opam-switch/sources
# Example: every package at the preview pin your switch uses
for d in "$SWITCH"/*.v0.18~preview.130.106+341; do
  pkg=$(basename "$d" | sed 's/\.v0.18~preview.130.106+341$//' )
  [ -d "opam/$pkg" ] || continue
  rsync -a --delete --exclude '.git' "$d/" "opam/$pkg/"
done
```

Also copy any **new** deps the updated packages need that aren’t in `opam/` yet (e.g. `nonempty_list_type`, `base_internalhash_types`, `ppx_builtin`).

For packages available in the ox opam-repo at that pin but **not** present under the switch sources, fetch the tarball URL from:

```text
~/.opam/repo/ox/packages/<pkg>/<pkg>.v0.18~preview.…/opam
```

and rsync the extracted tree into `opam/<pkg>/`.

### 3. Rebuild OxCaml-patched `ppxlib` from the ox package

Do **not** copy switch `ppxlib` sources alone — patches are applied at opam install time. Rebuild like the ox package does:

```bash
SWITCH_PPX=~/.opam/$(opam switch show)/.opam-switch/sources/ppxlib.0.33.0+ox2
OX_FILES=~/.opam/repo/ox/packages/ppxlib/ppxlib.0.33.0+ox2/files

rm -rf opam/ppxlib
cp -a "$SWITCH_PPX" opam/ppxlib
cd opam/ppxlib
for p in "$OX_FILES"/*.patch; do
  patch -p1 --forward --batch < "$p"
  cp "$p" .
done
```

Adjust version strings (`0.33.0+ox2`, etc.) to whatever `opam show ppxlib` reports.

### 4. Apply opam-only build fixes that aren’t in the tarball

Some packages rely on opam `build:` steps. Replicate those in the tree when needed.

**macOS + `basement`:** the ox opam file runs

```bash
sed -i '' -e 's/caml_state/Caml_state/' opam/basement/src/stubs.c
```

(`caml_state` is not visible in user C stubs on macOS; use the `Caml_state` macro.)

**arm64 + `ocaml_simd`:** keep SSE/AVX libraries amd64-only so portable code paths are selected (and you avoid simde / backend issues):

```dune
(enabled_if (= %{architecture} amd64))
```

on `ocaml_simd.sse`, `ocaml_simd.avx`, and `ocaml_simd.sexp`.

### 5. Small compiler-libs / locality fixups

If something still uses old `Printtyp` printers with `Format.printf`:

```ocaml
Printtyp.Compat.longident
Printtyp.Compat.signature
```

OxCaml may also reject partial applications that escape as local values; eta-expand, e.g. `List.iter (fun s -> Buffer.add_string buf s) xs`.

### 6. Verify

```bash
dune build avsm          # your packages
dune build               # full workspace
dune exec -- oxmono --help
```

## What not to expect from `oxmono sync`

`oxmono sync` updates pristine trees under `sources/` (and records pins in `sources.yaml`). It does **not** automatically rewrite `opam/`. After a compiler bump you must refresh `opam/` as above, then optionally update `sources.yaml` / re-run sync so `oxmono diff` stays meaningful.

## Rule of thumb

**The Compiler and `opam/` must move together.** Upgrading only the switch, or only a subset of vendored packages, produces exactly the errors at the top of this file.
