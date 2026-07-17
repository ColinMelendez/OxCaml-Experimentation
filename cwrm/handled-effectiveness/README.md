# handled-effectiveness

Interactive playground for Jane Street's
[handled_effect](https://github.com/janestreet/handled_effect) — typed algebraic
effects for OxCaml — visualized with
[Bonsai_term](https://github.com/janestreet/bonsai_term).

Each demo runs a real effectful program under an instrumented handler that
records a scrubbable execution trace. The TUI lets you step through Fork/Yield,
Get/Set, Send/Recv, and generator inversion and see the runtime / console /
timeline update.

## Demos

| Key | Demo | Effects |
|-----|------|---------|
| `1` | Shallow state | `Get` / `Set` |
| `2` | Send / Recv protocol | session-typed `Send` / `Recv` |
| `3` | Iterator → generator | `Yield` char (invert pattern) |
| `4` | Round-robin scheduler | `Fork` / `Yield` / `Say` |

## Build

From the oxmono repository root:

```bash
dune build cwrm/handled-effectiveness
```

## Run

```bash
dune exec -- handled-effectiveness
```

### Keys

| Key | Action |
|-----|--------|
| `q` / Ctrl-C | Quit |
| `Space` / `l` / `→` | Next step |
| `h` / `←` | Previous step |
| `p` | Toggle autoplay |
| `r` | Reset to first step |
| `[` / `]` (or `n`) | Previous / next demo |
| `1`–`4` | Jump to demo |

## Test

```bash
dune runtest cwrm/handled-effectiveness
```

Expect-test screenshots exercise the Bonsai_term view pipeline. Update them with:

```bash
dune runtest cwrm/handled-effectiveness --auto-promote
```

## Layout

```
 handled-effectiveness  -  scheduler  -  step 4/23  [stop]
╭ demo ─────────────────────────────────────────────╮
│ Round-robin scheduler                             │
│ Cooperative multitasking via Fork / Yield / Say   │
╰───────────────────────────────────────────────────╯
╭ event ────────────────────────────────────────────╮
│ > Yield A                                         │
│ A yields; continuation enqueued                   │
╰───────────────────────────────────────────────────╯
╭ runtime ──────────────────┬─ console ─────────────╮
│ running: B                │ "ABM..."              │
│ ready:   [A, main]        │                       │
╰───────────────────────────┴───────────────────────╯
╭ timeline ─────────────────────────────────────────╮
│ RFRYS...                                          │
│ ...O...                                           │
╰───────────────────────────────────────────────────╯
```
