# oxtop

A light [btop](https://github.com/aristocratos/btop)-style system monitor for the
terminal, built with Jane Street's [Bonsai_term](https://github.com/janestreet/bonsai_term)
on OxCaml.

## Features

- CPU and memory usage gauges plus a short CPU history sparkline
- Scrollable process table (PID, CPU%, MEM%, RSS, command)
- Sort by CPU / memory / PID / name
- Keyboard-driven selection
- Expect-test “screenshot” coverage of the TUI via `bonsai_term_test`

## Build

From the oxmono repository root:

```bash
dune build cwrm/oxtop
```

## Run

```bash
dune exec -- oxtop
```

### Keys

| Key | Action |
|-----|--------|
| `q` / Ctrl-C | Quit |
| `j` / `↓` | Move selection down |
| `k` / `↑` | Move selection up |
| `PgDn` / `PgUp` | Page through the process list |
| `g` / `G` | Jump to top / bottom |
| `c` / `m` / `p` / `n` | Sort by CPU / mem / PID / name |
| `s` | Cycle sort order |

## Test

```bash
dune runtest cwrm/oxtop
```

The tests render fixture system snapshots through the real Bonsai_term view
pipeline and assert the boxed terminal output. When you change the UI, re-run
with `--auto-promote` to update the screenshots:

```bash
dune runtest cwrm/oxtop --auto-promote
```

## Layout

```
 oxtop  hostname  ·  N cpu  ·  load …
 CPU ████░░░░  42.5%
 MEM ████░░░░  8.0G/16.0G
 hst ▂▄▆█▇▅▇▇
 ╭ processes · sort=cpu ╮
 │>  PID  CPU%  MEM%  … │
 ╰──────────────────────╯
  q:quit  j/k:move  …
```
