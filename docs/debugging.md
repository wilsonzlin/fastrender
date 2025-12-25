# Render and paint debugging

When a render comes out blank/black or is missing content, use the debug/profiling environment variables to narrow down where pixels disappear. These flags are read at runtime (no rebuild required).

The canonical list lives in [env-vars.md](env-vars.md). This page highlights the most useful ones and the common workflow.

`render_pages` writes per-page logs to `fetches/renders/<page>.log` and a summary to `fetches/renders/_summary.log`.

## Display-list dumps (paint pipeline)

- `FASTR_DUMP_STACK=1` – dump the stacking context tree
- `FASTR_DUMP_FRAGMENTS=1` – dump the fragment tree used for painting
- `FASTR_DUMP_COUNTS=1` – dump display-list counts only
- `FASTR_DUMP_TEXT_ITEMS=1` – dump text display items
- `FASTR_DUMP_COMMANDS=<N>` – dump the first N display commands (omit `N` to dump all)

## Helpful logging

- `FASTR_RENDER_TIMINGS=1` – high-level per-stage timings
- `FASTR_PAINT_STATS=1` – paint-stage timings
- `FASTR_LOG_IMAGE_FAIL=1` – image decode/load failures (raster + SVG)
- `FASTR_LOG_FRAG_BOUNDS=1` – fragment-tree bounds vs viewport
- `FASTR_TRACE_FRAGMENTATION=1` – trace fragmentation break opportunities/boundary choices (`inspect_frag --trace-fragmentation` sets this for you)

## Text-oriented debugging

- `FASTR_DUMP_TEXT_FRAGMENTS=<N>` – sample text fragments (positions + preview text)
- `FASTR_TRACE_TEXT=<substring>` – print a containment trail for the first text fragment containing the substring

## Quick examples

Dump the first 400 paint commands:

```bash
FASTR_DUMP_COMMANDS=400 cargo run --release --bin render_pages -- --pages news.ycombinator.com
```

Log timings + fragment bounds for a single render:

```bash
FASTR_RENDER_TIMINGS=1 FASTR_LOG_FRAG_BOUNDS=1 \
  cargo run --release --bin fetch_and_render -- https://example.com out.png --timeout 20
```
