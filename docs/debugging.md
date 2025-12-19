# Render and paint debugging flags

When a render comes out blank/black or has missing content, enable these environment flags to narrow down where pixels disappear. All flags are read at runtime (no rebuild needed).

`render_pages` writes per-page logs to `fetches/renders/<page>.log` and a summary to `fetches/renders/_summary.log`; inspect these alongside the flags below.

## Display-list dumps (progressively noisier)

- `FASTR_DUMP_STACK=1` – dump the stacking context tree
- `FASTR_DUMP_FRAGMENTS=1` – dump all fragments
- `FASTR_DUMP_COUNTS=1` – dump display list counts only
- `FASTR_DUMP_TEXT_ITEMS=1` – dump text display items
- `FASTR_DUMP_COMMANDS=<N>` – dump the first N display commands (omit N to dump all)

These help confirm that paint commands are emitted and where large backgrounds/text originate.

## Paint debug logging

- `FASTR_PAINT_STATS=1` – log paint stage timings
- `FASTR_LOG_IMAGE_FAIL=1` – log image decode/load failures
- `FASTR_LOG_FRAG_BOUNDS=1` – log fragment bounds intersecting the viewport
- `FASTR_RENDER_TIMINGS=1` – per-stage timings during render

### Oversized backgrounds

Use these to spot runaway background rects that blanket the viewport:

- `FASTR_LOG_LARGE_BACKGROUNDS[=<threshold>]` – log any background larger than `<threshold>x` the viewport (default 10× if omitted/invalid)
- `FASTR_LOG_LARGE_BACKGROUND[=<threshold>]` – exact alias for the flag above (singular form)

Both environment variables are accepted; if the value is missing or not a number the logger falls back to 10× the viewport.

Example: `FASTR_LOG_LARGE_BACKGROUND=5 cargo run --release --bin render_pages -- --pages example.com`

### Pixmap inspection

- `FASTR_DEBUG_PIXMAP_STATS=1` – after painting, log pixmap dimensions, checksum, and top colors. Useful to differentiate “nothing was painted” vs. “PNG encoding failed.”

### Text fragments

- `FASTR_DUMP_TEXT_FRAGMENTS=1` – dump text fragment positions and content to spot clipping/offset issues.

## Quick usage examples

Dump the first 400 display commands and log oversized backgrounds with a 8× threshold:

```bash
FASTR_DUMP_COMMANDS=400 FASTR_LOG_LARGE_BACKGROUNDS=8 \
  cargo run --release --bin render_pages -- --pages news.ycombinator.com
```

Log pixmap stats and fragment bounds for a single page:

```bash
FASTR_DEBUG_PIXMAP_STATS=1 FASTR_LOG_FRAG_BOUNDS=1 \
  cargo run --release --bin fetch_and_render https://example.com out.png --timeout 20
```

Only log paint timings:

```bash
FASTR_PAINT_STATS=1 cargo test --quiet paint::painter::tests
```
