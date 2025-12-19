# Render Debugging Guide

This guide collects practical tips for diagnosing blank or black renders.

## Useful environment flags

- `FASTR_TRACE_BACKGROUNDS=1` — Logs every background command (rect + color), useful for spotting oversized backgrounds that blanket the viewport.
- `FASTR_LOG_LARGE_BACKGROUNDS=1` — Logs backgrounds whose height/width far exceed the viewport (heuristic). Helps find misplaced fragments.
- `FASTR_DEBUG_PIXMAP_STATS=1` — After rendering, prints a histogram/summary of the pixmap (unique colors, non-white pixels, bounding boxes). Quickly tells you if the output is blank/black.
- `FASTR_DUMP_COMMANDS=<N>` — Dumps the first N display list commands to stderr so you can inspect ordering/positions/colors.

## Example commands

Render a cached page with background tracing and pixmap stats:

```
FASTR_TRACE_BACKGROUNDS=1 FASTR_DEBUG_PIXMAP_STATS=1 \
  cargo run --release --bin render_pages -- --pages developer.mozilla.org_en-US_docs_Web_CSS_writing-mode --timeout 20
```

Dump the first 400 display commands for inspection:

```
FASTR_DUMP_COMMANDS=400 \
  cargo run --release --bin render_pages -- --pages developer.mozilla.org_en-US_docs_Web_CSS_text-orientation --timeout 20
```

Log oversized backgrounds during render:

```
FASTR_LOG_LARGE_BACKGROUNDS=1 \
  cargo run --release --bin render_pages -- --pages news.ycombinator.com --timeout 20
```

## Interpreting logs

- Huge backgrounds anchored at (0,0) with heights far exceeding the viewport and painted late can blanket the canvas. Track the fragment/style producing them and fix its position/size.
- Pixmap stats with 0 non-white pixels (or 0 black pixels for white-only output) indicate nothing was painted or it was overwritten.
- Command dumps showing a full-viewport background painted after text often explain blank canvases; avoid repainting identical backgrounds at the end of the list.

## Next steps

- If backgrounds look wrong, check stacking context/root handling and fragment offsets.
- If text/objects are missing, verify clip regions and the final PNG write path.
- Capture command dumps and pixmap stats as artifacts when filing bugs, noting the environment flags used.

