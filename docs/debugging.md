# Render and paint debugging

When a render comes out blank/black or is missing content, use the debug/profiling environment variables to narrow down where pixels disappear. These flags are read at runtime (no rebuild required).

The canonical list lives in [env-vars.md](env-vars.md). This page highlights the most useful ones and the common workflow.

`render_pages` writes per-page logs to `fetches/renders/<page>.log` and a summary to `fetches/renders/_summary.log`.

## Hard sites (Akamai) fetch checklist

When a pageset run fails to fetch a page or subresource from a CDN-protected "hard" site (common examples: Akamai), the failure mode often looks like:

- HTTP/1.1 hangs/timeouts or "0 bytes"/empty-body responses.
- HTTP/2 errors such as `INTERNAL_ERROR` (backend-dependent).

Checklist (start here, in order):

1. **Use an HTTP/2-capable backend**: `FASTR_HTTP_BACKEND=curl` (or leave it at the default `auto` to use the Rust backend first with a `curl` fallback on retryable network/TLS/HTTP2 errors).
2. **Enable browser-like headers**: `FASTR_HTTP_BROWSER_HEADERS=1` (this is the default; set it explicitly when debugging to ensure it wasn't disabled).
   - Some font/CDN endpoints are sensitive to `Accept: */*` plus `Origin`/`Referer`; the browser-header profile is intended to match those expectations.
3. **Turn on retry logging** when debugging transient failures: `FASTR_HTTP_LOG_RETRIES=1`.
4. **Make sure you’re testing the network path** (not stale caches): use `fetch_pages --refresh` for HTML and consider disabling the disk cache (`DISK_CACHE=0`) when chasing subresource fetch behavior.

Example (pageset loop, targeted):

```bash
FASTR_HTTP_BACKEND=curl FASTR_HTTP_BROWSER_HEADERS=1 FASTR_HTTP_LOG_RETRIES=1 \
  cargo xtask pageset --pages tesco.com,washingtonpost.com
```

## inspect_frag overlays and dumps

- `inspect_frag --dump-json <dir> tests/fixtures/html/block_simple.html` writes `dom.json`, `styled.json`, `box_tree.json`, `fragment_tree.json`, and `display_list.json` for downstream tooling. Pair with `--filter-selector`/`--filter-id` to focus on a specific subtree.
- `inspect_frag --render-overlay out.png <file>` renders the document with overlays for fragment bounds, box ids, stacking contexts, and scroll containers to quickly correlate geometry with the rendered pixels.

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

## Chrome/Perfetto traces from `pageset_progress`

- `pageset_progress run --trace-failures` or `--trace-slow-ms <ms>` re-runs targeted pages with Chrome tracing enabled. Traces are written to `target/pageset/traces/<stem>.json`, and rerun progress/logs live under `target/pageset/trace-progress/` and `target/pageset/logs/*.trace.log`.
- Trace reruns default to more generous budgets: `--trace-timeout` defaults to `timeout * 2`, `--trace-soft-timeout-ms` defaults to the trace timeout minus a 250ms buffer, and `--trace-jobs` defaults to 1 to keep captures stable. Increase the trace timeouts instead of the main ones if the extra tracing overhead trips the hard kill.
- To inspect a trace locally, open Chrome and navigate to `chrome://tracing` then load the JSON, or drag the file into https://ui.perfetto.dev/. `pageset_progress report --include-trace` lists the stems and paths of traces that were collected.

## Pipeline dumps from `pageset_progress`

- `pageset_progress run --dump-failures summary` writes pipeline snapshots for failing pages under `target/pageset/dumps/<stem>/`. Use `--dump-slow-ms <ms> --dump-slow <summary|full>` to capture slow-but-OK pages.
- Dumps default to `--dump-timeout timeout*2` with a soft timeout 250ms earlier; bump the dump timeout instead of the main timeout when captures need extra headroom.
