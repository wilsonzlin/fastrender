# Performance logging

Helpful environment variables for profiling layout/cascade on large pages. The canonical list is in [env-vars.md](env-vars.md); this page highlights the ones that tend to be most actionable.

When using `render_pages`/`fetch_and_render`, per-page logs are written to `fetches/renders/<page>.log` and a summary to `fetches/renders/_summary.log`; review these alongside the flags below when investigating slow or blank renders.

- `FASTR_CASCADE_PROFILE=1`
  - Enables cascade profiling. Logs node count, candidate/match counts, and timing breakdown for selector matching, declaration application, and pseudo computation at the end of `apply_styles`.

- `FASTR_TRACE_OUT=/tmp/trace.json`
  - Writes a Chrome trace of the render pipeline (fetch/decode/parse/style/layout/paint). Open it in `chrome://tracing` or Perfetto to inspect spans.
  - `fetch_and_render` also supports `--trace-out trace.json`, and library consumers can set `RenderOptions::with_trace_output`.

- Container query second-pass logging (used in `render_pages`/`fetch_and_render`):
  - `FASTR_LOG_CONTAINER_PASS=1` prints the number of query containers (size vs inline-size) and a few samples of their dimensions/names when the second cascade/layout runs.
  - `FASTR_LOG_CONTAINER_REUSE=1` reports how many styled nodes are reused during the container-query recascade.
  - `FASTR_LOG_CONTAINER_DIFF=<n>` samples up to `n` styled ids whose fingerprints changed between passes (tag/#id/.class + fingerprint).
  - `FASTR_LOG_CONTAINER_IDS=<id,id,...>` dumps styled summaries for specific ids during the second pass to trace why they differ.
  - `FASTR_LOG_CONTAINER_FIELDS=1` lists which layout-affecting fields changed for the sampled diff entries.
  - `FASTR_LOG_CONTAINER_QUERY=1` logs container sizes while building the container-query context (useful when debugging “why did this query match?”).

These env vars are read in the rendering binaries (`render_pages`, `fetch_and_render`) and cascade internals; leave them unset for normal runs.

## Other useful profiling flags

- `FASTR_RENDER_TIMINGS=1` — prints high-level timing for parse/cascade/box/layout/paint per page in the render binaries.
- `FASTR_LAYOUT_PROFILE=1` — enables layout-context profiling (block/inline/flex/grid/table/absolute) with call counts and inclusive times.
- `FASTR_FLEX_PROFILE=1` — flex-specific profiling (measure/compute/convert stats, cache hits). Optional helpers:
  - `FASTR_FLEX_PROFILE_NODES=1`
  - `FASTR_FLEX_PROFILE_NODE_KEYS=1`
  - `FASTR_FLEX_PROFILE_HIST=1`
- `FASTR_INTRINSIC_STATS=1` — reports intrinsic sizing cache hits/misses/lookups after layout.
- `FASTR_LAYOUT_CACHE_STATS=1` — reports layout cache stats (intrinsic cache hits/misses, layout pass count).
- `FASTR_DISPLAY_LIST_PARALLEL_MIN=<N>` — lowers the display list parallel fan-out threshold when debugging determinism or forcing rayon paths in tests.

All profiling logs are best run in release builds to reflect real performance.

## Pipeline benchmarks (Criterion)

Run `cargo bench --bench perf_regressions` to exercise each stage of the rendering pipeline with fixed fixtures and bundled fonts (`tests/fixtures/fonts/DejaVuSans-subset.*`):

- `bench_parse_dom`
- `bench_css_parse`
- `bench_cascade`
- `bench_box_generation`
- `bench_layout_{block,flex,grid,table}`
- `bench_paint_{display_list_build,optimize,rasterize}`
- `bench_end_to_end_small` / `bench_end_to_end_realistic`

Outputs are written under `target/criterion/<bench>/new/estimates.json` and do not require network access.

### Comparing runs

Use the helper to flag regressions between two Criterion output trees:

```
cargo run --bin bench_compare \
  -- --baseline ../baseline/criterion --new target/criterion \
  --regression-threshold 0.05 --metric mean
```

`--metric` accepts `mean` or `median`; `--regression-threshold` is a relative delta (e.g., 0.05 = 5%). A non-zero exit status indicates regressions suitable for CI gating.

## Microbenchmarks

- Run `cargo bench --bench cascade_bench -- ":has"` to focus on the `:has()` traversal microbench alongside the existing cascade benchmark. The full suite is available via `cargo bench --bench cascade_bench`.
