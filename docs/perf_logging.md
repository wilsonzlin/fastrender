# Performance logging

Helpful environment variables for profiling layout/cascade on large pages. The canonical list is in [env-vars.md](env-vars.md); this page highlights the ones that tend to be most actionable.

When using `render_pages`/`fetch_and_render`, per-page logs are written to `fetches/renders/<page>.log` and a summary to `fetches/renders/_summary.log`; review these alongside the flags below when investigating slow or blank renders.

- `FASTR_CASCADE_PROFILE=1`
  - Enables cascade profiling. Logs node count, candidate/match counts, and timing breakdown for selector matching, declaration application, and pseudo computation at the end of `apply_styles`.

- Container query second-pass logging (used in `render_pages`/`fetch_and_render`):
  - `FASTR_LOG_CONTAINER_PASS=1` prints the number of query containers (size vs inline-size) and a few samples of their dimensions/names when the second cascade/layout runs.
  - `FASTR_LOG_CONTAINER_REUSE=1` reports how many styled nodes are reused during the container-query recascade.
  - `FASTR_LOG_CONTAINER_DIFF=<n>` samples up to `n` styled ids whose fingerprints changed between passes (tag/#id/.class + fingerprint).
  - `FASTR_LOG_CONTAINER_IDS=<id,id,...>` dumps styled summaries for specific ids during the second pass to trace why they differ.
  - `FASTR_LOG_CONTAINER_FIELDS=1` lists which layout-affecting fields changed for the sampled diff entries.
  - `FASTR_LOG_CONTAINER_QUERY=1` logs container sizes while building the container-query context (useful when debugging “why did this query match?”).

These env vars are read in the rendering binaries (`render_pages`, `fetch_and_render`) and cascade internals; leave them unset for normal runs.

Other useful profiling flags
----------------------------

- `FASTR_RENDER_TIMINGS=1` — prints high-level timing for parse/cascade/box/layout/paint per page in the render binaries.
- `FASTR_LAYOUT_PROFILE=1` — enables layout-context profiling (block/inline/flex/grid/table/absolute) with call counts and inclusive times.
- `FASTR_FLEX_PROFILE=1` — flex-specific profiling (measure/compute/convert stats, cache hits). Optional helpers:
  - `FASTR_FLEX_PROFILE_NODES=1`
  - `FASTR_FLEX_PROFILE_NODE_KEYS=1`
  - `FASTR_FLEX_PROFILE_HIST=1`
- `FASTR_INTRINSIC_STATS=1` — reports intrinsic sizing cache hits/misses/lookups after layout.

All profiling logs are best run in release builds to reflect real performance.
