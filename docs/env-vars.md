# Runtime environment variables (`FASTR_*`)

FastRender has many internal debug/profiling toggles controlled via environment variables. These are intentionally lightweight (read at runtime) and primarily used by:

- `render_pages`
- `fetch_and_render`
- `inspect_frag`

## Commonly useful

- `FASTR_RENDER_TIMINGS=1` – print per-stage timings during rendering (parse/cascade/box/layout/paint).
- `FASTR_FULL_PAGE=1` – expand output to the full document content size (instead of the viewport).

## Debug dumps (paint / display list)

These are emitted by the paint pipeline:

- `FASTR_DUMP_STACK=1`
- `FASTR_DUMP_FRAGMENTS=1`
- `FASTR_DUMP_COUNTS=1`
- `FASTR_DUMP_TEXT_ITEMS=1`
- `FASTR_DUMP_COMMANDS=<N>` (omit `N` to dump all)

## Debug dumps (layout / fragments)

- `FASTR_LOG_FRAG_BOUNDS=1` – log fragment-tree bounds vs the viewport.
- `FASTR_DUMP_TEXT_FRAGMENTS=<N>` – sample text fragments (default 20 when enabled).
- `FASTR_TRACE_TEXT=<substring>` – walk the fragment tree and print a containment trail for the first text fragment containing the substring.

## Performance / profiling

- `FASTR_CASCADE_PROFILE=1` – cascade profiling.
- `FASTR_LAYOUT_PROFILE=1` – layout-context profiling.
- `FASTR_FLEX_PROFILE=1` – flex profiling (with additional `FASTR_FLEX_PROFILE_*` knobs).
- `FASTR_INTRINSIC_STATS=1` – intrinsic sizing cache stats.
- `FASTR_LAYOUT_CACHE_STATS=1` – layout cache stats (intrinsic cache hits/misses and pass counts).
- `FASTR_SELECTOR_BLOOM=0` – disable selector bloom-filter hashing (useful for perf A/B checks).

## Parallelism tuning

- `FASTR_DISPLAY_LIST_PARALLEL=0|1` – enable/disable Rayon fan-out while building display lists (default enabled).
- `FASTR_DISPLAY_LIST_PARALLEL_MIN=<N>` – fragment-count threshold before the builder fans out (default 32).

## Media query overrides

These override user-preference media queries (and are also settable via CLI flags on the render binaries):

- `FASTR_PREFERS_COLOR_SCHEME=light|dark|no-preference`
- `FASTR_PREFERS_CONTRAST=more|high|less|low|custom|forced|no-preference`
- `FASTR_PREFERS_REDUCED_MOTION=reduce|no-preference`
- `FASTR_PREFERS_REDUCED_DATA=reduce|no-preference`
- `FASTR_PREFERS_REDUCED_TRANSPARENCY=reduce|no-preference`

## Source of truth

These flags are internal and evolve. To find the full current set, run:

`rg -o --no-filename "FASTR_[A-Z0-9_]+" -S src tests | sort -u`
