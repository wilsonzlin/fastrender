# Runtime environment variables (`FASTR_*`)

FastRender has many internal debug/profiling toggles controlled via environment variables. These are intentionally lightweight (read at runtime) and primarily used by:

- `render_pages`
- `fetch_and_render`
- `inspect_frag`

The rendering pipeline parses the environment once into a typed [`RuntimeToggles`](../src/debug/runtime.rs) structure. Library callers can override these values without mutating the process environment by constructing a `RuntimeToggles` instance and supplying it via `FastRender::builder().runtime_toggles(...)` or `RenderOptions::with_runtime_toggles(...)`.

## Commonly useful

- `FASTR_RENDER_TIMINGS=1` – print per-stage timings during rendering (parse/cascade/box/layout/paint).
- `FASTR_FULL_PAGE=1` – expand output to the full document content size (instead of the viewport).
- `FASTR_FETCH_LINK_CSS=0` – skip fetching linked stylesheets (defaults to on).

## Debug dumps (paint / display list)

These are emitted by the paint pipeline:

- `FASTR_DUMP_STACK=1`
- `FASTR_DUMP_FRAGMENTS=1`
- `FASTR_DUMP_COUNTS=1`
- `FASTR_DUMP_TEXT_ITEMS=1`
- `FASTR_DUMP_COMMANDS=<N>` (omit `N` to dump all)
- `FASTR_TRACE_IMAGE_PAINT=<N>` – log up to N image paints (empty value defaults to 50).
- `FASTR_LOG_IMAGE_FAIL=1` – log failed image loads/placeholders.

## Debug dumps (layout / fragments)

- `FASTR_LOG_FRAG_BOUNDS=1` – log fragment-tree bounds vs the viewport.
- `FASTR_DUMP_TEXT_FRAGMENTS=<N>` – sample text fragments (default 20 when enabled).
- `FASTR_TRACE_TEXT=<substring>` – walk the fragment tree and print a containment trail for the first text fragment containing the substring.
- `FASTR_FIND_TEXT` / `FASTR_FIND_BOX_TEXT` / `FASTR_INSPECT_MASK` – search for matching text fragments/boxes.
- `FASTR_NEEDLE=<string>` – generic string matcher used by various debug paths.

## Performance / profiling

- `FASTR_CASCADE_PROFILE=1` – cascade profiling.
- `FASTR_LAYOUT_PROFILE=1` – layout-context profiling.
- `FASTR_FLEX_PROFILE=1` – flex profiling (with additional `FASTR_FLEX_PROFILE_*` knobs).
- `FASTR_INTRINSIC_STATS=1` – intrinsic sizing cache stats.
- `FASTR_LAYOUT_CACHE_STATS=1` – layout cache stats (intrinsic cache hits/misses and pass counts).
- `FASTR_DISABLE_LAYOUT_CACHE=1` / `FASTR_DISABLE_FLEX_CACHE=1` – disable layout/flex caches.
- `FASTR_IMAGE_PROFILE_MS=<ms>` / `FASTR_STACK_PROFILE_MS=<ms>` / `FASTR_TEXT_PROFILE_MS=<ms>` / `FASTR_CMD_PROFILE_MS=<ms>` – emit timing when operations exceed the threshold.

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
- `FASTR_MEDIA_TYPE=screen|print|all|speech` and `FASTR_SCRIPTING=none|initial-only|enabled`

## Layout logging (grab-bag)

- `FASTR_LOG_CONTAINER_FIELDS` / `FASTR_LOG_CONTAINER_PASS` / `FASTR_LOG_CONTAINER_REUSE` – container query tracing.
- `FASTR_LOG_CONTAINER_IDS=<ids>` – restrict container logs to IDs.
- `FASTR_LOG_FLEX_*` – flexbox logging (constraints, child placement, overflow, drift; see `RuntimeToggles` for full list).
- `FASTR_LOG_BLOCK_PROGRESS_MS=<ms>` – per-child progress logging in block layout (with optional `FASTR_LOG_BLOCK_PROGRESS_IDS`/`MATCH` filters).
- `FASTR_LOG_LINE_WIDTH` / `FASTR_LOG_INLINE_BASELINE` / `FASTR_LOG_OVERFLOW_TEST` – inline layout diagnostics.
- `FASTR_LOG_TABLE` / `FASTR_DUMP_CELL_CHILD_Y` – table layout tracing.
- `FASTR_LOG_ABS_CLAMP` – clamp logging for absolutely positioned elements.

## Source of truth

These flags are internal and evolve. To find the full current set, run:

`rg -o --no-filename "FASTR_[A-Z0-9_]+" -S src tests | sort -u`
