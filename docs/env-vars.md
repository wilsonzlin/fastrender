# Runtime environment variables (`FASTR_*`)

FastRender has many internal debug/profiling toggles controlled via environment variables. These are intentionally lightweight (read at runtime) and primarily used by:

- `render_pages`
- `fetch_and_render`
- `inspect_frag`

Pageset wrappers (`cargo xtask pageset`, `scripts/pageset.sh`, and the profiling scripts) enable the `disk_cache` cargo feature by default to reuse cached assets; set `DISK_CACHE=0` when invoking the scripts to opt out and force fresh fetches.

The rendering pipeline parses the environment once into a typed [`RuntimeToggles`](../src/debug/runtime.rs) structure. Library callers can override these values without mutating the process environment by constructing a `RuntimeToggles` instance and supplying it via `FastRender::builder().runtime_toggles(...)` or `RenderOptions::with_runtime_toggles(...)`.

## Pageset disk cache tuning

These are parsed by the pageset CLI binaries (`prefetch_assets`, `render_pages`, `pageset_progress`, `fetch_and_render`) and wire into `DiskCacheConfig` when built with the `disk_cache` cargo feature.

- `FASTR_DISK_CACHE_MAX_BYTES=<bytes>` – on-disk subresource cache size limit (0 disables eviction; default 512MB).
- `FASTR_DISK_CACHE_MAX_AGE_SECS=<secs>` – cap cached entry age (0 disables age-based expiry; default 7 days).
- `FASTR_DISK_CACHE_LOCK_STALE_SECS=<secs>` – treat disk-cache `.lock` files older than this as stale and remove them (default 8 seconds).

CLI flag equivalents: `--disk-cache-max-bytes`, `--disk-cache-max-age-secs`, `--disk-cache-lock-stale-secs`.

## Commonly useful

- `FASTR_RENDER_TIMINGS=1` – print per-stage timings during rendering (parse/cascade/box/layout/paint).
- `FASTR_FULL_PAGE=1` – expand output to the full document content size (instead of the viewport).
- `FASTR_USE_BUNDLED_FONTS=1` – disable system font discovery and use the bundled fixtures (default in CI).
- `FASTR_BUNDLE_EMOJI_FONT=0|1` – explicitly enable/disable the bundled emoji font fixture (on by default in bundled mode/CI).
- `FASTR_FETCH_LINK_CSS=0` – skip fetching linked stylesheets from `<link>` elements (defaults to on; does not affect `@import` loads).
- `FASTR_FETCH_PRELOAD_STYLESHEETS=0|1` – control whether `<link rel=preload as=style>` entries are treated as stylesheet candidates (defaults to on).
- `FASTR_FETCH_MODULEPRELOAD_STYLESHEETS=0|1` – opt into treating `<link rel=modulepreload as=style>` as stylesheet candidates (defaults to off).
- `FASTR_FETCH_ALTERNATE_STYLESHEETS=0|1` – allow skipping `<link rel="alternate stylesheet">` entries when disabled (defaults to on).
- `FASTR_PAINT_BACKEND=display_list|legacy` – select the paint pipeline (defaults to `display_list`). Use `legacy` to force the immediate painter.

## HTTP fetch tuning

These env vars tune the retry/backoff behavior for HTTP(S) requests made by the CLI binaries that construct their fetcher via [`common::render_pipeline::build_http_fetcher`](../src/bin/common/render_pipeline.rs).

They map to [`fastrender::resource::HttpRetryPolicy`](../src/resource.rs) and are parsed once at process startup; invalid values are ignored.

- `FASTR_HTTP_MAX_ATTEMPTS=<N>` – total attempts per HTTP request (initial request + retries). Set to `1` to disable retries (default `3`).
- `FASTR_HTTP_BACKOFF_BASE_MS=<ms>` – base delay for exponential backoff (default `50`).
- `FASTR_HTTP_BACKOFF_CAP_MS=<ms>` – maximum delay between retries (default `500`).
- `FASTR_HTTP_RESPECT_RETRY_AFTER=0|false|no` – disable honoring `Retry-After` headers for retryable responses (enabled by default).

When a CLI timeout is configured (e.g. `fetch_pages --timeout 60` or `prefetch_assets --timeout 30`),
it is treated as a **total** wall-clock budget for a single fetch call when no render deadline is
installed: retry attempts and backoff sleeps are bounded by the remaining budget, and per-attempt
HTTP timeouts are clamped so one request cannot take `max_attempts × timeout`.

This also applies to `fetch_pages` (migrated to `build_http_fetcher` in Task 13), so pageset fetch runs can be tuned without adding new flags.

## Resource limits

- `FASTR_MAX_FOREIGN_OBJECT_CSS_BYTES=<N>` – cap the amount of document-level CSS injected into nested
  `<foreignObject>` HTML renders (default 262_144 bytes).
- `FASTR_INLINE_MAX_STYLESHEETS=<N>` – maximum number of stylesheets inlined across `<link>`/embedded
  discovery and `@import` chains (default 128).
- `FASTR_EMBEDDED_CSS_MAX_CANDIDATES=<N>` – cap the number of stylesheet URLs discovered via the
  embedded `.css` string heuristic (default 16; only used when the HTML has no `<link rel=stylesheet>`
  candidates and no inline `<style>` tags).
- `FASTR_INLINE_MAX_INLINE_CSS_BYTES=<N>` – total CSS bytes allowed when inlining stylesheets
  (default 2 MiB).
- `FASTR_INLINE_MAX_INLINE_IMPORT_DEPTH=<N>` – maximum @import nesting depth during inlining
  (default 8).

## Debug dumps (paint / display list)

These are emitted by the paint pipeline:

- `FASTR_DUMP_STACK=1`
- `FASTR_DUMP_FRAGMENTS=1`
- `FASTR_DUMP_COUNTS=1`
- `FASTR_DUMP_TEXT_ITEMS=1`
- `FASTR_DUMP_COMMANDS=<N>` (omit `N` to dump all)
- `FASTR_TRACE_IMAGE_PAINT=<N>` – log up to N image paints (empty value defaults to 50).
- `FASTR_LOG_IMAGE_FAIL=1` – log failed image loads/placeholders.
- `FASTR_PRESERVE3D_DEBUG=1` – log preserve-3d classification/fallback decisions.
- `FASTR_PRESERVE3D_DISABLE_WARP=1` – force preserve-3d to fall back to affine flattening (projective warping is enabled by default).
- `FASTR_PRESERVE3D_WARP=1` – opt into the warp path when building without the default `preserve3d_warp` feature.

## Debug dumps (layout / fragments)

- `FASTR_LOG_FRAG_BOUNDS=1` – log fragment-tree bounds vs the viewport.
- `FASTR_DUMP_TEXT_FRAGMENTS=<N>` – sample text fragments (default 20 when enabled).
- `FASTR_TEXT_DIAGNOSTICS=verbose` – log sampled clusters that relied on last-resort font fallback.
- `FASTR_TRACE_TEXT=<substring>` – walk the fragment tree and print a containment trail for the first text fragment containing the substring.
- `FASTR_TRACE_FRAGMENTATION=1` – trace fragmentation break opportunities/boundary choices (also available via `inspect_frag --trace-fragmentation`).
- `FASTR_FIND_TEXT` / `FASTR_FIND_BOX_TEXT` / `FASTR_INSPECT_MASK` – search for matching text fragments/boxes.
- `FASTR_NEEDLE=<string>` – generic string matcher used by various debug paths.

## Performance / profiling

- `FASTR_CASCADE_PROFILE=1` – cascade profiling (populates `RenderDiagnostics.stats.cascade` with
  selector candidate/match counters and `:has()` evaluation counters).
- `FASTR_LAYOUT_PROFILE=1` – layout-context profiling.
- `FASTR_FLEX_PROFILE=1` – flex profiling (with additional `FASTR_FLEX_PROFILE_*` knobs).
- `FASTR_PROFILE_FRAGMENT_CLONES=1` – count fragment clones when layout/flex/grid caches reuse measured/layout fragments and enable fragment instrumentation (deep clone counts, traversal).
- `FASTR_INTRINSIC_STATS=1` – intrinsic sizing cache stats.
- `FASTR_LAYOUT_CACHE_STATS=1` – layout cache stats (intrinsic cache hits/misses and pass counts).
- `FASTR_TABLE_STATS=1` – table auto-layout counters (cell intrinsic measurements + per-cell layout calls).
- `FASTR_LAYOUT_CACHE_MAX_ENTRIES=<N>` – per-thread layout cache entry cap (default 8192; set to 0 to disable).
- `FASTR_TRACE_OUT=/path/to/trace.json` – emit Chrome trace events for parse/style/layout/paint.
- `FASTR_DISABLE_LAYOUT_CACHE=1` / `FASTR_DISABLE_FLEX_CACHE=1` – disable layout/flex caches.
- `FASTR_IMAGE_PROFILE_MS=<ms>` / `FASTR_STACK_PROFILE_MS=<ms>` / `FASTR_TEXT_PROFILE_MS=<ms>` / `FASTR_CMD_PROFILE_MS=<ms>` – emit timing when operations exceed the threshold.
- `FASTR_SELECTOR_BLOOM=0` – disable selector bloom-filter hashing (useful for perf A/B checks).
- `FASTR_ANCESTOR_BLOOM=0` – disable the cascade's ancestor bloom filter fast-reject for descendant selectors.
- `FASTR_SVG_FILTER_CACHE_ITEMS=<N>` – SVG filter cache capacity (default 256).
- `FASTR_SVG_FILTER_CACHE_BYTES=<N>` – approximate SVG filter cache size limit in bytes (default 4 MiB).

## Parallelism tuning

- `FASTR_DISPLAY_LIST_PARALLEL=0|1` – enable/disable Rayon fan-out while building display lists (default enabled).
- `FASTR_DISPLAY_LIST_PARALLEL_MIN=<N>` – fragment-count threshold before the builder fans out (default 32).
- `FASTR_PAINT_PARALLEL=off|on|auto` – control tiled parallel rasterization when painting display lists (default `auto`).
- `FASTR_PAINT_PARALLEL_MAX_THREADS=<N>` – cap Rayon worker threads used during tiled paint fan-out (defaults to unlimited; useful when running many worker processes).
- `FASTR_LAYOUT_PARALLEL=off|on|auto` – override layout fan-out mode regardless of RenderOptions/FastRenderConfig (default auto).
- `FASTR_LAYOUT_PARALLEL_MIN_FANOUT=<N>` – sibling threshold before layout attempts to fan out (default 8).
- `FASTR_LAYOUT_PARALLEL_MAX_THREADS=<N>` – cap Rayon worker threads used during layout fan-out.
- `FASTR_LAYOUT_PARALLEL_MIN_NODES=<N>` – minimum box nodes before auto layout fan-out will engage (default 1024).
- `FASTR_LAYOUT_PARALLEL_DEBUG=1` – capture layout parallel debug counters (worker threads/work items) for diagnostics/logging.

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
- `FASTR_LOG_TRANSITIONS=1` – log applied @starting-style transitions with property names and progress.

## Source of truth

These flags are internal and evolve. To find the full current set, run:

`rg -o --no-filename "FASTR_[A-Z0-9_]+" -S src tests | sort -u`
