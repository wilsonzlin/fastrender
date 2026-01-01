# CLI tools

FastRender ships a few small binaries/examples intended for internal debugging and regression work. Prefer `--help` output for the source of truth. Shared flag schemas for viewport/DPR, media type and preferences, output formats, timeouts, and base URL overrides live in [`src/bin/common/args.rs`](../src/bin/common/args.rs).

Compatibility toggles are **opt-in** across the render CLIs. Pass `--compat-profile site` to enable site-specific hacks and `--dom-compat compat` to apply DOM class flips; both default to spec-only behavior. `cargo xtask pageset` and the shell wrappers leave these off unless you explicitly provide the flags so pageset triage can choose when to enable them.

## Convenience scripts (terminal-friendly)

These are optional wrappers for the most common loops:

- Pageset loop (`fetch_pages` → `prefetch_assets` (disk cache only) → `pageset_progress`, defaults to bundled fonts): `scripts/pageset.sh` (defaults to `--features disk_cache`; set `DISK_CACHE=0` or `NO_DISK_CACHE=1` or pass `--no-disk-cache` to opt out; pass extra `pageset_progress run` flags after `--`)
- Run any command under a hard memory cap (uses `prlimit` when available): `scripts/run_limited.sh --as 8G -- <command...>`
- Profile one page with samply (saves profile + prints summary): `scripts/profile_samply.sh <stem|--from-progress ...>` (builds `pageset_progress` with `disk_cache`)
- Profile one page with perf: `scripts/profile_perf.sh <stem|--from-progress ...>` (builds `pageset_progress` with `disk_cache`)
- Summarize a saved samply profile: `scripts/samply_summary.py <profile.json.gz>`

The full pageset workflow is:

`fetch_pages` (HTML) → `prefetch_assets` (CSS/@import/fonts into `fetches/assets/`) → `pageset_progress` (render + write `progress/pages/*.json`).

`cargo xtask pageset` runs all three steps (the prefetch step is skipped when disk cache is disabled). `scripts/pageset.sh` is a lighter wrapper that runs `fetch_pages` → `prefetch_assets` (disk cache only) → `pageset_progress`.

Pageset wrappers enable the disk-backed subresource cache by default, persisting assets under
`fetches/assets/` for repeatable/offline runs. Set `NO_DISK_CACHE=1` or `DISK_CACHE=0` (or pass
`--no-disk-cache` to the wrappers) to force in-memory-only fetches.

## `cargo xtask`

`cargo xtask` is the preferred entry point for common workflows; it wraps the binaries below but keeps them usable directly.

- Help: `cargo xtask --help`
- Tests: `cargo xtask test [core|style|fixtures|wpt|all]`
- Refresh goldens: `cargo xtask update-goldens [all|fixtures|reference|wpt]` (sets the appropriate `UPDATE_*` env vars)
- Pageset scoreboard (`fetch_pages` → `prefetch_assets` → `pageset_progress` when disk cache is enabled; bundled fonts by default): `cargo xtask pageset [--pages example.com,news.ycombinator.com] [--shard 0/4] [--no-fetch] [--no-disk-cache] [-- <pageset_progress args...>]`
  - Sharded example: `cargo xtask pageset --shard 0/4` (applies to fetch + prefetch (disk cache only) + render; add `--no-fetch` to reuse cached pages)
  - Forward compatibility gates when needed: `--compat-profile site` and/or `--dom-compat compat` are passed through to `pageset_progress run` but remain off by default.
  - Disk cache tuning flags passed after `--` (e.g. `--disk-cache-max-bytes`, `--disk-cache-max-age-secs`, `--disk-cache-lock-stale-secs`) are also forwarded to `prefetch_assets` when it runs.
- Pageset diff: `cargo xtask pageset-diff [--baseline <dir>|--baseline-ref <git-ref>] [--no-run] [--fail-on-regression] [--fail-on-missing-stages] [--fail-on-missing-stage-timings]` (extracts `progress/pages` from the chosen git ref by default and compares it to the freshly updated scoreboard). `--fail-on-regression` also enables the missing-stage gates by default (use `--no-fail-on-missing-stages` / `--no-fail-on-missing-stage-timings` to opt out).
- Render one page: `cargo xtask render-page --url https://example.com --output out.png [--viewport 1200x800 --dpr 1.0 --full-page]`
- Diff renders: `cargo xtask diff-renders --before fetches/renders/baseline --after fetches/renders/new [--output target/render-diffs]`
- Import a bundled capture into a `pages_regression` fixture: `cargo xtask import-page-fixture <bundle_dir|.tar> <fixture_name> [--output-root tests/pages/fixtures --overwrite --dry-run]`
- Update `tests/pages/pageset_timeouts.json` from the pageset scoreboard: `cargo xtask update-pageset-timeouts` (defaults to crawl-based capture for missing fixtures; use `--capture-missing-fixtures --capture-mode render` to force render-driven bundle capture).
- Perf smoke: `cargo xtask perf-smoke [--suite core|pageset-timeouts|all] [--only flex_dashboard,grid_news] [--top 5 --baseline baseline.json --threshold 0.05 --fail-on-regression]` (offline fixtures, bundled fonts, JSON summary at `target/perf_smoke.json`). Use `--suite pageset-timeouts` to time the captured pageset timeout fixtures offline; missing fixtures are skipped so the suite still runs when only a subset is available.

`render-page` wraps `fetch_and_render` in release mode by default (add `--debug` to keep a debug build).

## `fetch_pages`

- Purpose: fetch a curated set of real pages and cache HTML under `fetches/html/` (and metadata alongside).
- Entry: `src/bin/fetch_pages.rs`
- Run: `cargo run --release --bin fetch_pages -- --help`
- Supports deterministic sharding with `--shard <index>/<total>` when splitting the page list across workers.
- Cache filenames and `--pages` filters use the canonical stem from `normalize_page_name` (strip the scheme and a leading `www.`). Colliding stems fail fast unless you opt into `--allow-collisions`, which appends a deterministic suffix.
- **Migration:** cached HTML written before canonical stems were enforced may be ignored. Delete stale `fetches/html/*.html` entries and re-run `fetch_pages`.

## `prefetch_assets`

- Purpose: warm the subresource cache (`fetches/assets/`) by prefetching linked stylesheets and their `@import` chains (plus referenced fonts) for the cached pages under `fetches/html/`. This makes subsequent pageset renders more repeatable and reduces time spent fetching during `pageset_progress`.
- Entry: `src/bin/prefetch_assets.rs`
- Run: `cargo run --release --bin prefetch_assets -- --help`
- Most useful when built with `--features disk_cache` (so cache entries persist across processes).
- Key flags: page selection (`--pages`), deterministic sharding (`--shard <index>/<total>`), parallelism (`--jobs`), and fetch timeout (`--timeout`). See `--help` for the full flag list.
- Disk cache tuning flags (`--disk-cache-max-age-secs`, `--disk-cache-max-bytes`, `--disk-cache-lock-stale-secs`, or the corresponding `FASTR_DISK_CACHE_*` env vars) match the pageset render binaries.

## `render_pages`

- Purpose: render all cached HTML in `fetches/html/` to `fetches/renders/` (PNG + per-page logs + `_summary.log`).
- Entry: `src/bin/render_pages.rs`
- Run: `cargo run --release --bin render_pages -- --help`
- Accepts `--shard <index>/<total>` to render a slice of the cached pages in a stable order.
- `--pages` (and positional filters) use the same canonical stems as `fetch_pages` (strip scheme + leading `www.`). Cached filenames are normalized when matching filters so `www.`/non-`www` variants map consistently.
- Optional outputs:
  - `--diagnostics-json` writes `fetches/renders/<page>.diagnostics.json` containing status, timing, and `RenderDiagnostics`.
  - `--dump-intermediate {summary|full}` emits per-page summaries or full JSON dumps of DOM/styled/box/fragment/display-list stages (use `--only-failures` to gate large artifacts on errors); `full` also writes a combined `fetches/renders/<page>.snapshot.json` pipeline snapshot.
- Layout fan-out defaults to `auto` (only engages once the box tree is large enough and has sufficient independent sibling work); use `--layout-parallel off` to force serial layout, `--layout-parallel on` to force fan-out, or tune thresholds with `--layout-parallel-min-fanout`, `--layout-parallel-auto-min-nodes`, and `--layout-parallel-max-threads`.

## `fetch_and_render`

- Purpose: fetch one URL (or read one `file://` target) and render to a PNG.
- Entry: `src/bin/fetch_and_render.rs`
- Run: `cargo run --release --bin fetch_and_render -- --help`
- Security defaults mirror the library: `file://` subresources are blocked for HTTP(S) documents. Use `--allow-file-from-http` to override during local testing, `--block-mixed-content` to forbid HTTP under HTTPS, and `--same-origin-subresources` (plus optional `--allow-subresource-origin`) to block cross-origin CSS/images/fonts when rendering untrusted pages.
- Performance: layout fan-out defaults to `auto` (with optional `--layout-parallel-min-fanout` / `--layout-parallel-auto-min-nodes` / `--layout-parallel-max-threads`). Use `--layout-parallel off` to force serial layout or `--layout-parallel on` to force fan-out when chasing wall-time regressions on wide pages.

## `bundle_page`

- Purpose: capture a page (HTML + subresources) into a self-contained bundle and replay it offline.
- Entry: `src/bin/bundle_page.rs`
- Run:
  - Fetch: `cargo run --release --bin bundle_page -- fetch <url> --out <bundle_dir|.tar>`
    - For pages that crash or time out during capture, add `--no-render` (alias `--crawl`) to discover subresources by parsing HTML + CSS instead of rendering.
    - Use `--fetch-timeout-secs <secs>` to bound per-request network time when crawling large pages.
  - Render: `cargo run --release --bin bundle_page -- render <bundle> --out <png>`
- Security: `--same-origin-subresources` (plus optional `--allow-subresource-origin`) applies both when capturing and replaying bundles to keep cross-origin assets out of offline artifacts.
- Convert bundles to offline fixtures for the `pages_regression` harness: `cargo xtask import-page-fixture <bundle> <fixture_name> [--output-root tests/pages/fixtures --overwrite --dry-run]`. All HTML/CSS references are rewritten to hashed files under `assets/`, and the importer fails if any network URLs would remain.

## `inspect_frag`

- Purpose: inspect fragment output (and related style/layout state) for a single input.
- Entry: `src/bin/inspect_frag.rs`
- Run: `cargo run --release --bin inspect_frag -- --help`
- `--dump-json <dir>` writes deterministic snapshots of each pipeline stage (`dom.json`, `styled.json`, `box_tree.json`, `fragment_tree.json`, `display_list.json`). Pair with `--filter-selector` / `--filter-id` to focus on a subtree.
- `--dump-snapshot` prints a combined pipeline snapshot JSON to stdout (and exits).
- `--render-overlay <png>` renders the page with optional overlays for fragment bounds, box ids, stacking contexts, and scroll containers.
- Pagination/debugging: set `FASTR_FRAGMENTATION_PAGE_HEIGHT=<css px>` (and optional `FASTR_FRAGMENTATION_GAP=<css px>`) to paginate layout during inspection. The fixture at `tests/fixtures/inspect_frag_two_pages.html` forces two pages via `@page` and `break-after: page`:

  ```bash
  FASTR_FRAGMENTATION_PAGE_HEIGHT=200 cargo run --release --bin inspect_frag -- tests/fixtures/inspect_frag_two_pages.html
  ```

  Searching for `"Second page"` should show hits on `[root 1]`.

## `diff_renders`

- Purpose: compare two render output directories (e.g., `fetches/renders` from two revisions) and summarize pixel diffs.
- Entry: `src/bin/diff_renders.rs`
- Run: `cargo run --release --bin diff_renders -- --before <dir> --after <dir>`
- Outputs: `diff_report.json` and `diff_report.html` plus per-page diff PNGs alongside the HTML report.
- Tuning: `--tolerance` and `--max-diff-percent` accept the same values as the fixture harness (`FIXTURE_TOLERANCE`, `FIXTURE_MAX_DIFFERENT_PERCENT`, `FIXTURE_FUZZY` env vars are honored when flags are omitted).
- Supports deterministic sharding with `--shard <index>/<total>` to split large sets across workers.

## `diff_snapshots`

- Purpose: compare pipeline snapshots (`*.snapshot.json`) and highlight stage-level deltas that explain pixel diffs.
- Entry: `src/bin/diff_snapshots.rs`
- Run: `cargo run --release --bin diff_snapshots -- --before <dir|file> --after <dir|file>`
- Matching: directory inputs pair `*.snapshot.json` by stem (same matching rules as `diff_renders`).
- Outputs: `diff_snapshots.json` and `diff_snapshots.html` summarizing schema versions, per-stage counts, DOM/box/fragment/display list changes, and links to sibling `*.png` renders when present.

## `dump_a11y`

- Purpose: emit the computed accessibility tree for a document as JSON (without painting).
- Entry: `src/bin/dump_a11y.rs`
- Run: `cargo run --release --bin dump_a11y -- --help`

## Offline / cached captures

- Use `bundle_page fetch` to save a single reproducible capture (HTML bytes, content-type + final URL, all fetched CSS/image/font subresources with HTTP metadata, and a manifest mapping original URLs to bundle paths). Bundles can be directories or `.tar` archives and are deterministic.
- Replay with `bundle_page render <bundle> --out out.png` to render strictly from the bundle with zero network calls.
- For larger batch workflows, offline captures are also available via the existing on-disk caches:
  - `fetch_pages` writes HTML under `fetches/html/` and a `*.html.meta` sidecar with the original content-type and final URL.
  - `render_pages` and `fetch_and_render` use the shared disk-backed fetcher (when built with `--features disk_cache`; enabled by default in `scripts/pageset.sh`, `cargo xtask pageset`, and the profiling scripts) for subresources, writing into `fetches/assets/`. After one online render, you can re-run against the same caches without network access (new URLs will still fail). Use `--no-disk-cache`, `DISK_CACHE=0`, or `NO_DISK_CACHE=1` to opt out.
  - Fresh HTTP caching headers are honored by default for disk-backed fetches; add `--no-http-freshness` to `fetch_and_render`, `render_pages`, or `pageset_progress` to force revalidation even when Cache-Control/Expires mark entries as fresh.
  - Disk-backed cache tuning (applies to `prefetch_assets`, `pageset_progress`, `render_pages`, and `fetch_and_render` when built with `disk_cache`):
    - `--disk-cache-max-age-secs <secs>` (or `FASTR_DISK_CACHE_MAX_AGE_SECS=<secs>`) caps how long cached subresources are trusted before forcing a refetch. Use `0` to disable age-based expiry (never age out).
    - `--disk-cache-max-bytes <bytes>` (or `FASTR_DISK_CACHE_MAX_BYTES=<bytes>`) sets the eviction budget for on-disk cached bytes. Use `0` to disable eviction.
    - `--disk-cache-lock-stale-secs <secs>` (or `FASTR_DISK_CACHE_LOCK_STALE_SECS=<secs>`) controls how quickly stale `.lock` files are removed when workers are hard-killed mid-write (default 8 seconds).
    - Defaults are `512MB`, `7d`, and `8s`. Example: `pageset_progress run --disk-cache-max-age-secs 0` keeps cached subresources pinned to avoid surprise refetches during short timeout runs.
- Asset fetches in library code go through [`fastrender::resource::CachingFetcher`] in-memory by default, or [`fastrender::resource::DiskCachingFetcher`] behind the optional `disk_cache` feature.

## Diagnostics

- `render_pages` emits per-page logs in `fetches/renders/<page>.log` plus a summary in `_summary.log`. CSS fetch failures show up there and correspond to `ResourceKind::Stylesheet` entries in the library diagnostics model.
- The library API exposes structured diagnostics via `render_url_with_options` returning `RenderResult { pixmap, accessibility, diagnostics }`. Set `RenderOptions::allow_partial(true)` to receive a placeholder image and a `document_error` string when the root fetch fails; subresource fetch errors are collected in `diagnostics.fetch_errors` with status codes, final URLs, and any cache validators observed.
- `render_pages` can emit structured reports via `--diagnostics-json` (per-page) plus `--dump-intermediate` for summaries or full intermediate dumps.
- The shipped binaries print a concise diagnostics summary (including status/final URL). Pass `--verbose` to `fetch_and_render` or `render_pages` to include full error chains when something fails.

## `pageset_progress`

- Purpose: **update the committed pageset scoreboard** under `progress/pages/*.json`.
- Entry: `src/bin/pageset_progress.rs`
- Run:
  - Help: `cargo run --release --bin pageset_progress -- run --help`
  - Typical: `cargo run --release --bin pageset_progress -- run --timeout 5`
  - Compatibility (opt-in only): `--compat-profile site` enables site-specific hacks and
    `--dom-compat compat` applies DOM class flips. Defaults stay spec-only; `cargo xtask
    pageset` forwards the flags only when you provide them.
- Fonts: pass `--bundled-fonts` to skip system font discovery (default in the pageset wrappers) or
  `--font-dir <path>` to load fonts from a specific directory without hitting host fonts.
- Sync: `cargo run --release --bin pageset_progress -- sync [--prune] [--html-dir fetches/html --progress-dir progress/pages]` bootstraps one JSON per pageset URL without needing any caches. `--prune` removes stale progress files for URLs no longer in the list. Stems are collision-aware (`example.com--deadbeef` when needed) to keep cache and progress filenames unique.
- Migrate: `cargo run --release --bin pageset_progress -- migrate [--html-dir fetches/html --progress-dir progress/pages]` rewrites existing progress JSON without fetching or rendering. It applies legacy schema migrations (notably splitting mixed legacy `notes` into durable `notes` + machine `auto_notes`) and reserializes deterministically using the runner's canonical formatter.
- Progress filenames use the cache stem from `pageset_stem` (strip scheme + leading `www.` plus a deterministic hash suffix on collisions); `--pages` filters accept the URL, canonical stem, or cache stem. If you have older `fetches/html` entries with `www.` prefixes in the filename, re-run `fetch_pages` so progress filenames line up.
  - For temporary/test runs, `FASTR_PAGESET_URLS="https://a.com,https://b.com"` overrides the built-in pageset everywhere.
- Triage reruns (reuse existing `progress/pages/*.json` instead of typing stems):
  - `--from-progress <dir>` enables selection from saved progress files (default intersection of filters, use `--union` to OR them).
  - Filters: `--only-failures`, `--only-status timeout,panic,error`, `--slow-ms <ms> [--slow-ok-only]`, `--hotspot cascade|layout|paint|...`, `--top-slowest <n>`.
  - The deterministic stem list is printed before running; if nothing matches, the command exits cleanly without touching caches.
- Report: `cargo run --release --bin pageset_progress -- report [--progress-dir progress/pages --top 10 --fail-on-bad --compare <other> --fail-on-regression --regression-threshold-percent 10]` prints status counts, slowest pages, and hotspot histograms for the saved progress files. With `--compare`, it also prints status transitions plus the top regressions/improvements by `total_ms`; `--fail-on-regression` exits non-zero for ok→bad or > threshold slowdowns.
  - `--include-trace` lists saved Chrome traces (from `target/pageset/traces/` + `target/pageset/trace-progress/`).
  - `--verbose-stats` prints structured per-page stats when present (including resource cache hit/miss/bytes breakdowns, single-flight inflight wait time, disk cache lock waits, and network fetch totals). It also prints an aggregated "Resource totals" summary plus top-N rankings for network/inflight/disk cache time (including disk lock wait time), and top-N rankings per stage bucket (fetch/css/cascade/layout/paint).
- Safety: uses **panic containment** (per-page worker process) and a **hard timeout** (kills runaway workers) so one broken page cannot stall the whole run.
- Outputs:
  - `progress/pages/<stem>.json` — small, committed per-page progress artifact
  - `target/pageset/logs/<stem>.log` — per-page log (not committed)
  - `target/pageset/logs/<stem>.stderr.log` — worker stdout/stderr, including panic
    backtraces and a note if the parent kills the process on timeout (not committed)
  - Optional cascade profiling reruns: `--cascade-diagnostics` re-runs slow cascade pages and
    cascade timeouts with cascade profiling enabled (`FASTR_CASCADE_PROFILE=1`), then merges the
    resulting selector candidate/match counters into the committed progress JSON under
    `diagnostics.stats.cascade`.
    - Slow threshold: `--cascade-diagnostics-slow-ms <ms>` (defaults to 500ms).
    - Temp rerun progress dir (not committed): `--cascade-diagnostics-progress-dir <dir>` (defaults
      to `target/pageset/cascade-progress/`).
  - Optional traces: `--trace-failures` / `--trace-slow-ms <ms>` rerun targeted pages with Chrome tracing enabled; tune trace rerun budgets with `--trace-timeout` (defaults to `timeout * 2`), `--trace-soft-timeout-ms`, and `--trace-jobs` (defaults to 1 to avoid contention). Traces land in `target/pageset/traces/<stem>.json` with rerun progress under `target/pageset/trace-progress/<stem>.json` and logs at `target/pageset/logs/<stem>.trace.log`.
  - Workers accept `--layout-parallel {off|on|auto}` (plus `--layout-parallel-min-fanout` / `--layout-parallel-auto-min-nodes` / `--layout-parallel-max-threads`). The default is `auto`, so small pages stay serial while large pages can fan out across Rayon threads.
