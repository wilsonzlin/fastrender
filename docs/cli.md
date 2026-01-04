# CLI tools

FastRender ships a few small binaries/examples intended for internal debugging and regression work. Prefer `--help` output for the source of truth. Shared flag schemas for viewport/DPR, media type and preferences, output formats, timeouts, and base URL overrides live in [`src/bin/common/args.rs`](../src/bin/common/args.rs).

Compatibility toggles are **opt-in** across the render CLIs. Pass `--compat-profile site` to enable site-specific hacks and `--dom-compat compat` to apply DOM class flips; both default to spec-only behavior. `cargo xtask pageset` and the shell wrappers leave these off unless you explicitly provide the flags so pageset triage can choose when to enable them.

## Convenience scripts (terminal-friendly)

These are optional wrappers for the most common loops:

- Pageset loop (`fetch_pages` → `prefetch_assets` (disk cache only) → `pageset_progress`, defaults to bundled fonts): `scripts/pageset.sh`
  - Defaults to `--features disk_cache`; set `DISK_CACHE=0` or `NO_DISK_CACHE=1` or pass `--no-disk-cache` to opt out; pass `--disk-cache` to force-enable.
  - Supports `--jobs/-j`, `--fetch-timeout`, `--render-timeout`, `--cache-dir`, `--no-fetch`, `--allow-collisions`, and `--timings`.
  - Prefetch toggles like `--prefetch-fonts` passed after `--` are forwarded to `prefetch_assets`.
  - Pass extra `pageset_progress run` flags after `--`.
- Cached-pages Chrome-vs-FastRender diff (best-effort; non-deterministic): `scripts/chrome_vs_fastrender.sh [--pages example.com,github.com] [--out target/chrome_vs_fastrender]`
  - Wraps `scripts/chrome_baseline.sh`, `render_pages`, and `diff_renders` into one command.
  - Defaults to `viewport=1200x800`, `dpr=1.0`, JavaScript disabled (to match FastRender’s “no JS” model).
  - Writes a report at `<out>/diff_report.html` (default: `target/chrome_vs_fastrender/diff_report.html`).
- Run any command under a hard memory cap (uses `prlimit` when available): `scripts/run_limited.sh --as 8G -- <command...>`
- Profile one page with samply (saves profile + prints summary): `scripts/profile_samply.sh <stem|--from-progress ...>` (builds `pageset_progress` with `disk_cache`)
- Profile one page with perf: `scripts/profile_perf.sh <stem|--from-progress ...>` (builds `pageset_progress` with `disk_cache`)
- Summarize a saved samply profile: `scripts/samply_summary.py <profile.json.gz>`

The full pageset workflow is:

`fetch_pages` (HTML) → `prefetch_assets` (CSS/@import/fonts into `fetches/assets/` by default; override with `--cache-dir <dir>`) → `pageset_progress` (render + write `progress/pages/*.json`).

`cargo xtask pageset` runs all three steps (the prefetch step is skipped when disk cache is disabled). `scripts/pageset.sh` is a lighter wrapper that runs `fetch_pages` → `prefetch_assets` (disk cache only) → `pageset_progress`.

Pageset wrappers enable the disk-backed subresource cache by default, persisting assets under
`fetches/assets/` (override with `--cache-dir <dir>`) for repeatable/offline runs. Set
`NO_DISK_CACHE=1` or `DISK_CACHE=0` (or pass
`--no-disk-cache` to the wrappers) to force in-memory-only fetches. Pass `--disk-cache` to
`cargo xtask pageset` (or `scripts/pageset.sh`) to override an ambient `NO_DISK_CACHE=1` /
`DISK_CACHE=0` environment when you explicitly want the on-disk cache enabled.

## HTTP fetch knobs (env vars)

The pageset-oriented CLI binaries (`fetch_pages`, `prefetch_assets`, `render_pages`, `fetch_and_render`, and `pageset_progress`) all build their network fetcher through [`common::render_pipeline::build_http_fetcher`](../src/bin/common/render_pipeline.rs), so they honor the `FASTR_HTTP_*` environment variables documented in [`docs/env-vars.md#http-fetch-tuning`](env-vars.md#http-fetch-tuning).

This includes backend selection (`FASTR_HTTP_BACKEND`), browser header profiles (`FASTR_HTTP_BROWSER_HEADERS`), and retry logging (`FASTR_HTTP_LOG_RETRIES`). `cargo xtask pageset`, `scripts/pageset.sh`, and `pageset_progress` worker subprocesses inherit these env vars automatically—set them once on the outer command when triaging hard fetch failures.

Example:

```bash
FASTR_HTTP_BACKEND=reqwest FASTR_HTTP_BROWSER_HEADERS=1 cargo xtask pageset --pages tesco.com
```

Note: `fetch_pages` skips URLs already cached under `fetches/html/`. When iterating on HTTP fetch knobs for document fetch failures, re-run with `fetch_pages --refresh` (or delete the cached HTML) so the network path is exercised.

Example (re-fetch HTML for one page with an explicit backend + browser headers):

```bash
FASTR_HTTP_BACKEND=reqwest FASTR_HTTP_BROWSER_HEADERS=1 \
  cargo run --release --bin fetch_pages -- --refresh --pages tesco.com
```

## `cargo xtask`

`cargo xtask` is the preferred entry point for common workflows; it wraps the binaries below but keeps them usable directly.

- Help: `cargo xtask --help`
- Tests: `cargo xtask test [core|style|fixtures|wpt|all]`
- Refresh goldens: `cargo xtask update-goldens [all|fixtures|reference|wpt]` (sets the appropriate `UPDATE_*` env vars)
- Pageset scoreboard (`fetch_pages` → `prefetch_assets` → `pageset_progress` when disk cache is enabled; bundled fonts by default): `cargo xtask pageset [--pages example.com,news.ycombinator.com] [--shard 0/4] [--no-fetch] [--refresh] [--allow-http-error-status] [--allow-collisions] [--timings] [--disk-cache] [--no-disk-cache] [--cache-dir <dir>] [--cascade-diagnostics] [--cascade-diagnostics-slow-ms 500] [-- <pageset_progress args...>]`
  - Sharded example: `cargo xtask pageset --shard 0/4` (applies to fetch + prefetch (disk cache only) + render; add `--no-fetch` to reuse cached pages)
  - Forward compatibility gates when needed: `--compat-profile site` and/or `--dom-compat compat` are passed through to `pageset_progress run` but remain off by default.
  - Disk cache directory override: `--cache-dir <dir>` is forwarded to both `prefetch_assets` and `pageset_progress` so the warmed cache matches the render step (defaults to `fetches/assets/`).
  - Disk cache tuning flags passed after `--` (e.g. `--disk-cache-max-bytes`, `--disk-cache-max-age-secs`, `--disk-cache-lock-stale-secs`) are also forwarded to `prefetch_assets` when it runs.
  - Prefetch tuning flags passed after `--` (e.g. `--prefetch-fonts`, `--prefetch-images`, `--prefetch-iframes`) are also forwarded to `prefetch_assets` when it runs.
  - Cascade triage: `--cascade-diagnostics` re-runs slow-cascade ok pages (defaults to 500ms threshold; override with `--cascade-diagnostics-slow-ms`) plus cascade timeouts with cascade profiling enabled, then merges `diagnostics.stats.cascade` into the committed progress JSON.
- Pageset diff: `cargo xtask pageset-diff [--baseline <dir>|--baseline-ref <git-ref>] [--no-run] [--fail-on-regression] [--fail-on-missing-stages] [--fail-on-missing-stage-timings] [--fail-on-slow-ok-ms <ms>]`
  - Extracts `progress/pages` from the chosen git ref by default and compares it to the freshly updated scoreboard.
  - `--fail-on-regression` also enables the missing-stage gates and `--fail-on-slow-ok-ms=5000` by default (use `--no-fail-on-missing-stages` / `--no-fail-on-missing-stage-timings` / `--no-fail-on-slow-ok` to opt out, or pass `--fail-on-slow-ok-ms <ms>` to override the default threshold).
  - Stage-bucket sanity guardrail: when changing stage timing accounting, run `pageset_progress report --fail-on-stage-sum-exceeds-total` (tune `--stage-sum-tolerance-percent`, default 10%) to catch double-counting/CPU-sum mixups early.
- Render one page: `cargo xtask render-page --url https://example.com --output out.png [--viewport 1200x800 --dpr 1.0 --full-page]`
- Diff renders: `cargo xtask diff-renders --before fetches/renders/baseline --after fetches/renders/new [--output target/render-diffs]`
  - Supports directory diffs (recursive) and PNG file-to-file diffs.
  - Writes `diff_report.html` / `diff_report.json` into `--output` (diff images under `diff_report_files/diffs/`).
- Chrome baseline screenshots for offline fixtures (local-only; not committed): `cargo xtask chrome-baseline-fixtures`
- Chrome-vs-FastRender diff report for offline fixtures (deterministic; offline): `cargo xtask fixture-chrome-diff`
- Import a bundled capture into a `pages_regression` fixture: `cargo xtask import-page-fixture <bundle_dir|.tar> <fixture_name> [--output-root tests/pages/fixtures --overwrite --dry-run]`
- Update `tests/pages/pageset_guardrails.json` from the pageset scoreboard: `cargo xtask update-pageset-guardrails`
  - Defaults to `--strategy coverage`; always includes every `timeout`/`panic`/`error` page from `progress/pages/*.json` for offline triage, then adds a small set of slow `ok` pages for hotspot coverage.
  - Warns when failures exceed `--count`.
  - Defaults to crawl-based capture for missing fixtures.
  - Use `--capture-missing-fixtures --capture-mode cache` to build bundles from warmed pageset caches without network access (requires `--features disk_cache`; pass `--asset-cache-dir <dir>`/`--cache-dir <dir>` when the warmed cache is not under `fetches/assets/`).
  - Use `--capture-mode render` to force render-driven bundle capture.
  - Use `--bundle-fetch-timeout-secs <secs>` to bound per-request network time during capture when using `render`/`crawl`.
  - Note: `update-pageset-timeouts` remains as an alias; the legacy `tests/pages/pageset_timeouts.json` file is kept in sync for backwards compatibility.
- Update `budget_ms` entries in `tests/pages/pageset_guardrails.json` based on offline `perf_smoke` timings: `cargo xtask update-pageset-guardrails-budgets --write` (runs `perf_smoke --suite pageset-guardrails` with bundled fonts, then rewrites each fixture's `budget_ms` as `total_ms * --multiplier` clamped/rounded; use `--dry-run` to preview). (`update-pageset-timeout-budgets` remains as an alias; the legacy `tests/pages/pageset_timeouts.json` file is kept in sync for backwards compatibility.)
- Perf smoke: `cargo xtask perf-smoke [--suite core|pageset-guardrails|all] [--only flex_dashboard,grid_news]`
  `[--top 5 --baseline baseline.json --threshold 0.05 --count-threshold 0.20 --fail-on-regression --fail-on-failure --fail-fast]`
  `[--fail-on-budget] [--isolate|--no-isolate] [--fail-on-missing-fixtures|--allow-missing-fixtures] [-- <extra perf_smoke args...>]`
  - Offline fixtures, bundled fonts, JSON summary at `target/perf_smoke.json`, per-fixture `status`/`error`.
  - The `pageset-guardrails` suite runs in isolation and **fails on missing fixtures by default**; pass `--allow-missing-fixtures` to skip missing pageset-guardrails captures for local partial runs.
  - Pass `--fail-on-budget` to exit non-zero when a fixture exceeds its `budget_ms`.
  - Pass `--count-threshold`, `--fail-fast`, and `--fail-on-failure` to tune count regression and fixture failure gating.

`render-page` wraps `fetch_and_render` in release mode by default (add `--debug` to keep a debug build).

## `fetch_pages`

- Purpose: fetch a curated set of real pages and cache HTML under `fetches/html/` (and metadata alongside).
- Entry: `src/bin/fetch_pages.rs`
- Run: `cargo run --release --bin fetch_pages -- --help`
- HTTP fetch tuning: honors the `FASTR_HTTP_*` env vars described above (see [`docs/env-vars.md#http-fetch-tuning`](env-vars.md#http-fetch-tuning)).
- Supports deterministic sharding with `--shard <index>/<total>` when splitting the page list across workers.
- Cache filenames and `--pages` filters use the canonical stem from `normalize_page_name` (strip the scheme and a leading `www.`). Colliding stems fail fast unless you opt into `--allow-collisions`, which appends a deterministic suffix.
- `--allow-http-error-status` treats HTTP 4xx/5xx responses as fetch successes and allows caching them for debugging (e.g. Cloudflare challenges). When used with `--refresh`, `fetch_pages` will avoid clobbering an existing cached snapshot with a transient 4xx/5xx response unless the existing snapshot is also known to be an HTTP error page.
- **Migration:** cached HTML written before canonical stems were enforced may be ignored. Delete stale `fetches/html/*.html` entries and re-run `fetch_pages`.

## `prefetch_assets`

- Purpose: warm the subresource cache (`fetches/assets/`) by prefetching linked stylesheets and their `@import` chains (plus referenced fonts) for the cached pages under `fetches/html/`. Optional flags can also prefetch additional HTML-linked subresources (images, iframes, embeds, icons, video posters). This makes subsequent pageset renders more repeatable and reduces time spent fetching during `pageset_progress`.
- Entry: `src/bin/prefetch_assets.rs`
- Run: `cargo run --release --bin prefetch_assets -- --help`
- HTTP fetch tuning: honors the `FASTR_HTTP_*` env vars described above (see [`docs/env-vars.md#http-fetch-tuning`](env-vars.md#http-fetch-tuning)).
- Most useful when built with `--features disk_cache` (so cache entries persist across processes).
- Key flags: page selection (`--pages`), deterministic sharding (`--shard <index>/<total>`), parallelism (`--jobs`), and fetch timeout (`--timeout`). See `--help` for the full flag list.
  - Cache directory: `--cache-dir <dir>` overrides the disk-backed cache location (defaults to `fetches/assets/`). Use the same value for `pageset_progress` so warmed entries are reused during render.
  - Optional subresource warming:
    - `--prefetch-fonts`: prefetch font URLs referenced by fetched CSS (true/false, defaults to true).
    - `--prefetch-images`: prefetch common HTML image-like assets (`<img>`, `<picture><source srcset>`, video posters, icons/manifests (including `mask-icon`), and `<link rel="preload" as="image">`). This uses the renderer's responsive image selection (DPR/viewport + `srcset`/`sizes`/`picture`) instead of enumerating every candidate.
      - Safety valves: `--max-images-per-page` and `--max-image-urls-per-element` bound image prefetching when pages contain large `srcset` lists.
      - Note: if you only need a small subset (e.g. icons or video posters) without fetching all `<img>` content, use `--prefetch-icons` / `--prefetch-video-posters` instead.
    - `--prefetch-iframes` (alias `--prefetch-documents`): prefetch `<iframe src>` documents and best-effort warm their linked stylesheets (and images when `--prefetch-images` is enabled).
    - `--prefetch-embeds`: prefetch `<object data>` and `<embed src>` subresources. If the fetched resource is HTML, it is treated like a nested document and its CSS/images can also be warmed (same behavior as `--prefetch-iframes`).
    - `--prefetch-icons`: prefetch icon resources referenced by `<link rel=icon|shortcut icon|apple-touch-icon|mask-icon href=...>` without enabling full `--prefetch-images` (note: `--prefetch-images` already includes these).
    - `--prefetch-video-posters`: prefetch `<video poster>` images without enabling full `--prefetch-images` (note: `--prefetch-images` already includes posters).
    - `--prefetch-css-url-assets`: prefetch non-CSS assets referenced via CSS `url(...)` (including in `@import`ed stylesheets).
    - `--max-images-per-page`: cap how many image-like elements are considered during HTML discovery when `--prefetch-images` is enabled.
    - `--max-image-urls-per-element`: cap how many URLs are prefetched per image element (primary + fallbacks) when `--prefetch-images` is enabled.
    - `--max-discovered-assets-per-page`: safety valve for pathological pages (0 disables the cap).
- Disk cache tuning flags (`--disk-cache-max-age-secs`, `--disk-cache-max-bytes`, `--disk-cache-lock-stale-secs`, or the corresponding `FASTR_DISK_CACHE_*` env vars) match the pageset render binaries.

## `disk_cache_audit`

- Purpose: audit (and optionally clean) the disk-backed subresource cache directory (defaults to `fetches/assets/`) for common pageset poisoning cases such as cached 4xx/5xx responses or HTML responses stored for URLs that look like static subresources (CSS/images/fonts).
- Entry: `src/bin/disk_cache_audit.rs`
- Run: `cargo run --release --bin disk_cache_audit -- --help`
- Typical usage:
  - Audit: `cargo run --release --bin disk_cache_audit --`
  - JSON output (stable keys): `cargo run --release --bin disk_cache_audit -- --json`
  - Cleanup: `cargo run --release --bin disk_cache_audit -- --delete-http-errors --delete-html-subresources`
  - Match non-default cache directory: `cargo run --release --bin disk_cache_audit -- --cache-dir <dir>`

## `render_pages`

- Purpose: render all cached HTML in `fetches/html/` to `fetches/renders/` (PNG + per-page logs + `_summary.log`).

### Chrome baseline screenshots (from cached HTML)

If you want a “known-correct engine” visual baseline for the same cached HTML that FastRender renders, you can use headless Chrome/Chromium to screenshot `fetches/html/*.html`.

This workflow is **best-effort / non-deterministic** because it still depends on live subresources. Prefer the offline fixture loop (`render_fixtures` + `cargo xtask fixture-chrome-diff`) once you’ve captured a deterministic repro.

For convenience, `scripts/chrome_vs_fastrender.sh` wraps the full cached-pages loop and writes a
report at `target/chrome_vs_fastrender/diff_report.html` by default.

```bash
# Install deps on Ubuntu (python + fonts + chrome/chromium):
scripts/install_chrome_baseline_deps_ubuntu.sh

# 1) Ensure cached HTML exists:
cargo run --release --bin fetch_pages

# 2) Screenshot with Chrome/Chromium (JS disabled by default; injects <base href=...> from *.html.meta):
scripts/chrome_baseline.sh

# 3) Render FastRender output:
cargo run --release --bin render_pages

# 4) Diff the two directories:
cargo run --release --bin diff_renders -- \
  --before fetches/chrome_renders \
  --after fetches/renders \
  --html target/chrome_vs_fastrender.html
```

Notes:
- This is not fully deterministic (live subresources can change); it’s still excellent for rapid “why is our render different from Chrome on the same HTML?” debugging.
- Set `CHROME_BIN=/path/to/chrome` if auto-detection fails.
- Entry: `src/bin/render_pages.rs`
- Run: `cargo run --release --bin render_pages -- --help`
- HTTP fetch tuning: honors the `FASTR_HTTP_*` env vars described above (see [`docs/env-vars.md#http-fetch-tuning`](env-vars.md#http-fetch-tuning)).
- Accepts `--shard <index>/<total>` to render a slice of the cached pages in a stable order.
- `--pages` (and positional filters) use the same canonical stems as `fetch_pages` (strip scheme + leading `www.`). Cached filenames are normalized when matching filters so `www.`/non-`www` variants map consistently.
- Disk cache directory: `--cache-dir <dir>` overrides the disk-backed subresource cache location (defaults to `fetches/assets/`; only has an effect when built with `--features disk_cache`).
- Optional outputs:
  - `--diagnostics-json` writes `fetches/renders/<page>.diagnostics.json` containing status, timing, and `RenderDiagnostics`.
  - `--dump-intermediate {summary|full}` emits per-page summaries or full JSON dumps of DOM/styled/box/fragment/display-list stages (use `--only-failures` to gate large artifacts on errors); `full` also writes a combined `fetches/renders/<page>.snapshot.json` pipeline snapshot.
- Layout fan-out defaults to `auto` (only engages once the box tree is large enough and has sufficient independent sibling work); use `--layout-parallel off` to force serial layout, `--layout-parallel on` to force fan-out, or tune thresholds with `--layout-parallel-min-fanout`, `--layout-parallel-auto-min-nodes`, and `--layout-parallel-max-threads`.
- Worker Rayon threads: in the default per-page worker mode, `render_pages` sets `RAYON_NUM_THREADS` for each worker process to `available_parallelism()/jobs` (min 1, additionally clamped by a detected cgroup CPU quota on Linux) to avoid CPU oversubscription. Set `RAYON_NUM_THREADS` in the parent environment to override this.

## `render_fixtures`

- Purpose: render offline page fixtures (under `tests/pages/fixtures/`) to PNGs for deterministic debugging and Chrome-vs-FastRender diff reports.
- Run: `cargo run --release --bin render_fixtures -- --help`
- Defaults: fixed, deterministic viewport/DPR (1200x800 @ 1.0) unless overridden.
- Offline policy: fixtures are rendered **without network access**; only `file://` and `data:` subresources are allowed.
- Fonts: uses bundled fonts (`FontConfig::bundled_only`) so outputs are stable across machines.
- Output: by default writes `<fixture>.png` into `target/fixture_renders/` (override with `--out-dir`).
- Optional diagnostics: `--diagnostics-json` writes `<fixture>.diagnostics.json` next to each PNG (uses `DiagnosticsLevel::Basic`).
- Core flags:
  - Selection: `--only <csv>` and/or positional fixture names.
  - Paths: `--fixtures-root <dir>`, `--out-dir <dir>`.
  - Render params: `--viewport <WxH>`, `--dpr <float>`, `--media {screen|print}`, `--timeout <secs>`.
  - Parallelism: `--jobs/-j <n>`.
  - Diagnostics: `--diagnostics-json` to write structured diagnostics alongside each PNG.

## `cargo xtask chrome-baseline-fixtures`

- Purpose: render the same offline fixtures in headless Chrome/Chromium to generate a “known-correct engine” PNG baseline for comparisons.
- Notes:
  - These baselines are **local-only** artifacts under `target/` (they are not committed).
  - Defaults match the fixture runner viewport/DPR (1200x800 @ 1.0) unless overridden.
  - JavaScript is disabled by default to match FastRender’s “no JS” model.
  - Pass `--chrome /path/to/chrome` if auto-detection fails.
  - Output defaults to `target/chrome_fixture_renders/<fixture>.png` plus `*.chrome.log` alongside.

## `cargo xtask fixture-chrome-diff`

- Purpose: render offline fixtures with FastRender and headless Chrome, then generate a single HTML report comparing the two.
- Typical usage:
  - `cargo xtask fixture-chrome-diff` (writes `target/fixture_chrome_diff/report.html` and prints the path)
  - Reuse a previously generated Chrome render directory: `cargo xtask fixture-chrome-diff --chrome-dir <dir>`
  - For advanced filtering/output control, see `cargo xtask fixture-chrome-diff --help` (supports `--only`, `--viewport`, `--dpr`, and diff tolerances).
- Output layout:
  - `<out>/chrome/<fixture>.png` (+ `<fixture>.chrome.log`)
  - `<out>/fastrender/<fixture>.png` (+ optional `<fixture>.diagnostics.json` when `render_fixtures --diagnostics-json` is enabled)
  - `<out>/report.html`, `<out>/report.json`

## `fetch_and_render`

- Purpose: fetch one URL (or read one `file://` target) and render to a PNG.
- Entry: `src/bin/fetch_and_render.rs`
- Run: `cargo run --release --bin fetch_and_render -- --help`
- HTTP fetch tuning: honors the `FASTR_HTTP_*` env vars described above (see [`docs/env-vars.md#http-fetch-tuning`](env-vars.md#http-fetch-tuning)).
- Disk cache directory: `--cache-dir <dir>` overrides the disk-backed subresource cache location (defaults to `fetches/assets/`; only has an effect when built with `--features disk_cache`).
- Security defaults mirror the library: `file://` subresources are blocked for HTTP(S) documents. Use `--allow-file-from-http` to override during local testing, `--block-mixed-content` to forbid HTTP under HTTPS, and `--same-origin-subresources` (plus optional `--allow-subresource-origin`) to block cross-origin CSS/images/fonts when rendering untrusted pages. This flag does not block cross-origin iframe/embed document navigation.
- Performance: layout fan-out defaults to `auto` (with optional `--layout-parallel-min-fanout` / `--layout-parallel-auto-min-nodes` / `--layout-parallel-max-threads`). Use `--layout-parallel off` to force serial layout or `--layout-parallel on` to force fan-out when chasing wall-time regressions on wide pages.

## `bundle_page`

- Purpose: capture a page (HTML + subresources) into a self-contained bundle and replay it offline.
- Entry: `src/bin/bundle_page.rs`
- Run:
  - Fetch: `cargo run --release --bin bundle_page -- fetch <url> --out <bundle_dir|.tar>`
    - HTTP fetch tuning: `bundle_page fetch` honors the `FASTR_HTTP_*` env vars described above (see [`docs/env-vars.md#http-fetch-tuning`](env-vars.md#http-fetch-tuning)).
    - For pages that crash or time out during capture, add `--no-render` (alias `--crawl`) to discover subresources by parsing HTML + CSS instead of rendering.
    - Use `--fetch-timeout-secs <secs>` to bound per-request network time when crawling large pages.
  - Cache (offline, from pageset caches): `cargo run --release --bin bundle_page -- cache <stem> --out <bundle_dir|.tar>`
    - Reads HTML from `fetches/html/<stem>.html` (+ `.html.meta`) and subresources from the disk-backed cache under `fetches/assets/` (override with `--asset-cache-dir` (alias `--cache-dir`); this should match the `--cache-dir` used when warming/running the pageset).
    - Fails if a discovered subresource is missing from the cache; pass `--allow-missing` to insert empty placeholders.
    - The disk cache key namespace depends on request headers. If you warmed `fetches/assets/` with non-default values (e.g. `pageset_progress --user-agent ... --accept-language ...`, or `FASTR_HTTP_BROWSER_HEADERS=0`), pass matching `bundle_page cache --user-agent ... --accept-language ...` (and the same env var) so cache capture hits the correct entries.
  - Render: `cargo run --release --bin bundle_page -- render <bundle> --out <png>`
    - `bundle_page render` is offline and ignores `FASTR_HTTP_*` env vars (it uses the bundle contents only).
- Security: `--same-origin-subresources` (plus optional `--allow-subresource-origin`) applies both when capturing and replaying bundles to keep cross-origin assets out of offline artifacts. It does not block cross-origin iframe/embed document navigation.
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

- Purpose: compare two render output directories (e.g., `fetches/renders` from two revisions) and summarize pixel + perceptual diffs.
- Entry: `src/bin/diff_renders.rs`
- Run: `cargo run --release --bin diff_renders -- --before <dir> --after <dir>`
- Matching: directory inputs are walked recursively and paired by relative path (minus the `.png` extension). This allows diffing nested render trees (for example fixture render outputs or pageset dump layouts) without flattening them first.
- Outputs: `diff_report.json` and `diff_report.html` plus per-page diff PNGs alongside the HTML report.
- Tuning: `--tolerance`, `--max-diff-percent`, and `--max-perceptual-distance` accept the same values as the fixture harness (`FIXTURE_TOLERANCE`, `FIXTURE_MAX_DIFFERENT_PERCENT`, `FIXTURE_MAX_PERCEPTUAL_DISTANCE`, and `FIXTURE_FUZZY` env vars are honored when flags are omitted). Use `--sort-by perceptual` to rank diffs by SSIM-derived distance.
- Supports deterministic sharding with `--shard <index>/<total>` to split large sets across workers.

## `diff_snapshots`

- Purpose: compare pipeline snapshots (`*.snapshot.json`) and highlight stage-level deltas that explain pixel diffs.
- Entry: `src/bin/diff_snapshots.rs`
- Run: `cargo run --release --bin diff_snapshots -- --before <dir|file> --after <dir|file>`
- Matching:
  - Directory inputs support both the render-pages layout (`<stem>.snapshot.json`) and the pageset dump layout (`<stem>/snapshot.json` produced by `pageset_progress --dump-*`).
  - Entries are paired by stem (the `<stem>` part of the filename/directory). For pageset dumps, `render.png` is linked when present.
- Outputs: `diff_snapshots.json` and `diff_snapshots.html` summarizing schema versions, per-stage counts, DOM/box/fragment/display list changes, and links to sibling `*.png` renders when present.

## `dump_a11y`

- Purpose: emit the computed accessibility tree for a document as JSON (without painting).
- Entry: `src/bin/dump_a11y.rs`
- Run: `cargo run --release --bin dump_a11y -- --help`

## Offline / cached captures

- Use `bundle_page fetch` to save a single reproducible capture (HTML bytes, content-type + final URL, all fetched CSS/image/font subresources with HTTP metadata, and a manifest mapping original URLs to bundle paths). Bundles can be directories or `.tar` archives and are deterministic.
- Use `bundle_page cache <stem> --out <bundle>` to convert an already-warmed pageset cache entry (cached HTML + disk-backed assets) into a portable bundle **without network access**.
- Replay with `bundle_page render <bundle> --out out.png` to render strictly from the bundle with zero network calls.
- For larger batch workflows, offline captures are also available via the existing on-disk caches:
  - `fetch_pages` writes HTML under `fetches/html/` and a `*.html.meta` sidecar with the original content-type and final URL.
  - `render_pages` and `fetch_and_render` use the shared disk-backed fetcher (when built with `--features disk_cache`; enabled by default in `scripts/pageset.sh`, `cargo xtask pageset`, and the profiling scripts) for subresources, writing into `fetches/assets/` (override with `--cache-dir <dir>`). After one online render, you can re-run against the same caches without network access (new URLs will still fail). Use `--no-disk-cache`, `DISK_CACHE=0`, or `NO_DISK_CACHE=1` to opt out.
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
  - HTTP fetch tuning: honors the `FASTR_HTTP_*` env vars described above (see [`docs/env-vars.md#http-fetch-tuning`](env-vars.md#http-fetch-tuning)).
  - Compatibility (opt-in only): `--compat-profile site` enables site-specific hacks and
    `--dom-compat compat` applies DOM class flips. Defaults stay spec-only; `cargo xtask
    pageset` forwards the flags only when you provide them.
- Disk cache directory: `--cache-dir <dir>` overrides the disk-backed subresource cache location (defaults to `fetches/assets/`; only has an effect when built with `--features disk_cache`).
- Fonts: pass `--bundled-fonts` to skip system font discovery (default in the pageset wrappers) or
  `--font-dir <path>` to load fonts from a specific directory without hitting host fonts.
- Sync: `cargo run --release --bin pageset_progress -- sync [--prune] [--html-dir fetches/html --progress-dir progress/pages]` bootstraps one JSON per pageset URL without needing any caches. `--prune` removes stale progress files for URLs no longer in the list. Stems are collision-aware (`example.com--deadbeef` when needed) to keep cache and progress filenames unique.
- Migrate: `cargo run --release --bin pageset_progress -- migrate [--html-dir fetches/html --progress-dir progress/pages]` rewrites existing progress JSON without fetching or rendering. It applies legacy schema migrations (notably splitting mixed legacy `notes` into durable `notes` + machine `auto_notes`) and reserializes deterministically using the runner's canonical formatter.
- Progress filenames use the cache stem from `pageset_stem` (strip scheme + leading `www.` plus a deterministic hash suffix on collisions); `--pages` filters accept the URL, canonical stem, or cache stem. If you have older `fetches/html` entries with `www.` prefixes in the filename, re-run `fetch_pages` so progress filenames line up.
  - For temporary/test runs, `FASTR_PAGESET_URLS="https://a.com,https://b.com"` overrides the built-in pageset everywhere.
- Triage reruns (reuse existing `progress/pages/*.json` instead of typing stems):
  - `--from-progress <dir>` enables selection from saved progress files (default intersection of filters, use `--union` to OR them).
  - Filters: `--only-failures`, `--only-status timeout,panic,error`, `--slow-ms <ms> [--slow-ok-only]`, `--hotspot css|cascade|box_tree|layout|paint|...`, `--top-slowest <n>`.
  - The deterministic stem list is printed before running; if nothing matches, the command exits cleanly without touching caches.
- Report: `cargo run --release --bin pageset_progress -- report`
  `[--progress-dir progress/pages --top 10 --fail-on-bad --compare <other> --fail-on-regression --regression-threshold-percent 10 --fail-on-slow-ok-ms <ms> --fail-on-stage-sum-exceeds-total]`
  - Prints status counts, slowest pages, and hotspot histograms for the saved progress files.
  - With `--compare`, also prints status transitions plus the top regressions/improvements by `total_ms`.
  - `--fail-on-regression` exits non-zero for ok→bad or > threshold slowdowns.
  - `--fail-on-slow-ok-ms 5000` enforces the hard 5s/page budget for ok pages (entries missing `total_ms` are ignored by this gate).
  - `--include-trace` lists saved Chrome traces (from `target/pageset/traces/` + `target/pageset/trace-progress/`).
  - `--verbose-stats` prints structured per-page stats when present (including resource cache hit/miss/bytes breakdowns, single-flight inflight wait time, disk cache lock waits, and network fetch totals). It also prints an aggregated "Resource totals" summary plus top-N rankings for network/inflight/disk cache time (including disk lock wait time), and top-N rankings per stage bucket (fetch/css/cascade/box_tree/layout/paint).
  - Stage-bucket sanity guardrail (off by default): `--fail-on-stage-sum-exceeds-total` checks `status=ok` entries that have both `total_ms` and non-zero stage buckets, failing when `stages_ms.sum()` exceeds `total_ms` by more than `--stage-sum-tolerance-percent` (default 10%). This is intended as a regression guardrail for catching stage timing accounting bugs (double-counting or accidentally mixing CPU-summed metrics into wall-clock stage buckets).
  - Example:

    ```bash
    cargo run --release --bin pageset_progress -- report \
      --progress-dir progress/pages \
      --fail-on-stage-sum-exceeds-total
    ```
- Safety: uses **panic containment** (per-page worker process) and a **hard timeout** (kills runaway workers) so one broken page cannot stall the whole run.
- Worker Rayon threads: `pageset_progress run` spawns up to `--jobs` worker processes in parallel and sets `RAYON_NUM_THREADS` for each worker to `available_parallelism()/jobs` (min 1, additionally clamped by a detected cgroup CPU quota on Linux) unless the parent environment already defines it.
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
