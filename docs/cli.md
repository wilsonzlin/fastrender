# CLI tools

FastRender ships a few small binaries/examples intended for internal debugging and regression work. Prefer `--help` output for the source of truth. Shared flag schemas for viewport/DPR, media type and preferences, output formats, timeouts, and base URL overrides live in [`src/bin/common/args.rs`](../src/bin/common/args.rs).

## `cargo xtask`

`cargo xtask` is the preferred entry point for common workflows; it wraps the binaries below but keeps them usable directly.

- Help: `cargo xtask --help`
- Tests: `cargo xtask test [core|style|fixtures|wpt|all]`
- Refresh goldens: `cargo xtask update-goldens [all|fixtures|reference|wpt]` (sets the appropriate `UPDATE_*` env vars)
- Render one page: `cargo xtask render-page --url https://example.com --output out.png [--viewport 1200x800 --dpr 1.0 --full-page]`
- Diff renders: `cargo xtask diff-renders --before fetches/renders/baseline --after fetches/renders/new [--output target/render-diffs]`

`render-page` wraps `fetch_and_render` in release mode by default (add `--debug` to keep a debug build).

## `fetch_pages`

- Purpose: fetch a curated set of real pages and cache HTML under `fetches/html/` (and metadata alongside).
- Entry: `src/bin/fetch_pages.rs`
- Run: `cargo run --release --bin fetch_pages -- --help`
- Supports deterministic sharding with `--shard <index>/<total>` when splitting the page list across workers.

## `render_pages`

- Purpose: render all cached HTML in `fetches/html/` to `fetches/renders/` (PNG + per-page logs + `_summary.log`).
- Entry: `src/bin/render_pages.rs`
- Run: `cargo run --release --bin render_pages -- --help`
- Accepts `--shard <index>/<total>` to render a slice of the cached pages in a stable order.
- Optional outputs:
  - `--diagnostics-json` writes `fetches/renders/<page>.diagnostics.json` containing status, timing, and `RenderDiagnostics`.
  - `--dump-intermediate {summary|full}` emits per-page summaries or full JSON dumps of DOM/styled/box/fragment/display-list stages (use `--only-failures` to gate large artifacts on errors); `full` also writes a combined `fetches/renders/<page>.snapshot.json` pipeline snapshot.

## `fetch_and_render`

- Purpose: fetch one URL (or read one `file://` target) and render to a PNG.
- Entry: `src/bin/fetch_and_render.rs`
- Run: `cargo run --release --bin fetch_and_render -- --help`
- Security defaults mirror the library: `file://` subresources are blocked for HTTP(S) documents. Use `--allow-file-from-http` to override during local testing, `--block-mixed-content` to forbid HTTP under HTTPS, and `--same-origin-subresources` (plus optional `--allow-subresource-origin`) to block cross-origin CSS/images/fonts when rendering untrusted pages.

## `bundle_page`

- Purpose: capture a page (HTML + subresources) into a self-contained bundle and replay it offline.
- Entry: `src/bin/bundle_page.rs`
- Run:
  - Fetch: `cargo run --release --bin bundle_page -- fetch <url> --out <bundle_dir|.tar>`
  - Render: `cargo run --release --bin bundle_page -- render <bundle> --out <png>`
- Security: `--same-origin-subresources` (plus optional `--allow-subresource-origin`) applies both when capturing and replaying bundles to keep cross-origin assets out of offline artifacts.

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

## `dump_a11y`

- Purpose: emit the computed accessibility tree for a document as JSON (without painting).
- Entry: `src/bin/dump_a11y.rs`
- Run: `cargo run --release --bin dump_a11y -- --help`

## Offline / cached captures

- Use `bundle_page fetch` to save a single reproducible capture (HTML bytes, content-type + final URL, all fetched CSS/image/font subresources with HTTP metadata, and a manifest mapping original URLs to bundle paths). Bundles can be directories or `.tar` archives and are deterministic.
- Replay with `bundle_page render <bundle> --out out.png` to render strictly from the bundle with zero network calls.
- For larger batch workflows, offline captures are also available via the existing on-disk caches:
  - `fetch_pages` writes HTML under `fetches/html/` and a `*.html.meta` sidecar with the original content-type and final URL.
  - `render_pages` and `fetch_and_render` use the shared disk-backed fetcher (when built with `--features disk_cache`) for subresources, writing into `fetches/assets/`. After one online render, you can re-run against the same caches without network access (new URLs will still fail).
- Asset fetches in library code go through [`fastrender::resource::CachingFetcher`] in-memory by default, or [`fastrender::resource::DiskCachingFetcher`] behind the optional `disk_cache` feature.

## Diagnostics

- `render_pages` emits per-page logs in `fetches/renders/<page>.log` plus a summary in `_summary.log`. CSS fetch failures show up there and correspond to `ResourceKind::Stylesheet` entries in the library diagnostics model.
- The library API exposes structured diagnostics via `render_url_with_options` returning `RenderResult { pixmap, accessibility, diagnostics }`. Set `RenderOptions::allow_partial(true)` to receive a placeholder image and a `document_error` string when the root fetch fails; subresource fetch errors are collected in `diagnostics.fetch_errors` with status codes, final URLs, and any cache validators observed.
- `render_pages` can emit structured reports via `--diagnostics-json` (per-page) plus `--dump-intermediate` for summaries or full intermediate dumps.
- The shipped binaries print a concise diagnostics summary (including status/final URL). Pass `--verbose` to `fetch_and_render` or `render_pages` to include full error chains when something fails.
