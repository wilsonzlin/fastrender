# CLI tools

FastRender ships a few small binaries/examples intended for internal debugging and regression work. Prefer `--help` output for the source of truth.

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
  - `--dump-intermediate {summary|full}` emits per-page summaries or full JSON dumps of DOM/styled/box/fragment/display-list stages (use `--only-failures` to gate large artifacts on errors).

## `fetch_and_render`

- Purpose: fetch one URL (or read one `file://` target) and render to a PNG.
- Entry: `src/bin/fetch_and_render.rs`
- Run: `cargo run --release --bin fetch_and_render -- --help`

## `inspect_frag`

- Purpose: inspect fragment output (and related style/layout state) for a single input.
- Entry: `src/bin/inspect_frag.rs`
- Run: `cargo run --release --bin inspect_frag -- --help`

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

- There is no dedicated "bundle" format. Offline captures are the on-disk caches produced by the existing tools:
  - `fetch_pages` writes HTML under `fetches/html/` and a `*.html.meta` sidecar with the original content-type and final URL.
  - `render_pages` and `fetch_and_render` use the shared disk-backed fetcher (when built with `--features disk_cache`) for subresources, writing into `fetches/assets/`. After one online render, you can re-run against the same caches without network access (new URLs will still fail).
- Asset fetches made by the CLI tools go through the library cache layer: [`fastrender::resource::CachingFetcher`] in-memory by default, or [`fastrender::resource::DiskCachingFetcher`] behind the optional `disk_cache` feature.
- To replay a single saved page, point `fetch_and_render` at a `file://` URL or path; it will read an adjacent `.meta` file (if present) to recover the cached content-type and source URL.

## Diagnostics

- `render_pages` emits per-page logs in `fetches/renders/<page>.log` plus a summary in `_summary.log`. CSS fetch failures show up there and correspond to `ResourceKind::Stylesheet` entries in the library diagnostics model.
- The library API exposes structured diagnostics via `render_url_with_options` returning `RenderResult { pixmap, accessibility, diagnostics }`. Set `RenderOptions::allow_partial(true)` to receive a placeholder image and a `document_error` string when the root fetch fails; subresource fetch errors are collected in `diagnostics.fetch_errors` with status codes, final URLs, and any cache validators observed.
- `render_pages` can emit structured reports via `--diagnostics-json` (per-page) plus `--dump-intermediate` for summaries or full intermediate dumps.
- The shipped binaries print a concise diagnostics summary (including status/final URL). Pass `--verbose` to `fetch_and_render` or `render_pages` to include full error chains when something fails.
