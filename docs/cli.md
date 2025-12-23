# CLI tools

FastRender ships a few small binaries/examples intended for internal debugging and regression work. Prefer `--help` output for the source of truth.

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

## `fetch_and_render`

- Purpose: fetch one URL (or read one `file://` target) and render to a PNG.
- Entry: `src/bin/fetch_and_render.rs`
- Run: `cargo run --release --bin fetch_and_render -- --help`

## `inspect_frag`

- Purpose: inspect fragment output (and related style/layout state) for a single input.
- Entry: `src/bin/inspect_frag.rs`
- Run: `cargo run --release --bin inspect_frag -- --help`

## Caching behavior

- `fetch_pages` caches HTML responses under `fetches/html/`.
- Asset fetches made by the CLI tools go through the disk-backed `DiskCachingFetcher`
  helper in `src/bin/caching_fetcher.rs`, which stores responses under `fetches/assets/`
  so repeated runs avoid re-downloading.
- API consumers should use the library's in-memory [`fastrender::resource::CachingFetcher`]
  (single-flight, no disk writes) instead of the CLI-only disk cache.
