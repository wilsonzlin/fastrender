# CLI tools

FastRender ships a few small binaries/examples intended for internal debugging and regression work. Prefer `--help` output for the source of truth.

## `fetch_pages`

- Purpose: fetch a curated set of real pages and cache HTML under `fetches/html/` (and metadata alongside).
- Entry: `src/bin/fetch_pages.rs`
- Run: `cargo run --release --bin fetch_pages -- --help`

## `render_pages`

- Purpose: render all cached HTML in `fetches/html/` to `fetches/renders/` (PNG + per-page logs + `_summary.log`).
- Entry: `src/bin/render_pages.rs`
- Run: `cargo run --release --bin render_pages -- --help`

## `fetch_and_render`

- Purpose: fetch one URL (or read one `file://` target) and render to a PNG.
- Entry: `src/bin/fetch_and_render.rs`
- Run: `cargo run --release --bin fetch_and_render -- --help`

## `inspect_frag`

- Purpose: inspect fragment output (and related style/layout state) for a single input.
- Entry: `src/bin/inspect_frag.rs`
- Run: `cargo run --release --bin inspect_frag -- --help`
