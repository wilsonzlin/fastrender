# Running FastRender (local workflows)

This page describes how to build, test, and run the renderer using the repo as it exists today.

## Quick workflows (`cargo xtask`)

Use the `xtask` wrapper for common local loops:

- Test: `cargo xtask test [core|style|fixtures|wpt|all]` (default: `core`)
- Refresh goldens: `cargo xtask update-goldens [all|fixtures|reference|wpt]`
- Render a page: `cargo xtask render-page --url https://example.com --output out.png`
- Diff renders: `cargo xtask diff-renders --before fetches/renders/baseline --after fetches/renders/new`

`cargo xtask --help` and per-subcommand help describe available flags and defaults. All commands run offline unless you explicitly pass a network URL to `render-page`.

## Build & test

- Build: `cargo build`
- Tests: `cargo test --quiet`
- Style regression harness: `cargo test --quiet --test style_tests`

## Render real pages (batch)

FastRender’s real-page loop is:

1. Fetch pages (network): `cargo run --release --bin fetch_pages`
2. Render cached pages (no network): `cargo run --release --bin render_pages`
3. Inspect outputs under `fetches/renders/` (PNGs + per-page logs + `_summary.log`)

For large runs, both `fetch_pages` and `render_pages` accept `--shard <index>/<total>` to process a deterministic slice of the page list (0-based shard indices).

Cache layout:
- `fetches/html/` – cached HTML
- `fetches/assets/` – cached subresources (images/CSS/etc.)
- `fetches/renders/` – output PNGs and logs

## Render a single page

- `cargo run --release --bin fetch_and_render -- https://example.com out.png`

`fetch_and_render` supports `file://…` inputs for local repros. Run with `--help` for the full flag list.

## Capture and replay a self-contained bundle

- Fetch and bundle (captures HTML + subresources + metadata):  
  `cargo run --release --bin bundle_page -- fetch <url> --out capture_dir`
- Render strictly from the bundle without touching the network:  
  `cargo run --release --bin bundle_page -- render capture_dir --out output.png`

Bundles are deterministic directories or `.tar` archives containing:
- Raw document bytes with content-type and final URL
- All fetched CSS/image/font resources with bytes and HTTP metadata
- A manifest mapping the original URLs to bundle paths plus the render settings (viewport/dpr/scroll/full-page) used during capture

## Responsive captures and `<meta name="viewport">`

`<meta name="viewport">` is ignored by default so renders stay deterministic. When
building custom harnesses, enable it via `FastRenderConfig::with_meta_viewport(true)`
to respect author-provided `width`/`height` and zoom directives. The resolved zoom
is clamped to the 0.1–10 range and scales the effective device pixel ratio along
with the visual viewport used for `device-*` media features.

## Inspect layout/paint artifacts

The `inspect_frag` binary is the main “what happened?” tool:

- `cargo run --release --bin inspect_frag -- file:///abs/path/to/file.html`

It is designed for inspecting fragments and style/layout decisions on a single input.
