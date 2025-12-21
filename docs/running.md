# Running FastRender (local workflows)

This page describes how to build, test, and run the renderer using the repo as it exists today.

## Build & test

- Build: `cargo build`
- Tests: `cargo test --quiet`
- Style regression harness: `cargo test --quiet --test style_tests`

## Render real pages (batch)

FastRender’s real-page loop is:

1. Fetch pages (network): `cargo run --release --bin fetch_pages`
2. Render cached pages (no network): `cargo run --release --bin render_pages`
3. Inspect outputs under `fetches/renders/` (PNGs + per-page logs + `_summary.log`)

Cache layout:
- `fetches/html/` – cached HTML
- `fetches/assets/` – cached subresources (images/CSS/etc.)
- `fetches/renders/` – output PNGs and logs

## Render a single page

- `cargo run --release --bin fetch_and_render -- https://example.com out.png`

`fetch_and_render` supports `file://…` inputs for local repros. Run with `--help` for the full flag list.

## Inspect layout/paint artifacts

The `inspect_frag` binary is the main “what happened?” tool:

- `cargo run --release --bin inspect_frag -- file:///abs/path/to/file.html`

It is designed for inspecting fragments and style/layout decisions on a single input.
