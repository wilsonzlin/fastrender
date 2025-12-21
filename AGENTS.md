# FastRender (agent guide)

This repository is an internal HTML/CSS renderer in Rust. The goal is to render real pages into pixel-correct raster output while staying spec-faithful.

`docs/README.md` is the internal wiki entry point.

## Project goals

- **Spec-first correctness**: implement CSS/HTML algorithms as written; “incomplete but correct” beats “complete but wrong”.
- **Real pages must render**: when a feature is missing, fail gracefully and keep the rest of the page rendering.
- **No page-specific hacks**: avoid special-casing particular sites or element/class names.

## Non-goals

- This is not a browser and does not run author JavaScript.
- DOM mutation to emulate JS is not a general strategy; if a compatibility behavior is needed, keep it explicit, small, and isolated.

## Core invariants (keep these true)

- Pipeline is staged (parse → style → box tree → layout → paint). Avoid post-layout “pixel nudges” that contradict the spec.
- Tables use a native table algorithm (don’t route tables through flex/grid).
- Taffy is used for **flex and grid only**, and is vendored under `vendor/taffy/` (see `docs/vendoring.md`).

## How to work effectively

### Loop A: bug-driven (real pages)

1. Fetch: `cargo run --release --bin fetch_pages`
2. Render: `cargo run --release --bin render_pages`
3. Turn an observation into something actionable:
   - a minimal repro + a regression test, or
   - a clearly scoped spec gap with an implementation plan
4. Fix and re-run the relevant tests/pages.

Timebox “render and stare”: if you can’t turn it into a repro/spec task quickly, switch to Loop B.

### Loop B: spec-driven (always available)

Pick one spec area and implement a coherent slice end-to-end (parsing → cascade → layout → paint → tests). Prefer finishing a feature slice over scattered micro-changes.

## Tooling

- Binaries: `fetch_pages`, `render_pages`, `fetch_and_render` (see `docs/cli.md`)
- Deep inspection: `cargo run --release --example inspect_frag -- ...`
- Debug/profiling toggles: `docs/env-vars.md`, `docs/debugging.md`, `docs/perf_logging.md`

## Documentation policy

- `docs/` is the wiki and must reflect repo reality.
- Avoid “phase/wave/task” planning docs in the repo; keep planning elsewhere.
- Don’t add scratchpads/status logs to the repo. If a discovery is durable, write it up under `docs/notes/` (small and specific) or as a focused code comment in the relevant module.
