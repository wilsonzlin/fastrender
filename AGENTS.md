# AGENTS.md

## Task
Build a spec-faithful HTML/CSS renderer in Rust that outputs pixel-correct PNGs. Follow the HTML/CSS specs (CSS 2.1 tables, cascade, inline layout, replaced elements) and favor correctness over page-specific hacks. Real pages must render; incomplete features are acceptable when they fail gracefully.

## Dependencies
- **Taffy v0.5.2** (vendored in `vendor/taffy/`): used for flexbox and grid only. Do not update via Cargo; vendor patches must be applied manually.
- **resvg**: SVG rendering.
- **tiny-skia**: 2D raster pipeline for backgrounds, borders, shadows, and text.
- **image** (PNG + GIF enabled): image decoding/encoding.
- **scraper**: HTML parsing to the DOM tree.
- **cssparser**: CSS parsing.

## Pipeline
```
fetch (HttpFetcher/File/Data URL) → DOM parse → CSS parse → cascade/compute styles
→ box tree (anonymous fixup) → layout (block/inline/flex/grid/table/positioned)
→ fragments → display list → paint to PNG
```

## Key Modules
- `src/lib.rs`: public API entry points (`render_html_to_image`, `fetch_and_render`), renderer builder.
- `src/resource.rs`: `ResourceFetcher` abstraction (`HttpFetcher` default).
- `src/dom.rs`: DOM construction from HTML.
- `src/css/`: stylesheet parsing, selectors, at-rules.
- `src/style/`: cascade, computed styles, UA defaults (`src/user_agent.css`), logical properties, font/color parsing.
- `src/layout/`: formatting contexts:
  - Block (`contexts/block`), inline (`contexts/inline`), flex/grid (`contexts/flex`, `contexts/grid`, via Taffy), table (`table.rs` and `contexts/table`), positioned (`absolute_positioning.rs`, `relative_positioning.rs`).
  - Fragment conversion and anonymous wrapper fixup live in `src/tree/box_tree.rs` and `src/layout/table.rs`.
- `src/paint/`: display list builder and painter (backgrounds, borders, outlines, shadows, text, SVG/replaced elements) using tiny-skia/resvg; image loading in `src/image_loader.rs`.
- `src/text/`: font discovery/loading and shaping pipeline.
- Tooling: `examples/inspect_frag.rs` (fragment dumps), `src/bin/fetch_pages.rs` (fetch targets), `src/bin/render_pages.rs` (batch renders).

## Table Layout (CSS 2.1 Section 17)
- Native table algorithm (no flex fallbacks). Supports `table-layout: auto` and `table-layout: fixed`, separate vs. collapsed border models, border-spacing/empty-cells, captions, and anonymous table objects.
- Column sizing honors `<col>/<colgroup>` widths, cell spans, percentage bases, and min/max constraints. Row/column spans distribute width/height per spec; baselines follow 17.5.3 (row falls back to its bottom when no baseline contributes).
- Row height distribution accounts for border-spacing, collapsed borders, percent heights, and rowspans. Border conflict resolution and painting are implemented in layout/paint.

## Layout & Sizing
- Block formatting contexts with margin collapsing and BFC rules; inline formatting context with shaping, bidi ordering, list markers, text indent, `text-align-last`, and decoration synthesis.
- Flex/grid layout delegated to Taffy; constraints mapped to internal fragments with viewport/percentage resolution and shrink-to-fit probes.
- Replaced elements follow CSS 2.1 sizing with `object-fit`/`object-position` and `aspect-ratio`; positioned layout resolves containing blocks and percentage bases.

## Painting
- Display list built from fragments, respecting stacking contexts, clipping, border-radius/clip-path, shadows, outlines, and text decorations. SVG via resvg; raster via tiny-skia.
- Backgrounds respect `background-clip` and rounded corners; text rendering uses shaped glyph runs.

## Testing & Debugging
- `cargo test --quiet` for the unit suite.
- `cargo run --release --bin fetch_pages` to cache target HTML into `fetches/html/`; `cargo run --release --bin render_pages` to render cached pages into `fetches/renders/` with per-page logs and `_summary.log`.
- `examples/inspect_frag` (accepts `file://` paths) to inspect fragment positions/styles; environment knobs like `FASTR_RENDER_TIMINGS`, `FASTR_LOG_*`, and `FASTR_FULL_PAGE` aid profiling.
- Persistent notes live in `scratchpad.md`; update it when discoveries affect behavior or debugging strategy.

## Guidelines
- Spec-first: avoid site-specific behavior or post-layout overrides. Implement missing features conservatively instead of hacking around them.
- Keep Taffy vendored; integrate upstream changes manually to preserve custom behavior.
- Prefer instrumentation and tests over hard-coded tweaks; document major changes in `scratchpad.md` and this file.
