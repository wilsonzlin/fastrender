# Architecture overview

FastRender is a single-process HTML/CSS renderer that produces a raster image (PNG/JPEG/WebP) from an input document.

The main orchestration code lives in `src/api.rs` (`FastRender`).

## Pipeline (high level)

1. HTML parse → DOM tree (`src/dom.rs`)
2. Extract & parse CSS (including `<style>` and external stylesheets)
3. Cascade → computed styles (`src/style/`)
4. Box tree + anonymous fixup (`src/tree/`)
5. Layout → fragment tree (`src/layout/`)
6. Build paint commands / stacking contexts (`src/paint/`)
7. Paint to a `tiny-skia` pixmap and encode (`src/image_output.rs`)

After layout, scroll snapping and scroll/view timeline-driven animations are resolved before building the final display list (`scroll.rs`, `animation::apply_scroll_driven_animations`).

## Key intermediate structures

- DOM: `crate::dom::DomNode`
- Styled tree: `crate::style::cascade::StyledNode`
- Box tree: `crate::tree::box_tree::BoxTree` / `BoxNode`
- Fragment tree: `crate::tree::fragment_tree::FragmentTree` / `FragmentNode`
  - Fragmentation-aware: `FragmentTree` can hold multiple root fragments when
    pagination/columns are requested via `LayoutConfig::with_fragmentation`.
- Display list / stacking: `crate::paint::display_list::*`, `crate::paint::stacking::*`

For the most accurate view of the current flow, follow `FastRender::render_html_internal` and `FastRender::layout_document` in `src/api.rs`.

## Compatibility toggles

HTML parsing runs in a spec-only mode by default; FastRender does not inject JS-era bootstrap classes or other mutations. When a static render needs those compatibility shims, set `FastRenderConfig::with_dom_compat_mode(DomCompatibilityMode::Compatibility)` (or `DomParseOptions::compatibility()` for lower-level parsing). The current compatibility mode mirrors common JS-driven class flips like turning `no-js` into `js-enabled` and adding `jsl10n-visible` on `html/body`—see `docs/notes/dom-compatibility.md` for details.

## Accessibility output

`FastRender::accessibility_tree` / `accessibility_tree_json` expose a static accessibility tree derived from the styled DOM (`src/accessibility.rs`), covering HTML/ARIA roles, names, descriptions, and core state flags.
