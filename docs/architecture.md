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

## Key intermediate structures

- DOM: `crate::dom::DomNode`
- Styled tree: `crate::style::cascade::StyledNode`
- Box tree: `crate::tree::box_tree::BoxTree` / `BoxNode`
- Fragment tree: `crate::tree::fragment_tree::FragmentTree` / `FragmentNode`
- Display list / stacking: `crate::paint::display_list::*`, `crate::paint::stacking::*`

For the most accurate view of the current flow, follow `FastRender::render_html_internal` and `FastRender::layout_document` in `src/api.rs`.
