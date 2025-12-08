# Scratchpad – rendering engine session notes

## Context
- Project: Rust HTML/CSS renderer (`fastrender`), sandboxed workspace-write; network restricted.
- Taffy is vendored in `vendor/taffy/` and must not be updated via Cargo.
- Goal: make the renderer spec-faithful (tables, text shaping, painting) and remove site-specific hacks.

## Recent changes (this branch)
- Removed HN-specific hacks (vote arrow injection, navigation text forcing) from `src/tree/box_generation.rs`.
- Default font size restored to 16px (`src/style/mod.rs`); table margins stripped back to zero in defaults.
- Fixed BGRA → RGBA conversion during image encoding (`src/image_output.rs`).
- Inline text fragments now carry computed styles; painter uses shaping pipeline and real glyph outlines instead of box approximations (`src/paint/painter.rs`).
- Table layout rewritten to use computed display values, default border-spacing 0, column constraints via `column_distribution`, `<col>/<colgroup>` width handling, min/max from IFC, fallback equal widths if zero; cells laid out with BFC using computed column widths (`src/layout/table.rs`).
- Added `FragmentNode::new_inline_styled` convenience (`src/tree/fragment_tree.rs`).
- Tests currently pass (`cargo test`).

## Current issues / gaps
- Painter still renders each shaped run at the same origin (no pen advance, no bidi direction handling), so multi-run/fallback/bidi text overpaints.
- Inline layout uses heuristic splitting and drops shaping data (`TextItem::split_at` empty glyphs); line breaking is byte-based, not cluster/bidi aware. Min/max content widths use `font_size * 0.5` estimates.
- Baseline metrics are guessed (`font_size * 0.8` etc.), not taken from font ascenders/descenders.
- Table layout is partial: border-spacing hardcoded to 0; border-collapse unimplemented; padding/borders/vertical-align ignored in row heights; row/col spans not truly honored; percent/fixed widths not resolved per CSS 2.1; colspans split evenly.
- Replaced elements still paint gray placeholders even for images.
- Layout/paint disagreement: layout doesn’t use shaped advances, painting now does, so wrapping/positioning can diverge.

## To-do / next steps (spec-oriented)
1. Inline/text:
   - Use `ShapingPipeline` for measurement and line breaking (cluster and bidi aware); carry shaped runs through splits.
   - Advance pen between shaped runs using `run.advance`; honor `run.direction` when painting/laying out.
   - Replace heuristic baselines with real font metrics.
2. Tables:
   - Implement CSS 2.1 auto table layout properly: cell min/max via the correct formatting context (including padding/border), border-spacing/border-collapse initial values, percent/fixed widths resolved against containing block, correct colspan/rowspan distribution, vertical-align and padding/borders in row heights.
3. Replaced content:
   - Render images for `ReplacedType::Image` instead of placeholders.
4. General bidi:
   - Ensure visual ordering in layout and paint for RTL/mixed runs.

## Notes
- Current table code uses `InlineFormattingContext` for cell intrinsic widths; this fails for block/table/replaced content—needs a per-context intrinsic measurement.
- Default font family remains `"serif"`; UA defaults can be revisited when baseline metrics are real.
