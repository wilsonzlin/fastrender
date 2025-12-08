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
- Inline FC now shapes with `ShapingPipeline`, keeps shaped runs on fragments, and uses real font metrics for struts/baselines; painter reuses those runs with pen advance and rudimentary RTL handling (`src/layout/contexts/inline`, `src/paint/painter.rs`).
- Added embedded Roboto Regular fallback for fontless environments and generic fallbacks now pick the first available face when generic lookup fails (`resources/fonts`, `src/text/font_loader.rs`, `src/text/font_db.rs`).
- Block FC now buffers inline-level children (including inline replaced/inline-block) and lays them out with `InlineFormattingContext`; inline-blocks get laid out through their own formatting context as atomic inline items (`src/layout/contexts/block/mod.rs`, `src/layout/contexts/inline/mod.rs`).
- Tests currently pass (`cargo test`).
- Replaced elements now resolve intrinsic sizes from actual images via `ImageCache` (using base URL from API/bin), reuse those sizes in inline/block layout, and the painter renders decoded images instead of gray placeholders (`src/api.rs`, `src/layout/utils.rs`, `src/layout/contexts/{block,inline}`, `src/paint/painter.rs`).
- Inline text breaking now aligns break opportunities and splits to cluster boundaries using shaped glyphs; `TextItem` tracks cumulative cluster advances so wrapping and mandatory breaks no longer slice through ligatures/combining marks (`src/layout/contexts/inline/line_builder.rs`).
- Inline line finalization now performs bidi visual reordering per line with the Unicode algorithm, treating non-text inline items as object replacements; x-positions are reassigned in visual order so mixed-direction lines place RTL runs correctly (`src/layout/contexts/inline/line_builder.rs`).
- CSS `direction` is now parsed/inherited (initial `ltr`), carried through computed styles, and used as the base paragraph direction for bidi analysis and per-line reordering (`src/style/{types,mod,properties,cascade}.rs`, `src/text/pipeline.rs`, `src/layout/contexts/inline/mod.rs`).
- Added CSS `unicode-bidi` support (normal/embed/override/isolate/isolate-override/plaintext), plumbed through computed styles and applied during inline line reordering via appropriate UBA control characters; per-item direction comes from computed style and non-text items still participate via object replacement markers (`src/style/{types,mod,properties,cascade}.rs`, `src/layout/contexts/inline/line_builder.rs`).
- Inline splits now preserve shaped glyphs instead of reshaping: splitting uses existing runs at cluster boundaries, adjusts glyph offsets/advances without re-running HarfBuzz, so ligatures/kerning survive across line breaks (`src/layout/contexts/inline/line_builder.rs`).
- Non-text inline/replaced items now enter bidi reordering wrapped in directionally isolated control characters (LRI/RLI…PDI) rather than bare U+FFFC, improving structural ordering for mixed-direction lines (`src/layout/contexts/inline/line_builder.rs`).
- Inline boxes now contribute their children’s text into the bidi reorder string instead of collapsing to a single object, so nested inline wrappers reorder according to their actual textual content (`src/layout/contexts/inline/line_builder.rs`).
- Centralized bidi control generation with `bidi_controls`; logical-text construction now wraps inline boxes and atomic inline items per `unicode-bidi` values while retaining default isolation for inline-block/replaced placeholders when no controls apply (`src/layout/contexts/inline/line_builder.rs`).
- Inline-block sizing now uses CSS 2.1 shrink-to-fit when `width:auto`, deriving preferred and preferred-min widths from intrinsic measurements and adding padding/borders; specified widths avoid over-constrained margins, and padding/border percentages resolve against the available inline size (`src/layout/contexts/inline/mod.rs`).
- Block layout now clamps computed content widths to `min-width`/`max-width` (percentages resolved against containing block) and recomputes margins with the clamped width so the constraint equation still holds when author limits kick in (`src/layout/contexts/block/mod.rs`).
- Inline replaced elements now honor horizontal margins: margins are resolved in the inline context, included in line advance, and their fragments are offset so margins behave as spacing rather than painted area (`src/layout/contexts/inline/{mod.rs,line_builder.rs}`).
- Intrinsic sizing for inline content now excludes margins (replaced elements contribute their border-box width to min/max-content sums while layout still accounts for margins separately) to better match CSS sizing definitions (`src/layout/contexts/inline/{mod.rs,line_builder.rs}`).
- Inline-blocks now resolve horizontal margins, subtract them from available inline space for shrink-to-fit sizing, and include them in line advance while only the border-box is painted (`src/layout/contexts/inline/{mod.rs,line_builder.rs}`).

## Current issues / gaps
- Bidi: we still approximate isolation with control characters rather than building explicit isolate/embedding stacks from box boundaries; replaced/inline-block items remain modeled as U+FFFC. `unicode-bidi: plaintext` uses first-strong via BidiInfo, but paragraph segmentation is naive (whole line).
- Min/max content sizing still uses simple break-opportunity offsets rather than cluster-aware shaping widths; no hyphenation support yet in IFC.
- Table layout is partial: border-spacing hardcoded to 0; border-collapse unimplemented; padding/borders/vertical-align ignored in row heights; row/col spans not truly honored; percent/fixed widths not resolved per CSS 2.1; colspans split evenly.
- Replaced elements still ignore object-fit/object-position/backgrounds on the content box; SVG/iframe/video remain unrendered.
- Painting still bypasses display list; no shared font context between layout and paint (painter builds its own).
- Inline-block sizing is naïve (no shrink-to-fit; uses available width floor), and inline-level replaced elements rely on display hints rather than true anonymous box generation.

## To-do / next steps (spec-oriented)
1. Inline/text:
   - Make line breaking cluster-aware and bidi-sensitive (UAX #14 + UAX #9 ordering), and keep shaped runs when splitting.
   - Integrate shared `FontContext` across layout/paint to avoid divergent font fallback; propagate font metrics into inline box metrics consistently.
   - Harden RTL run positioning and visual reordering in painter/layout so mixed-direction text paints correctly.
2. Tables:
   - Implement CSS 2.1 auto table layout properly: cell min/max via the correct formatting context (including padding/border), border-spacing/border-collapse initial values, percent/fixed widths resolved against containing block, correct colspan/rowspan distribution, vertical-align and padding/borders in row heights.
3. Replaced content:
   - Render images for `ReplacedType::Image` instead of placeholders.
4. General bidi:
   - Ensure visual ordering in layout and paint for RTL/mixed runs.
5. Inline blocks:
   - Implement shrink-to-fit sizing for inline-block and inline replaced elements so they honor min/max-content and percentages instead of using the containing width directly.
- Verify whether default isolation of inline-block/replaced placeholders is spec-correct when `unicode-bidi` is `normal`; currently we fall back to LRI/RLI…PDI when no other controls are present.

## Notes
- Current table code uses `InlineFormattingContext` for cell intrinsic widths; this fails for block/table/replaced content—needs a per-context intrinsic measurement.
- Default font family remains `"serif"`; UA defaults can be revisited when baseline metrics are real.

## Remote branch (cursor/pixel-perfect-news-ycombinator-com-rendering-gpt-5.1-codex-high-b0ad) scan
- Block FC: buffers inline children and runs them through `InlineFormattingContext` as a group, with margin collapse handling. Better than placeholder inline fragments—worth adopting concept.
- Painter: introduces `FontContext` parameter to `paint_tree`/`Painter::new` and uses `TextShaper` + `TextRasterizer` (`shape_text_simple`) with guessed font size; still LTR-only and style-agnostic. We already have a more capable shaping pipeline; don’t regress, but sharing the font context between layout/paint is a good idea.
- Table: adds a simplistic cell-content layout by stacking child fragments or collected text; ignores padding/borders/inline flow—worse than current plan.
- Inline FC: minor guard for mandatory breaks past end-of-text.

Actionable borrowings:
- Integrate the block inline-buffering approach (layout inline groups via IFC, flush on block children).
- Consider passing a shared `FontContext` into paint_tree/painter so paint and layout use the same font state.
