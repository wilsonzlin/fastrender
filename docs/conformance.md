# Conformance targets

FastRender is spec-first: correctness is defined by the HTML/CSS specifications rather than by heuristics. This document captures the renderer's intended conformance targets, what is currently implemented, and the gaps that are explicitly out of scope.

## Targeted specifications

### HTML
- **Parsing**: HTML5 parsing via html5ever's spec-mode tree builder with scripting disabled (`ParseOpts` in `src/dom.rs`).
- **Encoding sniffing**: HTML Living Standard BOM/`Content-Type`/`<meta charset>` sniffing (`src/html/encoding.rs`).
- **Shadow DOM snapshots**: Static `<template shadowroot>` attachment with slot distribution during parse (`attach_shadow_roots` and `distribute_slots` in `src/dom.rs`).
- **Base URL & viewport**: `<base href>` resolution and `<meta viewport>` handling (`width`/`height`/`initial`/`min`/`max` scale with zoom clamped to 0.1–10).
- **Accessibility tree**: Static AOM-style export from the styled DOM with HTML/ARIA role/name/state mapping (`src/accessibility.rs`).

### CSS
- **Cascade & inheritance**: CSS Cascade 4, including origin ordering (UA stylesheet + author), `!important`, custom properties, cascade layers (`@layer`), `@import`, and scoped styles (`@scope`).
- **Selectors**: CSS Selectors Level 4 (:is/:where/:has, nth-* families, :lang/:dir, :target-within, form state pseudoclasses, ::before/::after/::marker/::backdrop, etc.). User-action states are opt-in via `data-fastr-*` flags.
- **Conditional rules**: @media (MQ4), @supports, size-only @container queries (Container Queries Level 1), and `@page`/page-margin rules.
- **At-rules with data**: `@font-face`, `@counter-style`, `@keyframes`, `@view-timeline`/`@scroll-timeline` names, `@page` pseudos.
- **Scroll-driven animations**: Scroll/view timelines plus @keyframes sampling (CSS Scroll-driven Animations 1 subset; time-based timelines are not executed yet).
- **Values & units**: CSS Values & Units 3/4 (`calc()`, percentages, viewport units, angle/time/frequency, color functions including Lab/LCH/OKLab/OKLCH).
- **Display & formatting**: CSS 2.1/2.2 visual formatting model, CSS Display 3, floats, positioning, logical properties, and writing-modes 3.
- **Layout modes**: CSS Flexbox 1, CSS Grid 1 (taffy), CSS Multi-column Layout 1, CSS Tables 3 / CSS 2.1 §17 (auto+fixed layout, border-collapse/separate), ruby, fragmentation/pagination.
- **Box decorations**: CSS Backgrounds & Borders 3 (border-radius, border-image, gradients), CSS Masking 1 (mask-*/clip-path), CSS Filters/Backdrop Filters, CSS Transforms (2D/3D + perspective), outline/appearance/cursors.
- **Typography**: CSS Fonts 4 (feature/variation settings, font-synthesis, font-size-adjust), CSS Text 3/4 (line-breaking, word-break/overflow-wrap, hyphenation, text-decoration, text-combine-upright), counter styles/list markers.

## Support matrix (repo reality)

Status legend: ✅ Supported, ⚠️ Partial/targeted, 🚫 Not supported.

| Stage  | Feature area | Status | Implementation | Tests | Notes |
| --- | --- | --- | --- | --- | --- |
| Parse | HTML5 tree builder | ✅ | [src/dom.rs](../src/dom.rs) | [tests/dom_compatibility_test.rs](../tests/dom_compatibility_test.rs) | html5ever spec mode, scripting disabled; optional DOM compatibility toggles for legacy class flips (`DomCompatibilityMode`). |
| Parse | Encoding sniffing | ✅ | [src/html/encoding.rs](../src/html/encoding.rs) | Module tests in [src/html/encoding.rs](../src/html/encoding.rs) | BOM → `Content-Type` → `<meta charset>` scan with Windows-1252 fallback. |
| Parse | Base URL & meta viewport | ✅ | [src/html/mod.rs](../src/html/mod.rs)<br>[src/html/viewport.rs](../src/html/viewport.rs) | [tests/integration_test.rs](../tests/integration_test.rs) | `<base href>` resolution drives URL absolutization; `<meta name="viewport">` parsed/applied when enabled via `FastRenderConfig::with_meta_viewport`. |
| Parse | Shadow DOM snapshots | ⚠️ | [src/dom.rs](../src/dom.rs) | [tests/tree/shadow_dom.rs](../tests/tree/shadow_dom.rs) | `<template shadowroot>` is attached eagerly and slots distributed; no runtime attach/detach or JS-driven shadow roots. |
| Parse | Target fragments | ✅ | [src/dom.rs](../src/dom.rs) | Target pseudo tests in [src/dom.rs](../src/dom.rs) | `with_target_fragment` drives :target/:target-within matching. |
| Style | Stylesheet parsing | ✅ | [src/css/parser.rs](../src/css/parser.rs) | [tests/css_loader_tests.rs](../tests/css_loader_tests.rs) | `@import`, @media/@supports/@container/@scope/@layer/@page/@font-face/@counter-style/@keyframes; error recovery keeps valid rules. |
| Style | Selectors level 4 | ✅ | [src/css/selectors.rs](../src/css/selectors.rs) | [tests/style/has_selector_test.rs](../tests/style/has_selector_test.rs) | :is/:where/:has, relative selectors, structural nth-* pseudos, :lang/:dir, :any-link/:target-within, form-state pseudos, ::before/::after/::marker/::backdrop. Hover/focus/visited require `data-fastr-*` hints. |
| Style | Cascade & inheritance | ✅ | [src/style/cascade.rs](../src/style/cascade.rs) | [tests/style/layer_important_test.rs](../tests/style/layer_important_test.rs) | UA stylesheet + author cascade, custom properties with fallback, cascade layers, scoped styles, counter styles, color-scheme propagation. |
| Style | Conditional eval | ⚠️ | [src/style/media.rs](../src/style/media.rs)<br>[src/style/cascade.rs](../src/style/cascade.rs) | [tests/style/media_test.rs](../tests/style/media_test.rs)<br>[tests/style/supports_rule_test.rs](../tests/style/supports_rule_test.rs) | @media MQ4 features, @supports, size-only container queries; container type `size`/`inline-size` resolved from ancestor styles. |
| Style | Values & colors | ✅ | [src/style/values.rs](../src/style/values.rs)<br>[src/style/color.rs](../src/style/color.rs) | [tests/style/css_numeric_functions.rs](../tests/style/css_numeric_functions.rs)<br>[tests/paint/color_mix_display_list_test.rs](../tests/paint/color_mix_display_list_test.rs) | `calc()`, viewport units, font-relative units, gradients, color spaces (sRGB/HSL/Lab/LCH/OKLab), system colors, currentColor. |
| Box | Box tree generation | ✅ | [src/tree/box_generation.rs](../src/tree/box_generation.rs) | [tests/tree/test_anonymous_boxes.rs](../tests/tree/test_anonymous_boxes.rs) | Display ↔ box mapping, anonymous block/inline/table wrappers, list markers, generated content (::before/::after/::marker), ruby boxes, top-layer dialogs/popovers. |
| Box | Replaced elements | ✅ | [src/tree/box_tree.rs](../src/tree/box_tree.rs) | [tests/tree/form_option_nonrendered.rs](../tests/tree/form_option_nonrendered.rs) | Images/video canvas placeholders and UA form controls (`ReplacedType::FormControl`) with intrinsic sizing; `appearance:none` drops native painting. |
| Box | Table structure | ✅ | [src/tree/table_fixup.rs](../src/tree/table_fixup.rs)<br>[src/layout/table.rs](../src/layout/table.rs) | [tests/layout/table_columns_test.rs](../tests/layout/table_columns_test.rs) | Anonymous table fixup, row/rowgroup/cell generation, captions/columns tracked for layout; see `docs/research/table-layout-spec.md`. |
| Layout | Block/inline formatting | ✅ | [src/layout/contexts/block/mod.rs](../src/layout/contexts/block/mod.rs)<br>[src/layout/contexts/inline/mod.rs](../src/layout/contexts/inline/mod.rs) | [tests/layout/test_inline_float.rs](../tests/layout/test_inline_float.rs) | BFC/IFC with floats, shape-outside, margin-collapsing, inline baseline alignment, list markers, ruby layout. |
| Layout | Positioning | ✅ | [src/layout/absolute_positioning.rs](../src/layout/absolute_positioning.rs) | [tests/layout/test_positioned.rs](../tests/layout/test_positioned.rs) | relative/absolute/fixed/sticky; containing-block resolution respects transforms/filters; scroll offset handled via `ContainingBlock`. |
| Layout | Flexbox | ✅ | [src/layout/contexts/flex.rs](../src/layout/contexts/flex.rs) | [tests/layout/flex_box_sizing_test.rs](../tests/layout/flex_box_sizing_test.rs) | Taffy-backed flex layout (flex-basis/min/max sizing, alignment, order). |
| Layout | Grid | ⚠️ | [src/layout/contexts/grid.rs](../src/layout/contexts/grid.rs)<br>[src/layout/taffy_integration.rs](../src/layout/taffy_integration.rs) | [tests/layout/subgrid.rs](../tests/layout/subgrid.rs) | CSS Grid 1 via taffy, including named lines/areas; subgrid support exists but is still maturing. |
| Layout | Tables | ✅ | [src/layout/table.rs](../src/layout/table.rs) | [tests/layout/table_anonymous_inheritance.rs](../tests/layout/table_anonymous_inheritance.rs) | Native table layout (auto+fixed), border-collapse/separate, captions, column/row spans, baseline alignment; bidirectional constraint solving. |
| Layout | Multi-column | ✅ | [src/layout/contexts/block/mod.rs](../src/layout/contexts/block/mod.rs) | [tests/layout/multicol.rs](../tests/layout/multicol.rs) | column-count/width/gap/rule, column-span `all`, nested multicol segments. |
| Layout | Fragmentation/pagination | ⚠️ | [src/layout/fragmentation.rs](../src/layout/fragmentation.rs)<br>[src/layout/pagination.rs](../src/layout/pagination.rs) | [tests/layout/paged_media.rs](../tests/layout/paged_media.rs) | Fragment tree supports paged/columnar fragmentation (`LayoutConfig::with_fragmentation`); pagination tested against page-margin boxes. |
| Layout | Scroll/overflow | ⚠️ | [src/scroll.rs](../src/scroll.rs) | [tests/layout/scrollbar_gutter.rs](../tests/layout/scrollbar_gutter.rs) | `overflow`/scrollbar-gutter/clipping honored with scroll snap metadata; no scrollbars rendered yet (visuals are non-goal). |
| Layout/Paint | CSS Containment (layout/size/paint) | ⚠️ | [src/layout/contexts/block/mod.rs](../src/layout/contexts/block/mod.rs)<br>[src/layout/contexts/inline/mod.rs](../src/layout/contexts/inline/mod.rs)<br>[src/layout/contexts/flex.rs](../src/layout/contexts/flex.rs)<br>[src/layout/contexts/grid.rs](../src/layout/contexts/grid.rs)<br>[src/paint/display_list_builder.rs](../src/paint/display_list_builder.rs) | [tests/layout/contain_layout.rs](../tests/layout/contain_layout.rs)<br>[tests/paint/contain_paint.rs](../tests/paint/contain_paint.rs) | Layout/inline-size containment establishes formatting context boundaries and removes descendant intrinsic inline contributions; size containment clamps auto block sizes to padding/border. Layout/paint containment provides containing blocks for abs/fixed descendants, and paint containment clips contained stacking contexts to the padding edge with radii. Style containment remains TODO. |
| Paint | Stacking & compositing | ✅ | [src/paint/stacking.rs](../src/paint/stacking.rs)<br>[src/paint/display_list.rs](../src/paint/display_list.rs) | [tests/paint/stacking_test.rs](../tests/paint/stacking_test.rs) | Stacking contexts, isolation, mix-blend-mode/backdrop-filter, z-index ordering, top layer. |
| Paint | Backgrounds/borders/masks | ✅ | [src/paint/display_list.rs](../src/paint/display_list.rs)<br>[src/paint/clip_path.rs](../src/paint/clip_path.rs) | [tests/paint/display_list_test.rs](../tests/paint/display_list_test.rs) | Gradients, border-image, border-radius, mask layers, clip-path basic shapes, object-fit/object-position. |
| Paint | Responsive images (`srcset`/`sizes`/`picture`/`image-set()`) | ✅ | [src/html/images.rs](../src/html/images.rs)<br>[src/paint/painter.rs](../src/paint/painter.rs)<br>[src/style/properties.rs](../src/style/properties.rs) | [tests/paint/intrinsic_srcset.rs](../tests/paint/intrinsic_srcset.rs)<br>[tests/paint/picture_source_selection.rs](../tests/paint/picture_source_selection.rs)<br>[tests/paint/responsive_srcset_golden.rs](../tests/paint/responsive_srcset_golden.rs)<br>[tests/paint/responsive_width_srcset_golden.rs](../tests/paint/responsive_width_srcset_golden.rs) | Density + width descriptors with `sizes` media evaluation, `<picture>` type/media filters, CSS `image-set()` DPR selection; avoids fetching non-selected candidates. |
| Paint | Transforms & effects | ⚠️ | [src/paint/display_list_renderer.rs](../src/paint/display_list_renderer.rs)<br>[src/paint/svg_filter.rs](../src/paint/svg_filter.rs) | [tests/paint/display_list_renderer_test.rs](../tests/paint/display_list_renderer_test.rs) | 2D + 3D transforms with perspective flattened for tiny-skia, backface-visibility, filters/backdrop-filter mapped to tiny-skia/SVG filters (subset of SVG filter graph). |
| Paint | Text & decorations | ✅ | [src/paint/text_rasterize.rs](../src/paint/text_rasterize.rs)<br>[src/paint/text_shadow.rs](../src/paint/text_shadow.rs) | [tests/paint/text_rasterize_test.rs](../tests/paint/text_rasterize_test.rs) | Text shadows, underline/overline/line-through with offsets/decoration thickness, emphasis marks, ruby annotations painted inline. |
| Text | Shaping pipeline | ✅ | [src/text/pipeline.rs](../src/text/pipeline.rs) | [tests/text/pipeline_test.rs](../tests/text/pipeline_test.rs) | UAX #9 bidi, script itemization, RustyBuzz shaping with OpenType features/variations, emoji/color font fallback (`text/color_fonts.rs`). |
| Text | Line breaking & hyphenation | ✅ | [src/text/line_break.rs](../src/text/line_break.rs)<br>[src/text/hyphenation.rs](../src/text/hyphenation.rs) | [tests/text/line_break_test.rs](../tests/text/line_break_test.rs)<br>[tests/text/hyphenation_test.rs](../tests/text/hyphenation_test.rs) | Unicode line breaking, word-break/line-break modes, soft hyphens + hyphenate-character, text-combine-upright for vertical text. |
| Text | Justification & spacing | ⚠️ | [src/text/justify.rs](../src/text/justify.rs) | [tests/text/justify_test.rs](../tests/text/justify_test.rs) | Text-justify (inter-word) and letter/word spacing applied; advanced justification variants (inter-character/etc.) are TODO. |
| Animation | Scroll/view timeline animations | ⚠️ | [src/animation/mod.rs](../src/animation/mod.rs) | [tests/animation_tests.rs](../tests/animation_tests.rs)<br>[tests/transitions_starting_style_test.rs](../tests/transitions_starting_style_test.rs) | Samples @keyframes on scroll/view timelines and render-time @starting-style transitions when an animation timestamp is provided; starting-style declarations override the normal cascade in the starting snapshot while scroll/view animations override transitions for shared properties. Time-based @keyframes remain unimplemented. |
| Accessibility | Accessibility tree export | ⚠️ | [src/accessibility.rs](../src/accessibility.rs) | [tests/accessibility_test.rs](../tests/accessibility_test.rs), [tests/accessibility_name_computation.rs](../tests/accessibility_name_computation.rs) | Static accessibility tree with HTML/ARIA role/name/state mapping and ACCNAME 1.2-style naming/description (aria-labelledby token order, aria-label, native labels, placeholder/value/alt/title, caption/legend/summary, recursive content traversal, aria-describedby/aria-description; aria-hidden/inert pruning). |

## Non-goals (reiterated)

- **No JavaScript execution**: FastRender is a static renderer; author scripts are not run. JS-driven DOM/class changes must be mirrored explicitly (e.g., compatibility mode class flips in `docs/notes/dom-compatibility.md`).
- **No page-specific hacks by default**: Site compatibility shims are opt-in (`CompatProfile::SiteCompatibility`), and the pipeline stays spec-faithful otherwise (`docs/notes/site-compat-hacks.md`).
- **No “table as flex/grid” shortcuts**: Tables use the native algorithms; flex/grid substitutions are invalid (`docs/research/table-layout-spec.md`).
- **No pixel-nudging after layout**: The pipeline is staged (parse → style → box → layout → paint). Paint should not override layout decisions.

## Research & notes

- Tables: [`docs/research/table-layout-spec.md`](research/table-layout-spec.md)
- Visual formatting model: [`docs/research/css21-visual-formatting-model.md`](research/css21-visual-formatting-model.md)
- 3D transforms: [`docs/notes/3d-transforms.md`](notes/3d-transforms.md)
- Logical backgrounds: [`docs/notes/logical-background-mapping.md`](notes/logical-background-mapping.md)
- Form controls as replaced elements: [`docs/notes/form-controls.md`](notes/form-controls.md)
- DOM compatibility mode: [`docs/notes/dom-compatibility.md`](notes/dom-compatibility.md)
- Site compat toggles: [`docs/notes/site-compat-hacks.md`](notes/site-compat-hacks.md)

## Maintaining this matrix

When adding or tightening conformance:

1. **Declare the target**: Add a row or bullet here describing the new spec slice and its expected status.
2. **Add tests**: Cover the change with unit tests (module-level), fixture/render tests (`tests/fixtures` or `tests/layout`/`tests/paint`), and WPT cases when applicable (`tests/wpt`). Keep the Implementation/Tests links above accurate so doc presence tests stay green.
3. **Link rationale**: If design choices reference research or internal notes, add links in the section above (or create a new short note under `docs/notes/`).
4. **Keep status accurate**: Mark entries as ⚠️ when behavior is partial but spec-correct for the implemented subset; use 🚫 for intentional gaps.
