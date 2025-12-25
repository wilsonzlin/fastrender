# Conformance targets

FastRender is spec-first: correctness is defined by the HTML/CSS specifications rather than by heuristics. This document captures the renderer's intended conformance targets, what is currently implemented, and the gaps that are explicitly out of scope.

## Targeted specifications

### HTML
- **Parsing**: HTML5 parsing via html5ever's spec-mode tree builder with scripting disabled (`ParseOpts` in `src/dom.rs`).
- **Encoding sniffing**: HTML Living Standard BOM/`Content-Type`/`<meta charset>` sniffing (`src/html/encoding.rs`).
- **Shadow DOM snapshots**: Static `<template shadowroot>` attachment with slot distribution during parse (`attach_shadow_roots` and `distribute_slots` in `src/dom.rs`).
- **Base URL & viewport**: `<base href>` resolution and `<meta viewport>` handling (`width`/`height`/`initial`/`min`/`max` scale with zoom clamped to 0.1–10).

### CSS
- **Cascade & inheritance**: CSS Cascade 4, including origin ordering (UA stylesheet + author), `!important`, custom properties, cascade layers (`@layer`), `@import`, and scoped styles (`@scope`).
- **Selectors**: CSS Selectors Level 4 (:is/:where/:has, nth-* families, :lang/:dir, :target-within, form state pseudoclasses, ::before/::after/::marker/::backdrop, etc.). User-action states are opt-in via `data-fastr-*` flags.
- **Conditional rules**: @media (MQ4), @supports, size-only @container queries (Container Queries Level 1), and `@page`/page-margin rules.
- **At-rules with data**: `@font-face`, `@counter-style`, `@keyframes`, `@view-timeline`/`@scroll-timeline` names, `@page` pseudos.
- **Values & units**: CSS Values & Units 3/4 (`calc()`, percentages, viewport units, angle/time/frequency, color functions including Lab/LCH/OKLab/OKLCH).
- **Display & formatting**: CSS 2.1/2.2 visual formatting model, CSS Display 3, floats, positioning, logical properties, and writing-modes 3.
- **Layout modes**: CSS Flexbox 1, CSS Grid 1 (taffy), CSS Multi-column Layout 1, CSS Tables 3 / CSS 2.1 §17 (auto+fixed layout, border-collapse/separate), ruby, fragmentation/pagination.
- **Box decorations**: CSS Backgrounds & Borders 3 (border-radius, border-image, gradients), CSS Masking 1 (mask-*/clip-path), CSS Filters/Backdrop Filters, CSS Transforms (2D/3D + perspective), outline/appearance/cursors.
- **Typography**: CSS Fonts 4 (feature/variation settings, font-synthesis, font-size-adjust), CSS Text 3/4 (line-breaking, word-break/overflow-wrap, hyphenation, text-decoration, text-combine-upright), counter styles/list markers.

## Support matrix (repo reality)

Status legend: ✅ Supported, ⚠️ Partial/targeted, 🚫 Not supported.

| Stage  | Feature area | Status | Notes |
| --- | --- | --- | --- |
| Parse | HTML5 tree builder | ✅ | html5ever spec mode, scripting disabled; optional DOM compatibility toggles for legacy class flips (`DomCompatibilityMode`). |
| Parse | Encoding sniffing | ✅ | BOM → `Content-Type` → `<meta charset>` scan with Windows-1252 fallback. |
| Parse | Shadow DOM snapshots | ⚠️ | `<template shadowroot>` is attached eagerly and slots distributed; no runtime attach/detach or JS-driven shadow roots. |
| Parse | Target fragments | ✅ | `with_target_fragment` drives :target/:target-within matching. |
| Parse | Meta viewport directives | ✅ | `<meta name="viewport">` width/device-width/height plus initial/min/max scale parsed; zoom clamped to 0.1–10 feeds layout vs visual viewports and DPR when enabled via config. |
| Style | Stylesheet parsing | ✅ | `@import`, @media/@supports/@container/@scope/@layer/@page/@font-face/@counter-style/@keyframes; error recovery keeps valid rules. |
| Style | Selectors level 4 | ✅ | :is/:where/:has, relative selectors, structural nth-* pseudos, :lang/:dir, :any-link/:target-within, form-state pseudos, ::before/::after/::marker/::backdrop. Hover/focus/visited require `data-fastr-*` hints. |
| Style | Cascade & inheritance | ✅ | UA stylesheet + author cascade, custom properties with fallback, cascade layers, scoped styles, counter styles, color-scheme propagation. |
| Style | Conditional eval | ⚠️ | @media MQ4 features, @supports, size-only container queries; container type `size`/`inline-size` resolved from ancestor styles. |
| Style | Values & colors | ✅ | `calc()`, viewport units, font-relative units, gradients, color spaces (sRGB/HSL/Lab/LCH/OKLab), system colors, currentColor. |
| Box | Box tree generation | ✅ | Display ↔ box mapping, anonymous block/inline/table wrappers, list markers, generated content (::before/::after/::marker), ruby boxes, top-layer dialogs/popovers. |
| Box | Replaced elements | ✅ | Images/video canvas placeholders and UA form controls (`ReplacedType::FormControl`) with intrinsic sizing; `appearance:none` drops native painting. |
| Box | Table structure | ✅ | Anonymous table fixup, row/rowgroup/cell generation, captions/columns tracked for layout; see `docs/research/table-layout-spec.md`. |
| Layout | Block/inline formatting | ✅ | BFC/IFC with floats, shape-outside, margin-collapsing, inline baseline alignment, list markers, ruby layout. |
| Layout | Positioning | ✅ | relative/absolute/fixed/sticky; containing-block resolution respects transforms/filters; scroll offset handled via `ContainingBlock`. |
| Layout | Flexbox | ✅ | Taffy-backed flex layout (flex-basis/min/max sizing, alignment, order). |
| Layout | Grid | ⚠️ | CSS Grid 1 via taffy, including named lines/areas; subgrid support exists but is still maturing (see `tests/layout/subgrid.rs`). |
| Layout | Tables | ✅ | Native table layout (auto+fixed), border-collapse/separate, captions, column/row spans, baseline alignment; bidirectional constraint solving. |
| Layout | Multi-column | ✅ | column-count/width/gap/rule, column-span `all`, nested multicol segments (`tests/layout/multicol.rs`). |
| Layout | Fragmentation/pagination | ⚠️ | Fragment tree supports paged/columnar fragmentation (`LayoutConfig::with_fragmentation`); pagination tested in `tests/layout/paged_media.rs`. |
| Layout | Scroll/overflow | ⚠️ | `overflow`/scrollbar-gutter/clipping honored; no scrollbars rendered yet (visuals are non-goal). |
| Paint | Stacking & compositing | ✅ | Stacking contexts, isolation, mix-blend-mode/backdrop-filter, z-index ordering, top layer. |
| Paint | Backgrounds/borders/masks | ✅ | Gradients, border-image, border-radius, mask layers, clip-path basic shapes, object-fit/object-position. |
| Paint | Transforms & effects | ⚠️ | 2D + 3D transforms with perspective flattened for tiny-skia, backface-visibility, filters/backdrop-filter mapped to tiny-skia/SVG filters (subset of SVG filter graph). |
| Paint | Text & decorations | ✅ | Text shadows, underline/overline/line-through with offsets/decoration thickness, emphasis marks, ruby annotations painted inline. |
| Text | Shaping pipeline | ✅ | UAX #9 bidi, script itemization, RustyBuzz shaping with OpenType features/variations, emoji/color font fallback (`text/color_fonts.rs`). |
| Text | Line breaking & hyphenation | ✅ | Unicode line breaking, word-break/line-break modes, soft hyphens + hyphenate-character, text-combine-upright for vertical text. |
| Text | Justification & spacing | ⚠️ | Text-justify (inter-word) and letter/word spacing applied; advanced justification variants (inter-character/etc.) are TODO. |
| Layout | MathML (presentation) | ⚠️ | Parser/layout for common Presentation MathML (fractions/roots/scripts/mtable with row/col alignment/mfenced/mspace/mstyle/menclose, mathvariant fonts), display/inline sizing, and stretchy operators sized to surrounding content, integrated with the text shaping pipeline. |

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
2. **Add tests**: Cover the change with unit tests (module-level), fixture/render tests (`tests/fixtures` or `tests/layout`/`tests/paint`), and WPT cases when applicable (`tests/wpt`). New docs should also be gated by presence tests when they are repo policy.
3. **Link rationale**: If design choices reference research or internal notes, add links in the section above (or create a new short note under `docs/notes/`).
4. **Keep status accurate**: Mark entries as ⚠️ when behavior is partial but spec-correct for the implemented subset; use 🚫 for intentional gaps.
