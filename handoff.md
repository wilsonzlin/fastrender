## Summary

- Reworked fragmentation planning to honor break hints (`before/after/inside`), widows/orphans, and avoid-inside blocks while propagating fragment metadata. Fragments now account for fragmentainer gaps and are translated per fragmentainer for correct stacking, with pagination-aware config and fragmentainer-height fallback slices when no explicit break candidates exist.
- Added CSS paged media support: @page parsing (selectors, size/margins, margin boxes), page property recognition, and pagination that selects left/right/first/named pages, clips content per page, and lays out margin boxes.
- Implemented `text-wrap: balance`, `pretty`, and `stable` handling that re-runs line construction with measured width factors and raggedness scoring, penalizing hyphenated endings/short last lines. Stability uses a conservative 90% effective inline width and skips balancing when floats shorten widths; regression tests cover Latin/CJK, hyphenation on/off, pretty, and stable modes.
- Added full parsing/resolution for `color-contrast()` (with optional `vs` reference defaulting to currentColor) using WCAG contrast with alpha compositing, plus `color(from …)` relative color syntax across srgb/srgb-linear/hsl/hwb/lab/lch/oklab/oklch/xyz with conversions, channel defaults, and currentColor-aware resolution. Tests cover unit, integration, gradients, and cascade cases.
- Introduced an accessibility tree builder that computes implicit/explicit roles, accessible names/descriptions, state flags (focusable, disabled, required/invalid, visited/pressed, checked/selected), heading levels, and form values while skipping hidden nodes. New APIs expose the tree and JSON output, with tests covering common controls, landmarks, tables, labels, and hidden filtering.
- Added `font-display` descriptor support with async web-font loading that respects block/swap/fallback/optional deadlines, filters by `unicode-range`, bumps font generation to invalidate shaping, and exposes pluggable fetchers plus `wait_for_pending_web_fonts`. Tests cover display phases, unicode-range filtering, block waiting, swap upgrades, and failure fallback.
- Implemented CSS Media Queries Level 5 features (scripting, update frequency, light-level, display-mode) with env overrides and builder helpers, dynamic viewport units (dvw/dvh/dvmin/dvmax), MediaContext fingerprinting updates, and media link extraction that respects media type (skips print styles in screen contexts). Tests cover MQ5 evaluation/invalids, env overrides, dynamic viewport units, cache invalidation, and print stylesheet handling.
- Added parsing/matching for `:has()` using relative selectors with proper specificity and `:scope` anchoring, plus traversal support for relative combinators and feature-query reporting. Regression coverage includes combinators, `:is`/`:not` interactions, and specificity.
- Expanded scrolling: post-layout scroll snap metadata with viewport/element offsets and snap-stop tie-breaking, plus a scroll chaining model honoring `overscroll-behavior`, clamping to bounds even without snap containers, and applying snap targets during chaining.
- Added mask-layer parsing/rendering and display-list mask application (alpha/luminance from gradients/painted images), alongside manual HSL/HSV/OKLCH/plus-darker blending, backdrop-filter isolation, and background-blend fixes across painter/display list. Inline SVG rendering now serializes styled subtrees with document `<style>` CSS, applies computed fonts/colors to roots, provides a foreignObject fallback, and covers masks/gradients/tests.
- Expanded CSS math support: trig/exponential/log/round/mod/rem/clamped functions, shared angle parsing for calc/math across properties/gradients/transforms, and stricter unit validation.
- Added motion path transforms with path flattening, tests, and transform ordering fixes.
- Added shape-outside image/gradient support for float contours with extensive layout regressions.
- Improved ::first-letter/::first-line handling with RTL-aware punctuation/graphemes, inline float handling, and cascade filtering plus tests.
- Implemented MathML parsing/layout/paint pipeline with rendering regressions.
- Implemented CSS ruby layout/spacing with regression coverage.
- Improved subgrid style plumbing, taffy grid cloning, and added subgrid regression tests.

## Commits

- `6d404bf`, `cb9b24c` (fragmentation/pagination planning)
- `d0f48b5`, `e10517b`, `c51ae16` (text-wrap balance/pretty/stable)
- `2c46d71`, `2311c93`, `c7bf57e`, `574de39` (color-contrast/relative colors)
- `2a039ea`, `7a2085e`, `f1c0507` (accessibility tree + handoff)
- `cff1950`, `71466ae`, `b16d4a2`, `bbe4a10`, `02fb894` (font-display/async web fonts)
- `3ffe5a2`, `0968d8d`, `c744b29`, `f944732` (MQ5 features, dynamic viewport units, media links)
- `79112ff`, `69a385d`, `703252b` (:has selectors and tests)
- `e142fa0`, `0eda8a4`, `f1f99a0` (scroll snap metadata and notes)
- `b0d7544`, `0a06054`, `d5bc737` (scroll chaining/overscroll, handoff)
- `e16e267`, `27e698f`, `f1049b6`, `7661b81`, `ea4ea30` (blend modes/backdrop isolation fixes)
- `ab5fc0a`, `4602f39`, `dcbb68b` (inline SVG serialization/tests)
- `b10cb6f`, `c49323b`, `589273b` (CSS math/trig functions and coverage)
- `9ad0e6e`, `36a7689`, `b0983da`, `4ebc08c` (mask layers, display list masks, notes/tests)
- `bf8a1f5` (shape-outside images/gradients and tests)
- `f84d085`, `31d74c5`, `52ace09`, `221fe1e` (first-letter/first-line handling and tests)
- `adf2356`, `c4d899f`, `e9d5de9` (MathML parsing/layout/paint)
- `aefb736` (CSS ruby layout/spacing)
- `5a12b39`, `5e8bf8e` (subgrid plumbing and tests)
- `c60291d` (motion path transforms)
- `a9b5431`, `db17be3`, `82fb7b7`, `8ecd045` (paged media parsing/pagination/tests)

## Testing

- Not run here (workers ran relevant suites incl. `cargo test fragmentation --quiet`, `cargo test text_wrap_ -- --nocapture`, `cargo test color_contrast -- --nocapture` / `cargo test`, `cargo test accessibility_`, `cargo test font_loader -- --nocapture`, `cargo test media_level5_features_evaluate -- --nocapture`, `cargo test extract_css_links_skips_print_only_for_screen -- --nocapture`, `cargo test has_selector_test -- --nocapture`, `cargo test supports_selector_test -- --nocapture`, `cargo test scroll_snap -- --nocapture`, `cargo test scroll -- --nocapture`, `cargo test inline_svg`, blend regressions (`backdrop_filter_isolates_blend_mode`, `stacking_context_hsl_blend_preserves_backdrop_luminance`, `background_blend_mode_combines_multiple_layers`, `plus_darker_blend_clamps_to_black`, `hue_hsv_blend_mode_uses_source_hue`, `color_oklch_blend_uses_source_chroma_and_hue`), `cargo test border_image_accepts_conic_gradients`, `cargo test mask_image_gradient_repeats`, `cargo test display_list_renderer -- --nocapture`, `cargo test trig_and_math_functions_resolve_for_numbers`, paged media layout tests, `cargo test shape_outside -- --nocapture`, `cargo test first_letter_and_first_line_styles_apply_to_fragments --quiet` plus related first-letter/first-line tests, `cargo test --test paint_tests math_render -- --test-threads=1 --nocapture`, and `cargo test --test layout_tests` for ruby.)

## Notes / Caveats

- Fragmentation remains a post-layout pass; column-count options are not yet integrated beyond metadata/translation. Fragmentainer coordinates are offset by `(fragmentainer_size + gap) * index`.
- Paged media: painter still renders only the first page; layout uses the first page’s content size for the main pass and does not reflow widths per page.
- Relative color `calc()` support is minimal (single numeric/percentage values only), only convertible spaces are allowed, and `color-contrast()` lacks `to` thresholds. Contrast evaluation composites transparent colors against white when selecting options.
- Accessibility tree flattens non-semantic containers without names/roles, skips hidden nodes (`display:none`/`visibility:hidden`/`aria-hidden`/`hidden`) for inclusion and label resolution, and uses the first cascade pass (no container-query reruns).
- Font-display timing constants are shortened for tests (block/auto ~300ms; fallback block 100ms + 400ms swap; optional 100ms). Block/auto wait only through the block window, loads continue asynchronously, and initial shaping may use fallbacks until web fonts finish loading.
- Scroll snapping requires providing element offsets in `ScrollState.elements`; `ScrollSnapResult.updates` lists per-container behavior. Scroll chaining processes innermost→outermost containers, honors `overscroll-behavior`, clamps to bounds, and applies snap targets during chaining.
- Display-list masks currently handle gradient-generated sources; URL mask images remain unhandled on the display-list path.
- Animations/selection/cursor/content-visibility updates from pending workers are not integrated (workers 21, 29, 30, 4, 7, 3, 13 not merged).
- CSS math: trig/inverse trig expect angle inputs; nonlinear math on lengths requires simple single-unit lengths (unit-mismatched expressions error).
- Inline SVG serialization inlines only document `<style>` CSS (no external/imported sheets), applies computed fonts/colors to the root, and falls back to painting foreignObject children when unsupported.
