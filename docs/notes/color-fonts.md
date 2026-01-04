# Color font rendering

FastRender supports multiple OpenType color glyph formats. This note captures the
renderer entry points, format preference order, palette plumbing, and known
limitations.

## Rendering entry points

- Low-level rasterization: [`TextRasterizer`](../../src/paint/text_rasterize.rs)
  - Main entry points are `render_shaped_run*`, `render_runs`, and `render_glyph_run`.
  - `TextRasterizer` delegates per-glyph color rasterization to
    [`ColorFontRenderer`](../../src/text/color_fonts/mod.rs); when it returns `None`,
    glyphs fall back to monochrome outline rendering.
  - `TextRenderState` (transform, clip, opacity, blend mode) applies to both
    outline and color glyph draws.

- Default page renderer: display list
  - [`DisplayListRenderer`](../../src/paint/display_list_renderer.rs) executes a
    [`DisplayList`](../../src/paint/display_list.rs) into a
    [`Canvas`](../../src/paint/canvas.rs).
  - `Canvas` owns a `TextRasterizer`. `DisplayListRenderer::new_with_text_state`
    constructs it with shared glyph + color-glyph caches so bitmap/SVG/COLR rasters
    are reused across display-list tiles and threads.
  - Display-list text items are built from shaped runs in
    [`DisplayListBuilder`](../../src/paint/display_list_builder.rs) and rendered via
    `Canvas::draw_text` plus the display-list text-shadow path.

## Supported formats (preference order)

[`ColorFontRenderer::render`](../../src/text/color_fonts/mod.rs) tries, in order:

1. Embedded bitmaps (CBDT/CBLC or sbix):
   [`bitmap.rs`](../../src/text/color_fonts/bitmap.rs)
   - `sbix` strikes are preferred when present; supported encodings are PNG
     (`"png "`) and JPEG (`"jpg "`/`"jpeg"`).
   - CBDT/CBLC payloads surfaced by `ttf_parser::Face::glyph_raster_image` are
     decoded as PNG (`RasterImageFormat::PNG`), premultiplied BGRA32
     (`BitmapPremulBgra32`), or (where possible) via the `image` crate.
2. SVG-in-OT glyphs: [`svg.rs`](../../src/text/color_fonts/svg.rs) rendered with
   `resvg`/`usvg` and scaled from the font's units-per-em to the requested size.
3. COLR v1 paint graphs: [`colr_v1.rs`](../../src/text/color_fonts/colr_v1.rs).
4. COLR/CPAL v0 layered outlines: [`colr_v0.rs`](../../src/text/color_fonts/colr_v0.rs).
5. Fallback: monochrome outline rendering tinted with the text color.

## Palette handling

### From CSS to shaped runs

- CSS parsing/cascade:
  - `font-palette` is stored on `ComputedStyle` as `FontPalette` (see
    [`src/style/types.rs`](../../src/style/types.rs)).
  - `@font-palette-values` rules are stored in a `FontPaletteRegistry` (see
    [`src/style/font_palette.rs`](../../src/style/font_palette.rs)).

- Shaping:
  - During shaping in [`src/text/pipeline.rs`](../../src/text/pipeline.rs)
    (`push_font_run`), FastRender resolves the active palette for the chosen font
    family via
    [`resolve_font_palette_for_font`](../../src/style/font_palette.rs).
    - Returns a base palette preference (`normal|light|dark|index(n)`), plus a list
      of `(palette_entry_index, Rgba)` overrides with `currentColor` already
      resolved.
    - Computes `override_hash` (0 when there are no overrides).
  - The base preference is converted to a CPAL palette index via
    [`select_cpal_palette`](../../src/text/color_fonts/cpal.rs) and stored on the run
    as `palette_index`.
  - The resolved overrides + hash are stored on the run as
    `palette_overrides: Arc<Vec<(u16, Rgba)>>` and `palette_override_hash`.

### During rasterization

- `TextRasterizer::render_glyph_run` passes `palette_index`, `palette_overrides`,
  and `palette_override_hash` into `ColorFontRenderer::render`.
- COLR v0/v1 apply overrides by replacing entries in the selected CPAL palette
  before resolving layer colors (see
  [`colr_v0.rs`](../../src/text/color_fonts/colr_v0.rs) and
  [`colr_v1.rs`](../../src/text/color_fonts/colr_v1.rs)).
- `palette_override_hash` is included in color-glyph cache keys (e.g.
  `ColorGlyphCacheKey` in `TextRasterizer` and `PaintCacheKey` for COLRv1 plans) so
  different override sets do not alias.

### What the display list backend uses today

- `TextItem` in the display list carries `palette_index` (see
  [`src/paint/display_list.rs`](../../src/paint/display_list.rs)) and the renderer
  passes it through when drawing text.
- Palette overrides are currently *not* recorded in display list text items:
  - `DisplayListBuilder::emit_shaped_runs` copies `run.palette_index` but drops
    `run.palette_overrides` / `run.palette_override_hash` (see
    [`src/paint/display_list_builder.rs`](../../src/paint/display_list_builder.rs)).
  - `Canvas::draw_text` therefore calls `TextRasterizer::render_glyph_run` with
    `palette_overrides = &[]` and `palette_override_hash = 0` (see
    [`src/paint/canvas.rs`](../../src/paint/canvas.rs)).
- Net effect: base palette selection (`font-palette: normal|light|dark` and
  `@font-palette-values base-palette`) affects the default renderer, but
  `@font-palette-values override-colors` is not applied end-to-end via the display
  list path.

## COLR v1 coverage and variations

COLRv1 rendering lives in
[`src/text/color_fonts/colr_v1.rs`](../../src/text/color_fonts/colr_v1.rs) and is
exercised by tests such as:

- `tests/colr_v1_color_font_test.rs`
- `tests/colr_v1_radial_gradient_test.rs`
- `tests/colr_v1_sweep_gradient_test.rs` (see also `colrv1-sweep-test.ttf` /
  `colrv1-var-sweep-test.ttf` in
  [`tests/fixtures/fonts/README.md`](../../tests/fixtures/fonts/README.md))

Implemented COLRv1 features include:

- `PaintColrLayers` / `PaintColrGlyph` / `PaintGlyph` outline painting.
- Solids (`PaintSolid`, `PaintVarSolid`).
- Gradients: linear, radial, and sweep (`PaintSweepGradient` /
  `PaintVarSweepGradient`), with PAD/REPEAT/REFLECT extends.
- Transform nodes (translate/scale/rotate/skew and around-center variants,
  including Var* forms).
- `PaintComposite` with `CompositeMode` mapped onto tiny-skia blend modes.
- Variable font support:
  - Run variations are applied to outlines (`FontInstance` /
    `apply_rustybuzz_variations`).
  - Var* gradients and `VarColorLine` stop deltas are resolved via the COLR
    `ItemVariationStore` using normalized variation coordinates.
- Encountering an unsupported/unknown COLRv1 paint record causes the COLRv1 path
  to abort so `ColorFontRenderer` can fall back to COLRv0 or monochrome.

## Transforms, opacity, and shadows

- `TextRasterizer` applies global text alpha/opacity at draw time (cached rasters
  keep `currentColor` paints opaque), so changing CSS opacity does not require
  re-rasterizing color glyphs (see `tests/color_glyph_opacity.rs`).
- `Canvas` passes stacking-context transforms, clips, opacity, and blend modes down
  to `TextRasterizer` so both outline and color-glyph rendering participate in
  those effects.
- Display-list text shadows are rendered by rasterizing the glyph run into an
  offscreen pixmap (via `TextRasterizer`) and then blurring it. For color fonts,
  this means palette/gradient layers remain colored in the shadow; the
  `text-shadow` color primarily affects `currentColor` paints.

## Limitations

- Display-list text currently does not carry `palette_overrides`, so
  `@font-palette-values override-colors` only affect callers rendering `ShapedRun`s
  directly (or callers that pass overrides explicitly).
- `sbix` supports PNG + JPEG only; other `sbix` tags are skipped (see
  [`bitmap.rs`](../../src/text/color_fonts/bitmap.rs)).
- SVG glyphs are rendered with `usvg` resources disabled and are rejected if the
  SVG contains external references (`href`/`url()`). Only fragment references and
  bounded-size `data:` URLs are allowed (see
  [`svg.rs`](../../src/text/color_fonts/svg.rs)).
- COLR v1 rendering is best-effort: malformed tables, unknown extend modes, unknown
  composite modes, or cyclic paint graphs abort color rendering for that glyph and
  fall back to COLRv0/monochrome.
