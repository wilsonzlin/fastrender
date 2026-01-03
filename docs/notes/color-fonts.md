# Color font rendering

FastRender supports a subset of OpenType color glyph formats. This note captures the current renderers, palette handling, and limitations so callers know what to expect.

## Rendering entry points

- [`TextRasterizer`](../../src/paint/text_rasterize.rs) is the only renderer that consults [`ColorFontRenderer`](../../src/text/color_fonts/mod.rs). `render_shaped_run` tries, in order:
  1. Embedded bitmaps from sbix (PNG/JPEG) or CBDT/CBLC (`ttf_parser::Face::glyph_raster_image`). Payloads are decoded into premultiplied RGBA and oversized strikes are rejected.
  2. SVG-in-OT glyphs rendered with `resvg`/`usvg`, scaled from the font's units-per-em to the requested size (after sanitizing the embedded SVG; see limitations below).
  3. COLR v1 paint graphs (`text/color_fonts/colr_v1.rs`) with solid/gradient brushes, transforms, and composite modes. Linear/radial/sweep gradients (including Var* forms and VarColorLine stops) honor run variations; unsupported/unknown paint parameters abort COLR rendering and trigger a fallback.
  4. COLR/CPAL v0 layered outlines filled with palette colors (0xFFFF resolves to the resolved text color).

  When none of these apply, glyphs fall back to monochrome outlines tinted with the text color. `TextRasterizer` can also be called with an explicit [`TextRenderState`](../../src/paint/text_rasterize.rs) to apply additional transforms, clips, opacity, and blend modes.

- The display list path ([`display_list_renderer`](../../src/paint/display_list_renderer.rs) → [`canvas`](../../src/paint/canvas.rs)) uses `TextRasterizer` + `ColorFontRenderer` when drawing text items. This means bitmap/SVG/COLR fonts render in the main pipeline and share the same glyph/color caches as the direct `TextRasterizer` APIs.

## Palette handling

- CSS `font-palette` and `@font-palette-values` are parsed and resolved via [`style/font_palette.rs`](../../src/style/font_palette.rs); shaped runs carry a `palette_index` and `palette_overrides` ([`text/pipeline.rs`](../../src/text/pipeline.rs)) derived from the active style.
- CPAL parsing/selection lives in [`text/color_fonts/cpal.rs`](../../src/text/color_fonts/cpal.rs) (re-exported via [`text::cpal`](../../src/text/cpal.rs)). `select_cpal_palette` respects CPAL v1 `paletteTypes` bits for light/dark palettes and clamps indices to the available count. `parse_cpal_palette` returns palette colors and optional palette type metadata.
- Override colors are applied to COLR v0/v1 rendering; missing entries and palette indices of 0xFFFF resolve to the resolved text color. CPAL palette labels are not consulted yet.
- **Display list caveat:** display list [`TextItem`](../../src/paint/display_list.rs) currently stores only `palette_index`, so `@font-palette-values override-colors` are honoured when rendering `ShapedRun`s directly via `TextRasterizer` but are not yet plumbed through display-list text items.

## COLR v1 coverage and variations

- COLR v1 paints are parsed with `read_fonts` and rendered by [`text/color_fonts/colr_v1.rs`](../../src/text/color_fonts/colr_v1.rs), reusing the CPAL palette + per-run overrides and any per-glyph clip boxes to bound the raster.
- Supported paints include `PaintColrLayers`/`PaintColrGlyph`, `PaintGlyph` filled by `PaintSolid`/`PaintVarSolid`, linear/radial gradients (and their Var* counterparts) with PAD/REPEAT/REFLECT extends, transform nodes (translate/scale/rotate/skew, including around-center and Var* forms), and `PaintComposite` with blend modes mapped to tiny-skia equivalents.
- Sweep gradients (`PaintSweepGradient`/`PaintVarSweepGradient`) are implemented, including transform accumulation and PAD/REPEAT/REFLECT extends (see `tests/colr_v1_sweep_gradient_test.rs` and the `colrv1-sweep-test.ttf` / `colrv1-var-sweep-test.ttf` fixtures described in [`tests/fixtures/fonts/README.md`](../../tests/fixtures/fonts/README.md)).
- Shaped-run variations (`rustybuzz::Variation`) are applied before color rasterization: they are set on the `ttf_parser` face for outlines, and `read_fonts` resolves Var* paints/`VarColorLine` stops against the same normalized coords via the COLR `ItemVariationStore`. The `colrv1-var-test.ttf` fixtures cover gradient endpoint + stop adjustments at `wght=1`, and color glyph cache keys include variations to keep instances distinct.

## Transforms, opacity, and shadows

- `TextRasterizer` composites color glyph pixmaps with SourceOver blending and optional run rotation. It applies global text alpha/opacity at draw time (cached rasters keep `currentColor` paints opaque), so changing CSS opacity does not require re-rasterizing color glyphs (see `tests/color_glyph_opacity.rs`).
- `Canvas` passes stacking-context transforms, clips, opacity, and blend modes down to `TextRasterizer` so both outline and color-glyph rendering participate in those effects.
- Display-list text shadows are rendered by rasterizing the glyph run into an offscreen pixmap (via `TextRasterizer`) and then blurring it. For color fonts, this means palette/gradient layers remain colored in the shadow; the `text-shadow` color primarily affects `currentColor` paints.

## Limitations

- Display-list text currently does not carry `palette_overrides`, so `@font-palette-values override-colors` only affect callers rendering `ShapedRun`s directly (or callers that pass overrides explicitly).
- sbix bitmap glyphs support PNG and JPEG payloads; other sbix image tags are skipped.
- SVG-in-OT glyphs are rendered from the embedded document only; external references are rejected and large `data:` URLs are capped (see `MAX_SVG_GLYPH_*` constants in [`text/color_fonts/svg.rs`](../../src/text/color_fonts/svg.rs)).
- COLR v1 rendering is best-effort: malformed tables, unknown extend modes, unknown composite modes, or cyclic paint graphs abort color rendering for that glyph and fall back to monochrome outlines.
