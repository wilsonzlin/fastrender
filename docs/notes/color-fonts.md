# Color font rendering

FastRender supports a subset of OpenType color glyph formats. This note captures the current renderers, palette handling, and limitations so callers know what to expect.

## Rendering entry points

- [`TextRasterizer`](../../src/paint/text_rasterize.rs) is the only renderer that consults [`ColorFontRenderer`](../../src/text/color_fonts/mod.rs). `render_shaped_run` tries, in order:
  1. CBDT/CBLC or sbix bitmaps decoded as PNG (`glyph_raster_image`); other bitmap encodings are skipped.
  2. SVG-in-OT glyphs rendered with `resvg`/`usvg`, scaled from the font's units-per-em to the requested size.
  3. COLR/CPAL v0 layered outlines filled with palette colors (0xFFFF resolves to the resolved text color).

  When none of these apply, glyphs fall back to monochrome outlines tinted with the text color. Per-run rotation is applied; additional transforms, opacity, or shadow effects must be applied by the caller around the destination pixmap.

- The display list path ([`display_list_renderer`](../../src/paint/display_list_renderer.rs) → [`canvas`](../../src/paint/canvas.rs)) now reuses `TextRasterizer` and `ColorFontRenderer` when drawing text items. Color glyph rasters are cached and shared across display-list tiles so bitmap/SVG/COLR fonts render with the same palette/currentColor behavior as the legacy path.

## Palette handling

- CSS `font-palette` and `@font-palette-values` are parsed and resolved; shaped runs carry a `palette_index` and `palette_overrides` (`text/pipeline.rs`) derived from the active style.
- CPAL parsing/selection lives in [`text/color_fonts/cpal.rs`](../../src/text/color_fonts/cpal.rs) (re-exported via [`text::cpal`](../../src/text/cpal.rs)). `select_cpal_palette` respects CPAL v1 `paletteTypes` bits for light/dark palettes and clamps indices to the available count. `parse_cpal_palette` returns palette colors and optional palette type metadata.
- Override colors are applied to COLR v0/v1 rendering; missing entries and palette indices of 0xFFFF resolve to the resolved text color. CPAL palette labels are not consulted yet.

## Transforms, opacity, and shadows

- `TextRasterizer` composites color glyph pixmaps with SourceOver blending and optional run rotation. It does not synthesize text shadows or decorations; those must be layered by the caller if needed.
- `DisplayListRenderer` and `Canvas` apply stacking-context transforms, clips, and opacity to outline rendering and compute shadows/decorations from glyph geometry. Because color tables are ignored, those effects apply to monochrome outlines.

## Limitations

- COLR v1 paint graphs (gradients, transforms, composite modes) are not implemented; COLR v1 glyphs fall back to outline rendering.
- Palette selection is fixed to CPAL palette 0; CSS palette overrides (`font-palette`, `@font-palette-values`, `override-colors`) are not supported.
- Bitmap glyphs only decode PNG data from CBDT/CBLC or sbix; JPEG/TIFF/BMP sbix entries are skipped.
- SVG glyphs are rendered from the embedded document only; external resources and palette overrides are not loaded.
- Color glyph rendering does not apply variable-font variation settings when building color layers or bitmaps.
