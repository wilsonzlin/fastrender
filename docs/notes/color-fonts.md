# Color font rendering

FastRender supports a subset of OpenType color glyph formats. This note captures the current renderers, palette handling, and limitations so callers know what to expect.

## Rendering entry points

- [`TextRasterizer`](../../src/paint/text_rasterize.rs) is the only renderer that consults [`ColorFontRenderer`](../../src/text/color_fonts.rs). `render_shaped_run` tries, in order:
  1. CBDT/CBLC or sbix bitmaps decoded as PNG (`glyph_raster_image`); other bitmap encodings are skipped.
  2. SVG-in-OT glyphs rendered with `resvg`/`usvg`, scaled from the font's units-per-em to the requested size.
  3. COLR/CPAL v0 layered outlines filled with palette colors (0xFFFF resolves to the resolved text color).

  When none of these apply, glyphs fall back to monochrome outlines tinted with the text color. Per-run rotation is applied; additional transforms, opacity, or shadow effects must be applied by the caller around the destination pixmap.

- The display list path ([`display_list_renderer`](../../src/paint/display_list_renderer.rs) → [`canvas`](../../src/paint/canvas.rs)) renders text from outlines only. Color tables are ignored there, so color fonts appear as monochrome outlines, but shadows/decorations are still derived from the shaped geometry.

## Palette handling

- Shaped runs carry a `palette_index` (`text/pipeline.rs`), but CSS `font-palette` / `@font-palette-values` are not parsed, so the index remains 0.
- [`parse_cpal_palette`](../../src/text/color_fonts.rs) reads CPAL v0/v1 color records and clamps the requested palette to the available count. Palette labels/types (e.g., light/dark) are ignored. Missing entries and 0xFFFF palette indices resolve to the text color.
- Override colors and named palettes are unsupported; CPAL v1 `paletteTypes`/`paletteLabels` are not consulted.

## Transforms, opacity, and shadows

- `TextRasterizer` composites color glyph pixmaps with SourceOver blending and optional run rotation. It does not synthesize text shadows or decorations; those must be layered by the caller if needed.
- `DisplayListRenderer` and `Canvas` apply stacking-context transforms, clips, and opacity to outline rendering and compute shadows/decorations from glyph geometry. Because color tables are ignored, those effects apply to monochrome outlines.

## Limitations

- COLR v1 paint graphs (gradients, transforms, composite modes) are not implemented; COLR v1 glyphs fall back to outline rendering.
- Palette selection is fixed to CPAL palette 0; CSS palette overrides (`font-palette`, `@font-palette-values`, `override-colors`) are not supported.
- Bitmap glyphs only decode PNG data from CBDT/CBLC or sbix; JPEG/TIFF/BMP sbix entries are skipped.
- SVG glyphs are rendered from the embedded document only; external resources and palette overrides are not loaded.
- Color glyph rendering does not apply variable-font variation settings when building color layers or bitmaps.
