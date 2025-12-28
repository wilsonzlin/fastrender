## Font fixtures

This directory contains self-contained fixtures for deterministic rendering tests. The fonts
documented below are generated in-repo and released into the public domain (CC0) so they can
be safely checked in alongside their regeneration scripts.

### `colrv1-test.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1 (no upstream binary).
- **License:** CC0 / Public Domain.
- **Glyphs:** U+0047 (`G`) defines the color glyph. Supporting glyphs `box` and `triangle`
  are referenced by the COLR graph.
- **Features exercised:** COLR v1 paint graph with a transformed linear gradient and a second
  solid overlay layer. CPAL v1 provides two palettes (light and dark) to test palette switching.
- **Regeneration:** From the repository root:
  ```bash
  python - <<'PY'
  from fontTools.fontBuilder import FontBuilder
  from fontTools.pens.ttGlyphPen import TTGlyphPen
  from fontTools.ttLib.tables.otTables import PaintFormat, ExtendMode
  from fontTools.colorLib.builder import buildCOLR, buildCPAL

  upem = 1000
  glyph_order = ['.notdef', 'colr', 'box', 'triangle']
  fb = FontBuilder(upem, isTTF=True)
  fb.setupGlyphOrder(glyph_order)
  fb.setupCharacterMap({0x0047: 'colr'})

  def rect_glyph(x0, y0, x1, y1):
      pen = TTGlyphPen(None)
      pen.moveTo((x0, y0)); pen.lineTo((x1, y0)); pen.lineTo((x1, y1)); pen.lineTo((x0, y1))
      pen.closePath()
      return pen.glyph()

  def triangle(points):
      pen = TTGlyphPen(None); pen.moveTo(points[0])
      for pt in points[1:]: pen.lineTo(pt)
      pen.closePath(); return pen.glyph()

  glyphs = {
      '.notdef': rect_glyph(100, 0, 900, 800),
      'colr': rect_glyph(100, 0, 900, 800),
      'box': rect_glyph(80, -50, 920, 850),
      'triangle': triangle([(150, 100), (850, 150), (500, 780)]),
  }
  metrics = {name: (1000, 100 if name in ('.notdef', 'colr') else 80) for name in glyphs}
  metrics['triangle'] = (1000, 150)
  fb.setupGlyf(glyphs)
  fb.setupHorizontalMetrics(metrics)
  fb.setupHorizontalHeader(ascent=850, descent=-200)
  fb.setupOS2(sTypoAscender=850, sTypoDescender=-200, usWinAscent=850, usWinDescent=200)
  fb.setupNameTable({
      'familyName': 'Test COLRv1',
      'styleName': 'Regular',
      'fullName': 'Test COLRv1 Regular',
      'uniqueFontIdentifier': 'Test COLRv1 Regular',
      'psName': 'TestCOLRv1-Regular',
      'version': 'Version 1.0',
      'licenseDescription': 'Public Domain / CC0',
      'licenseInfoURL': 'https://creativecommons.org/publicdomain/zero/1.0/',
  })
  fb.setupPost(); fb.setupMaxp()

  palette0 = [(0.95, 0.2, 0.2, 1.0), (1.0, 0.75, 0.2, 1.0), (0.15, 0.35, 0.9, 1.0)]
  palette1 = [(0.3, 0.8, 0.4, 1.0), (0.1, 0.4, 0.8, 1.0), (0.95, 0.85, 0.2, 1.0)]
  fb.font['CPAL'] = buildCPAL([palette0, palette1])

  gradient = {
      'Format': PaintFormat.PaintLinearGradient,
      'ColorLine': {'Extend': ExtendMode.PAD, 'ColorStop': [
          {'StopOffset': 0.0, 'PaletteIndex': 0, 'Alpha': 1.0},
          {'StopOffset': 1.0, 'PaletteIndex': 1, 'Alpha': 1.0},
      ]},
      'x0': 0, 'y0': 0, 'x1': 0, 'y1': 900, 'x2': 900, 'y2': 0,
  }
  paint_box = {'Format': PaintFormat.PaintTransform, 'Paint': {
      'Format': PaintFormat.PaintGlyph, 'Glyph': 'box', 'Paint': gradient,
  }, 'Transform': {'xx': 0.92, 'yx': 0.12, 'xy': -0.08, 'yy': 0.95, 'dx': 25.0, 'dy': -25.0}}
  paint_triangle = {'Format': PaintFormat.PaintGlyph, 'Glyph': 'triangle', 'Paint': {
      'Format': PaintFormat.PaintSolid, 'PaletteIndex': 2, 'Alpha': 0.95,
  }}
  fb.font['COLR'] = buildCOLR({'colr': {
      'Format': PaintFormat.PaintColrLayers, 'Layers': [paint_box, paint_triangle],
  }}, version=1, glyphMap=fb.font.getReverseGlyphMap())

  fb.save('tests/fixtures/fonts/colrv1-test.ttf')
  PY
  ```

### `PaletteTestCOLRv1.ttf`
- **Source:** Existing COLR v1 palette test fixture included with the FastRender test suite.
- **License:** Not specified in the font metadata; used only for automated tests.
- **Glyphs:** U+0041 (`A`) provides layered color data used by `tests/text/font_palette.rs` to
  exercise CPAL palette selection.

### `svg-color-glyph-test.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1
  via `tests/fixtures/fonts/generate_svg_color_font.py`.
- **License:** CC0 / Public Domain.
- **Glyphs:** U+0041 (`A`) embeds an SVG-in-OT glyph that uses `currentColor` for its fill color.
- **Regeneration:** From the repository root:
  ```bash
  python tests/fixtures/fonts/generate_svg_color_font.py
  ```

### `TestSbixJPEG.ttf`
- **Source:** Custom test font generated with FontTools/Pillow for exercising bitmap color glyphs.
- **License:** Created specifically for FastRender tests; no third-party assets or licensing obligations.
- **Glyphs:** Single sbix strike at 32ppem for the glyph `A`, embedding a small JPEG with a red background and green square.

### `DejaVuSans-subset.ttf` / `DejaVuSans-subset.woff2`
- **Source:** Subset of the DejaVu Sans family bundled for deterministic text rendering during tests.
- **License:** Bitstream Vera/DejaVu fonts license.

## Variable font fixtures

- **Font:** AmstelvarAlpha (variable)
- **Source:** <https://github.com/google/fonts/tree/main/ofl/amstelvaralpha>
- **License:** SIL Open Font License 1.1 (see upstream `OFL.txt`)
- **Subset command:**

  ```bash
  pyftsubset AmstelvarAlpha-VF.ttf \
    --output-file=VariableTestFont-AmstelvarAlpha.ttf \
    --text="HA " \
    --layout-features='*' \
    --name-IDs='*' \
    --name-languages='*' \
    --glyph-names \
    --notdef-glyph \
    --notdef-outline \
    --no-hinting
  ```

  The subset keeps the variation tables (`fvar`, `gvar`, `HVAR`, `MVAR`, etc.) so tests can assert that variable font axes (e.g. weight or stretch) affect glyph outlines.

  - Relevant axes (from `fvar`): `wght` 38–250 (default 88), `wdth` 60–402 (default 402), `opsz` 10–72 (default 14).
  - Tests exercise the extremes of the `wght` axis (40 vs 250) to ensure glyph outlines visibly change when variations are applied during rasterization.

### `TestVar.ttf`
- **Source:** Generated in-repo with `fontTools.fontBuilder` for deterministic variable font testing.
- **License:** Created specifically for FastRender tests; no third-party assets or licensing obligations.
- **Glyphs / axes:** Minimal `A` glyph with a `wght` axis (100–900, default 400) that widens at heavier weights to validate variation application.
