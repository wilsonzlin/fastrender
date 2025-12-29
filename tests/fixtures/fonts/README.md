## Font fixtures

This directory contains self-contained fixtures for deterministic rendering tests. The fonts
documented below are generated in-repo and released into the public domain (CC0) so they can
be safely checked in alongside their regeneration scripts. They live under `tests/fixtures/fonts`
so CI and tests can run in fully hermetic environments without relying on platform fonts.

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

### `colrv1-var-test.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1.
- **License:** CC0 / Public Domain.
- **Glyphs:** U+0041 (`A`) references a `rect` outline filled by a COLR v1 `PaintVarLinearGradient`.
- **Variation:** Single `wght` axis (0–1, default 0). A VarStore entry tied to `wght` shifts the gradient's end point down by +200 design units at `wght=1`, using VarIndexBase slots for the gradient coordinates and color stop offsets/alphas.
- **Regeneration:** From the repository root:
  ```bash
  python - <<'PY'
  from fontTools.fontBuilder import FontBuilder
  from fontTools.pens.ttGlyphPen import TTGlyphPen
  from fontTools.colorLib.builder import buildCOLR, buildCPAL
  from fontTools.ttLib.tables.otTables import PaintFormat, ExtendMode
  from fontTools.varLib import builder as varBuilder

  upem = 1000
  glyph_order = ['.notdef', 'color', 'rect']
  fb = FontBuilder(upem, isTTF=True)
  fb.setupGlyphOrder(glyph_order)
  fb.setupCharacterMap({0x0041: 'color'})

  def rect(x0, y0, x1, y1):
      pen = TTGlyphPen(None)
      pen.moveTo((x0, y0)); pen.lineTo((x1, y0)); pen.lineTo((x1, y1)); pen.lineTo((x0, y1))
      pen.closePath(); return pen.glyph()

  glyphs = {
      '.notdef': rect(100, 0, 900, 800),
      'color': rect(100, 0, 900, 800),
      'rect': rect(80, -20, 920, 840),
  }
  metrics = {name: (1000, 0) for name in glyphs}
  fb.setupGlyf(glyphs)
  fb.setupHorizontalMetrics(metrics)
  fb.setupHorizontalHeader(ascent=900, descent=-200)
  fb.setupOS2(sTypoAscender=900, sTypoDescender=-200, usWinAscent=900, usWinDescent=200)
  fb.setupNameTable({
      'familyName': 'COLRv1 Var Test',
      'styleName': 'Regular',
      'fullName': 'COLRv1 Var Test',
      'uniqueFontIdentifier': 'COLRv1 Var Test',
      'psName': 'COLRv1VarTest-Regular',
      'version': 'Version 1.0',
      'licenseDescription': 'Public Domain / CC0',
      'licenseInfoURL': 'https://creativecommons.org/publicdomain/zero/1.0/',
  })
  fb.setupPost(); fb.setupMaxp()

  fb.setupFvar(
      axes=[('wght', 0.0, 0.0, 1.0, 'Weight')],
      instances=[
          {'stylename': 'Regular', 'location': {'wght': 0.0}},
          {'stylename': 'Bold', 'location': {'wght': 1.0}},
      ],
  )

  palette = [(0.85, 0.25, 0.2, 1.0), (0.2, 0.45, 0.95, 1.0)]
  fb.font['CPAL'] = buildCPAL([palette])

  axis_tags = ['wght']
  supports = [{'wght': (0.0, 1.0, 1.0)}]
  var_region_list = varBuilder.buildVarRegionList(supports, axis_tags)
  items = [[0], [0], [0], [200], [0], [0], [0], [0], [0], [0]]
  var_data = [varBuilder.buildVarData([0], items, optimize=False)]
  var_store = varBuilder.buildVarStore(var_region_list, var_data)
  var_index_map = varBuilder.buildDeltaSetIndexMap(range(len(items)))

  gradient = {
      'Format': PaintFormat.PaintVarLinearGradient,
      'ColorLine': {'Extend': ExtendMode.PAD, 'ColorStop': [
          {'StopOffset': 0.0, 'PaletteIndex': 0, 'Alpha': 1.0, 'VarIndexBase': 6},
          {'StopOffset': 1.0, 'PaletteIndex': 1, 'Alpha': 1.0, 'VarIndexBase': 8},
      ]},
      'x0': 120, 'y0': 80, 'x1': 120, 'y1': 820, 'x2': 880, 'y2': 80,
      'VarIndexBase': 0,
  }
  paint = {'Format': PaintFormat.PaintGlyph, 'Glyph': 'rect', 'Paint': gradient}
  fb.font['COLR'] = buildCOLR(
      {'color': paint},
      version=1,
      glyphMap=fb.font.getReverseGlyphMap(),
      varStore=var_store,
      varIndexMap=var_index_map,
  )

  fb.save('tests/fixtures/fonts/colrv1-var-test.ttf')
  PY
  ```

### `colrv1-linear-shear.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1 (no upstream binary).
- **License:** CC0 / Public Domain.
- **Glyphs:** U+0047 (`G`) references a `rect` outline filled via `PaintGlyph` with a COLR v1
  `PaintLinearGradient` whose third point (`x2`/`y2`) shears the gradient axis to form a
  parallelogram mapping.
- **Features exercised:** 3-stop palette-backed linear gradient that visibly depends on the
  non-orthogonal third control point.
- **Regeneration:** From the repository root:
  ```bash
  python tests/fixtures/fonts/generate_colrv1_linear_shear_font.py
  ```

### `colrv1-radial-two-circle.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1.
- **License:** CC0 / Public Domain.
- **Glyphs:** U+0052 (`R`) fills the `rect` outline via `PaintGlyph` with a COLR v1
  `PaintRadialGradient` using two distinct circles: `(x0=260, y0=320, r0=140)` and
  `(x1=720, y1=420, r1=420)`.
- **Features exercised:** Radial gradient with a non-zero inner radius and offset centers, plus a
  3-stop palette-backed `ColorLine` to make the two-circle definition visible.
- **Regeneration:** From the repository root:
  ```bash
  python tests/fixtures/fonts/generate_colrv1_radial_two_circle_font.py
  ```

### `colrv1-var-outline-test.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1.
- **License:** CC0 / Public Domain.
- **Glyphs / variation:** U+0041 (`A`) maps to a COLR v1 `PaintGlyph` that fills the `outline`
  glyph with a solid palette color. A single `wght` axis (0–1, default 0) drives `gvar` deltas
  that push the right edge and angled segment outward while nudging the left inset, changing the
  base outline itself rather than paint parameters.
- **Regeneration:** From the repository root:
  ```bash
  python tests/fixtures/fonts/generate_colrv1_var_outline_font.py
  ```

### `colrv1-var-clip-test.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1.
- **License:** CC0 / Public Domain.
- **Glyphs / variation:** Maps `A` to a solid-filled rectangle via a COLRv1 `PaintGlyph`. A
  `ClipList` entry uses `ClipBoxFormat2` with a `varIndexBase` covering four coordinates; only
  `yMax` has a delta (+200) at `wght=1` so the clip expands vertically with weight while the paint
  and outline stay static.
- **Regeneration:** From the repository root:
  ```bash
  python tests/fixtures/fonts/generate_colrv1_var_clip_font.py
  ```

### `colrv1-gvar-test.ttf`
- **Source:** Derived in-repo from `TestVar.ttf` by adding COLR/CPAL tables with [FontTools](https://github.com/fonttools/fonttools) v4.61.1.
- **License:** Created specifically for FastRender tests; no third-party assets or licensing obligations.
- **Glyphs / variation:** Reuses `TestVar`'s single glyph `a` mapped to U+0041 (`A`). The inherited `wght`
  axis (100–900, default 400) drives `gvar` deltas that widen the outline at higher weights. COLR v1 paints
  the glyph with a solid palette color so variation effects come solely from outline changes.
- **Regeneration:** From the repository root:
  ```bash
  python - <<'PY'
  from fontTools.ttLib import TTFont
  from fontTools.colorLib.builder import buildCOLR, buildCPAL
  from fontTools.ttLib.tables.otTables import PaintFormat
  import pathlib

  base = pathlib.Path('tests/fixtures/fonts')
  font = TTFont(base / 'TestVar.ttf')
  palette = [
      (0.1, 0.65, 0.9, 1.0),
      (0.85, 0.25, 0.25, 1.0),
  ]
  font['CPAL'] = buildCPAL([palette])
  solid = {'Format': PaintFormat.PaintSolid, 'PaletteIndex': 0, 'Alpha': 1.0}
  paint = {'Format': PaintFormat.PaintGlyph, 'Glyph': 'a', 'Paint': solid}
  font['COLR'] = buildCOLR({'a': paint}, version=1, glyphMap=font.getReverseGlyphMap())
  font.save(base / 'colrv1-gvar-test.ttf')
  PY
  ```

### `colrv1-sweep-test.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1.
- **License:** CC0 / Public Domain.
- **Glyphs:** U+0047 (`G`), U+0048 (`H`), and U+0049 (`I`) all reference a shared `rect`
  outline via `PaintGlyph`, each filled with a COLR v1 `PaintSweepGradient` using a different
  `ColorLine.Extend` mode (PAD, REPEAT, REFLECT). U+004A (`J`) reuses the PAD sweep gradient but
  wraps it in a translate → scale → `PaintTransform` chain (scaleX=1.08, scaleY=0.92, dx=42,
  dy=-48, then xx=0.88, yx=0.33, xy=-0.27, yy=1.02, dx=32, dy=-36) to exercise accumulation of
  COLRv1 transforms for sweep gradients.
- **Features exercised:** Sweep gradients with a non-trivial center (500, 400), start angle
  0.125 and end angle 1.625 (half-turn units), and a 3-stop palette-backed `ColorLine` to make
  extend-mode differences obvious. CPAL v1 provides a single 3-color palette.
- **Regeneration:** From the repository root:
  ```bash
  python tests/fixtures/fonts/generate_colrv1_sweep_fonts.py
  ```

### `colrv1-var-sweep-test.ttf`
- **Source:** Generated in-repo with [FontTools](https://github.com/fonttools/fonttools) v4.61.1.
- **License:** CC0 / Public Domain.
- **Glyphs / variation:** U+0041 (`A`) references `rect` via `PaintGlyph`, filled by a
  `PaintVarSweepGradient`. A single `wght` axis (0–1, default 0) drives a VarStore delta that
  adds +0.5 to `endAngle` at `wght=1` and shifts the middle color stop's offset (+0.1) and alpha
  (-0.2), using `VarIndexBase` slots for the sweep parameters and color stop fields. CPAL v1
  carries a 3-color palette.
- **Regeneration:** Generated alongside `colrv1-sweep-test.ttf`:
  ```bash
  python tests/fixtures/fonts/generate_colrv1_sweep_fonts.py
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

### `FastRenderEmoji.ttf`
- **Source:** Hand-authored COLRv0 emoji subset generated with `fontTools` 4.61.1.
- **License:** Public Domain / CC0.
- **Glyphs:** U+1F600 (😀), U+2764 (❤), U+1F44D (👍), and U+0020 (space) drawn with simple layered shapes and a single palette to keep CI emoji renders deterministic when bundled fonts are used.

### `DejaVuSans-subset.ttf` / `DejaVuSans-subset.woff2`
- **Source:** Subset of the DejaVu Sans family bundled for deterministic text rendering during tests.
- **License:** Bitstream Vera/DejaVu fonts license.
- **Notes:** Used as the primary text face for bundled rendering.

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
- **Usage:** Exercised by `painter_applies_variable_font_variations` to ensure the painter applies variation coordinates consistently with shaping.
