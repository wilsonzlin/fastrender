#!/usr/bin/env python3
"""Generate a COLRv1 font with variable glyph outlines.

The resulting font is released into the public domain (CC0) and checked in under
`tests/fixtures/fonts/colrv1-var-outline-test.ttf`.
"""

from __future__ import annotations

from pathlib import Path
from typing import Dict, List, Tuple

from fontTools.colorLib.builder import buildCOLR, buildCPAL
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib.tables._g_v_a_r import TupleVariation
from fontTools.ttLib.tables.otTables import PaintFormat

UNITS_PER_EM = 1000
ASCENT = 900
DESCENT = -200


def polygon(points: List[Tuple[int, int]]):
  pen = TTGlyphPen(None)
  start, *rest = points
  pen.moveTo(start)
  for point in rest:
    pen.lineTo(point)
  pen.closePath()
  return pen.glyph()


def setup_basic_tables(fb: FontBuilder, glyphs: Dict[str, object]) -> None:
  metrics = {name: (UNITS_PER_EM, 0) for name in glyphs}
  fb.setupGlyf(glyphs)
  fb.setupHorizontalMetrics(metrics)
  fb.setupHorizontalHeader(ascent=ASCENT, descent=DESCENT)
  fb.setupOS2(
    sTypoAscender=ASCENT,
    sTypoDescender=DESCENT,
    usWinAscent=ASCENT,
    usWinDescent=abs(DESCENT),
    usWeightClass=400,
  )
  fb.setupPost()
  fb.setupMaxp()
  fb.setupHead()
  fb.font["head"].created = 0
  fb.font["head"].modified = 0


def build_variable_outline_font(path: Path) -> None:
  fb = FontBuilder(UNITS_PER_EM, isTTF=True)
  glyph_order = [".notdef", "color", "outline"]
  fb.setupGlyphOrder(glyph_order)
  fb.setupCharacterMap({ord("A"): "color"})

  glyphs = {
    ".notdef": polygon([(120, 0), (900, 0), (900, 800), (120, 800)]),
    "color": polygon([(180, 0), (860, 0), (880, 820), (180, 820)]),
    "outline": polygon(
      [
        (220, 0),
        (760, 0),
        (860, 260),
        (680, 900),
        (240, 900),
        (160, 420),
      ]
    ),
  }
  setup_basic_tables(fb, glyphs)
  fb.setupNameTable(
    {
      "familyName": "COLRv1 Var Outline Test",
      "styleName": "Regular",
      "fullName": "COLRv1 Var Outline Test",
      "uniqueFontIdentifier": "COLRv1 Var Outline Test",
      "psName": "COLRv1VarOutlineTest-Regular",
      "version": "Version 1.0",
      "licenseDescription": "Public Domain / CC0",
      "licenseInfoURL": "https://creativecommons.org/publicdomain/zero/1.0/",
    }
  )
  fb.setupFvar(
    axes=[("wght", 0.0, 0.0, 1.0, "Weight")],
    instances=[
      {"stylename": "Regular", "location": {"wght": 0.0}},
      {"stylename": "Bold", "location": {"wght": 1.0}},
    ],
  )

  coords, _end_pts, _flags = fb.font["glyf"]["outline"].getCoordinates(fb.font["glyf"])
  deltas = [(0, 0)] * (len(coords) + 4)  # +4 phantom points
  # Push the right edge and angled segment outwards at wght=1 while keeping the
  # left edge mostly stable.
  outline_shifts = {
    1: (100, 0),   # lower right
    2: (140, 60),  # angled segment
    3: (80, 40),   # upper right
    5: (-40, 0),   # left inset
  }
  for idx, delta in outline_shifts.items():
    deltas[idx] = delta
  fb.setupGvar(
    {
      ".notdef": [],
      "color": [],
      "outline": [TupleVariation({"wght": (0.0, 1.0, 1.0)}, deltas)],
    }
  )

  fb.font["CPAL"] = buildCPAL([[(0.15, 0.4, 0.9, 1.0)]], paletteTypes=[0])
  paint = {
    "Format": PaintFormat.PaintGlyph,
    "Glyph": "outline",
    "Paint": {"Format": PaintFormat.PaintSolid, "PaletteIndex": 0, "Alpha": 1.0},
  }
  fb.font["COLR"] = buildCOLR(
    {"color": paint}, version=1, glyphMap=fb.font.getReverseGlyphMap()
  )

  fb.save(path)
  print(f"Wrote {path}")


def main() -> None:
  base_dir = Path(__file__).parent
  build_variable_outline_font(base_dir / "colrv1-var-outline-test.ttf")


if __name__ == "__main__":
  main()
