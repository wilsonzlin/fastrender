#!/usr/bin/env python3
"""Generate a variable font fixture with MVAR-driven metric deltas.

The output font is released into the public domain (CC0) and checked in under
`tests/fixtures/fonts/mvar-metrics-test.ttf`.

The font defines a single `wght` axis (100–900, default 400). An `MVAR` table
contains deltas for line metrics (OS/2 typo ascender/descender/lineGap) and
text-decoration metrics (post underline position/thickness, OS/2 strikeout),
so layout and decoration placement must be variation-aware.
"""

from __future__ import annotations

from pathlib import Path

from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import newTable
from fontTools.ttLib.tables import otTables as ot
from fontTools.varLib.models import VariationModel
from fontTools.varLib import varStore

UNITS_PER_EM = 1000

# Default (wght=400) metrics. These are intentionally chosen so "line-height:
# normal" becomes obviously different under the heaviest instance.
ASCENT = 800
DESCENT = -200
LINE_GAP = 0
X_HEIGHT = 500
CAP_HEIGHT = 700

UNDERLINE_POS = -80
UNDERLINE_THICKNESS = 40
STRIKE_POS = 300
STRIKE_THICKNESS = 50


def rect(x0: int, y0: int, x1: int, y1: int):
  pen = TTGlyphPen(None)
  pen.moveTo((x0, y0))
  pen.lineTo((x1, y0))
  pen.lineTo((x1, y1))
  pen.lineTo((x0, y1))
  pen.closePath()
  return pen.glyph()


def empty_glyph():
  return TTGlyphPen(None).glyph()


def add_mvar_table(font) -> None:
  axis_tags = ["wght"]
  model = VariationModel(
    [
      {},  # default at 0.0 (wght=400)
      {"wght": -1.0},  # wght min (100)
      {"wght": 1.0},  # wght max (900)
    ],
    axisOrder=axis_tags,
  )
  store_builder = varStore.OnlineVarStoreBuilder(axis_tags)
  store_builder.setModel(model)

  records = []

  def store(tag: str, table: str, field: str, base: int, light: int, heavy: int) -> None:
    base_value, var_idx = store_builder.storeMasters([base, light, heavy], round=int)
    setattr(font[table], field, int(base_value))
    if var_idx is None or var_idx == ot.NO_VARIATION_INDEX:
      return
    rec = ot.MetricsValueRecord()
    rec.ValueTag = tag
    rec.VarIdx = var_idx
    records.append(rec)

  # Line metrics (used for `line-height: normal`).
  store("hasc", "OS/2", "sTypoAscender", ASCENT, 720, 920)
  store("hdsc", "OS/2", "sTypoDescender", DESCENT, -180, -260)
  store("hlgp", "OS/2", "sTypoLineGap", LINE_GAP, 0, 300)
  store("xhgt", "OS/2", "sxHeight", X_HEIGHT, 460, 540)
  store("cpht", "OS/2", "sCapHeight", CAP_HEIGHT, 640, 760)

  # Underline/strikeout metrics (used for `text-decoration-thickness: from-font` and
  # `text-underline-position: from-font`).
  store("undo", "post", "underlinePosition", UNDERLINE_POS, -60, -140)
  store("unds", "post", "underlineThickness", UNDERLINE_THICKNESS, 30, 70)
  store("stro", "OS/2", "yStrikeoutPosition", STRIKE_POS, 260, 360)
  store("strs", "OS/2", "yStrikeoutSize", STRIKE_THICKNESS, 40, 80)

  store_obj = store_builder.finish()
  if not records:
    return

  # Optimize and update indices to match optimized store layout.
  mapping = store_obj.optimize()
  for rec in records:
    rec.VarIdx = mapping[rec.VarIdx]

  mvar_table = font["MVAR"] = newTable("MVAR")
  mvar = mvar_table.table = ot.MVAR()
  mvar.Version = 0x00010000
  mvar.Reserved = 0
  mvar.VarStore = store_obj
  # MetricsValueRecord = Tag (4 bytes) + VarIdx (4 bytes).
  mvar.ValueRecordSize = 8
  mvar.ValueRecordCount = len(records)
  mvar.ValueRecord = sorted(records, key=lambda r: r.ValueTag)


def build_font(path: Path) -> None:
  fb = FontBuilder(UNITS_PER_EM, isTTF=True)
  glyph_order = [".notdef", "space", "A", "H"]
  fb.setupGlyphOrder(glyph_order)
  fb.setupCharacterMap({ord(" "): "space", ord("A"): "A", ord("H"): "H"})

  glyphs = {
    ".notdef": rect(100, 0, 900, 800),
    "space": empty_glyph(),
    # Simple outlines that keep the font visually legible; outline variation is
    # not needed for the MVAR metrics tests.
    "A": rect(160, 0, 840, 820),
    "H": rect(120, 0, 880, 820),
  }
  fb.setupGlyf(glyphs)

  advance = 1000
  metrics = {
    ".notdef": (advance, 0),
    "space": (advance // 2, 0),
    "A": (advance, 0),
    "H": (advance, 0),
  }
  fb.setupHorizontalMetrics(metrics)
  fb.setupHorizontalHeader(ascent=ASCENT, descent=DESCENT)
  # Keep hhea and OS/2 typo metrics in sync (see fontTools.varLib.instancer).
  fb.font["hhea"].lineGap = LINE_GAP

  fb.setupOS2(
    sTypoAscender=ASCENT,
    sTypoDescender=DESCENT,
    sTypoLineGap=LINE_GAP,
    usWinAscent=ASCENT,
    usWinDescent=abs(DESCENT),
    sxHeight=X_HEIGHT,
    sCapHeight=CAP_HEIGHT,
    yStrikeoutPosition=STRIKE_POS,
    yStrikeoutSize=STRIKE_THICKNESS,
    usWeightClass=400,
  )

  fb.setupPost()
  fb.font["post"].underlinePosition = UNDERLINE_POS
  fb.font["post"].underlineThickness = UNDERLINE_THICKNESS

  fb.setupMaxp()
  fb.setupHead()
  fb.font["head"].created = 0
  fb.font["head"].modified = 0

  fb.setupNameTable(
    {
      "familyName": "MVAR Metrics Test",
      "styleName": "Regular",
      "fullName": "MVAR Metrics Test Regular",
      "uniqueFontIdentifier": "MVAR Metrics Test Regular",
      "psName": "MVARMetricsTest-Regular",
      "version": "Version 1.0",
      "licenseDescription": "Public Domain / CC0",
      "licenseInfoURL": "https://creativecommons.org/publicdomain/zero/1.0/",
    }
  )

  fb.setupFvar(
    axes=[("wght", 100.0, 400.0, 900.0, "Weight")],
    instances=[
      {"stylename": "Thin", "location": {"wght": 100.0}},
      {"stylename": "Regular", "location": {"wght": 400.0}},
      {"stylename": "Black", "location": {"wght": 900.0}},
    ],
  )

  add_mvar_table(fb.font)
  fb.save(path)


def main() -> None:
  out = Path(__file__).resolve().parent / "mvar-metrics-test.ttf"
  build_font(out)
  print(f"Wrote {out}")


if __name__ == "__main__":
  main()

