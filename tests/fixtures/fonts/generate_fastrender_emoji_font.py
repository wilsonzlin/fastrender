#!/usr/bin/env python3
"""
Generate `FastRenderEmoji.ttf` deterministically.

This is a tiny COLRv0 emoji font (CC0) used for hermetic bundled-font runs.
It intentionally covers only the emoji sequences we observe in pageset runs,
keeping the binary small while avoiding slow system-font fallback paths.
"""

from __future__ import annotations

import pathlib

try:
  from fontTools.colorLib.builder import buildCOLR, buildCPAL
  from fontTools.feaLib.builder import addOpenTypeFeaturesFromString
  from fontTools.fontBuilder import FontBuilder
  from fontTools.pens.ttGlyphPen import TTGlyphPen
except ImportError as exc:
  raise SystemExit(
    "Missing dependency: fontTools.\n\n"
    "Regenerate font fixtures by installing the pinned Python deps.\n"
    "From the repository root:\n"
    "  python3 -m venv .venv && . .venv/bin/activate\n"
    "  pip install -r tests/fixtures/fonts/requirements.txt\n"
  ) from exc


FIXTURES = pathlib.Path(__file__).parent
OUTPUT = FIXTURES / "FastRenderEmoji.ttf"


def rect(pen: TTGlyphPen, x0: int, y0: int, x1: int, y1: int) -> None:
  pen.moveTo((x0, y0))
  pen.lineTo((x1, y0))
  pen.lineTo((x1, y1))
  pen.lineTo((x0, y1))
  pen.closePath()


def rect_glyph(x0: int, y0: int, x1: int, y1: int):
  pen = TTGlyphPen(None)
  rect(pen, x0, y0, x1, y1)
  return pen.glyph()


def grin_features_glyph():
  pen = TTGlyphPen(None)
  # Eyes.
  rect(pen, 300, 520, 380, 600)
  rect(pen, 620, 520, 700, 600)
  # Mouth.
  rect(pen, 360, 300, 640, 360)
  return pen.glyph()


def thumb_outline_glyph():
  pen = TTGlyphPen(None)
  # Simple border/crease.
  rect(pen, 250, 120, 760, 720)
  rect(pen, 300, 480, 720, 540)
  return pen.glyph()


def regional_indicator_u_glyph():
  pen = TTGlyphPen(None)
  rect(pen, 220, 120, 780, 720)  # background box
  # U shape cut-out is omitted; keep it simple but distinct.
  rect(pen, 320, 220, 420, 620)
  rect(pen, 580, 220, 680, 620)
  rect(pen, 420, 220, 580, 320)
  return pen.glyph()


def regional_indicator_s_glyph():
  pen = TTGlyphPen(None)
  rect(pen, 220, 120, 780, 720)  # background box
  # A blocky S.
  rect(pen, 320, 540, 680, 620)
  rect(pen, 320, 420, 420, 540)
  rect(pen, 320, 320, 680, 400)
  rect(pen, 580, 200, 680, 320)
  rect(pen, 320, 120, 680, 200)
  return pen.glyph()


def flag_stripes_glyph():
  pen = TTGlyphPen(None)
  # Three red stripes (leave implicit white stripes via the background layer).
  rect(pen, 150, 630, 850, 720)
  rect(pen, 150, 420, 850, 510)
  rect(pen, 150, 210, 850, 300)
  return pen.glyph()


def main() -> None:
  upem = 1000
  ascent = 900
  descent = -200

  glyph_order = [
    ".notdef",
    "space",
    "grin",
    "grin.layer1",
    "grin.layer2",
    "heart",
    "heart.layer1",
    "thumb",
    "thumb.layer1",
    "thumb.layer2",
    "ri_u",
    "ri_s",
    "flag_us",
    "flag_us.layer1",
    "flag_us.layer2",
    "flag_us.layer3",
  ]

  fb = FontBuilder(upem, isTTF=True)
  fb.setupGlyphOrder(glyph_order)
  fb.setupCharacterMap(
    {
      0x0020: "space",
      0x1F600: "grin",
      0x2764: "heart",
      0x1F44D: "thumb",
      0x1F1FA: "ri_u",
      0x1F1F8: "ri_s",
    }
  )

  glyphs = {
    ".notdef": rect_glyph(100, 0, 900, 800),
    "space": rect_glyph(0, 0, 0, 0),
    # 😀
    "grin": rect_glyph(0, 0, 0, 0),
    "grin.layer1": rect_glyph(150, 150, 850, 850),
    "grin.layer2": grin_features_glyph(),
    # ❤
    "heart": rect_glyph(0, 0, 0, 0),
    "heart.layer1": rect_glyph(300, 250, 700, 650),
    # 👍
    "thumb": rect_glyph(0, 0, 0, 0),
    "thumb.layer1": rect_glyph(260, 140, 740, 720),
    "thumb.layer2": thumb_outline_glyph(),
    # Regional indicators used for 🇺🇸.
    "ri_u": regional_indicator_u_glyph(),
    "ri_s": regional_indicator_s_glyph(),
    # Flag glyph reached via GSUB ligature.
    "flag_us": rect_glyph(0, 0, 0, 0),
    "flag_us.layer1": rect_glyph(150, 180, 850, 720),  # white background
    "flag_us.layer2": flag_stripes_glyph(),
    "flag_us.layer3": rect_glyph(150, 480, 500, 720),  # blue canton
  }

  advance = 1000
  metrics = {name: (advance, 0) for name in glyph_order}
  metrics["space"] = (500, 0)

  fb.setupGlyf(glyphs)
  fb.setupHorizontalMetrics(metrics)
  fb.setupHorizontalHeader(ascent=ascent, descent=descent)
  fb.setupOS2(
    sTypoAscender=ascent,
    sTypoDescender=descent,
    usWinAscent=ascent,
    usWinDescent=-descent,
  )
  fb.setupNameTable(
    {
      "familyName": "FastRender Emoji",
      "styleName": "Regular",
      "fullName": "FastRender Emoji Regular",
      "uniqueFontIdentifier": "FastRender Emoji Regular",
      "psName": "FastRenderEmoji-Regular",
      "version": "Version 1.0",
      "licenseDescription": "Public Domain / CC0",
      "licenseInfoURL": "https://creativecommons.org/publicdomain/zero/1.0/",
    }
  )
  fb.setupPost()
  fb.setupMaxp()

  # Palette indices:
  # 0 yellow, 1 black, 2 red, 3 skin, 4 white, 5 blue.
  palette = [
    (0.98, 0.86, 0.12, 1.0),
    (0.05, 0.05, 0.05, 1.0),
    (0.80, 0.12, 0.18, 1.0),
    (0.95, 0.78, 0.60, 1.0),
    (0.98, 0.98, 0.98, 1.0),
    (0.12, 0.25, 0.75, 1.0),
  ]
  fb.font["CPAL"] = buildCPAL([palette])
  fb.font["COLR"] = buildCOLR(
    {
      "grin": [("grin.layer1", 0), ("grin.layer2", 1)],
      "heart": [("heart.layer1", 2)],
      "thumb": [("thumb.layer1", 3), ("thumb.layer2", 1)],
      "flag_us": [
        ("flag_us.layer1", 4),
        ("flag_us.layer2", 2),
        ("flag_us.layer3", 5),
      ],
    },
    version=0,
    glyphMap=fb.font.getReverseGlyphMap(),
  )

  # Minimal ligature for 🇺🇸 so HarfBuzz can shape it as a single glyph.
  addOpenTypeFeaturesFromString(
    fb.font,
    """
languagesystem DFLT dflt;

feature liga {
  sub ri_u ri_s by flag_us;
} liga;
""",
  )

  # Deterministic timestamps (seconds since 1904-01-01).
  fb.font["head"].created = 0
  fb.font["head"].modified = 0

  fb.save(OUTPUT)


if __name__ == "__main__":
  main()
