#!/usr/bin/env python3
"""Generate a COLRv1 linear gradient fixture with a sheared 3-point mapping."""

from __future__ import annotations

from pathlib import Path
from typing import Dict

from fontTools.colorLib.builder import buildCOLR, buildCPAL
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib.tables.otTables import ExtendMode, PaintFormat

UNITS_PER_EM = 1000
ASCENT = 900
DESCENT = -200


def rect_glyph(x0: int, y0: int, x1: int, y1: int):
    pen = TTGlyphPen(None)
    pen.moveTo((x0, y0))
    pen.lineTo((x1, y0))
    pen.lineTo((x1, y1))
    pen.lineTo((x0, y1))
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


def build_linear_shear_font(path: Path) -> None:
    fb = FontBuilder(UNITS_PER_EM, isTTF=True)
    glyph_order = [".notdef", "shear", "rect"]
    fb.setupGlyphOrder(glyph_order)
    fb.setupCharacterMap({ord("G"): "shear"})

    glyphs = {
        ".notdef": rect_glyph(80, 0, 920, 850),
        "shear": rect_glyph(100, 0, 900, 800),
        "rect": rect_glyph(120, 80, 880, 820),
    }
    setup_basic_tables(fb, glyphs)
    fb.setupNameTable(
        {
            "familyName": "COLRv1 Linear Shear",
            "styleName": "Regular",
            "fullName": "COLRv1 Linear Shear Regular",
            "uniqueFontIdentifier": "COLRv1 Linear Shear Regular",
            "psName": "COLRv1LinearShear-Regular",
            "version": "Version 1.0",
            "licenseDescription": "Public Domain / CC0",
            "licenseInfoURL": "https://creativecommons.org/publicdomain/zero/1.0/",
        }
    )

    palette = [
        (0.2, 0.55, 0.95, 1.0),
        (0.9, 0.35, 0.25, 1.0),
        (0.95, 0.88, 0.25, 1.0),
    ]
    fb.font["CPAL"] = buildCPAL([palette], paletteTypes=[0])

    gradient = {
        "Format": PaintFormat.PaintLinearGradient,
        "ColorLine": {
            "Extend": ExtendMode.PAD,
            "ColorStop": [
                {"StopOffset": 0.0, "PaletteIndex": 0, "Alpha": 1.0},
                {"StopOffset": 0.5, "PaletteIndex": 1, "Alpha": 1.0},
                {"StopOffset": 1.0, "PaletteIndex": 2, "Alpha": 1.0},
            ],
        },
        "x0": 120,
        "y0": 120,
        "x1": 880,
        "y1": 120,
        "x2": 280,
        "y2": 900,
    }
    paint = {"Format": PaintFormat.PaintGlyph, "Glyph": "rect", "Paint": gradient}
    fb.font["COLR"] = buildCOLR(
        {"shear": paint},
        version=1,
        glyphMap=fb.font.getReverseGlyphMap(),
    )

    path.parent.mkdir(parents=True, exist_ok=True)
    fb.save(path)


if __name__ == "__main__":
    build_linear_shear_font(Path(__file__).with_name("colrv1-linear-shear.ttf"))
