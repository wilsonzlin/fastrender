#!/usr/bin/env python3
"""Generate a COLRv1 font with a variable clip box."""

from pathlib import Path

from fontTools.colorLib.builder import buildCOLR, buildCPAL
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib.tables.otTables import PaintFormat
from fontTools.varLib import builder as varBuilder


def build_font(path: Path) -> None:
    units_per_em = 1000
    ascent = 900
    descent = -200

    fb = FontBuilder(units_per_em, isTTF=True)
    glyph_order = [".notdef", "rect", "clip"]
    fb.setupGlyphOrder(glyph_order)
    fb.setupCharacterMap({ord("A"): "clip"})

    pen = TTGlyphPen(None)
    pen.moveTo((0, 0))
    pen.lineTo((600, 0))
    pen.lineTo((600, 700))
    pen.lineTo((0, 700))
    pen.closePath()
    notdef = pen.glyph()

    pen = TTGlyphPen(None)
    pen.moveTo((100, 100))
    pen.lineTo((900, 100))
    pen.lineTo((900, 900))
    pen.lineTo((100, 900))
    pen.closePath()
    rect = pen.glyph()

    pen = TTGlyphPen(None)
    pen.moveTo((120, 80))
    pen.lineTo((880, 80))
    pen.lineTo((880, 920))
    pen.lineTo((120, 920))
    pen.closePath()
    clip_outline = pen.glyph()

    fb.setupGlyf({".notdef": notdef, "rect": rect, "clip": clip_outline})
    fb.setupHorizontalMetrics({
        ".notdef": (1000, 0),
        "rect": (1000, 0),
        "clip": (1000, 0),
    })
    fb.setupHorizontalHeader(ascent=ascent, descent=descent)
    fb.setupOS2(
        sTypoAscender=ascent,
        sTypoDescender=descent,
        usWinAscent=ascent,
        usWinDescent=abs(descent),
    )
    fb.setupNameTable({
        "familyName": "COLRv1 Var Clip Test",
        "styleName": "Regular",
        "fullName": "COLRv1 Var Clip Test",
        "uniqueFontIdentifier": "COLRv1 Var Clip Test",
        "psName": "COLRv1VarClipTest-Regular",
        "version": "Version 1.0",
    })
    fb.setupHead()
    fb.setupPost()
    fb.setupMaxp()
    fb.setupDummyDSIG()
    fb.setupFvar(
        axes=[("wght", 0.0, 0.0, 1.0, "Weight")],
        instances=[
            {"stylename": "Regular", "location": {"wght": 0.0}},
            {"stylename": "Bold", "location": {"wght": 1.0}},
        ],
    )

    axis_tags = ["wght"]
    supports = [{"wght": (0.0, 1.0, 1.0)}]
    var_region_list = varBuilder.buildVarRegionList(supports, axis_tags)
    deltas = [
        [0],    # xMin
        [0],    # yMin
        [0],    # xMax
        [200],  # yMax expands at wght=1
    ]
    var_data = [varBuilder.buildVarData([0], deltas, optimize=False)]
    var_store = varBuilder.buildVarStore(var_region_list, var_data)
    var_index_map = varBuilder.buildDeltaSetIndexMap(range(len(deltas)))

    fb.font["CPAL"] = buildCPAL([[(0.15, 0.55, 0.9, 1.0)]])
    paint = {
        "Format": PaintFormat.PaintGlyph,
        "Glyph": "rect",
        "Paint": {"Format": PaintFormat.PaintSolid, "PaletteIndex": 0, "Alpha": 1.0},
    }
    fb.font["COLR"] = buildCOLR(
        {"clip": paint},
        version=1,
        glyphMap=fb.font.getReverseGlyphMap(),
        varStore=var_store,
        varIndexMap=var_index_map,
        clipBoxes={"clip": (150, 150, 850, 650, 0)},
    )

    fb.save(path)


if __name__ == "__main__":
    out_path = Path(__file__).with_name("colrv1-var-clip-test.ttf")
    build_font(out_path)
    print(f"Wrote {out_path}")
