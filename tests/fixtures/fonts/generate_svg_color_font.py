#!/usr/bin/env python3
"""Generate a tiny SVG-in-OpenType test font used by the color glyph tests."""

from pathlib import Path

from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib.tables.S_V_G_ import SVGDocument, table_S_V_G_


def build_font(path: Path) -> None:
    units_per_em = 1000
    ascent = 900
    descent = -200

    fb = FontBuilder(units_per_em, isTTF=True)
    glyph_order = [".notdef", "A"]
    fb.setupGlyphOrder(glyph_order)
    fb.setupCharacterMap({ord("A"): "A"})

    pen = TTGlyphPen(None)
    pen.moveTo((0, 0))
    pen.lineTo((600, 0))
    pen.lineTo((600, 700))
    pen.lineTo((0, 700))
    pen.closePath()
    notdef = pen.glyph()

    pen = TTGlyphPen(None)
    pen.moveTo((100, 0))
    pen.lineTo((900, 0))
    pen.lineTo((900, 800))
    pen.lineTo((100, 800))
    pen.closePath()
    glyph_a = pen.glyph()

    fb.setupGlyf({".notdef": notdef, "A": glyph_a})
    fb.setupHorizontalMetrics({
        ".notdef": (1000, 0),
        "A": (1000, 0),
    })
    fb.setupHorizontalHeader(ascent=ascent, descent=descent)
    fb.setupOS2(
        sTypoAscender=ascent,
        sTypoDescender=descent,
        usWinAscent=ascent,
        usWinDescent=abs(descent),
        sxHeight=500,
        sCapHeight=800,
    )
    fb.setupNameTable({
        "familyName": "SVG Color Glyph Test",
        "styleName": "Regular",
        "fullName": "SVG Color Glyph Test Regular",
        "uniqueFontIdentifier": "SVGColorGlyphTest",
        "psName": "SVGColorGlyphTest-Regular",
    })
    fb.setupHead()
    fb.setupPost()
    fb.setupMaxp()
    fb.setupDummyDSIG()

    svg_doc = (
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1000 1000">'
        '<rect x="100" y="0" width="800" height="800" fill="currentColor"/>'
        "</svg>"
    )
    svg_table = table_S_V_G_()
    svg_table.docList = [SVGDocument(svg_doc, 1, 1, False)]
    fb.font["SVG "] = svg_table

    fb.save(path)


if __name__ == "__main__":
    out_path = Path(__file__).with_name("svg-color-glyph-test.ttf")
    build_font(out_path)
    print(f"Wrote {out_path}")
