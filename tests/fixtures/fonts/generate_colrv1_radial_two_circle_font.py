from fontTools.colorLib.builder import buildCOLR, buildCPAL
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib.tables.otTables import ExtendMode, PaintFormat


def rect(x0, y0, x1, y1):
    pen = TTGlyphPen(None)
    pen.moveTo((x0, y0))
    pen.lineTo((x1, y0))
    pen.lineTo((x1, y1))
    pen.lineTo((x0, y1))
    pen.closePath()
    return pen.glyph()


upem = 1000
glyph_order = [".notdef", "grad", "rect"]
fb = FontBuilder(upem, isTTF=True)
fb.setupGlyphOrder(glyph_order)
fb.setupCharacterMap({0x0052: "grad"})

glyphs = {
    ".notdef": rect(80, -50, 920, 850),
    "grad": rect(80, -50, 920, 850),
    "rect": rect(120, 30, 880, 870),
}
metrics = {name: (1000, 0) for name in glyphs}
fb.setupGlyf(glyphs)
fb.setupHorizontalMetrics(metrics)
fb.setupHorizontalHeader(ascent=900, descent=-200)
fb.setupOS2(sTypoAscender=900, sTypoDescender=-200, usWinAscent=900, usWinDescent=200)
fb.setupNameTable(
    {
        "familyName": "COLRv1 Radial Two Circle",
        "styleName": "Regular",
        "fullName": "COLRv1 Radial Two Circle Regular",
        "uniqueFontIdentifier": "COLRv1 Radial Two Circle Regular",
        "psName": "COLRv1RadialTwoCircle-Regular",
        "version": "Version 1.0",
        "licenseDescription": "Public Domain / CC0",
        "licenseInfoURL": "https://creativecommons.org/publicdomain/zero/1.0/",
    }
)
fb.setupPost()
fb.setupMaxp()

palette = [
    (0.88, 0.18, 0.2, 1.0),
    (0.1, 0.55, 0.9, 1.0),
    (0.95, 0.85, 0.35, 1.0),
]
fb.font["CPAL"] = buildCPAL([palette])

gradient = {
    "Format": PaintFormat.PaintRadialGradient,
    "ColorLine": {
        "Extend": ExtendMode.PAD,
        "ColorStop": [
            {"StopOffset": 0.0, "PaletteIndex": 0, "Alpha": 1.0},
            {"StopOffset": 0.55, "PaletteIndex": 1, "Alpha": 1.0},
            {"StopOffset": 1.0, "PaletteIndex": 2, "Alpha": 1.0},
        ],
    },
    "x0": 260,
    "y0": 320,
    "r0": 140,
    "x1": 720,
    "y1": 420,
    "r1": 420,
}
paint = {"Format": PaintFormat.PaintGlyph, "Glyph": "rect", "Paint": gradient}
fb.font["COLR"] = buildCOLR(
    {"grad": paint}, version=1, glyphMap=fb.font.getReverseGlyphMap()
)

fb.save("tests/fixtures/fonts/colrv1-radial-two-circle.ttf")
