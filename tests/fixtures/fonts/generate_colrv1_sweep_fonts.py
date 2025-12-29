#!/usr/bin/env python3
"""Generate deterministic COLRv1 sweep gradient test fonts."""

from __future__ import annotations

from pathlib import Path
from typing import Dict

from fontTools.colorLib.builder import buildCOLR, buildCPAL
from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib.tables.otTables import ExtendMode, PaintFormat
from fontTools.varLib import builder as varBuilder

UNITS_PER_EM = 1000
ASCENT = 850
DESCENT = -200


def f2dot14(value: float) -> int:
    return int(round(value * (1 << 14)))


def half_turns_to_degrees(value: float) -> float:
    """COLRv1 sweep angles are encoded as biased degrees."""
    return value * 180.0


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


def build_static_sweep_font(path: Path) -> None:
    fb = FontBuilder(UNITS_PER_EM, isTTF=True)
    glyph_order = [".notdef", "rect", "pad", "repeat", "reflect", "transform"]
    fb.setupGlyphOrder(glyph_order)
    fb.setupCharacterMap({
        ord("G"): "pad",
        ord("H"): "repeat",
        ord("I"): "reflect",
        ord("J"): "transform",
    })

    glyphs = {
        ".notdef": rect_glyph(120, 0, 880, 780),
        "rect": rect_glyph(100, -20, 900, 820),
        "pad": rect_glyph(100, 0, 900, 800),
        "repeat": rect_glyph(100, 0, 900, 800),
        "reflect": rect_glyph(100, 0, 900, 800),
        "transform": rect_glyph(100, 0, 900, 800),
    }
    setup_basic_tables(fb, glyphs)
    fb.setupNameTable({
        "familyName": "COLRv1 Sweep Test",
        "styleName": "Regular",
        "fullName": "COLRv1 Sweep Test Regular",
        "uniqueFontIdentifier": "COLRv1 Sweep Test Regular",
        "psName": "COLRv1SweepTest-Regular",
        "version": "Version 1.0",
        "licenseDescription": "Public Domain / CC0",
        "licenseInfoURL": "https://creativecommons.org/publicdomain/zero/1.0/",
    })

    palette = [
        (0.9, 0.2, 0.2, 1.0),
        (0.2, 0.75, 0.3, 1.0),
        (0.15, 0.35, 0.85, 1.0),
    ]
    fb.font["CPAL"] = buildCPAL([palette], paletteTypes=[0])

    color_line = {
        "ColorStop": [
            {"StopOffset": 0.0, "PaletteIndex": 0, "Alpha": 1.0},
            {"StopOffset": 0.45, "PaletteIndex": 1, "Alpha": 1.0},
            {"StopOffset": 1.0, "PaletteIndex": 2, "Alpha": 1.0},
        ],
    }

    def sweep_gradient(extend: ExtendMode):
        return {
            "Format": PaintFormat.PaintSweepGradient,
            "ColorLine": {**color_line, "Extend": extend},
            "centerX": 500,
            "centerY": 400,
            "startAngle": half_turns_to_degrees(0.125),
            "endAngle": half_turns_to_degrees(1.625),
        }

    def sweep_paint(extend: ExtendMode):
        return {"Format": PaintFormat.PaintGlyph, "Glyph": "rect", "Paint": sweep_gradient(extend)}

    transformed_gradient = {
        "Format": PaintFormat.PaintTransform,
        "Paint": {
            "Format": PaintFormat.PaintTranslate,
            "Paint": {
                "Format": PaintFormat.PaintScale,
                "Paint": sweep_gradient(ExtendMode.PAD),
                "scaleX": 1.08,
                "scaleY": 0.92,
            },
            "dx": 42.0,
            "dy": -48.0,
        },
        "Transform": {
            "xx": 0.88,
            "yx": 0.33,
            "xy": -0.27,
            "yy": 1.02,
            "dx": 32.0,
            "dy": -36.0,
        },
    }

    fb.font["COLR"] = buildCOLR(
        {
            "pad": sweep_paint(ExtendMode.PAD),
            "repeat": sweep_paint(ExtendMode.REPEAT),
            "reflect": sweep_paint(ExtendMode.REFLECT),
            "transform": {
                "Format": PaintFormat.PaintGlyph,
                "Glyph": "rect",
                "Paint": transformed_gradient,
            },
        },
        version=1,
        glyphMap=fb.font.getReverseGlyphMap(),
    )

    fb.save(path)


def build_variable_sweep_font(path: Path) -> None:
    fb = FontBuilder(UNITS_PER_EM, isTTF=True)
    glyph_order = [".notdef", "var_sweep", "rect"]
    fb.setupGlyphOrder(glyph_order)
    fb.setupCharacterMap({ord("A"): "var_sweep"})

    glyphs = {
        ".notdef": rect_glyph(120, 0, 880, 780),
        "var_sweep": rect_glyph(100, 0, 900, 800),
        "rect": rect_glyph(80, -40, 920, 840),
    }
    setup_basic_tables(fb, glyphs)
    fb.setupNameTable({
        "familyName": "COLRv1 Var Sweep Test",
        "styleName": "Regular",
        "fullName": "COLRv1 Var Sweep Test",
        "uniqueFontIdentifier": "COLRv1 Var Sweep Test",
        "psName": "COLRv1VarSweepTest-Regular",
        "version": "Version 1.0",
        "licenseDescription": "Public Domain / CC0",
        "licenseInfoURL": "https://creativecommons.org/publicdomain/zero/1.0/",
    })
    fb.setupFvar(
        axes=[("wght", 0.0, 0.0, 1.0, "Weight")],
        instances=[
            {"stylename": "Regular", "location": {"wght": 0.0}},
            {"stylename": "Bold", "location": {"wght": 1.0}},
        ],
    )

    palette = [
        (0.8, 0.25, 0.2, 1.0),
        (0.2, 0.65, 0.9, 1.0),
        (0.95, 0.85, 0.3, 1.0),
    ]
    fb.font["CPAL"] = buildCPAL([palette], paletteTypes=[0])

    axis_tags = ["wght"]
    supports = [{"wght": (0.0, 1.0, 1.0)}]
    var_region_list = varBuilder.buildVarRegionList(supports, axis_tags)

    deltas = [
        [0],  # centerX
        [0],  # centerY
        [0],  # startAngle
        [f2dot14(0.5)],  # endAngle +0.5 at wght=1
        [0],  # stop 0 offset
        [0],  # stop 0 alpha
        [f2dot14(0.1)],  # stop 1 offset +0.1
        [f2dot14(-0.2)],  # stop 1 alpha -0.2
        [0],  # stop 2 offset
        [0],  # stop 2 alpha
    ]
    var_data = [varBuilder.buildVarData([0], deltas, optimize=False)]
    var_store = varBuilder.buildVarStore(var_region_list, var_data)
    var_index_map = varBuilder.buildDeltaSetIndexMap(range(len(deltas)))

    sweep_gradient = {
        "Format": PaintFormat.PaintVarSweepGradient,
        "ColorLine": {
            "Extend": ExtendMode.PAD,
            "ColorStop": [
                {"StopOffset": 0.0, "PaletteIndex": 0, "Alpha": 1.0, "VarIndexBase": 4},
                {"StopOffset": 0.5, "PaletteIndex": 1, "Alpha": 1.0, "VarIndexBase": 6},
                {"StopOffset": 1.0, "PaletteIndex": 2, "Alpha": 1.0, "VarIndexBase": 8},
            ],
        },
        "centerX": 480,
        "centerY": 420,
        "startAngle": half_turns_to_degrees(0.2),
        "endAngle": half_turns_to_degrees(1.4),
        "VarIndexBase": 0,
    }

    fb.font["COLR"] = buildCOLR(
        {"var_sweep": {"Format": PaintFormat.PaintGlyph, "Glyph": "rect", "Paint": sweep_gradient}},
        version=1,
        glyphMap=fb.font.getReverseGlyphMap(),
        varStore=var_store,
        varIndexMap=var_index_map,
    )

    fb.save(path)


def main() -> None:
    base_dir = Path(__file__).parent
    static_path = base_dir / "colrv1-sweep-test.ttf"
    var_path = base_dir / "colrv1-var-sweep-test.ttf"
    build_static_sweep_font(static_path)
    build_variable_sweep_font(var_path)
    print(f"Wrote {static_path}")
    print(f"Wrote {var_path}")


if __name__ == "__main__":
    main()
