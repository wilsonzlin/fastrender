#!/usr/bin/env python3
"""
Generate subsetted bundled fonts (Noto families + STIX math) for text rendering.

These subsets keep glyph coverage focused on common UI ranges so bundled-font
pageset runs stay hermetic without pulling in multi-megabyte upstream files.
"""
import pathlib
import tempfile
import urllib.request

from urllib import parse

try:
  from fontTools import subset
  from fontTools.ttLib import TTFont
  from fontTools.varLib import instancer
except ImportError as exc:
  raise SystemExit(
    "Missing dependency: fontTools.\n\n"
    "Regenerate bundled font subsets by installing the pinned Python deps.\n"
    "From the repository root:\n"
    "  python3 -m venv .venv && . .venv/bin/activate\n"
    "  pip install -r tests/fixtures/fonts/requirements.txt\n"
  ) from exc


FIXTURES = pathlib.Path(__file__).parent

COMMON_OPTIONS = [
    "--glyph-names",
    "--layout-features=*",
    "--name-IDs=*",
    "--name-languages=*",
    "--notdef-glyph",
    "--notdef-outline",
    "--no-hinting",
    "--recalc-timestamp=0",
    "--drop-tables+=DSIG",
]

FONT_SOURCES = {
    "sans": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosans/NotoSans%5Bwdth,wght%5D.ttf",
    "serif": "https://raw.githubusercontent.com/google/fonts/main/ofl/notoserif/NotoSerif%5Bwdth,wght%5D.ttf",
    "mono": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosansmono/NotoSansMono%5Bwdth,wght%5D.ttf",
    "arabic": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosansarabic/NotoSansArabic%5Bwdth,wght%5D.ttf",
    "hebrew": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosanshebrew/NotoSansHebrew%5Bwdth,wght%5D.ttf",
    "devanagari": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosansdevanagari/NotoSansDevanagari%5Bwdth,wght%5D.ttf",
    "bengali": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosansbengali/NotoSansBengali%5Bwdth,wght%5D.ttf",
    "tamil": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosanstamil/NotoSansTamil%5Bwdth,wght%5D.ttf",
    "thai": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosansthai/NotoSansThai%5Bwdth,wght%5D.ttf",
    "cjk_sc": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosanssc/NotoSansSC%5Bwght%5D.ttf",
    "cjk_jp": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosansjp/NotoSansJP%5Bwght%5D.ttf",
    "cjk_kr": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosanskr/NotoSansKR%5Bwght%5D.ttf",
    "symbols": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosanssymbols2/NotoSansSymbols2-Regular.ttf",
    "symbols_arrows": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosanssymbols/NotoSansSymbols%5Bwght%5D.ttf",
    "license": "https://raw.githubusercontent.com/google/fonts/main/ofl/notosans/OFL.txt",
    "stix_math": "https://raw.githubusercontent.com/stipub/stixfonts/v2.14/archive/STIXv2.0.0/OTF/STIX2Math.otf",
    "stix_license": "https://raw.githubusercontent.com/stipub/stixfonts/v2.14/OFL.txt",
}

CJK_SHARED_UNICODES = [
    "U+0020-007E",
    "U+0300-036F",
    "U+2000-206F",
    "U+3000-303F",
    "U+3400-4DBF",
    "U+4E00-9FFF",
    "U+F900-FAFF",
    "U+FF00-FFEF",
]

CJK_JP_UNICODES = [
    *CJK_SHARED_UNICODES,
    "U+3040-30FF",
    "U+31F0-31FF",
]

CJK_KR_UNICODES = [
    *CJK_SHARED_UNICODES,
    "U+1100-11FF",
    "U+AC00-D7AF",
]

SYMBOLS_UI_TEXT = "✓✔✕✖★☆•…©®™°§€£¥☑☒☂⚠☎✈✉☕☰"
SYMBOLS_ARROW_TEXT = "←→↑↓↔↕⇔⇦⇧⇨⇩↩↪"
STIX_MATH_UNICODES = [
    "U+0000-00FF",
    "U+0100-024F",
    "U+0370-03FF",
    "U+2000-206F",
    "U+2070-209F",
    "U+20A0-20CF",
    "U+2100-214F",
    "U+2190-21FF",
    "U+2200-22FF",
    "U+2300-23FF",
    "U+2460-24FF",
    "U+25A0-25FF",
    "U+27C0-27EF",
    "U+2980-29FF",
    "U+2A00-2AFF",
    "U+1D400-1D7FF",
]

FONT_DEFS = [
    {
        "source": "sans",
        "output": "NotoSans-subset.ttf",
        "unicodes": [
            "U+0000-00FF",
            "U+0100-024F",
            "U+0250-02AF",
            "U+02B0-02FF",
            "U+0300-036F",
            "U+0370-03FF",
            "U+0400-04FF",
            "U+1D00-1D7F",
            "U+2000-206F",
        ],
    },
    {
        "source": "serif",
        "output": "NotoSerif-subset.ttf",
        "unicodes": [
            "U+0000-00FF",
            "U+0100-024F",
            "U+0250-02AF",
            "U+02B0-02FF",
            "U+0300-036F",
            "U+0370-03FF",
            "U+0400-04FF",
            "U+1D00-1D7F",
            "U+2000-206F",
        ],
    },
    {
        "source": "mono",
        "output": "NotoSansMono-subset.ttf",
        "unicodes": [
            "U+0000-00FF",
            "U+2000-206F",
            "U+2190-21FF",
            "U+25A0-25FF",
        ],
    },
    {
        "source": "arabic",
        "output": "NotoSansArabic-subset.ttf",
        "unicodes": [
            "U+0020-007E",
            "U+0300-036F",
            "U+0600-06FF",
            "U+0750-077F",
            "U+08A0-08FF",
            "U+200C-200F",
            "U+FB50-FDFF",
            "U+FE70-FEFF",
        ],
    },
    {
        "source": "hebrew",
        "output": "NotoSansHebrew-subset.ttf",
        "unicodes": [
            "U+0020-007E",
            "U+0300-036F",
            "U+0590-05FF",
            "U+2000-206F",
            "U+200C-200F",
            "U+FB1D-FB4F",
        ],
    },
    {
        "source": "devanagari",
        "output": "NotoSansDevanagari-subset.ttf",
        "unicodes": [
            "U+0020-007E",
            "U+0300-036F",
            "U+0900-097F",
            "U+1CD0-1CFF",
            "U+200C-200D",
            "U+A8E0-A8FF",
        ],
    },
    {
        "source": "bengali",
        "output": "NotoSansBengali-subset.ttf",
        "unicodes": [
            "U+0020-007E",
            "U+0300-036F",
            "U+0980-09FF",
            "U+200C-200D",
        ],
    },
    {
        "source": "tamil",
        "output": "NotoSansTamil-subset.ttf",
        "unicodes": [
            "U+0020-007E",
            "U+0300-036F",
            "U+0B80-0BFF",
            "U+2000-206F",
            "U+200C-200D",
        ],
    },
    {
        "source": "thai",
        "output": "NotoSansThai-subset.ttf",
        "unicodes": [
            "U+0020-007E",
            "U+0300-036F",
            "U+0E00-0E7F",
            "U+2000-206F",
            "U+200C-200D",
        ],
    },
    {
        "source": "cjk_sc",
        "output": "NotoSansSC-subset.ttf",
        "unicodes": CJK_SHARED_UNICODES,
        "variations": {"wght": 400},
    },
    {
        "source": "cjk_jp",
        "output": "NotoSansJP-subset.ttf",
        "unicodes": CJK_JP_UNICODES,
        "variations": {"wght": 400},
    },
    {
        "source": "cjk_kr",
        "output": "NotoSansKR-subset.ttf",
        "unicodes": CJK_KR_UNICODES,
        "variations": {"wght": 400},
    },
    {
        "source": "symbols",
        "output": "NotoSansSymbols2-subset.ttf",
        "text": SYMBOLS_UI_TEXT,
    },
    {
        "source": "symbols_arrows",
        "output": "NotoSansSymbols-subset.ttf",
        "text": SYMBOLS_ARROW_TEXT,
    },
    {
        "source": "stix_math",
        "output": "STIXTwoMath-Regular.otf",
        "unicodes": STIX_MATH_UNICODES,
    },
]


def download(url: str, dest: pathlib.Path) -> pathlib.Path:
  dest.write_bytes(urllib.request.urlopen(url).read())
  return dest


def subset_font(font_def: dict, download_dir: pathlib.Path) -> None:
  source_url = FONT_SOURCES[font_def["source"]]
  src_path = download_dir / pathlib.Path(parse.urlparse(source_url).path).name
  download(source_url, src_path)
  subset_src = src_path

  if variations := font_def.get("variations"):
    font = TTFont(src_path)
    instancer.instantiateVariableFont(font, variations, inplace=True)
    subset_src = src_path.with_suffix(".inst.ttf")
    font.save(subset_src)

  args = [
      str(subset_src),
      f"--output-file={FIXTURES / font_def['output']}",
      *COMMON_OPTIONS,
  ]

  if unicodes := font_def.get("unicodes"):
    args.append("--unicodes=" + ",".join(unicodes))
  if text := font_def.get("text"):
    args.append("--text=" + text)

  subset.main(args)


def write_licenses() -> None:
  download(FONT_SOURCES["license"], FIXTURES / "Noto-LICENSE-OFL.txt")
  download(FONT_SOURCES["stix_license"], FIXTURES / "STIXTwoMath-OFL.txt")


def main() -> None:
  with tempfile.TemporaryDirectory() as tmp:
    tmp_path = pathlib.Path(tmp)
    for font_def in FONT_DEFS:
      subset_font(font_def, tmp_path)
    write_licenses()


if __name__ == "__main__":
  main()
