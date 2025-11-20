# Phase 8: Reference Materials - Specifications and Browser Code

**Duration:** Reference document (permanent)
**Prerequisites:** None (foundational reference)
**Dependencies:** Internet access for downloading specs
**Output:** Comprehensive reference to all authoritative sources for browser rendering

## Objectives

This document is **THE definitive reference** for where to find authoritative information when implementing FastRender:

- **Complete list of CSS specifications** - Every CSS module needed for rendering
- **Unicode standards** - Text processing algorithms (BiDi, line breaking, segmentation)
- **Browser implementations** - Where to find Servo, WebKit, and Blink/Chromium code
- **Direct download links** - URLs to specs, git repos, PDFs
- **How to read specs** - Understanding normative language and test suites
- **How to navigate browser code** - Entry points and key files
- **Books and articles** - Additional learning resources
- **Organization by topic** - Grouped for easy navigation

This is a **living document** that will be updated as specs evolve and new standards are released.

## Why This Document Exists

Browser rendering is **complex** with standards spread across:
- W3C specifications (50+ CSS modules)
- Unicode Technical Reports (15+ relevant standards)
- WHATWG HTML Living Standard
- Browser source code (millions of lines)
- Academic papers and books

**Without a central reference**, implementers:
- Waste time searching for the right spec
- Miss critical edge cases
- Implement outdated algorithms
- Duplicate effort across browser codebases

**This document solves that** by providing a **single source of truth** for all reference materials.

---

# CSS Specifications

All CSS specifications are published by the **W3C CSS Working Group**: https://www.w3.org/Style/CSS/

## How to Access CSS Specs

### Official Specs

**W3C Technical Reports:** https://www.w3.org/TR/
- Published recommendations (stable)
- Working drafts (in progress)
- Candidate recommendations (near final)

**Editor's Drafts:** https://drafts.csswg.org/
- Most up-to-date version (use for implementation)
- May change frequently
- Reflects latest working group decisions

**GitHub Repository:** https://github.com/w3c/csswg-drafts
- Source code for all specs
- Issues and discussions
- Can submit pull requests

### Recommendation: Use Editor's Drafts

Published W3C recommendations are often **years out of date**. Always use editor's drafts for implementation:

```
❌ Don't use: https://www.w3.org/TR/css-flexbox-1/
✅ Use this:  https://drafts.csswg.org/css-flexbox-1/
```

## CSS 2.1 - The Foundation

**CSS 2.1 is THE most important specification.** All modern CSS builds on CSS 2.1.

**Official Spec:** https://www.w3.org/TR/CSS21/
**Editor's Draft:** https://drafts.csswg.org/css2/
**PDF Download:** https://www.w3.org/TR/CSS21/css2.pdf

**Key Chapters:**

| Chapter | Topic | URL | Priority |
|---------|-------|-----|----------|
| 8 | Box model | https://www.w3.org/TR/CSS21/box.html | P0 |
| 9 | Visual formatting model | https://www.w3.org/TR/CSS21/visuren.html | P0 |
| 10 | Visual formatting model details | https://www.w3.org/TR/CSS21/visudet.html | P0 |
| 11 | Visual effects | https://www.w3.org/TR/CSS21/visufx.html | P0 |
| 12 | Generated content | https://www.w3.org/TR/CSS21/generate.html | P1 |
| 13 | Paged media | https://www.w3.org/TR/CSS21/page.html | P2 |
| 14 | Colors and backgrounds | https://www.w3.org/TR/CSS21/colors.html | P0 |
| 15 | Fonts | https://www.w3.org/TR/CSS21/fonts.html | P0 |
| 16 | Text | https://www.w3.org/TR/CSS21/text.html | P0 |
| 17 | Tables | https://www.w3.org/TR/CSS21/tables.html | P1 |
| 18 | User interface | https://www.w3.org/TR/CSS21/ui.html | P2 |

**Critical Sections to Master:**

- **9.3:** Positioning schemes (normal flow, float, absolute)
- **9.4:** Block formatting contexts
- **9.5:** Floats
- **10.3:** Width calculation
- **10.6:** Height calculation
- **17:** Entire table chapter (complex!)

**Test Suite:** https://test.csswg.org/suites/css2.1/

## CSS3+ Module Specifications

CSS3+ is split into **modules** that evolve independently.

### Layout Modules

#### CSS Flexbox Module Level 1

**The flexbox specification.**

- **Editor's Draft:** https://drafts.csswg.org/css-flexbox-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-flexbox-1/
- **GitHub:** https://github.com/w3c/csswg-drafts/tree/main/css-flexbox-1
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-flexbox
- **Status:** Stable (Recommendation)
- **Priority:** P0

**Key Sections:**
- Section 4: Flex containers
- Section 7: Flex item sizing (the complex part!)
- Section 9: Main axis alignment
- Appendix A: Glossary

**Implementation Note:** Use Taffy library for this - it's excellent.

#### CSS Grid Layout Module Level 1

**The grid specification.**

- **Editor's Draft:** https://drafts.csswg.org/css-grid-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-grid-1/
- **GitHub:** https://github.com/w3c/csswg-drafts/tree/main/css-grid-1
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-grid
- **Status:** Stable (Recommendation)
- **Priority:** P0

**Key Sections:**
- Section 7: Grid definition
- Section 11: Grid item sizing
- Section 12: Grid item placement
- Appendix A: Track sizing algorithm (complex!)

**Implementation Note:** Use Taffy library for this too.

#### CSS Grid Layout Module Level 2

**Grid subgrid feature.**

- **Editor's Draft:** https://drafts.csswg.org/css-grid-2/
- **W3C Status:** Working Draft
- **Priority:** P2 (advanced feature)

**New in Level 2:**
- `subgrid` value
- Masonry layout (under discussion)

#### CSS Box Alignment Module Level 3

**Alignment properties shared by flexbox, grid, and block layout.**

- **Editor's Draft:** https://drafts.csswg.org/css-align-3/
- **W3C Status:** Working Draft
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-align
- **Priority:** P0 (for flex/grid), P1 (for block)

**Properties:**
- `justify-content`, `justify-items`, `justify-self`
- `align-content`, `align-items`, `align-self`
- `gap`, `row-gap`, `column-gap`

#### CSS Table Module Level 3

**Modern table layout specification.**

- **Editor's Draft:** https://drafts.csswg.org/css-tables-3/
- **W3C Status:** Working Draft
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-tables
- **Priority:** P1

**Warning:** This spec is incomplete. CSS 2.1 Chapter 17 is more detailed for now.

**Key Sections:**
- Section 3: Table structure
- Section 4: Width calculation (critical!)
- Section 5: Height and vertical alignment

#### CSS Multi-column Layout Module Level 1

**Newspaper-style column layout.**

- **Editor's Draft:** https://drafts.csswg.org/css-multicol-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-multicol-1/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-multicol
- **Priority:** P2

**Properties:**
- `column-count`, `column-width`, `columns`
- `column-gap`, `column-rule`
- `column-span`, `column-fill`

### Text and Fonts

#### CSS Fonts Module Level 3

**Font selection and properties.**

- **Editor's Draft:** https://drafts.csswg.org/css-fonts-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-fonts-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-fonts
- **Priority:** P0

**Key Sections:**
- Section 3: Font family
- Section 4: Font styling (`font-weight`, `font-style`, `font-stretch`)
- Section 5: Font size
- Section 6: Font variant (OpenType features)

#### CSS Fonts Module Level 4

**Variable fonts and new features.**

- **Editor's Draft:** https://drafts.csswg.org/css-fonts-4/
- **W3C Status:** Working Draft
- **Priority:** P2

**New in Level 4:**
- Variable fonts (`font-variation-settings`)
- `font-palette`
- Color fonts

#### CSS Text Module Level 3

**Text properties (spacing, transform, decoration).**

- **Editor's Draft:** https://drafts.csswg.org/css-text-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-text-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-text
- **Priority:** P0

**Key Sections:**
- Section 4: White space and wrapping (`white-space`, `word-break`, `overflow-wrap`)
- Section 5: Line breaking and word boundaries
- Section 6: Alignment and justification
- Section 7: Spacing (`letter-spacing`, `word-spacing`)
- Section 8: Text transform

**Critical for:** Line breaking algorithm implementation.

#### CSS Text Module Level 4

**Advanced text features.**

- **Editor's Draft:** https://drafts.csswg.org/css-text-4/
- **W3C Status:** Working Draft
- **Priority:** P3

**New in Level 4:**
- `text-spacing`
- `word-space-transform`
- Advanced justification

#### CSS Inline Layout Module Level 3

**How inline elements are laid out.**

- **Editor's Draft:** https://drafts.csswg.org/css-inline-3/
- **W3C Status:** Working Draft
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-inline
- **Priority:** P0

**Critical sections:**
- Section 2: Inline layout model
- Section 3: Line boxes
- Section 4: Baseline alignment
- Appendix A: Line box height algorithm

**Warning:** This spec is still being written. Use CSS 2.1 Chapter 10 for now.

#### CSS Writing Modes Level 3

**Vertical text and RTL support.**

- **Editor's Draft:** https://drafts.csswg.org/css-writing-modes-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-writing-modes-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-writing-modes
- **Priority:** P2 (P0 for international apps)

**Properties:**
- `writing-mode` (horizontal-tb, vertical-rl, vertical-lr)
- `direction` (ltr, rtl)
- `text-orientation`
- `unicode-bidi`

### Visual Effects

#### CSS Backgrounds and Borders Module Level 3

**Backgrounds, borders, and shadows.**

- **Editor's Draft:** https://drafts.csswg.org/css-backgrounds-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-backgrounds-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-backgrounds
- **Priority:** P1

**Key Sections:**
- Section 3: Backgrounds (`background-color`, `background-image`, etc.)
- Section 4: Borders (`border-radius`, `border-image`)
- Section 5: Box shadows

**Critical for:** `border-radius` and `box-shadow`.

#### CSS Color Module Level 3

**Color values and properties.**

- **Editor's Draft:** https://drafts.csswg.org/css-color-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-color-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-color
- **Priority:** P0

**Color formats:**
- Named colors (red, blue, etc.)
- Hex colors (#RGB, #RRGGBB, #RRGGBBAA)
- `rgb()`, `rgba()`
- `hsl()`, `hsla()`
- `currentColor`
- `transparent`

#### CSS Color Module Level 4

**Modern color spaces.**

- **Editor's Draft:** https://drafts.csswg.org/css-color-4/
- **W3C Status:** Working Draft
- **Priority:** P3

**New in Level 4:**
- `lab()`, `lch()` (perceptual colors)
- `hwb()` (hue-whiteness-blackness)
- `color()` (arbitrary color spaces)

#### CSS Images Module Level 3

**Images and gradients.**

- **Editor's Draft:** https://drafts.csswg.org/css-images-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-images-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-images
- **Priority:** P1

**Key Sections:**
- Section 2: Image values (`url()`, gradients)
- Section 4: Image rendering (`object-fit`, `object-position`)
- Section 5: Image resolution

#### CSS Transforms Module Level 1

**2D and 3D transforms.**

- **Editor's Draft:** https://drafts.csswg.org/css-transforms-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-transforms-1/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-transforms
- **Priority:** P1

**Transform functions:**
- `translate()`, `translateX()`, `translateY()`, `translate3d()`
- `rotate()`, `rotateX()`, `rotateY()`, `rotateZ()`, `rotate3d()`
- `scale()`, `scaleX()`, `scaleY()`, `scale3d()`
- `skew()`, `skewX()`, `skewY()`
- `matrix()`, `matrix3d()`

#### CSS Transforms Module Level 2

**Individual transform properties.**

- **Editor's Draft:** https://drafts.csswg.org/css-transforms-2/
- **W3C Status:** Working Draft
- **Priority:** P2

**New in Level 2:**
- `translate`, `rotate`, `scale` (individual properties)
- `transform-origin` improvements

#### Filter Effects Module Level 1

**Image filters and effects.**

- **Editor's Draft:** https://drafts.fxtf.org/filter-effects-1/
- **W3C Recommendation:** https://www.w3.org/TR/filter-effects-1/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/filter-effects
- **Priority:** P2

**Filter functions:**
- `blur()`, `brightness()`, `contrast()`
- `grayscale()`, `hue-rotate()`, `invert()`
- `opacity()`, `saturate()`, `sepia()`
- `drop-shadow()`

#### Compositing and Blending Level 1

**How elements blend together.**

- **Editor's Draft:** https://drafts.fxtf.org/compositing-1/
- **W3C Recommendation:** https://www.w3.org/TR/compositing-1/
- **Priority:** P2

**Properties:**
- `opacity`
- `background-blend-mode`
- `mix-blend-mode`

### Values and Units

#### CSS Values and Units Module Level 3

**All CSS value types and units.**

- **Editor's Draft:** https://drafts.csswg.org/css-values-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-values-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-values
- **Priority:** P0

**Key Sections:**
- Section 4: Combinators (value syntax)
- Section 5: Numeric types
- Section 6: Lengths (px, em, rem, vh, vw, etc.)
- Section 7: Percentages
- Section 8: URLs
- Section 9: Angles (deg, rad, grad, turn)
- Section 11: `calc()`

**Critical for:** Unit conversion and `calc()` implementation.

#### CSS Values and Units Module Level 4

**New functions and units.**

- **Editor's Draft:** https://drafts.csswg.org/css-values-4/
- **W3C Status:** Working Draft
- **Priority:** P2

**New in Level 4:**
- `min()`, `max()`, `clamp()`
- Container query units (cqw, cqh, etc.)
- Trigonometric functions (`sin()`, `cos()`, `tan()`)

### Custom Properties and Cascade

#### CSS Custom Properties for Cascading Variables Module Level 1

**CSS variables.**

- **Editor's Draft:** https://drafts.csswg.org/css-variables-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-variables-1/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-variables
- **Priority:** P1

**Key Sections:**
- Section 2: Defining custom properties (`--*`)
- Section 3: Using variables (`var()`)
- Section 4: Invalid values and fallbacks
- Section 5: Cycle detection

#### CSS Cascading and Inheritance Level 3

**How CSS cascade works.**

- **Editor's Draft:** https://drafts.csswg.org/css-cascade-3/
- **W3C Recommendation:** https://www.w3.org/TR/css-cascade-3/
- **Priority:** P0

**Key Sections:**
- Section 3: Value processing (specified → computed → used → actual)
- Section 6: Cascade sorting order
- Section 7: Inheritance

#### CSS Cascading and Inheritance Level 4

**New cascade features.**

- **Editor's Draft:** https://drafts.csswg.org/css-cascade-4/
- **W3C Status:** Working Draft
- **Priority:** P2

**New in Level 4:**
- `revert` keyword
- Scoped styles

#### CSS Cascading and Inheritance Level 5

**Cascade layers.**

- **Editor's Draft:** https://drafts.csswg.org/css-cascade-5/
- **W3C Status:** Working Draft
- **Priority:** P2

**New in Level 5:**
- `@layer` rule
- Layer ordering

### Selectors

#### Selectors Level 3

**CSS selectors.**

- **Editor's Draft:** https://drafts.csswg.org/selectors-3/
- **W3C Recommendation:** https://www.w3.org/TR/selectors-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/selectors
- **Priority:** P0

**Selector types:**
- Type selectors (`div`)
- Class selectors (`.class`)
- ID selectors (`#id`)
- Attribute selectors (`[attr]`, `[attr=value]`)
- Pseudo-classes (`:hover`, `:nth-child()`, etc.)
- Pseudo-elements (`::before`, `::after`)

#### Selectors Level 4

**New selectors.**

- **Editor's Draft:** https://drafts.csswg.org/selectors-4/
- **W3C Status:** Working Draft
- **Priority:** P2

**New in Level 4:**
- `:is()`, `:where()`, `:has()`
- `:not()` with multiple selectors

### Pseudo-elements and Generated Content

#### CSS Pseudo-Elements Module Level 4

**Pseudo-elements like ::before and ::after.**

- **Editor's Draft:** https://drafts.csswg.org/css-pseudo-4/
- **W3C Status:** Working Draft
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-pseudo
- **Priority:** P1

**Pseudo-elements:**
- `::before`, `::after`
- `::first-line`, `::first-letter`
- `::marker`
- `::selection`
- `::placeholder`

#### CSS Generated Content Module Level 3

**The `content` property.**

- **Editor's Draft:** https://drafts.csswg.org/css-content-3/
- **W3C Status:** Working Draft
- **Priority:** P1

**Content values:**
- Strings
- `attr()`
- `counter()`, `counters()`
- Images

### Responsive Design

#### CSS Media Queries Level 3

**Media queries for responsive design.**

- **Editor's Draft:** https://drafts.csswg.org/mediaqueries-3/
- **W3C Recommendation:** https://www.w3.org/TR/mediaqueries-3/
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/mediaqueries
- **Priority:** P1

**Media features:**
- `width`, `height`, `min-width`, `max-width`
- `orientation` (portrait, landscape)
- `resolution`
- `color`, `color-index`, `monochrome`

#### CSS Media Queries Level 4

**New media features.**

- **Editor's Draft:** https://drafts.csswg.org/mediaqueries-4/
- **W3C Status:** Working Draft
- **Priority:** P1

**New in Level 4:**
- `hover`, `pointer` (interaction capabilities)
- Range syntax (`width > 600px`)

#### CSS Media Queries Level 5

**User preference queries.**

- **Editor's Draft:** https://drafts.csswg.org/mediaqueries-5/
- **W3C Status:** Working Draft
- **Priority:** P2

**New in Level 5:**
- `prefers-color-scheme` (dark mode)
- `prefers-reduced-motion`
- `prefers-contrast`

#### CSS Containment Module Level 1

**Performance optimization.**

- **Editor's Draft:** https://drafts.csswg.org/css-contain-1/
- **W3C Status:** Working Draft
- **Priority:** P2

**Properties:**
- `contain` (size, layout, paint, style)

#### CSS Containment Module Level 3

**Container queries.**

- **Editor's Draft:** https://drafts.csswg.org/css-contain-3/
- **W3C Status:** Working Draft
- **Priority:** P3 (cutting edge)

**New in Level 3:**
- `@container` rule
- `container-type`, `container-name`
- Container query units (cqw, cqh, etc.)

### Overflow and Scrolling

#### CSS Overflow Module Level 3

**Overflow behavior.**

- **Editor's Draft:** https://drafts.csswg.org/css-overflow-3/
- **W3C Status:** Working Draft
- **Priority:** P1

**Properties:**
- `overflow`, `overflow-x`, `overflow-y`
- `overflow-clip-margin`

#### CSS Scroll Snap Module Level 1

**Scroll snapping.**

- **Editor's Draft:** https://drafts.csswg.org/css-scroll-snap-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-scroll-snap-1/
- **Priority:** P3 (for static rendering)

### Other Important Specs

#### CSS Positioned Layout Module Level 3

**Absolute and fixed positioning.**

- **Editor's Draft:** https://drafts.csswg.org/css-position-3/
- **W3C Status:** Working Draft
- **Test Suite:** https://github.com/web-platform-tests/wpt/tree/master/css/css-position
- **Priority:** P0

**Position values:**
- `static`, `relative`, `absolute`, `fixed`, `sticky`

#### CSS Display Module Level 3

**The `display` property.**

- **Editor's Draft:** https://drafts.csswg.org/css-display-3/
- **W3C Status:** Working Draft
- **Priority:** P0

**Display values:**
- Outer: `block`, `inline`
- Inner: `flow`, `flow-root`, `flex`, `grid`, `table`
- Box: `none`, `contents`

#### CSS Box Sizing Module Level 3

**The `box-sizing` property.**

- **Editor's Draft:** https://drafts.csswg.org/css-sizing-3/
- **W3C Status:** Working Draft
- **Priority:** P0

**Properties:**
- `box-sizing` (content-box, border-box)
- `width`, `height`, `min-width`, `max-width`, etc.

#### CSS Fragmentation Module Level 3

**Page and column breaks.**

- **Editor's Draft:** https://drafts.csswg.org/css-break-3/
- **W3C Status:** Working Draft
- **Priority:** P2

**Properties:**
- `break-before`, `break-after`, `break-inside`

#### CSS Masking Module Level 1

**Clipping and masking.**

- **Editor's Draft:** https://drafts.fxtf.org/css-masking-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-masking-1/
- **Priority:** P2

**Properties:**
- `clip-path`
- `mask`, `mask-image`, `mask-mode`, etc.

#### CSS Shapes Module Level 1

**Text wrapping around shapes.**

- **Editor's Draft:** https://drafts.csswg.org/css-shapes-1/
- **W3C Recommendation:** https://www.w3.org/TR/css-shapes-1/
- **Priority:** P3

**Properties:**
- `shape-outside`, `shape-margin`, `shape-image-threshold`

---

# Unicode Standards

Unicode provides critical algorithms for text processing.

**Official Site:** https://unicode.org/
**Standards:** https://unicode.org/reports/

## Unicode Standard Core

**The Unicode Standard (Version 15.0):**
- **PDF:** https://www.unicode.org/versions/Unicode15.0.0/
- **Online:** https://www.unicode.org/versions/Unicode15.0.0/UnicodeStandard-15.0.pdf
- **Size:** ~1000 pages
- **Priority:** P0 (reference, not to read cover-to-cover)

**Key Chapters:**
- Chapter 2: General Structure (encoding, code points)
- Chapter 3: Conformance (normalization)
- Chapter 4: Character Properties
- Chapter 23: Special Areas and Format Characters

## Unicode Technical Reports (UAX)

### UAX #9: Unicode Bidirectional Algorithm

**Bidirectional text (Arabic, Hebrew).**

- **URL:** https://www.unicode.org/reports/tr9/
- **PDF:** https://www.unicode.org/reports/tr9/tr9-46.pdf
- **Version:** 46 (Unicode 15.0)
- **Priority:** P2 (P0 for international apps)

**Key Sections:**
- Section 3: Bidirectional Character Types
- Section 4: Directional Formatting Codes
- Section X1-X10: Bidirectional Algorithm (the complex part!)

**Implementation:**
- Use `unicode-bidi` Rust crate: https://crates.io/crates/unicode-bidi
- Don't implement from scratch (very complex)

**Test Data:** https://www.unicode.org/Public/UCD/latest/ucd/BidiTest.txt

### UAX #11: East Asian Width

**Character width properties.**

- **URL:** https://www.unicode.org/reports/tr11/
- **Priority:** P2

**Width categories:**
- Narrow (halfwidth)
- Wide (fullwidth)
- Ambiguous (context-dependent)

### UAX #14: Unicode Line Breaking Algorithm

**Where lines can break.**

- **URL:** https://www.unicode.org/reports/tr14/
- **PDF:** https://www.unicode.org/reports/tr14/tr14-51.pdf
- **Version:** 51 (Unicode 15.0)
- **Priority:** P0

**Key Sections:**
- Section 4: Line Breaking Classes
- Section 5: Tailoring (language-specific rules)
- Section 6: Line Breaking Algorithm
- Section 7: Pair Table (break opportunities)

**Implementation:**
- Use `unicode-linebreak` Rust crate: https://crates.io/crates/unicode-linebreak
- Or `xi-unicode`: https://crates.io/crates/xi-unicode

**Test Data:** https://www.unicode.org/Public/UCD/latest/ucd/LineBreakTest.txt

**Critical for:** Text wrapping in inline layout.

### UAX #15: Unicode Normalization Forms

**Text normalization (NFC, NFD, NFKC, NFKD).**

- **URL:** https://www.unicode.org/reports/tr15/
- **Priority:** P1

**Normalization forms:**
- NFC: Canonical composition
- NFD: Canonical decomposition
- NFKC: Compatibility composition
- NFKD: Compatibility decomposition

**Implementation:**
- Use `unicode-normalization` crate: https://crates.io/crates/unicode-normalization

### UAX #24: Unicode Script Property

**Script detection (Latin, Cyrillic, Han, etc.).**

- **URL:** https://www.unicode.org/reports/tr24/
- **Priority:** P1

**Use for:** Font fallback, shaping runs

**Implementation:**
- Use `unicode-script` crate: https://crates.io/crates/unicode-script

### UAX #29: Unicode Text Segmentation

**Word, sentence, and grapheme boundaries.**

- **URL:** https://www.unicode.org/reports/tr29/
- **PDF:** https://www.unicode.org/reports/tr29/tr29-43.pdf
- **Priority:** P1

**Segmentation types:**
- Grapheme clusters (user-perceived characters)
- Word boundaries
- Sentence boundaries

**Implementation:**
- Use `unicode-segmentation` crate: https://crates.io/crates/unicode-segmentation

**Critical for:** Cursor movement, text selection, word-wrap

### UAX #38: Unicode Han Database

**CJK character properties.**

- **URL:** https://www.unicode.org/reports/tr38/
- **Priority:** P2 (for CJK support)

### UAX #44: Unicode Character Database

**Structure of UCD data files.**

- **URL:** https://www.unicode.org/reports/tr44/
- **Priority:** P1 (reference)

**Describes:**
- Format of UnicodeData.txt
- Property definitions
- Data file formats

### UAX #50: Unicode Vertical Text Layout

**Vertical text orientation.**

- **URL:** https://www.unicode.org/reports/tr50/
- **Priority:** P2

**Critical for:** East Asian vertical text.

## Unicode Technical Standards (UTS)

### UTS #10: Unicode Collation Algorithm

**String sorting and comparison.**

- **URL:** https://www.unicode.org/reports/tr10/
- **Priority:** P3 (not needed for rendering)

### UTS #18: Unicode Regular Expressions

**Regex for Unicode.**

- **URL:** https://www.unicode.org/reports/tr18/
- **Priority:** P3

### UTS #51: Unicode Emoji

**Emoji properties and sequences.**

- **URL:** https://www.unicode.org/reports/tr51/
- **Priority:** P2

**Emoji data:**
- Emoji sequences (👨‍👩‍👧‍👦 is multiple code points)
- Emoji ZWJ sequences
- Emoji modifiers (skin tone)

**Test Data:** https://unicode.org/Public/emoji/

## Unicode Character Database (UCD)

**Download all Unicode data files:**

- **Latest:** https://www.unicode.org/Public/UCD/latest/
- **All versions:** https://www.unicode.org/Public/

**Key files:**
- `UnicodeData.txt` - Character properties
- `LineBreak.txt` - Line breaking classes
- `EastAsianWidth.txt` - Character widths
- `Scripts.txt` - Script assignments
- `emoji-data.txt` - Emoji properties

**Rust access:** Use crates that bundle UCD data:
- `unicode-bidi`
- `unicode-linebreak`
- `unicode-segmentation`
- `unicode-script`

---

# Browser Implementations to Study

Studying real browser code is essential for understanding edge cases.

## Servo (Rust)

**The best reference for Rust-based rendering.**

- **GitHub:** https://github.com/servo/servo
- **Documentation:** https://servo.org/
- **Priority:** P0 (same language as FastRender!)

### Key Directories

**Layout Engine:**
- **Path:** `components/layout_2020/`
- **URL:** https://github.com/servo/servo/tree/main/components/layout_2020
- **What to study:** Modern layout implementation (2020 rewrite)

**Key files:**
- `components/layout_2020/flow/mod.rs` - Formatting contexts
- `components/layout_2020/flow/inline.rs` - Inline layout
- `components/layout_2020/flow/flex.rs` - Flexbox
- `components/layout_2020/sizing.rs` - Box sizing
- `components/layout_2020/fragment_tree/mod.rs` - Fragment tree

**Style System:**
- **Path:** `components/style/`
- **URL:** https://github.com/servo/servo/tree/main/components/style

**Key files:**
- `components/style/selector_matching.rs` - Selector matching
- `components/style/properties/mod.rs` - CSS properties
- `components/style/values/computed/` - Computed values

**Text:**
- **Path:** `components/gfx/`
- **URL:** https://github.com/servo/servo/tree/main/components/gfx

**Fonts:**
- **Path:** `components/gfx/font.rs`
- **URL:** https://github.com/servo/servo/blob/main/components/gfx/font.rs

**Layout 2013 (older, simpler):**
- **Path:** `components/layout/`
- **URL:** https://github.com/servo/servo/tree/main/components/layout
- **Note:** Deprecated but simpler to understand

### Servo Resources

- **Architecture docs:** https://github.com/servo/servo/wiki
- **Rendering overview:** https://github.com/servo/servo/wiki/Rendering
- **Style system:** https://github.com/servo/servo/wiki/Style-system
- **Layout 2020 design:** https://github.com/servo/servo/wiki/Layout-2020

## WebKit (C++)

**Apple's browser engine (Safari).**

- **GitHub:** https://github.com/WebKit/WebKit
- **Browsable:** https://github.com/WebKit/WebKit/tree/main/Source/WebCore
- **Priority:** P1 (production-quality, well-documented)

### Key Directories

**Layout:**
- **Path:** `Source/WebCore/layout/`
- **URL:** https://github.com/WebKit/WebKit/tree/main/Source/WebCore/layout

**Modern layout (2019+):**
- **Path:** `Source/WebCore/layout/formattingContexts/`
- **URL:** https://github.com/WebKit/WebKit/tree/main/Source/WebCore/layout/formattingContexts

**Key files:**
- `BlockFormattingContext.cpp` - Block layout
- `InlineFormattingContext.cpp` - Inline layout
- `FlexFormattingContext.cpp` - Flexbox
- `TableFormattingContext.cpp` - Table layout

**Rendering:**
- **Path:** `Source/WebCore/rendering/`
- **URL:** https://github.com/WebKit/WebKit/tree/main/Source/WebCore/rendering

**Key classes:**
- `RenderBox.cpp` - Box model
- `RenderBlock.cpp` - Block elements
- `RenderInline.cpp` - Inline elements
- `RenderTable.cpp` - Tables
- `RenderFlexibleBox.cpp` - Flexbox

**Style:**
- **Path:** `Source/WebCore/style/`
- **URL:** https://github.com/WebKit/WebKit/tree/main/Source/WebCore/style

**Text:**
- **Path:** `Source/WebCore/platform/graphics/`
- **Complex text:** `ComplexTextController.cpp`

### WebKit Resources

- **WebKit blog:** https://webkit.org/blog/
- **Layout posts:** Search for "layout" and "rendering"
- **Code overview:** https://trac.webkit.org/wiki/WebCore

## Blink/Chromium (C++)

**Google's browser engine (Chrome, Edge).**

- **Git:** https://chromium.googlesource.com/chromium/src/
- **GitHub mirror:** https://github.com/chromium/chromium (read-only)
- **Priority:** P1 (most market share)

### Key Directories

**Layout:**
- **Path:** `third_party/blink/renderer/core/layout/`
- **URL:** https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/layout/

**LayoutNG (modern layout, 2016+):**
- **Path:** `third_party/blink/renderer/core/layout/ng/`
- **URL:** https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/layout/ng/

**Key files:**
- `ng_block_layout_algorithm.cc` - Block layout
- `ng_inline_layout_algorithm.cc` - Inline layout
- `ng_flex_layout_algorithm.cc` - Flexbox
- `ng_grid_layout_algorithm.cc` - Grid
- `ng_table_layout_algorithm.cc` - Table

**Style:**
- **Path:** `third_party/blink/renderer/core/css/`
- **URL:** https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/css/

**Paint:**
- **Path:** `third_party/blink/renderer/core/paint/`
- **URL:** https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/paint/

**Text:**
- **Path:** `third_party/blink/renderer/platform/fonts/`
- **Shaping:** `shaping/` subdirectory

### Blink/Chromium Resources

- **Design docs:** https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/layout/README.md
- **LayoutNG design:** https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/layout/ng/README.md
- **Chromium dev docs:** https://chromium.googlesource.com/chromium/src/+/main/docs/
- **Rendering pipeline:** https://chromium.googlesource.com/chromium/src/+/main/docs/how_cc_works.md

### Accessing Chromium Code

**Option 1: GitHub mirror (easiest)**
```bash
# Browse online
https://github.com/chromium/chromium

# Or clone (warning: huge!)
git clone https://github.com/chromium/chromium.git
```

**Option 2: Chromium source (official)**
```bash
# Requires depot_tools
git clone https://chromium.googlesource.com/chromium/tools/depot_tools.git
export PATH="$PATH:/path/to/depot_tools"

# Fetch code (warning: 30+ GB!)
fetch chromium
```

**Option 3: Code search (no download)**
- **URL:** https://source.chromium.org/chromium/chromium/src
- **Best for:** Quick lookups without downloading

## Firefox/Gecko (C++)

**Mozilla's browser engine.**

- **GitHub:** https://github.com/mozilla/gecko-dev (mirror)
- **Official:** https://hg.mozilla.org/mozilla-central/
- **Priority:** P2

### Key Directories

**Layout:**
- **Path:** `layout/generic/`
- **URL:** https://github.com/mozilla/gecko-dev/tree/master/layout/generic

**Key files:**
- `nsBlockFrame.cpp` - Block layout
- `nsInlineFrame.cpp` - Inline layout
- `nsFlexContainerFrame.cpp` - Flexbox
- `nsGridContainerFrame.cpp` - Grid
- `nsTableFrame.cpp` - Table

**Style:**
- **Path:** `servo/components/style/` (uses Servo's style system!)
- **URL:** https://github.com/mozilla/gecko-dev/tree/master/servo/components/style

**Text:**
- **Path:** `gfx/thebes/`

### Firefox Resources

- **Platform docs:** https://firefox-source-docs.mozilla.org/
- **Layout docs:** https://firefox-source-docs.mozilla.org/layout/

---

# How to Read CSS Specifications

CSS specs have specific conventions and terminology.

## Specification Terminology

### Normative Language (RFC 2119)

Specs use specific keywords with defined meanings:

- **MUST / REQUIRED / SHALL** - Absolute requirement
- **MUST NOT / SHALL NOT** - Absolute prohibition
- **SHOULD / RECOMMENDED** - Should do unless good reason not to
- **SHOULD NOT / NOT RECOMMENDED** - Should not do unless good reason
- **MAY / OPTIONAL** - Truly optional

**Example:**
> "User agents MUST support the `width` property."
>
> This means FastRender **must** implement `width` to be spec-compliant.

### Value Definition Syntax

CSS specs use a specific grammar for property values:

**Combinators:**
- `A B` - A followed by B (in order)
- `A | B` - A or B (exclusive)
- `A || B` - A or B or both (any order)
- `A && B` - A and B (any order)
- `[ A B ]` - Grouping

**Multipliers:**
- `A*` - Zero or more A
- `A+` - One or more A
- `A?` - Zero or one A (optional)
- `A{2}` - Exactly 2 A
- `A{2,4}` - 2 to 4 A
- `A#` - One or more A, comma-separated
- `A#?` - Zero or more A, comma-separated

**Example:**
```
margin: [ <length> | <percentage> | auto ]{1,4}
```
Meaning: 1 to 4 values, each being length OR percentage OR auto.

**Reference:** https://drafts.csswg.org/css-values-3/#value-defs

## Anatomy of a CSS Spec

### 1. Abstract

Quick summary of what the spec defines.

### 2. Status of This Document

Current state (recommendation, working draft, etc.).

### 3. Introduction

Background and scope.

### 4. Definitions and Examples

Core concepts with examples.

**Focus here** - This is the normative part.

### 5. Algorithms

Step-by-step procedures.

**Critical** - Implement these exactly.

### 6. Appendices

Non-normative information (often helpful examples).

### 7. References

Normative and informative references to other specs.

### 8. Changes

What's new since last version.

## Reading Strategy

### For New Specs

1. **Read abstract and introduction** - Understand scope
2. **Skim examples** - Get intuition
3. **Read definitions carefully** - Understand model
4. **Study algorithms** - How to implement
5. **Check references** - What other specs are needed
6. **Find test suite** - What to test

### For Implementation

1. **Print or PDF the spec** - Easier to read deeply
2. **Highlight normative sections** - What MUST be implemented
3. **Note edge cases** - Examples often show these
4. **Cross-reference browser code** - See how they handled it
5. **Write tests first** - From spec examples
6. **Implement** - Following algorithms exactly
7. **Test** - Against WPT and spec examples

### Example: Reading Flexbox Spec

**Goal:** Implement flexbox layout

**Steps:**
1. Read introduction (Section 1) - Understand flexbox model
2. Study flex container (Section 4) - How flex containers work
3. **Critical:** Read flex item sizing (Section 7) - Complex algorithm!
4. Study alignment (Section 9) - How items align
5. Read algorithms in order:
   - Section 9.2: Line sizing
   - Section 9.3: Main axis alignment
   - Section 9.4: Cross axis alignment
6. Check examples - Edge cases
7. Compare with Taffy source code - See how they did it
8. Write tests from spec examples
9. Implement step-by-step

## Common Spec Patterns

### Computed Value Definition

Many properties define how values are computed:

```
Computed value: as specified, with lengths made absolute
```

Means: Convert relative lengths (em, %) to pixels.

### Inheritance

```
Inherited: yes
```

Means: Child elements inherit this property from parent.

### Initial Value

```
Initial value: auto
```

Means: If not specified, value is `auto`.

### Applies To

```
Applies to: block containers
```

Means: Property only affects block containers, ignore on others.

## Spec Reading Tools

### CSS Spec Preprocessor

Specs are written in Bikeshed markup.

- **GitHub:** https://github.com/tabatkins/bikeshed
- **Allows:** Local building of specs

### Spec Links

Many specs have internal links. Click them to jump to definitions.

### Test Suites

Every major spec has a test suite.

- **WPT:** https://github.com/web-platform-tests/wpt
- **CSS WG tests:** https://github.com/w3c/csswg-test

**Always check tests** - They clarify ambiguous spec text.

---

# How to Navigate Browser Code

Browser codebases are massive (millions of lines). Here's how to find what you need.

## General Strategy

### 1. Start with Layout/Rendering

Don't start with DOM or parsing. Start where layout happens.

**Servo:** `components/layout_2020/`
**WebKit:** `Source/WebCore/layout/`
**Chromium:** `third_party/blink/renderer/core/layout/ng/`

### 2. Find Formatting Context Implementations

Look for:
- Block layout algorithm
- Inline layout algorithm
- Flex layout algorithm
- Grid layout algorithm
- Table layout algorithm

**Example (Chromium):**
- `ng_block_layout_algorithm.cc`
- `ng_inline_layout_algorithm.cc`
- `ng_flex_layout_algorithm.cc`

### 3. Study One Algorithm Deeply

Pick block layout (simplest) and understand it completely:
- How boxes are sized
- How children are positioned
- How margin collapse works
- Edge cases

### 4. Then Study Inline Layout

Inline is the most complex:
- Line breaking
- Baseline alignment
- Mixed inline/block
- Text runs

### 5. Follow Data Structures

Understand key structures:
- Box/element representation
- Fragment representation
- Computed style storage
- Layout constraints

## Chromium LayoutNG Entry Points

**Start here for understanding modern Chromium layout:**

### Main Layout Algorithm

**File:** `ng_block_layout_algorithm.cc`
**Function:** `NGBlockLayoutAlgorithm::Layout()`

**What it does:**
1. Compute inline size
2. Lay out children
3. Compute block size
4. Handle overflow

**URL:** https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/layout/ng/ng_block_layout_algorithm.cc

### Inline Layout

**File:** `ng_inline_layout_algorithm.cc`
**Function:** `NGInlineLayoutAlgorithm::Layout()`

**URL:** https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/layout/ng/inline/ng_inline_layout_algorithm.cc

### Flex Layout

**File:** `ng_flex_layout_algorithm.cc`

**URL:** https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/layout/ng/flex/ng_flex_layout_algorithm.cc

### Grid Layout

**File:** `ng_grid_layout_algorithm.cc`

**URL:** https://source.chromium.org/chromium/chromium/src/+/main:third_party/blink/renderer/core/layout/ng/grid/ng_grid_layout_algorithm.cc

## WebKit Entry Points

**Modern WebKit layout:**

### Block Layout

**File:** `BlockFormattingContext.cpp`
**Class:** `Layout::BlockFormattingContext`

**URL:** https://github.com/WebKit/WebKit/blob/main/Source/WebCore/layout/formattingContexts/block/BlockFormattingContext.cpp

### Inline Layout

**File:** `InlineFormattingContext.cpp`

**URL:** https://github.com/WebKit/WebKit/blob/main/Source/WebCore/layout/formattingContexts/inline/InlineFormattingContext.cpp

### Flex Layout

**File:** `FlexFormattingContext.cpp`

**URL:** https://github.com/WebKit/WebKit/blob/main/Source/WebCore/layout/formattingContexts/flex/FlexFormattingContext.cpp

## Servo Entry Points

**Servo Layout 2020:**

### Flow Layout

**File:** `flow/mod.rs`
**Struct:** `BlockFormattingContext`

**URL:** https://github.com/servo/servo/blob/main/components/layout_2020/flow/mod.rs

### Inline Layout

**File:** `flow/inline.rs`

**URL:** https://github.com/servo/servo/blob/main/components/layout_2020/flow/inline.rs

### Flex Layout

**File:** `flow/flex.rs`

**URL:** https://github.com/servo/servo/blob/main/components/layout_2020/flow/flex.rs

## Code Search Tips

### Search by Spec Section

Specs often reference code:
```
"9.4.2 Inline formatting contexts"
```

Search for "9.4.2" or "Inline formatting context" in code comments.

### Search by Property Name

```cpp
// Find width calculation
git grep "ComputeWidth" --include="*.cc"

// Find margin collapse
git grep "CollapseMargins" --include="*.cc"
```

### Search by Test Name

WPT tests often have clear names:
```
css/css-flexbox/flexbox-align-baseline-001.html
```

Search for "align-baseline" in code to find implementation.

### Use Code Search Tools

**Chromium:** https://source.chromium.org/
**WebKit:** GitHub search
**Servo:** GitHub search

## Understanding Browser Architecture

### Typical Pipeline

```
HTML → DOM → Style → Layout → Paint → Composite → Display

FastRender pipeline:
HTML → DOM → Box Tree → Fragment Tree → Display List → Rasterize
```

### Where to Focus

**For layout engine:**
- Focus on: Style → Layout → Paint
- Ignore: Network, JavaScript, DOM APIs

**For each browser:**
- Servo: Best Rust reference
- WebKit: Clean C++, good comments
- Chromium: Most production testing, LayoutNG is modern
- Firefox: Uses Servo's style system

---

# Books and Articles

## Books

### "Web Browser Engineering" by Pavel Panchekha and Chris Harrelson

**The best introductory book on browser rendering.**

- **Website:** https://browser.engineering/
- **Free online:** Yes
- **GitHub:** https://github.com/browserengineering/book
- **Priority:** P0 - Read this first!

**Covers:**
- HTML parsing
- CSS parsing and cascade
- Layout (block, inline, text)
- Rendering
- JavaScript integration

**Pros:** Hands-on, builds a browser from scratch
**Cons:** Simplified (doesn't cover all edge cases)

### "CSS: The Definitive Guide" by Eric Meyer and Estelle Weyl

**Comprehensive CSS reference.**

- **Publisher:** O'Reilly
- **Edition:** 5th (2023)
- **Priority:** P1

**Covers:**
- All CSS properties
- Visual formatting model
- Selectors
- Cascade and inheritance

### "Fonts & Encodings" by Yannis Haralambous

**Deep dive into font technology.**

- **Publisher:** O'Reilly
- **Priority:** P2

**Covers:**
- Font formats (TrueType, OpenType)
- Text encoding
- International typography

## Articles and Papers

### "How Browsers Work" by Tali Garsiel

**Classic article on browser rendering.**

- **URL:** https://www.html5rocks.com/en/tutorials/internals/howbrowserswork/
- **Priority:** P0

**Covers:**
- Browser architecture
- Parsing
- Rendering tree
- Layout
- Painting

### LayoutNG Design Docs (Chromium)

**Modern layout architecture.**

- **Main doc:** https://chromium.googlesource.com/chromium/src/+/main/third_party/blink/renderer/core/layout/ng/README.md
- **Priority:** P1

**Topics:**
- Box tree vs fragment tree
- Constraint spaces
- Formatting contexts
- Paint fragments

### WebKit Layout Documents

**WebKit's layout architecture.**

- **Modern layout:** https://github.com/WebKit/WebKit/blob/main/Source/WebCore/layout/README.md
- **Priority:** P1

### "Line Breaking" by Unicode Consortium

**Line breaking algorithm explanation.**

- **URL:** https://www.unicode.org/reports/tr14/
- **Priority:** P0 for text

### "Bidirectional Algorithm" by Unicode Consortium

**BiDi algorithm explanation.**

- **URL:** https://www.unicode.org/reports/tr9/
- **Priority:** P2

### Academic Papers

#### "Fitting Text to Boxes"

**Text layout in constrained spaces.**

- Research on word breaking and hyphenation

#### "SILE Typesetting System"

**Modern typesetting.**

- **Website:** https://sile-typesetter.org/
- **GitHub:** https://github.com/sile-typesetter/sile

## Blogs to Follow

### WebKit Blog

- **URL:** https://webkit.org/blog/
- **Topics:** Layout, rendering, CSS features

### Chromium Blog

- **URL:** https://blog.chromium.org/
- **Topics:** New features, architecture

### Mozilla Hacks

- **URL:** https://hacks.mozilla.org/
- **Topics:** Web platform features

### CSS Working Group Blog

- **URL:** https://www.w3.org/blog/CSS/
- **Topics:** New specs, discussions

---

# Organization Structure

This section provides different ways to organize and access the references above.

## By Implementation Phase

### Phase 1: Foundation (Current)

**Must read:**
- CSS 2.1 Chapters 8, 9, 10 (box model, visual formatting)
- "How Browsers Work" article
- Servo layout_2020/ overview

**Browser code:**
- Servo: `components/layout_2020/`
- Chromium: LayoutNG README

### Phase 2: Layout Algorithms

**Block layout:**
- CSS 2.1 Chapter 9 (normal flow)
- Chromium: `ng_block_layout_algorithm.cc`
- WebKit: `BlockFormattingContext.cpp`

**Inline layout:**
- CSS 2.1 Chapter 10 (inline formatting)
- CSS Inline Module Level 3
- UAX #14 (Line Breaking)
- Chromium: `ng_inline_layout_algorithm.cc`

**Flexbox:**
- CSS Flexbox Level 1
- Chromium: `ng_flex_layout_algorithm.cc`

**Grid:**
- CSS Grid Level 1
- Chromium: `ng_grid_layout_algorithm.cc`

**Table:**
- CSS 2.1 Chapter 17
- CSS Tables Level 3
- Chromium: `ng_table_layout_algorithm.cc`

### Phase 3: Text Rendering

**Font system:**
- CSS Fonts Level 3
- "Fonts & Encodings" book
- HarfBuzz documentation

**Text shaping:**
- HarfBuzz tutorial: https://harfbuzz.github.io/
- UAX #24 (Script)
- OpenType spec: https://learn.microsoft.com/en-us/typography/opentype/spec/

**Line breaking:**
- UAX #14 (Line Breaking)
- CSS Text Level 3
- unicode-linebreak crate docs

**BiDi:**
- UAX #9 (Bidirectional Algorithm)
- unicode-bidi crate docs

### Phase 4: Painting

**Display list:**
- Chromium: `third_party/blink/renderer/core/paint/`
- WebKit: `Source/WebCore/rendering/`

**Backgrounds and borders:**
- CSS Backgrounds and Borders Level 3
- tiny-skia documentation

**Text rendering:**
- Font rasterization with freetype
- rustybuzz for shaping

## By Topic

### Box Model
- CSS 2.1 Chapter 8
- CSS Box Sizing Level 3
- Chromium: `ng_box_fragment.cc`

### Positioning
- CSS 2.1 Chapter 9.3
- CSS Positioned Layout Level 3
- Chromium: `ng_absolute_utils.cc`

### Floats
- CSS 2.1 Chapter 9.5
- Chromium: `ng_float_utils.cc`

### Margin Collapse
- CSS 2.1 Section 8.3.1
- CSS Box Model Level 3
- Chromium: `ng_margin_strut.cc`

### Text Properties
- CSS Text Level 3
- CSS Fonts Level 3
- UAX #14, #29

### Colors
- CSS Color Level 3/4
- Color space conversion algorithms

### Cascade and Inheritance
- CSS Cascade Level 3/4/5
- Servo: `components/style/`

### Custom Properties
- CSS Variables Level 1
- Chromium: CSS variable resolution code

## By Difficulty

### Beginner (Start Here)

1. "Web Browser Engineering" book (chapters 1-5)
2. "How Browsers Work" article
3. CSS 2.1 Chapter 8 (Box Model)
4. Simple block layout example

### Intermediate

1. CSS 2.1 Chapter 9-10 (Visual formatting)
2. CSS Flexbox spec
3. Servo layout_2020 code
4. Chromium LayoutNG README

### Advanced

1. CSS Inline Level 3
2. CSS Tables
3. UAX #14 (Line Breaking)
4. UAX #9 (BiDi)
5. Full browser layout code

### Expert

1. Margin collapse algorithm
2. Table auto-layout algorithm
3. Complex text shaping
4. Performance optimization

---

# Quick Reference Cards

## Essential Specs Checklist

- [ ] CSS 2.1 (all chapters)
- [ ] CSS Flexbox Level 1
- [ ] CSS Grid Level 1
- [ ] CSS Box Alignment Level 3
- [ ] CSS Fonts Level 3
- [ ] CSS Text Level 3
- [ ] CSS Inline Level 3
- [ ] CSS Backgrounds and Borders Level 3
- [ ] CSS Values and Units Level 3
- [ ] CSS Variables Level 1
- [ ] UAX #14 (Line Breaking)
- [ ] UAX #9 (BiDi)
- [ ] UAX #29 (Text Segmentation)

## Essential Browser Code to Study

- [ ] Servo: `components/layout_2020/flow/mod.rs`
- [ ] Servo: `components/layout_2020/flow/inline.rs`
- [ ] Chromium: `ng_block_layout_algorithm.cc`
- [ ] Chromium: `ng_inline_layout_algorithm.cc`
- [ ] WebKit: `BlockFormattingContext.cpp`
- [ ] WebKit: `InlineFormattingContext.cpp`

## Essential Tools

- [ ] WPT test suite (https://github.com/web-platform-tests/wpt)
- [ ] CSS spec preprocessor (Bikeshed)
- [ ] Browser code search (source.chromium.org, GitHub)
- [ ] Unicode data files (UCD)

## Essential Crates

- [ ] `taffy` - Flexbox/Grid layout
- [ ] `rustybuzz` - Text shaping (HarfBuzz)
- [ ] `unicode-bidi` - BiDi algorithm
- [ ] `unicode-linebreak` - Line breaking
- [ ] `unicode-segmentation` - Text segmentation
- [ ] `fontdb` - Font discovery
- [ ] `ttf-parser` - Font parsing
- [ ] `tiny-skia` - Rasterization

---

# Downloading and Organizing References

## Directory Structure

Recommended local organization:

```
~/fastrender-refs/
├── specs/
│   ├── css21.pdf
│   ├── css-flexbox-1.pdf
│   ├── css-grid-1.pdf
│   ├── css-fonts-3.pdf
│   ├── css-text-3.pdf
│   ├── css-inline-3.pdf
│   ├── uax9-bidi.pdf
│   ├── uax14-linebreak.pdf
│   └── uax29-segmentation.pdf
├── browser-code/
│   ├── servo/
│   ├── webkit/
│   └── chromium/ (or links to online)
├── books/
│   ├── web-browser-engineering.pdf
│   └── css-definitive-guide.pdf
└── articles/
    ├── how-browsers-work.pdf
    └── layoutng-design.pdf
```

## Download Scripts

### Download CSS Specs

```bash
#!/bin/bash
# download-specs.sh

mkdir -p ~/fastrender-refs/specs
cd ~/fastrender-refs/specs

# CSS 2.1
wget https://www.w3.org/TR/CSS21/css2.pdf -O css21.pdf

# Flexbox
wget https://www.w3.org/TR/css-flexbox-1/ -O css-flexbox-1.html
# Convert to PDF with browser print

# Grid
wget https://www.w3.org/TR/css-grid-1/ -O css-grid-1.html

# Fonts
wget https://www.w3.org/TR/css-fonts-3/ -O css-fonts-3.html

# Text
wget https://www.w3.org/TR/css-text-3/ -O css-text-3.html

# Unicode specs
wget https://www.unicode.org/reports/tr9/tr9-46.pdf -O uax9-bidi.pdf
wget https://www.unicode.org/reports/tr14/tr14-51.pdf -O uax14-linebreak.pdf
wget https://www.unicode.org/reports/tr29/tr29-43.pdf -O uax29-segmentation.pdf
```

### Clone Browser Source

```bash
#!/bin/bash
# clone-browsers.sh

mkdir -p ~/fastrender-refs/browser-code
cd ~/fastrender-refs/browser-code

# Servo (recommended - Rust!)
git clone https://github.com/servo/servo.git

# WebKit
git clone https://github.com/WebKit/WebKit.git

# Chromium (huge - use online instead)
# https://source.chromium.org/
```

---

# Updates and Maintenance

## Spec Update Frequency

**CSS Specs:**
- Editor's drafts: Updated weekly (follow on GitHub)
- W3C Recommendations: Updated yearly

**Unicode:**
- New version annually (usually September)
- Subscribe to: https://www.unicode.org/announcements/

## Tracking Changes

### CSS Working Group

**GitHub:** https://github.com/w3c/csswg-drafts
**Watch:** Click "Watch" → "Custom" → "Releases"

**Mailing list:** https://lists.w3.org/Archives/Public/www-style/

### Unicode

**Announcements:** https://www.unicode.org/announcements/
**Mailing list:** https://www.unicode.org/consortium/mailinglists.html

### Browser Changes

**Servo:**
- GitHub: https://github.com/servo/servo/commits/main
- Blog: https://servo.org/blog/

**Chromium:**
- Blink-dev: https://groups.google.com/a/chromium.org/g/blink-dev

**WebKit:**
- Blog: https://webkit.org/blog/

## Document Maintenance

**This document should be updated:**
- When new CSS modules are published
- When new Unicode versions release
- When browser architecture changes significantly
- When new learning resources become available

**Review schedule:** Quarterly

**Owner:** FastRender maintainers

---

# Frequently Asked Questions

## Q: Which specs should I read first?

**A:** Start with:
1. CSS 2.1 Chapters 8, 9, 10
2. CSS Flexbox Level 1
3. CSS Fonts Level 3
4. UAX #14 (Line Breaking)

## Q: Should I read W3C Recommendations or Editor's Drafts?

**A:** **Always use Editor's Drafts** for implementation. W3C Recommendations are often years out of date.

## Q: Which browser code is best to study?

**A:**
- **Servo** for Rust implementation (same language)
- **Chromium LayoutNG** for modern architecture
- **WebKit** for clean, well-commented code

## Q: Do I need to implement every CSS property?

**A:** No. See `05-css-features-roadmap.md` for priority levels. Focus on P0 and P1 first.

## Q: Where can I find test cases?

**A:** Web Platform Tests (WPT): https://github.com/web-platform-tests/wpt

## Q: How do I know if my implementation is correct?

**A:**
1. Run WPT tests
2. Compare with browser output
3. Test spec examples
4. Visual regression tests

## Q: What if the spec is ambiguous?

**A:**
1. Check browser implementations
2. Ask on CSS Working Group GitHub issues
3. Check WPT tests
4. When in doubt, do what Chromium does

## Q: Should I implement deprecated features?

**A:** No. Skip deprecated properties (e.g., `clip` vs `clip-path`).

---

# Acknowledgments

This document references work from:
- W3C CSS Working Group
- Unicode Consortium
- Servo project
- Chromium/Blink project
- WebKit project
- Mozilla Firefox/Gecko project

All specifications are copyright their respective organizations.

---

# Appendix: Complete Spec URLs

## CSS Specifications (Alphabetical)

| Spec | Editor's Draft | Status |
|------|----------------|--------|
| CSS 2.1 | https://drafts.csswg.org/css2/ | Rec |
| CSS Backgrounds and Borders 3 | https://drafts.csswg.org/css-backgrounds-3/ | CR |
| CSS Box Alignment 3 | https://drafts.csswg.org/css-align-3/ | WD |
| CSS Box Sizing 3 | https://drafts.csswg.org/css-sizing-3/ | WD |
| CSS Cascade 3 | https://drafts.csswg.org/css-cascade-3/ | CR |
| CSS Cascade 4 | https://drafts.csswg.org/css-cascade-4/ | CR |
| CSS Cascade 5 | https://drafts.csswg.org/css-cascade-5/ | WD |
| CSS Color 3 | https://drafts.csswg.org/css-color-3/ | Rec |
| CSS Color 4 | https://drafts.csswg.org/css-color-4/ | WD |
| CSS Compositing 1 | https://drafts.fxtf.org/compositing-1/ | CR |
| CSS Conditional 3 | https://drafts.csswg.org/css-conditional-3/ | CR |
| CSS Contain 1 | https://drafts.csswg.org/css-contain-1/ | Rec |
| CSS Contain 3 | https://drafts.csswg.org/css-contain-3/ | WD |
| CSS Content 3 | https://drafts.csswg.org/css-content-3/ | WD |
| CSS Display 3 | https://drafts.csswg.org/css-display-3/ | CR |
| CSS Flexbox 1 | https://drafts.csswg.org/css-flexbox-1/ | CR |
| CSS Fonts 3 | https://drafts.csswg.org/css-fonts-3/ | Rec |
| CSS Fonts 4 | https://drafts.csswg.org/css-fonts-4/ | WD |
| CSS Fragmentation 3 | https://drafts.csswg.org/css-break-3/ | CR |
| CSS Generated Content 3 | https://drafts.csswg.org/css-content-3/ | WD |
| CSS Grid 1 | https://drafts.csswg.org/css-grid-1/ | CR |
| CSS Grid 2 | https://drafts.csswg.org/css-grid-2/ | CR |
| CSS Images 3 | https://drafts.csswg.org/css-images-3/ | CR |
| CSS Images 4 | https://drafts.csswg.org/css-images-4/ | WD |
| CSS Inline 3 | https://drafts.csswg.org/css-inline-3/ | WD |
| CSS Masking 1 | https://drafts.fxtf.org/css-masking-1/ | CR |
| CSS Media Queries 3 | https://drafts.csswg.org/mediaqueries-3/ | Rec |
| CSS Media Queries 4 | https://drafts.csswg.org/mediaqueries-4/ | CR |
| CSS Media Queries 5 | https://drafts.csswg.org/mediaqueries-5/ | WD |
| CSS Multicol 1 | https://drafts.csswg.org/css-multicol-1/ | WD |
| CSS Overflow 3 | https://drafts.csswg.org/css-overflow-3/ | WD |
| CSS Position 3 | https://drafts.csswg.org/css-position-3/ | WD |
| CSS Pseudo-Elements 4 | https://drafts.csswg.org/css-pseudo-4/ | WD |
| CSS Scroll Snap 1 | https://drafts.csswg.org/css-scroll-snap-1/ | CR |
| CSS Selectors 3 | https://drafts.csswg.org/selectors-3/ | Rec |
| CSS Selectors 4 | https://drafts.csswg.org/selectors-4/ | WD |
| CSS Shapes 1 | https://drafts.csswg.org/css-shapes-1/ | CR |
| CSS Tables 3 | https://drafts.csswg.org/css-tables-3/ | WD |
| CSS Text 3 | https://drafts.csswg.org/css-text-3/ | CR |
| CSS Text 4 | https://drafts.csswg.org/css-text-4/ | WD |
| CSS Transforms 1 | https://drafts.csswg.org/css-transforms-1/ | CR |
| CSS Transforms 2 | https://drafts.csswg.org/css-transforms-2/ | WD |
| CSS Transitions 1 | https://drafts.csswg.org/css-transitions-1/ | WD |
| CSS UI 3 | https://drafts.csswg.org/css-ui-3/ | Rec |
| CSS UI 4 | https://drafts.csswg.org/css-ui-4/ | WD |
| CSS Values 3 | https://drafts.csswg.org/css-values-3/ | CR |
| CSS Values 4 | https://drafts.csswg.org/css-values-4/ | WD |
| CSS Variables 1 | https://drafts.csswg.org/css-variables-1/ | CR |
| CSS Writing Modes 3 | https://drafts.csswg.org/css-writing-modes-3/ | Rec |
| CSS Writing Modes 4 | https://drafts.csswg.org/css-writing-modes-4/ | CR |
| Filter Effects 1 | https://drafts.fxtf.org/filter-effects-1/ | WD |

**Legend:**
- Rec = W3C Recommendation (stable)
- CR = Candidate Recommendation (near stable)
- WD = Working Draft (in progress)

## Unicode Technical Reports

| Report | Title | URL |
|--------|-------|-----|
| UAX #9 | Bidirectional Algorithm | https://www.unicode.org/reports/tr9/ |
| UAX #11 | East Asian Width | https://www.unicode.org/reports/tr11/ |
| UAX #14 | Line Breaking | https://www.unicode.org/reports/tr14/ |
| UAX #15 | Normalization Forms | https://www.unicode.org/reports/tr15/ |
| UAX #24 | Script Property | https://www.unicode.org/reports/tr24/ |
| UAX #29 | Text Segmentation | https://www.unicode.org/reports/tr29/ |
| UAX #38 | Han Database | https://www.unicode.org/reports/tr38/ |
| UAX #44 | Character Database | https://www.unicode.org/reports/tr44/ |
| UAX #50 | Vertical Text | https://www.unicode.org/reports/tr50/ |
| UTS #10 | Collation Algorithm | https://www.unicode.org/reports/tr10/ |
| UTS #18 | Regular Expressions | https://www.unicode.org/reports/tr18/ |
| UTS #51 | Emoji | https://www.unicode.org/reports/tr51/ |

---

**Last Updated:** 2025-11-19
**Status:** Living Document - Updated Quarterly
**Maintained By:** FastRender Team
**Next Review:** 2026-02-19
**Version:** 1.0

**Contributions:** If you find a broken link or want to add a resource, please submit a pull request!
