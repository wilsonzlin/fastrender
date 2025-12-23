# CSS 2.1 Visual Formatting Model (research notes)
**Primary sources:**
- CSS 2.1 Chapter 9 (Visual Formatting Model)
- CSS 2.1 Chapter 10 (Visual Formatting Model Details)
- CSS 2.1 Chapter 8 Section 8.3.1 (Margin Collapsing)

---

## Executive Summary

This document contains comprehensive research notes on the CSS 2.1 Visual Formatting Model, which defines how user agents (browsers) process document trees for visual media. This research provides the foundational knowledge required to implement all layout algorithms in FastRender V2.

### What the Visual Formatting Model Covers

The Visual Formatting Model is the complete specification for how CSS boxes are generated, positioned, and sized. It encompasses:

1. **Box Generation (9.2)** - How HTML elements create CSS boxes with different display types
2. **Positioning Schemes (9.3)** - Three fundamental layout modes: normal flow, floats, and absolute positioning
3. **Normal Flow Layout (9.4)** - Block and inline formatting contexts, the default layout modes
4. **Float Layout (9.5)** - How floated elements are positioned and how content flows around them
5. **Absolute Positioning (9.6)** - Positioning boxes relative to containing blocks
6. **Width Computation (10.3)** - Precise algorithms for calculating box widths in all scenarios
7. **Height Computation (10.5-10.6)** - Algorithms for calculating box heights
8. **Line Height (10.8)** - Inline box height and baseline alignment calculations
9. **Margin Collapsing (8.3.1)** - Complex rules for when adjacent margins merge

### Why CSS 2.1 vs Newer Specs

CSS 2.1 was deliberately chosen as the foundation because:
- It's stable, complete, and fully specified (unlike evolving CSS3 modules)
- All modern browsers implement it as their baseline
- Newer layout modes (flexbox, grid) are built on top of these fundamentals
- It defines "normal flow" which is the default for all HTML content
- The algorithms are precise and testable

### Critical Insight: Box Tree ≠ Fragment Tree

One of the key architectural insights from the spec is the distinction between:
- **Box Generation (what boxes exist)** - determined by HTML + CSS
- **Box Layout (where boxes go)** - determined by formatting contexts

This naturally leads to a two-tree architecture:
- **Box Tree** - immutable representation of CSS boxes
- **Fragment Tree** - mutable layout results with concrete positions/sizes

### Scope of This Research

This research focuses on the **core layout algorithms** that every browser must implement:
- Block layout (vertical stacking)
- Inline layout (horizontal text flow with line breaking)
- Float layout (shifted boxes with wrapping)
- Absolute positioning (out-of-flow positioning)

Out of scope for this research (covered separately):
- Flexbox and Grid (modern layout modes, handled separately)
- Table layout (complex enough to warrant its own research)
- Text shaping and font rendering (separate typography concerns)

---

## Table of Contents

1. [Box Generation (CSS 2.1 Section 9.2)](#box-generation)
2. [Positioning Schemes (CSS 2.1 Section 9.3)](#positioning-schemes)
3. [Containing Block Definition (CSS 2.1 Section 10.1)](#containing-block)
4. [Block Formatting Context (CSS 2.1 Section 9.4.1)](#block-formatting-context)
5. [Inline Formatting Context (CSS 2.1 Section 9.4.2)](#inline-formatting-context)
6. [Relative Positioning (CSS 2.1 Section 9.4.3)](#relative-positioning)
7. [Floats (CSS 2.1 Section 9.5)](#floats)
8. [Absolute Positioning (CSS 2.1 Section 9.6)](#absolute-positioning)
9. [Display/Position/Float Relationships (CSS 2.1 Section 9.7)](#display-position-float-relationships)
10. [Layered Presentation (CSS 2.1 Section 9.9)](#layered-presentation)
11. [Width Computation (CSS 2.1 Chapter 10.3)](#width-computation)
12. [Height Computation (CSS 2.1 Chapter 10.5-10.6)](#height-computation)
13. [Line Height Calculations (CSS 2.1 Section 10.8)](#line-height)
14. [Margin Collapsing (CSS 2.1 Section 8.3.1)](#margin-collapsing)
15. [Ambiguities and Open Questions](#ambiguities)
16. [Implementation Recommendations](#implementation-recommendations)
17. [Key Spec Quotes](#spec-quotes)
18. [Glossary](#glossary)
19. [References](#references)

---
<a name="box-generation"></a>
## Box Generation (CSS 2.1 Section 9.2)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#box-gen

### Overview

Box generation is the process by which HTML elements create CSS boxes. Each element generates at least one **principal box** that contains descendant boxes and generated content.

### Block-Level Elements and Block Boxes

**Definition from spec:** "Block-level elements are those elements of the source document that are formatted visually as blocks (e.g., paragraphs)."

**Display values that create block-level boxes:**
- `display: block`
- `display: list-item`
- `display: table`

**Key distinction - Block-level vs Block Container vs Block Box:**

1. **Block-level box** - participates in a block formatting context (as a block)
2. **Block container box** - establishes a block formatting context (contains blocks)
3. **Block box** - is BOTH block-level AND a block container

**Examples:**
```
<div>            → block box (both block-level and block container)
<p>              → block box
<li>             → block-level box (also generates marker box)
<table>          → block-level box (generates table wrapper box)
```

**Special cases:**
- `inline-block` - creates a block container that is NOT block-level (it's inline-level)
- `table-cell` - creates a block container that is NOT block-level
- Non-replaced inline elements - are block-level but NOT block containers

### Inline-Level Elements and Inline Boxes

**Definition from spec:** Inline-level elements distribute content across lines rather than forming new content blocks.

**Display values that create inline-level boxes:**
- `display: inline`
- `display: inline-block`
- `display: inline-table`

**Key distinction - Inline-level vs Inline Box:**

1. **Inline box** - both inline-level AND its content participates in the inline formatting context
2. **Atomic inline-level box** - inline-level but participates as a single opaque unit

**Inline boxes:**
- Generated by non-replaced elements with `display: inline`
- Example: `<span>`, `<em>`, `<strong>`

**Atomic inline-level boxes:**
- Replaced elements: `<img>`, `<input>`
- `inline-block` elements
- `inline-table` elements

**From the spec:** "Atomic inline-level boxes participate as a single opaque box in an inline formatting context"

### Anonymous Box Generation

The spec requires anonymous boxes in two scenarios:

#### Anonymous Block Boxes

**When:** A block container contains mixed inline and block content

**Rule from spec:** "When an inline box contains an in-flow block-level box, the inline box (and its inline ancestors within the same line box) are broken around the block-level box."

**Example:**
```html
<div>
  Some text
  <p>A paragraph</p>
  More text
</div>
```

**Box tree:**
```
Block box (div)
├─ Anonymous block box
│  └─ Anonymous inline box: "Some text"
├─ Block box (p)
│  └─ Anonymous inline box: "A paragraph"
└─ Anonymous block box
   └─ Anonymous inline box: "More text"
```

**Properties of anonymous block boxes:**
- Inherit inheritable properties from enclosing non-anonymous box
- Non-inherited properties take their initial values
- Ignored for percentage value resolution (uses closest non-anonymous ancestor)

#### Anonymous Inline Boxes

**When:** Text directly inside a block container (not wrapped in an inline element)

**Example:**
```html
<p>Some <em>emphasized</em> text</p>
```

**Box tree:**
```
Block box (p)
├─ Anonymous inline box: "Some "
├─ Inline box (em)
│  └─ Anonymous inline box: "emphasized"
└─ Anonymous inline box: " text"
```

### Display Property Mapping

| Display Value | Box Type | Participates In | Establishes |
|--------------|----------|-----------------|-------------|
| `block` | Block box | Block FC (as block) | Block FC or Inline FC |
| `inline` | Inline box | Inline FC | - |
| `inline-block` | Inline-level block container | Inline FC (atomic) | Block FC |
| `list-item` | Block box + marker | Block FC (as block) | Block FC |
| `table` | Block-level table | Block FC (as block) | Table FC |
| `inline-table` | Inline-level table | Inline FC (atomic) | Table FC |
| `table-cell` | Table cell (block container) | Table FC | Block FC |
| `none` | No box generated | - | - |

### Implementation Notes

- **Box node type:** Need distinct types for Block, Inline, InlineBlock, ListItem, Table, TableCell, etc. Track whether a box is block-level, inline-level, or a block container. Anonymous boxes must be tracked for percentage resolution rules.
- **Box generation:** Generate anonymous block boxes for mixed content and anonymous inline boxes for unwrapped text. Anonymous boxes inherit only inheritable properties; other properties take initial values.

### Edge Cases and Gotchas

1. **Percentage heights ignore anonymous boxes**
   - When resolving `height: 50%`, skip anonymous ancestors
   - Use closest non-anonymous ancestor's height

2. **Empty anonymous boxes still exist**
   - `<div><p></p></div>` - no anonymous boxes (pure block content)
   - `<div> <p></p></div>` - anonymous box for the space character

3. **List items are special**
   - Generate principal block box PLUS marker box
   - Marker box positioned relative to principal box

4. **Display: none is complete removal**
   - From spec: "element and its content are removed entirely from formatting structure"
   - No box generation, no descendant boxes

---
<a name="positioning-schemes"></a>
## Positioning Schemes (CSS 2.1 Section 9.3)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#positioning-scheme

### Three Positioning Schemes

CSS 2.1 defines three distinct algorithms for positioning boxes:

#### 1. Normal Flow

**From spec:** "In CSS 2.1, normal flow includes block formatting of block-level boxes, inline formatting of inline-level boxes, and relative positioning of block-level and inline-level boxes."

**Characteristics:**
- Default positioning scheme
- Boxes affect siblings' positions
- Includes both block and inline formatting
- Relative positioning is a variant (offset applied after layout)

#### 2. Floats

**From spec:** "In the float model, a box is first laid out according to the normal flow, then taken out of the flow and shifted to the left or right as far as possible."

**Characteristics:**
- Box positioned in normal flow first
- Then removed from flow and shifted
- Content flows around the float
- Floats still affect line boxes (unlike absolutely positioned boxes)

#### 3. Absolute Positioning

**From spec:** "In the absolute positioning model, a box is removed from the normal flow entirely (it has no impact on later siblings) and assigned a position with respect to a containing block."

**Characteristics:**
- Completely removed from normal flow
- No impact on sibling positions
- Positioned relative to containing block
- Includes both `position: absolute` and `position: fixed`

### In-Flow vs Out-of-Flow

**From spec:** "An element is out of flow if it is floated, absolutely positioned, or is the root element."

**In-flow elements:**
- `position: static` (default)
- `position: relative` (still in flow, just offset)
- Normal block and inline boxes

**Out-of-flow elements:**
- Floated elements (`float: left` or `float: right`)
- Absolutely positioned (`position: absolute` or `position: fixed`)
- Root element (the `<html>` element)

**Critical distinction:** Relatively positioned boxes are IN flow (they occupy space and affect siblings), but absolutely positioned boxes are OUT of flow (they don't affect siblings).

### Position Property Values

#### static (default)

**From spec:** "The box is a normal box, laid out according to the normal flow. The top, right, bottom, and left properties do not apply."

**Behavior:**
- Normal flow positioning
- Offset properties ignored
- Most common value

#### relative

**Behavior:**
- Box positioned in normal flow first
- Then offset by `top`, `right`, `bottom`, `left` properties
- Original space is preserved (other boxes don't move)
- Can cause overlapping

**Example:**
```css
.box {
  position: relative;
  top: 10px;
  left: 20px;
}
/* Box shifts 10px down, 20px right from its normal position */
/* Other boxes stay in their normal positions */
```

#### absolute

**From spec:** "The box's position (and possibly size) is specified with the top, right, bottom, and left properties."

**Behavior:**
- Box removed from normal flow
- Positioned relative to containing block
- No impact on siblings
- Can be sized by offset properties

#### fixed

**From spec (paraphrased):** "Fixed positioning is a variant of absolute positioning where the box is fixed with respect to the viewport and does not move when scrolled."

**Behavior:**
- Like `absolute` but containing block is always the viewport
- Doesn't scroll with page content
- Common for fixed headers/toolbars

---

<a name="containing-block"></a>
## Containing Block Definition (CSS 2.1 Section 10.1)

**Specification:** https://www.w3.org/TR/CSS21/visudet.html#containing-block-details

The containing block is THE critical concept for all layout calculations. Every box's position and size is calculated relative to its containing block.

### Containing Block Determination

The containing block depends on the element's `position` property:

#### For position: static OR position: relative

**From spec:** "The containing block is formed by the content edge of the nearest block container ancestor box."

**Example:**
```html
<div style="padding: 10px">     <!-- Containing block -->
  <div style="position: relative; width: 50%">
    <!-- Width is 50% of parent's CONTENT width (excludes padding) -->
  </div>
</div>
```

**Key point:** Content edge, not padding edge. Percentages resolve against content box.

#### For position: fixed

**From spec:** "The containing block is established by the viewport in the case of continuous media or the page area in the case of paged media."

**Behavior:**
- Containing block is always the viewport
- Not affected by any ancestor positioning
- Useful for fixed headers, modals, etc.

#### For position: absolute

**From spec:** "The containing block is established by the nearest ancestor with a 'position' of 'absolute', 'relative' or 'fixed'."

**Detailed rules:**

**If ancestor is block-level:**
- Containing block is formed by the **padding edge** (not content edge!)
- This is different from static/relative positioning

**If ancestor is inline-level:**
- Containing block is formed by bounding box of first and last inline boxes' padding boxes
- Complex for inline boxes that wrap across lines

**If no positioned ancestor:**
- Containing block is the **initial containing block**
- In continuous media: the viewport dimensions
- In paged media: the page box

**Example:**
```html
<body>                                    <!-- Not positioned -->
  <div>                                   <!-- Not positioned -->
    <div style="position: relative; padding: 20px">  <!-- Positioned! -->
      <div style="position: absolute; top: 0; left: 0">
        <!-- Containing block is the relative div -->
        <!-- Positioned at top-left of its PADDING edge -->
      </div>
    </div>
  </div>
</body>
```

### Root Element Containing Block

**From spec:** "The containing block in which the root element lives is a rectangle called the initial containing block."

**For continuous media (screens):**
- Width: viewport width
- Height: viewport height
- Anchored at canvas origin (0, 0)

**For paged media (print):**
- Dimensions: page box dimensions
- May vary per page

### Implementation Implications
 
**Block layout:**
```rust
fn compute_width(box: &BoxNode, containing_block_width: f32) -> f32 {
    // Width percentages resolve against containing block width
    if let Some(width_percent) = box.style.width.as_percentage() {
        return containing_block_width * width_percent / 100.0;
    }
    // ...
}
```

**Positioned layout:**
```rust
fn get_containing_block(box: &BoxNode) -> ContainingBlock {
    match box.style.position {
        Position::Fixed => ContainingBlock::Viewport,
        Position::Absolute => find_positioned_ancestor(box),
        _ => find_block_container_ancestor(box),
    }
}
```

### Critical Edge Cases

1. **Padding edge vs content edge**
   - Static/relative: use content edge
   - Absolute: use padding edge of positioned ancestor
   - This affects percentage resolution

2. **Inline-level containing blocks**
   - Extremely complex when inline box wraps across lines
   - Bounding box may be non-rectangular
   - Need to handle carefully

3. **Percentage heights**
   - Only work if containing block has explicit height
   - Otherwise compute to `auto`
   - Common source of layout bugs

---
<a name="block-formatting-context"></a>
## Block Formatting Context (CSS 2.1 Section 9.4.1)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#block-formatting

The Block Formatting Context (BFC) is the fundamental layout algorithm for vertically stacked content. It's the default layout mode for most HTML documents.

### What Establishes a BFC

**From spec:** "Floats, absolutely positioned elements, block containers (such as inline-blocks, table-cells, and table-captions) that are not block boxes, and block boxes with 'overflow' other than 'visible'"

**Complete list of BFC establishers:**

1. **Root element** (`<html>`)
2. **Floats** (`float: left` or `float: right`)
3. **Absolutely positioned elements** (`position: absolute` or `fixed`)
4. **Block containers that are not block boxes**:
   - `display: inline-block`
   - `display: table-cell`
   - `display: table-caption`
   - `display: flow-root` (CSS3, but useful)
5. **Block boxes with overflow other than visible**:
   - `overflow: hidden`
   - `overflow: scroll`
   - `overflow: auto`

**Note:** Exception - `overflow: visible` on root element propagates to viewport, root still establishes BFC.

### Layout Rules Within a BFC

#### Vertical Stacking

**From spec:** "In a block formatting context, boxes are laid out one after the other, vertically, beginning at the top of a containing block."

**Algorithm (pseudocode):**
```
function layout_bfc(containing_width: f32, children: &[BoxNode]) -> Vec<Fragment> {
    let mut fragments = vec![];
    let mut current_y = 0.0;
    let mut previous_margin_bottom = 0.0;

    for child in children {
        // 1. Compute child width
        let child_width = compute_width(child, containing_width);

        // 2. Layout child (recursive)
        let child_fragment = layout_child(child, child_width);

        // 3. Handle margin collapsing
        let margin_top = get_margin_top(child);
        let collapsed = collapse_margins(previous_margin_bottom, margin_top);

        // 4. Position child
        current_y += collapsed;
        child_fragment.set_y(current_y);

        // 5. Advance Y position
        current_y += child_fragment.height();
        previous_margin_bottom = get_margin_bottom(child);

        fragments.push(child_fragment);
    }

    return fragments;
}
```

#### Horizontal Alignment

**From spec:** "Each box's left outer edge touches the left edge of the containing block (for right-to-left formatting, right edges touch). This is true even in the presence of floats (although a box's line boxes may shrink due to the floats), unless the box establishes a new block formatting context."

**Key points:**
- Block boxes start at left edge (LTR) or right edge (RTL)
- Even with floats present, block box itself touches edge
- But line boxes within the block may shrink around floats
- UNLESS the block establishes its own BFC (then it avoids floats)

**Example:**
```html
<div style="float: left; width: 100px; height: 100px;"></div>
<p>This text wraps around the float...</p>
<!-- The <p> box still starts at left edge -->
<!-- But line boxes inside <p> are shortened by the float -->
```

#### Margin Collapsing in BFC

**From spec:** "Vertical margins between adjacent block-level boxes in a block formatting context collapse."

This is a DEFINING characteristic of BFCs. See [Margin Collapsing](#margin-collapsing) section for complete algorithm.

**Key principle:** Margins collapse between:
- Adjacent siblings
- Parent and first/last child (in certain conditions)
- Empty blocks' own top and bottom margins

### BFC Interaction with Floats

**From spec:** "The border box of a block formatting context must not overlap the margin box of any floats in the same block formatting context."

**This means:**
- If a box establishes a new BFC, it moves to avoid floats
- Used for "clearfix" technique
- Common pattern: `overflow: hidden` to contain floats

**Example:**
```html
<div style="width: 500px;">
  <div style="float: left; width: 100px; height: 100px;"></div>

  <!-- Normal block: wraps around float -->
  <div style="width: 200px; height: 200px; background: blue;">
    <!-- Starts at left edge, but is obscured by float -->
  </div>

  <!-- BFC block: avoids float -->
  <div style="overflow: hidden; width: 200px; height: 200px; background: red;">
    <!-- Positioned to the right of float -->
    <!-- Left edge is at x=100 (after float) -->
  </div>
</div>
```

### BFC as Float Container

When a BFC contains floated children, the BFC's height expands to contain the floats.

**From spec (implicit):** The auto height of a BFC includes floated children.

**Example:**
```html
<div style="overflow: hidden;">  <!-- Establishes BFC -->
  <div style="float: left; height: 200px;"></div>
  <!-- Parent height will be at least 200px to contain float -->
</div>

<div>  <!-- Normal block, doesn't establish BFC -->
  <div style="float: left; height: 200px;"></div>
  <!-- Parent height is 0! (Floats don't contribute to parent height) -->
</div>
```

### Implementation Notes

**Block layout implementation sketch:**

```rust
pub struct BlockFormattingContext {
    // BFC is stateless - each layout is independent
}

impl FormattingContext for BlockFormattingContext {
    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // 1. Verify this box should be laid out in a BFC
        assert!(box_node.establishes_bfc() || parent_is_bfc);

        // 2. Get containing block width
        let containing_width = constraints.available_width;

        // 3. Layout each child vertically
        let children = self.layout_children_vertically(
            &box_node.children,
            containing_width,
        )?;

        // 4. Compute auto height (if needed)
        let height = self.compute_height(box_node, &children)?;

        // 5. Create fragment
        Ok(FragmentNode::new(
            Rect::new(Point::zero(), Size::new(containing_width, height)),
            children,
        ))
    }
}
```

### Edge Cases and Gotchas

1. **First/last child margin collapsing with parent**
   - Parent's top margin can collapse with first child's top margin
   - UNLESS parent has border/padding/establishes BFC
   - Complex clearance rules with floats

2. **Empty blocks collapse through**
   - Empty block's top and bottom margins collapse together
   - Can affect margins of siblings

3. **Negative margins**
   - Can cause boxes to overlap
   - Margin collapsing handles negatives specially (see margin section)

4. **Overflow: visible propagates from root**
   - Root element's `overflow: visible` becomes `auto` on viewport
   - Root still establishes BFC

5. **BFC does NOT prevent margin collapsing within itself**
   - Common misconception
   - BFC prevents collapsing WITH external boxes
   - But margins still collapse WITHIN the BFC

---
<a name="inline-formatting-context"></a>
## Inline Formatting Context (CSS 2.1 Section 9.4.2)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#inline-formatting

The Inline Formatting Context (IFC) is the layout algorithm for horizontal text flow. It handles line breaking, baseline alignment, and mixed inline/inline-block content.

### Overview

**From spec:** "In an inline formatting context, boxes are laid out horizontally, one after the other, beginning at the top of a containing block."

Key responsibilities:
- Arrange inline boxes horizontally
- Break into multiple lines when necessary
- Align boxes vertically within each line (baseline alignment)
- Handle text, inline boxes, and atomic inline-level boxes together

### The Line Box Concept

**From spec:** "The rectangular area that contains the boxes that form a line is called a line box."

**Line box properties:**
- Width determined by containing block and presence of floats
- Height determined by line-height calculations (see Section 10.8)
- Always tall enough to contain all boxes, but may be taller

**From spec:** "Line boxes are stacked with no vertical separation (except as specified elsewhere) and they never overlap."

### Layout Algorithm

**High-level algorithm:**
```
function layout_inline_formatting_context(
    inline_items: &[InlineItem],
    containing_width: f32
) -> Vec<LineBox> {
    let mut lines = vec![];
    let mut current_line = LineBuilder::new();
    let mut current_x = 0.0;

    for item in inline_items {
        match item {
            InlineItem::Text(text) => {
                // Try to fit on current line
                if current_x + text.width <= containing_width {
                    current_line.add_text(text, current_x);
                    current_x += text.width;
                } else {
                    // Find break opportunity
                    if let Some(break_point) = find_break_opportunity() {
                        // Break text at opportunity
                        let (first_part, second_part) = split_at_break(text, break_point);
                        current_line.add_text(first_part, current_x);

                        // Start new line
                        lines.push(current_line.finish());
                        current_line = LineBuilder::new();
                        current_x = 0.0;

                        current_line.add_text(second_part, current_x);
                        current_x += second_part.width;
                    } else {
                        // Force break (overflow)
                        lines.push(current_line.finish());
                        current_line = LineBuilder::new();
                        current_x = 0.0;

                        current_line.add_text(text, current_x);
                        current_x += text.width;
                    }
                }
            }
            InlineItem::AtomicInline(box) => {
                // Treat as single unit (like a character)
                if current_x + box.width <= containing_width {
                    current_line.add_atomic(box, current_x);
                    current_x += box.width;
                } else {
                    // Start new line for atomic box
                    if !current_line.is_empty() {
                        lines.push(current_line.finish());
                        current_line = LineBuilder::new();
                        current_x = 0.0;
                    }
                    current_line.add_atomic(box, current_x);
                    current_x += box.width;
                }
            }
            InlineItem::HardBreak => {
                // <br> - force line break
                lines.push(current_line.finish());
                current_line = LineBuilder::new();
                current_x = 0.0;
            }
        }
    }

    // Finish last line
    if !current_line.is_empty() {
        lines.push(current_line.finish());
    }

    return lines;
}
```

### Horizontal Layout Within Lines

**From spec:** "Horizontal margins, borders, and padding are respected between these boxes."

**Text-align property:**
When inline boxes don't fill the line, `text-align` determines their distribution:

| Value | Behavior |
|-------|----------|
| `left` | Boxes aligned to left edge of line box |
| `right` | Boxes aligned to right edge |
| `center` | Boxes centered horizontally |
| `justify` | Spaces/words stretched to fill line (not last line) |

**From spec (justify):** "If that property has the value 'justify', the user agent may stretch spaces and words in inline boxes."

### Vertical Alignment Within Lines

This is complex - see [Line Height section](#line-height) for complete details.

**From spec:** "The boxes may be aligned vertically in different ways: their bottoms or tops may be aligned, or the baselines of text within them may be aligned."

The `vertical-align` property controls vertical positioning within the line box.

### Handling Content Overflow

**From spec:** "When an inline box exceeds the width of a line box, it is split into several boxes and these boxes are distributed across several line boxes."

**For replaced elements and inline-blocks:**
**From spec:** "If an inline box cannot be split (e.g., if the inline box contains a single character, or language specific word breaking rules disallow a break within the inline box, or if the inline box is affected by a white-space value of nowrap or pre), then the inline box overflows the line box."

### Inline Box Splitting

When an inline box wraps across multiple lines, margins/borders/padding have special behavior:

**From spec:** "When an inline box is split, margins, borders, and padding have no visual effect where the split occurs."

**Example:**
```html
<span style="border: 2px solid black; padding: 5px;">
  This text is long enough to wrap to multiple lines
</span>
```

**Visual result:**
```
Line 1: [border-left][padding-left] This text is long [NO border-right]
Line 2: [NO border-left] enough to wrap to multiple [NO border-right]
Line 3: [NO border-left] lines [padding-right][border-right]
```

**Implementation:** Need separate fragment for each line segment, apply borders/padding only at start/end.

### Empty Line Boxes

**From spec (implicit):** Empty lines still have height determined by the containing block's font and line-height.

**The "strut":** An invisible inline box with the element's font and line-height properties.

### Floats Within Inline Formatting Context

**From spec:** "Line boxes are created as needed to hold inline-level content within an inline formatting context. Line boxes that contain no text, no preserved white space, no inline elements with non-zero margins, padding, or borders, and no other in-flow content (such as images, inline blocks or inline tables), and do not end with a preserved newline must be treated as zero-height line boxes."

**Impact of floats:**
**From spec:** "The line box width is determined by the containing block and the presence of floats."

Floats can cause line boxes to be shortened on left or right side.

### Implementation Notes

**Inline layout implementation sketch:**

```rust
pub struct InlineFormattingContext {
    shaping_pipeline: ShapingPipeline,
    line_breaker: LineBreaker,
}

impl FormattingContext for InlineFormattingContext {
    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode> {
        // 1. Build flat list of inline items
        let inline_items = self.build_inline_items(box_node, font_context)?;

        // 2. Break into lines
        let lines = self.line_breaker.break_lines(
            &inline_items,
            constraints.available_width,
        )?;

        // 3. Perform baseline alignment for each line
        let positioned_lines = self.align_lines(&lines, font_context)?;

        // 4. Stack lines vertically
        let mut fragments = vec![];
        let mut current_y = 0.0;

        for line in positioned_lines {
            let mut line_fragments = line.fragments;

            // Offset all fragments by line's Y position
            for fragment in &mut line_fragments {
                fragment.offset_y(current_y);
            }

            current_y += line.height;
            fragments.extend(line_fragments);
        }

        // 5. Create containing fragment
        Ok(FragmentNode::new(
            Rect::new(
                Point::zero(),
                Size::new(constraints.available_width, current_y),
            ),
            fragments,
        ))
    }
}
```

### Edge Cases and Gotchas

1. **Inline box splitting**
   - Must create separate fragments per line
   - Borders/padding only at start/end of entire inline box
   - Background applies to all fragments

2. **Empty lines have height**
   - `<br>` creates empty line with font's line-height
   - `<div><br/></div>` has height (the line box height)

3. **Atomic inline-level boxes**
   - Inline-block, replaced elements act as single unit
   - Cannot be split across lines
   - May overflow if too wide

4. **White-space property affects line breaking**
   - `nowrap` prevents soft breaks
   - `pre` preserves all whitespace and prevents breaks
   - `pre-wrap` preserves whitespace but allows breaks

5. **Bidirectional text**
   - Complex Unicode bidirectional algorithm
   - May require reordering glyphs
   - Out of scope for initial implementation

---
<a name="relative-positioning"></a>
## Relative Positioning (CSS 2.1 Section 9.4.3)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#relative-positioning

### How Relative Positioning Works

**From spec:** "Once a box has been laid out according to the normal flow or floated, it may be shifted relative to this position."

**Key characteristic from spec:** "Offsetting a box (B1) in this way has no effect on the box (B2) that follows: B2 is given a position as if B1 were not offset."

**Algorithm:**
```
1. Layout box in normal flow (or as float)
2. Record its position as the "normal position"
3. Apply offset using top/right/bottom/left properties
4. Leave a "ghost" at the normal position (affects other boxes)
5. Painted box appears at offset position
```

### Offset Properties

**top/bottom constraint:** `top = -bottom`

**From spec:** "If both 'top' and 'bottom' are 'auto', their used values are both '0'. If one of them is 'auto', it becomes the negative of the other. If neither is 'auto', 'bottom' is ignored."

**left/right constraint:** `left = -right`

Same rules as top/bottom.

**Example:**
```css
.box {
  position: relative;
  top: 20px;    /* Move down 20px */
  left: 10px;   /* Move right 10px */
}
/* bottom computes to -20px */
/* right computes to -10px */
```

### Space Reservation

**From spec:** "A relatively positioned box keeps its normal flow size, including line breaks and the space originally reserved for it."

**Implications:**
- Other boxes don't move when a box is relatively positioned
- Can cause overlapping
- Useful for minor adjustments without reflowing

**Example:**
```html
<div>Box 1</div>
<div style="position: relative; top: 50px;">Box 2</div>
<div>Box 3</div>

<!-- Box 3 is positioned as if Box 2 was at its normal position -->
<!-- Box 2's visual rendering is 50px down, but its "space" is unchanged -->
<!-- This causes Box 2 to overlap Box 3 -->
```

---

<a name="floats"></a>
## Floats (CSS 2.1 Section 9.5)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#floats

Floats are one of the most complex parts of CSS layout. They enable text wrapping around images and other elements.

### The Nine Positioning Rules

**From spec, Section 9.5.1:** These rules constrain the positioning of floated boxes:

1. **Left edge constraint:** "The left outer edge of a left-floating box may not be to the left of the left edge of its containing block."

2. **Earlier float avoidance:** "If the current box is left-floating, and there are any left-floating boxes generated by elements earlier in the source document, then for each such earlier box, either the left outer edge of the current box must be to the right of the right outer edge of the earlier box, or its top must be lower than the bottom of the earlier box."

3. **Right float separation:** "The right outer edge of a left-floating box may not be to the right of the left outer edge of any right-floating box that is next to it."

4. **Top constraint:** "A floating box's outer top may not be higher than the top of its containing block."

5. **Earlier element alignment:** "The outer top of a floating box may not be higher than the outer top of any block or floated box generated by an element earlier in the source document."

6. **Line box alignment:** "The outer top of an element's floating box may not be higher than the top of any line-box containing a box generated by an element earlier in the source document."

7. **Left edge for overlapping:** "A left-floating box that has another left-floating box to its left may not have its right outer edge to the right of its containing block's right edge."

8. **Height maximization:** "A floating box must be placed as high as possible."

9. **Horizontal positioning:** "A left-floating box must be put as far to the left as possible, a right-floating box as far to the right as possible. A higher position is preferred over one that is further to the left/right."

### Float Positioning Algorithm (Simplified)

```
function position_float(float_box: Box, containing_block: Rect, existing_floats: &[Float]) -> Point {
    // Start at top of containing block
    let mut y = containing_block.top;
    let mut x = containing_block.left; // (or right for right floats)

    // Rule 4: Can't go higher than containing block top
    y = max(y, containing_block.top);

    // Rule 5 & 6: Can't go higher than earlier content
    y = max(y, highest_earlier_content());

    // Try to place at current Y
    loop {
        // Find available X position at this Y level
        x = find_available_x_at_y(y, float_box.width, existing_floats, float_box.is_left);

        // Check if it fits within containing block
        if fits_in_containing_block(x, float_box.width, containing_block) {
            return Point::new(x, y);
        }

        // Doesn't fit - move down below next float
        y = find_next_float_bottom(y, existing_floats);
    }
}
```

### Content Flow Around Floats

**Line box shortening from spec:** "A line box is next to a float when there exists a vertical position that satisfies all of these four conditions:
1. at or below the top of the line box
2. at or above the bottom of the line box
3. below the top margin edge of the float
4. above the bottom margin edge of the float"

**When line box is next to float:** Line box is shortened by the float's width.

**Block boxes vs line boxes:**
- Block boxes ignore floats (box edge touches containing block edge)
- Line boxes within blocks shorten to avoid floats
- UNLESS block establishes new BFC (then entire block avoids float)

### Clear Property

**From spec:** The `clear` property prevents elements from being positioned next to floats.

**Values:** `left`, `right`, `both`, `none`

**Algorithm involves computing "clearance"** - extra space added above the element's top margin to push it below floats.

**From spec:** Clearance calculation is complex and has two possible interpretations (both valid pending testing):

**Method 1:**
```
clearance = max(0, lowest_float_bottom - hypothetical_top_position)
```

**Method 2:**
```
clearance = lowest_float_bottom - hypothetical_top_position
# (can be negative or zero)
```

**Both methods maintain:** The border edge clears the float's bottom margin edge.

### Float Containment

**BFC contains floats:** When an element establishes a BFC, its auto height includes floated children.

**From spec (implicit):** This is why `overflow: hidden` became the "clearfix" hack - it establishes BFC which contains floats.

---

<a name="absolute-positioning"></a>
## Absolute Positioning (CSS 2.1 Section 9.6)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#absolute-positioning

### Overview

**From spec:** Absolutely positioned boxes are "removed from the normal flow entirely (have no impact on later siblings)" and positioned "with respect to a containing block."

**Two variants:**
- `position: absolute` - relative to positioned ancestor
- `position: fixed` - relative to viewport

### Fixed Positioning

**From spec:** "For fixed positioned boxes, the box is fixed with respect to some reference. In the case of continuous media, the box is fixed with respect to the viewport and doesn't move when the document is scrolled."

**For print media:** "The box is rendered on every page, and is fixed with respect to the page box."

**Limitation from spec:** "UAs must not paginate the content of fixed boxes."

### Containing Block for Absolute Positioning

See [Containing Block section](#containing-block) for details.

**Key difference from static/relative:**
- Uses **padding edge** of positioned ancestor (not content edge)
- If no positioned ancestor, uses initial containing block

### Static Position

**From spec (10.3.7):** The "static position" is the hypothetical position the element would have if it were statically positioned.

Used when offset properties are `auto`.

**For 'ltr' direction:**
- When `left` is auto, it's set to the static position's left edge
- When `right` is auto, it's computed from the constraint equation

### Offset Properties

**top, right, bottom, left** specify offsets from edges of containing block.

**From spec:** "The values have the following meanings:
- length: The offset is a fixed distance from the reference edge
- percentage: The offset is a percentage of the containing block's dimension
- auto: See constraint equations in section 10.3.7"

### Absolute Positioning Edge Cases

1. **Over-constrained scenarios**
   - All four offsets + width/height specified
   - One offset is ignored based on direction (`left` ignored in RTL, `right` in LTR)

2. **Shrink-to-fit width**
   - When width is auto and both left and right are specified
   - Width becomes "shrink-to-fit" (min of preferred, available, and min content)

3. **Centering with auto margins**
   - If width is specified and both margins are auto
   - Margins become equal (element is centered)

4. **Fixed position and transforms**
   - CSS Transforms spec: transformed elements become containing block for fixed descendants
   - Not in CSS 2.1 but important for implementation

---
<a name="display-position-float-relationships"></a>
## Display, Position, and Float Relationships (CSS 2.1 Section 9.7)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#choose-position

These three properties interact in complex ways. The spec defines a priority system.

### The Interaction Algorithm

**From spec:** "The three properties that affect box generation and layout — 'display', 'position', and 'float' — interact as follows:"

**Step 1: display: none takes absolute priority**

**From spec:** "If 'display' has the value 'none', then 'position' and 'float' do not apply. In this case, the element generates no box."

```rust
if display == Display::None {
    return None; // No box generated, position and float are irrelevant
}
```

**Step 2: Absolute/fixed positioning overrides float**

**From spec:** "Otherwise, if 'position' has the value 'absolute' or 'fixed', the box is absolutely positioned, the computed value of 'float' is 'none', and display is set according to the table below."

```rust
if position == Position::Absolute || position == Position::Fixed {
    float = Float::None; // Float is cleared
    display = transform_display_for_absolute(display); // See table below
    // Position absolutely
}
```

**Step 3: Float modifies display**

**From spec:** "Otherwise, if 'float' has a value other than 'none', the box is floated and 'display' is set according to the table below."

```rust
if float != Float::None {
    display = transform_display_for_float(display); // See table below
    // Position as float
}
```

**Step 4: Root element special case**

**From spec:** "Otherwise, if the element is the root element, 'display' is set according to the table below, except that it is undefined in CSS 2.1 whether a specified value of 'list-item' becomes a computed value of 'block' or 'list-item'."

**Step 5: Normal case**

**From spec:** "Otherwise, the remaining 'display' property values apply as specified."

### Display Value Transformation Table

**From spec:**

| Specified Value | Computed Value |
|----------------|----------------|
| `inline-table` | `table` |
| `inline`, `run-in`, `table-row-group`, `table-column`, `table-column-group`, `table-header-group`, `table-footer-group`, `table-row`, `table-cell`, `table-caption`, `inline-block` | `block` |
| others | same as specified |

**Implementation:**
```rust
fn transform_display_for_float_or_absolute(display: Display) -> Display {
    match display {
        Display::InlineTable => Display::Table,
        Display::Inline | Display::InlineBlock | Display::TableRow |
        Display::TableCell | Display::TableCaption => Display::Block,
        _ => display,
    }
}
```

### Why These Rules Exist

**Rationale:** Floating and absolute positioning fundamentally change box behavior:
- Floats need to be block-level (can't float inline boxes)
- Absolutely positioned boxes are removed from flow (inline behavior doesn't make sense)
- Simplifies implementation (fewer combinations to handle)

---

<a name="layered-presentation"></a>
## Layered Presentation (CSS 2.1 Section 9.9)

**Specification:** https://www.w3.org/TR/CSS21/visuren.html#layers

Layering (z-axis positioning) is controlled by stacking contexts and the `z-index` property.

### Stacking Contexts

**From spec:** "The root element forms the root stacking context. Other stacking contexts are generated by any positioned element (including relatively positioned elements) having a computed value of 'z-index' other than 'auto'."

**What establishes a stacking context:**
1. Root element (`<html>`)
2. Positioned elements (relative, absolute, fixed) with `z-index` != `auto`
3. (CSS3 additions: opacity < 1, transforms, filters, etc. - not in CSS 2.1)

**Critical property from spec:** "Stacking contexts can contain further stacking contexts. A stacking context is atomic from the point of view of its parent stacking context; boxes in other stacking contexts may not come between any of its boxes."

**Atomicity means:**
- All descendants of a stacking context are grouped together
- They cannot interleave with siblings at the parent level
- The stacking context's z-index determines its position among siblings

### Z-Index Property

**Values:** `<integer>` | `auto`

**From spec:**
- Integer: "This integer is the stack level of the generated box in the current stacking context. The box also establishes a new stacking context."
- Auto: "The stack level of the generated box in the current stacking context is 0. The box does not establish a new stacking context unless it is the root element."

**Stack levels can be negative:** `z-index: -1` positions behind its parent.

### Painting Order Within a Stacking Context

**From spec (Section 9.9.1, Appendix E):** Elements are painted in the following order (back to front):

1. **Background and borders** of the element forming the stacking context
2. **Positioned descendants with negative z-index values** (most negative first)
3. **In-flow, non-inline-level, non-positioned descendants**
4. **Non-positioned floats**
5. **In-flow, inline-level, non-positioned descendants** (including inline tables and inline blocks)
6. **Positioned descendants with z-index: auto** and **positioned descendants with z-index: 0**
7. **Positioned descendants with positive z-index values** (lowest first)

**Algorithm:**
```rust
fn paint_stacking_context(context: &StackingContext, canvas: &mut Canvas) {
    // 1. Paint background and borders
    paint_background_and_borders(context.root, canvas);

    // 2. Paint negative z-index children (most negative first)
    for child in context.children.iter().filter(|c| c.z_index < 0).sorted_by_z_index() {
        paint_stacking_context(child, canvas);
    }

    // 3. Paint in-flow non-inline, non-positioned descendants
    for descendant in context.in_flow_block_descendants() {
        paint_box(descendant, canvas);
    }

    // 4. Paint floats
    for float in context.floats() {
        paint_box(float, canvas);
    }

    // 5. Paint inline content
    for descendant in context.inline_descendants() {
        paint_inline(descendant, canvas);
    }

    // 6. Paint z-index: 0 and auto positioned descendants
    for child in context.children.iter().filter(|c| c.z_index == 0) {
        paint_stacking_context(child, canvas);
    }

    // 7. Paint positive z-index children (lowest first)
    for child in context.children.iter().filter(|c| c.z_index > 0).sorted_by_z_index() {
        paint_stacking_context(child, canvas);
    }
}
```

### Stacking Context Examples

**Example 1: No new stacking context**
```html
<div style="position: relative; z-index: 1;">
  <div style="position: absolute; z-index: 100;">High z-index</div>
</div>
<div style="position: relative; z-index: 2;">
  <div style="position: absolute; z-index: -1;">Negative z-index</div>
</div>
```

**Result:** The "Negative z-index" div appears on top because its parent has z-index: 2, which is higher than the other parent's z-index: 1. The child z-indexes only matter within their respective stacking contexts.

**Example 2: z-index on non-positioned element**
```html
<div style="z-index: 100;">No effect</div>
<div style="position: relative;">Has effect (creates stacking context if z-index != auto)</div>
```

**From spec:** z-index only applies to positioned elements.

### Implementation Notes

**Paint system implementation note:**

```rust
pub struct StackingContext {
    pub root: FragmentNode,
    pub z_index: i32,
    pub children: Vec<StackingContext>,
}

impl StackingContext {
    pub fn build(root: &FragmentNode) -> Self {
        // Collect positioned descendants with explicit z-index
        let children = root.descendants()
            .filter(|d| d.is_positioned() && d.z_index != ZIndex::Auto)
            .map(|d| StackingContext::build(d))
            .collect();

        Self {
            root: root.clone(),
            z_index: root.style.z_index.as_integer().unwrap_or(0),
            children,
        }
    }

    pub fn paint(&self, canvas: &mut Canvas) {
        // Follow 7-layer painting order
        self.paint_layer_by_layer(canvas);
    }
}
```

### Edge Cases

1. **Opacity and transforms (CSS3)**
   - Not in CSS 2.1 but widely implemented
   - `opacity < 1` establishes stacking context
   - Any transform establishes stacking context

2. **Fixed positioning and stacking contexts**
   - Fixed elements participate in root stacking context
   - UNLESS they have z-index or are inside transformed element

3. **Inline stacking contexts**
   - Relatively positioned inline elements with z-index
   - Rare but valid

---
<a name="width-computation"></a>
## Width Computation (CSS 2.1 Chapter 10.3)

**Specification:** https://www.w3.org/TR/CSS21/visudet.html#Computing_widths_and_margins

Width computation is one of the most complex parts of CSS. Different element types have different algorithms.

### The Fundamental Constraint Equation

**From spec (Section 10.3.3):**

```
'margin-left' + 'border-left-width' + 'padding-left' + 'width' +
'padding-right' + 'border-right-width' + 'margin-right' = containing block width
```

This equation MUST be satisfied. When values are under-specified or over-specified, the spec defines resolution rules.

---

### 10.3.3: Block-Level, Non-Replaced Elements in Normal Flow

This is the most common case (`<div>`, `<p>`, etc.).

#### Constraint Equation

All properties must sum to containing block width.

#### Resolution Algorithm

**Case 1: width is not auto**

```rust
if width != Auto {
    // Width is specified

    if margin_left == Auto && margin_right == Auto {
        // Both margins auto: CENTER the box
        let available = containing_width - borders - padding - width;
        margin_left = available / 2.0;
        margin_right = available / 2.0;
    }
    else if margin_left == Auto {
        // Only left margin auto: it gets the remainder
        margin_left = containing_width - margin_right - borders - padding - width;
    }
    else if margin_right == Auto {
        // Only right margin auto: it gets the remainder
        margin_right = containing_width - margin_left - borders - padding - width;
    }
    else {
        // Neither margin is auto: OVER-CONSTRAINED
        // From spec: "Ignore margin-right (in LTR) or margin-left (in RTL)"
        if direction == LTR {
            margin_right = containing_width - margin_left - borders - padding - width;
        } else {
            margin_left = containing_width - margin_right - borders - padding - width;
        }
    }
}
```

**Case 2: width is auto**

**From spec:** "If 'width' is 'auto', any other 'auto' values become '0' and 'width' follows from the constraint."

```rust
if width == Auto {
    // Auto margins become 0
    if margin_left == Auto {
        margin_left = 0.0;
    }
    if margin_right == Auto {
        margin_right = 0.0;
    }

    // Width fills remaining space
    width = containing_width - margin_left - margin_right - borders - padding;

    // Width cannot be negative
    width = max(width, 0.0);
}
```

#### Implementation

```rust
fn compute_block_width_normal_flow(
    style: &ComputedStyle,
    containing_width: f32,
) -> (f32, f32, f32) { // Returns (margin_left, width, margin_right)

    let pl = resolve_padding(style.padding_left, containing_width);
    let pr = resolve_padding(style.padding_right, containing_width);
    let bl = style.border_left_width;
    let br = style.border_right_width;

    match (style.width, style.margin_left, style.margin_right) {
        // width is specified
        (Width::Length(w), MarginLeft::Auto, MarginRight::Auto) => {
            // Center
            let width = resolve_length(w, containing_width);
            let available = containing_width - pl - pr - bl - br - width;
            let ml = available / 2.0;
            let mr = available / 2.0;
            (ml, width, mr)
        }
        (Width::Length(w), MarginLeft::Auto, mr) => {
            // Left margin auto
            let width = resolve_length(w, containing_width);
            let mr_val = resolve_margin(mr, containing_width);
            let ml = containing_width - pl - pr - bl - br - width - mr_val;
            (ml, width, mr_val)
        }
        (Width::Length(w), ml, MarginRight::Auto) => {
            // Right margin auto
            let width = resolve_length(w, containing_width);
            let ml_val = resolve_margin(ml, containing_width);
            let mr = containing_width - pl - pr - bl - br - width - ml_val;
            (ml_val, width, mr)
        }
        (Width::Length(w), ml, mr) => {
            // Over-constrained
            let width = resolve_length(w, containing_width);
            let ml_val = resolve_margin(ml, containing_width);
            let mr_val = containing_width - pl - pr - bl - br - width - ml_val; // Recompute MR
            (ml_val, width, mr_val)
        }

        // width is auto
        (Width::Auto, ml, mr) => {
            let ml_val = if ml == MarginLeft::Auto { 0.0 } else { resolve_margin(ml, containing_width) };
            let mr_val = if mr == MarginRight::Auto { 0.0 } else { resolve_margin(mr, containing_width) };
            let width = containing_width - ml_val - mr_val - pl - pr - bl - br;
            let width = max(width, 0.0);
            (ml_val, width, mr_val)
        }
    }
}
```

---

### 10.3.4: Block-Level, Replaced Elements in Normal Flow

Replaced elements (`<img>`, `<video>`, etc.) have intrinsic dimensions.

#### Algorithm

1. **Determine width** using intrinsic dimensions and aspect ratio rules (Section 10.4)
2. **Compute margins** using the same rules as non-replaced elements (Section 10.3.3)

**Example:**
```html
<img src="photo.jpg" style="width: auto; height: 200px;">
<!-- Intrinsic size: 400x300 -->
<!-- Aspect ratio: 4:3 -->
<!-- Height set to 200px, so width = 200 * (4/3) = 266.67px -->
```

---

### 10.3.7: Absolutely Positioned, Non-Replaced Elements

This is significantly more complex due to the addition of `left`, `right`, `top`, and `bottom` properties.

#### Extended Constraint Equation

**From spec:**

```
'left' + 'margin-left' + 'border-left-width' + 'padding-left' + 'width' +
'padding-right' + 'border-right-width' + 'margin-right' + 'right' = containing block width
```

Note: `left` and `right` are now part of the equation.

#### Static Position

**From spec:** "The static position for 'left' is the distance from the left edge of the containing block to the left margin edge of a hypothetical box that would have been the first box of the element if its specified 'position' value had been 'static'."

**In other words:** Where the element *would* be if it wasn't absolutely positioned.

#### Resolution Algorithm (Simplified)

This has many cases. Here are the key ones:

**Case 1: All three (left, width, right) are auto**
```rust
// Set auto margins to 0
margin_left = 0;
margin_right = 0;

// Use static position for left (LTR) or right (RTL)
if direction == LTR {
    left = static_position_left;
    // Compute width as shrink-to-fit
    width = shrink_to_fit_width();
    // Solve for right
    right = containing_width - left - margins - borders - padding - width;
} else {
    right = static_position_right;
    width = shrink_to_fit_width();
    left = containing_width - right - margins - borders - padding - width;
}
```

**Case 2: Width and one offset are specified**
```rust
// Example: width and left specified, right is auto
margin_left = if margin_left == Auto { 0 } else { margin_left };
margin_right = if margin_right == Auto { 0 } else { margin_right };

right = containing_width - left - margin_left - borders - padding - width - margin_right;
```

**Case 3: All specified (over-constrained)**
```rust
// Ignore one offset based on direction
if direction == LTR {
    // Ignore 'right' (recompute it)
    right = containing_width - left - margins - borders - padding - width;
} else {
    // Ignore 'left' (recompute it)
    left = containing_width - right - margins - borders - padding - width;
}
```

**Case 4: Width is auto, both offsets specified**
```rust
// Width can be computed from the constraint
if margin_left == Auto && margin_right == Auto {
    // Solve for width, then center with equal margins
    let available_for_width_and_margins = containing_width - left - right - borders - padding;

    // Try to center
    width = content_width(); // Preferred width
    if width <= available_for_width_and_margins {
        margin_left = (available_for_width_and_margins - width) / 2.0;
        margin_right = margin_left;
    } else {
        // Can't center, give priority to margin-left
        margin_left = 0;
        margin_right = 0;
        width = available_for_width_and_margins;
    }
} else {
    // Solve for width directly
    width = containing_width - left - right - margins - borders - padding;
}
```

#### Shrink-to-Fit Width

**From spec:** "Calculation of the shrink-to-fit width is similar to calculating the width of a table cell using the automatic table layout algorithm."

**Algorithm:**
```
shrink_to_fit_width = min(max(preferred_minimum_width, available_width), preferred_width)

where:
- preferred_width = width if content never wrapped (max-content)
- preferred_minimum_width = width with all soft-break opportunities taken (min-content)
- available_width = containing block width - margins - borders - padding - offsets
```

**Example:**
```html
<div style="position: absolute; left: 10px; right: 10px; width: auto;">
  This text determines the width using shrink-to-fit
</div>
<!-- Width will be between min-content and max-content -->
<!-- Bounded by available space (containing width - 20px) -->
```

---

### Other Width Computation Scenarios

**10.3.1: Inline, non-replaced elements**
- `width` property doesn't apply
- Width determined by content

**10.3.2: Inline, replaced elements**
- Use intrinsic width or specified width

**10.3.5: Floating, non-replaced elements**
- Similar to block-level but uses shrink-to-fit for `width: auto`

**10.3.6: Floating, replaced elements**
- Use intrinsic dimensions

**10.3.8: Absolutely positioned, replaced elements**
- Combine absolute positioning rules with intrinsic sizing

**10.3.9: Inline-block, non-replaced elements**
- Uses shrink-to-fit for `width: auto`

**10.3.10: Inline-block, replaced elements**
- Use intrinsic dimensions

---

### Implementation Notes

- **Block layout:** Focus on 10.3.3 (normal flow blocks). Handle centering when both margins are auto and handle over-constrained cases by ignoring the appropriate margin.
- **Float layout:** Implement shrink-to-fit width.
- **Positioned layout:** Handle all absolute positioning cases, including static position calculation and shrink-to-fit sizing where required.

---
<a name="height-computation"></a>
## Height Computation (CSS 2.1 Sections 10.5-10.6)

**Specification:** https://www.w3.org/TR/CSS21/visudet.html#the-height-property

Height computation is generally simpler than width, but has critical edge cases around percentage heights and auto height.

### 10.5: The 'height' Property

**Initial value:** `auto`
**Applies to:** All elements except non-replaced inline elements, table columns, and column groups
**Percentages:** Refer to height of containing block

#### Value Resolution

**Length values:**
```rust
if let Height::Length(h) = style.height {
    height = resolve_length(h, containing_height);
}
```

**Percentage values:**

**From spec:** "If the height of the containing block is not specified explicitly (i.e., it depends on content height), and this element is not absolutely positioned, the value computes to 'auto'."

**Critical rule:**
```rust
if let Height::Percentage(pct) = style.height {
    if containing_block_has_explicit_height {
        height = containing_height * pct / 100.0;
    } else {
        // Percentage height with auto-height parent becomes auto
        height = Auto;
    }
}
```

**This is a common source of confusion!**
```html
<div>  <!-- height: auto (depends on content) -->
  <div style="height: 50%;">  <!-- Computes to auto! Parent height not explicit -->
    Won't be 50% of parent!
  </div>
</div>

<div style="height: 400px;">  <!-- Explicit height -->
  <div style="height: 50%;">  <!-- Works! 200px -->
    This is 50% of parent.
  </div>
</div>
```

**Exception for absolutely positioned elements:**
```rust
if position == Absolute && containing_height == Auto {
    // For absolutely positioned, percentage works even if containing block height is auto
    // Uses the actual computed height of containing block
    height = actual_containing_height * pct / 100.0;
}
```

**Auto values:**
Depend on element type and context. See section 10.6.

**Negative heights:**
**From spec (implicit):** Not allowed (would be clamped to 0 or rejected).

---

### 10.6: Calculating Heights and Margins

Different element types have different rules for `height` when set to `auto`.

---

#### 10.6.3: Block-Level Non-Replaced Elements in Normal Flow

**When `overflow` computes to `visible`:**

**Vertical margins:**
**From spec:** "If 'margin-top', or 'margin-bottom' are 'auto', their used value is 0."

Unlike horizontal margins, vertical auto margins become 0 (no centering).

**Auto height calculation:**

**From spec:** "If 'height' is 'auto', the height depends on whether the element has any block-level children and whether it has padding or borders:"

**The height is the distance from:**
1. **Top content edge to** the first applicable of:
   - The bottom edge of the bottom (margin edge) of its last in-flow child, if the bottom margin does not collapse with the element's bottom margin
   - The bottom border edge of the last in-flow child whose top margin doesn't collapse with the element's bottom margin
   - Zero if the element has no in-flow children

**Simplified algorithm:**
```rust
fn compute_auto_height_block(element: &Element, children: &[Fragment]) -> f32 {
    if children.is_empty() {
        return 0.0;
    }

    // Find last in-flow child
    let last_child = children.iter()
        .filter(|c| c.is_in_flow())
        .last();

    if let Some(last) = last_child {
        // Height is bottom edge of last child relative to our content top edge
        let child_bottom = last.rect.bottom();

        // Check margin collapsing
        if last_child_bottom_margin_collapses_with_parent {
            // Use border edge instead of margin edge
            return child_bottom - last.margin_bottom;
        } else {
            // Use margin edge
            return child_bottom;
        }
    }

    0.0
}
```

**Out-of-flow children are ignored:**
- Floats don't contribute to parent height (unless parent establishes BFC)
- Absolutely positioned children never contribute

---

#### 10.6.4: Absolutely Positioned, Non-Replaced Elements

Similar to width, there's a constraint equation:

**From spec:**
```
'top' + 'margin-top' + 'border-top-width' + 'padding-top' + 'height' +
'padding-bottom' + 'border-bottom-width' + 'margin-bottom' + 'bottom' = containing block height
```

**Resolution follows similar logic to width:**
- Static position for `top` when auto
- Auto margins can center vertically (unlike normal flow!)
- Over-constrained ignores `bottom`

**Auto height:**
```rust
// If height is auto and top/bottom are specified
height = containing_height - top - bottom - margins - borders - padding;

// If height is auto and top/bottom are also auto
// Height becomes content height (shrink-to-fit vertically)
height = content_height();
```

---

#### 10.6.7: 'Auto' Heights for Block Formatting Context Roots

**When an element establishes a BFC:**

**From spec (implicit):** Auto height includes floated children.

```rust
fn compute_auto_height_bfc_root(element: &Element) -> f32 {
    let mut max_bottom = 0.0;

    // Include in-flow children
    for child in element.in_flow_children() {
        max_bottom = max_bottom.max(child.margin_bottom_edge());
    }

    // ALSO include floated children (unlike normal blocks)
    for float in element.floats() {
        max_bottom = max_bottom.max(float.margin_bottom_edge());
    }

    max_bottom
}
```

**This is why `overflow: hidden` contains floats!**

---

<a name="line-height"></a>
## Line Height Calculations (CSS 2.1 Section 10.8)

**Specification:** https://www.w3.org/TR/CSS21/visudet.html#line-height

Line height determines:
1. Height of inline boxes (for text)
2. Minimum height of line boxes
3. Vertical spacing in inline formatting contexts

### The 'line-height' Property

**Values:**
- `normal`: Reasonable value based on font (typically 1.0-1.2 × font-size)
- `<length>`: Explicit height (e.g., `20px`)
- `<number>`: Multiplier of font-size (e.g., `1.5` means 1.5 × font-size)
- `<percentage>`: Percentage of font-size (e.g., `150%` = 1.5 × font-size)

**Initial value:** `normal`
**Inheritance:** Yes (but computed value for numbers vs percentages differs - see below)

**From spec:** "On a block container element whose content is composed of inline-level elements, 'line-height' specifies the minimal height of line boxes within the element."

#### Leading and Half-Leading

**Typographic model from spec:**

Let:
- **A** = font ascent (distance from baseline to top of font)
- **D** = font descent (distance from baseline to bottom of font)
- **AD** = A + D (font height)
- **L** = line-height - AD (leading)

**Half-leading:** L/2 is added above and below the text.

**From spec:** "The height of the inline box encloses all glyphs and their half-leading on each side and is thus exactly 'line-height'."

```
┌─────────────────────────────┐  ← Top of inline box
│     Half-leading (L/2)      │
├─────────────────────────────┤  ← Top of font (ascent)
│                             │
│         Text glyphs         │  ← Baseline
│                             │
├─────────────────────────────┤  ← Bottom of font (descent)
│     Half-leading (L/2)      │
└─────────────────────────────┘  ← Bottom of inline box
         ↑
    line-height total
```

#### Computation of Line-Height

**For numbers:**
```rust
// line-height: 1.5;
let line_height = 1.5 * font_size;
```

**For percentages:**
```rust
// line-height: 150%;
let line_height = 1.5 * font_size; // Computed value
```

**Difference:** Numbers inherit as numbers, percentages inherit as computed values.

**Example:**
```html
<div style="font-size: 16px; line-height: 1.5;">
  <span style="font-size: 32px;">
    Large text
  </span>
</div>

<!-- With number: span's line-height = 1.5 × 32px = 48px -->

<div style="font-size: 16px; line-height: 150%;">
  <span style="font-size: 32px;">
    Large text
  </span>
</div>

<!-- With percentage: span's line-height = 24px (inherited from parent's computed value) -->
```

**This is why numbers are preferred!**

### Line Box Height Calculation

**From spec:** "The height of a line box is determined as follows:"

1. **Compute height of each inline-level box:**
   - For inline boxes: use `line-height`
   - For replaced elements and inline-blocks: use margin box height

2. **Align boxes vertically** according to `vertical-align`

3. **Line box height** = distance from top of highest box to bottom of lowest box

**The "strut":**
**From spec:** "A hypothetical inline box, for each line box in a block container element, with zero width and the font and line-height of that element."

The strut ensures minimum line height even for empty lines.

### The 'vertical-align' Property

**Applies to:** Inline-level and table-cell elements
**Does not apply to:** Block-level elements

**Values:**

| Value | Meaning |
|-------|---------|
| `baseline` | Align box's baseline with parent's baseline |
| `middle` | Align box's vertical midpoint with parent baseline + half x-height |
| `sub` | Lower baseline for subscript |
| `super` | Raise baseline for superscript |
| `text-top` | Align top with parent's content area top |
| `text-bottom` | Align bottom with parent's content area bottom |
| `top` | Align top with line box top |
| `bottom` | Align bottom with line box bottom |
| `<percentage>` | Raise/lower by percentage of line-height |
| `<length>` | Raise/lower by absolute distance |

**top and bottom are special:**

**From spec:** "The following values align the element relative to the line box. Since the element may have children aligned relative to it (which in turn may have descendants aligned relative to them), these values use the bounds of the aligned subtree."

**"Aligned subtree":** The element and all descendants with vertical-align other than `top` or `bottom`.

### Baseline Alignment Algorithm

**Simplified algorithm:**
```rust
fn align_line_boxes(line: &Line) -> PositionedLine {
    // 1. Compute metrics for each box
    let mut boxes_with_metrics = vec![];
    for box in &line.boxes {
        let metrics = compute_box_metrics(box);
        boxes_with_metrics.push((box, metrics));
    }

    // 2. Find baseline position
    // Start with strut baseline
    let mut baseline_y = strut_ascent;

    // Adjust for baseline-aligned boxes
    for (box, metrics) in &boxes_with_metrics {
        if box.vertical_align == VerticalAlign::Baseline {
            baseline_y = baseline_y.max(metrics.ascent);
        }
    }

    // 3. Position each box relative to baseline
    let mut positioned_boxes = vec![];
    for (box, metrics) in boxes_with_metrics {
        let y = match box.vertical_align {
            VerticalAlign::Baseline => baseline_y - metrics.ascent,
            VerticalAlign::Top => 0, // Will adjust after computing line height
            VerticalAlign::Bottom => 0, // Will adjust after computing line height
            VerticalAlign::Middle => {
                let parent_middle = baseline_y - (parent_x_height / 2.0);
                parent_middle - (metrics.height / 2.0)
            }
            VerticalAlign::Length(offset) => (baseline_y - metrics.ascent) + offset,
            // ... other cases
        };

        positioned_boxes.push((box, y));
    }

    // 4. Compute line box height
    let line_top = positioned_boxes.iter().map(|(b, y)| *y).min();
    let line_bottom = positioned_boxes.iter().map(|(b, y)| y + b.height).max();
    let line_height = line_bottom - line_top;

    // 5. Adjust top/bottom aligned boxes
    for (box, y) in &mut positioned_boxes {
        if box.vertical_align == VerticalAlign::Top {
            *y = line_top;
        } else if box.vertical_align == VerticalAlign::Bottom {
            *y = line_bottom - box.height;
        }
    }

    PositionedLine {
        height: line_height,
        baseline: baseline_y - line_top,
        boxes: positioned_boxes,
    }
}
```

### Implementation Notes

- **Inline layout:** Must integrate with font metrics from FontContext. Baseline alignment is iterative (top/bottom boxes affect line height, which affects their positions). The strut is critical for empty lines.

---
<a name="margin-collapsing"></a>
## Margin Collapsing (CSS 2.1 Section 8.3.1)

**Specification:** https://www.w3.org/TR/CSS21/box.html#collapsing-margins

Margin collapsing is one of the most complex and misunderstood parts of CSS layout. It's critical to get this right for correct block layout.

### Fundamental Principle

**From spec:** "In CSS, the adjoining margins of two or more boxes (which might or might not be siblings) can combine to form a single margin. Margins that combine this way are said to collapse."

**Key insight:** Margins can collapse between:
- Adjacent siblings
- Parent and first/last child
- Empty elements' own top and bottom margins

**From spec:** "Horizontal margins never collapse."

### When Margins Are "Adjoining"

This is the critical definition. Two margins are adjoining if and only if:

**From spec:**
- "both belong to in-flow block-level boxes that participate in the same block formatting context"
- "no line boxes, no clearance, no padding and no border separate them"
- "both belong to vertically-adjacent box edges"

**Vertically-adjacent box edges:**
- Top margin of a box and top margin of its first in-flow child
- Bottom margin of a box and top margin of its next in-flow sibling
- Bottom margin of a last in-flow child and bottom margin of its parent (if parent has `auto` computed height)
- Top and bottom margins of a box that doesn't establish a new BFC, has zero computed `min-height`, zero or auto computed `height`, and no in-flow children

### When Margins Do NOT Collapse

**From spec and derivable rules:**

1. **Between float and any other box**
   - Floats are out of flow, their margins never collapse

2. **Between absolutely positioned box and any other box**
   - Absolutely positioned boxes are out of flow

3. **Root element margins**
   - Root element's margins don't collapse with anything

4. **When boxes establish new block formatting context**
   - Elements with `overflow` != `visible`
   - Floats
   - Absolutely positioned elements
   - `inline-block`, `table-cell`, `table-caption`

5. **When padding or border separates margins**
   - Parent and child: if parent has padding-top or border-top, margins don't collapse
   - If parent has padding-bottom or border-bottom, bottom margins don't collapse

6. **Elements with clearance**
   - From spec: "Margin collapsing is handled specially when dealing with clearance"
   - An element with clearance has its top margin separated from previous margins

7. **Inline-level boxes**
   - Only block-level boxes' margins collapse

### The Collapsing Algorithm

**From spec:** "When two or more margins collapse, the resulting margin width is the maximum of the collapsing margins' widths."

**But it's more complex for negative margins:**

**Three cases:**

#### Case 1: All margins are positive
```rust
collapsed_margin = max(margin1, margin2, margin3, ...);
```

**Example:** 20px and 30px collapse to 30px

#### Case 2: All margins are negative
```rust
collapsed_margin = min(margin1, margin2, margin3, ...); // Most negative
```

**Example:** -20px and -30px collapse to -30px

#### Case 3: Mixed positive and negative
**From spec:** "If there are both negative and positive margins, the margin is the sum of the smallest (most negative) negative margin and the largest positive margin."

```rust
let max_positive = margins.filter(|m| m > 0).max().unwrap_or(0);
let min_negative = margins.filter(|m| m < 0).min().unwrap_or(0); // Most negative

collapsed_margin = max_positive + min_negative; // Note: min_negative is negative, so this is subtraction
```

**Example:** 30px and -10px collapse to 20px (30 + (-10))

### Data Structure for Collapsing

**Recommended representation:**

```rust
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CollapsibleMargin {
    /// Maximum positive margin value
    pub positive: f32,

    /// Maximum negative margin value (stored as absolute value)
    pub negative: f32,
}

impl CollapsibleMargin {
    pub fn new(value: f32) -> Self {
        if value >= 0.0 {
            Self { positive: value, negative: 0.0 }
        } else {
            Self { positive: 0.0, negative: -value }
        }
    }

    /// Collapses with another margin
    pub fn collapse_with(self, other: Self) -> Self {
        Self {
            positive: self.positive.max(other.positive),
            negative: self.negative.max(other.negative),
        }
    }

    /// Resolves to the final collapsed value
    pub fn resolve(self) -> f32 {
        self.positive - self.negative
    }
}
```

### Margin Collapsing Scenarios

#### Scenario 1: Adjacent Siblings

**Example:**
```html
<div style="margin-bottom: 20px;">First</div>
<div style="margin-top: 30px;">Second</div>
```

**Result:** Margins collapse to 30px (max of 20px and 30px)

#### Scenario 2: Parent and First Child

**Example:**
```html
<div style="margin-top: 20px;">  <!-- Parent -->
  <div style="margin-top: 30px;">Child</div>
</div>
```

**If parent has no border-top, padding-top, or doesn't establish BFC:**
- Parent's top margin and child's top margin collapse
- Resulting margin is 30px
- The margin appears OUTSIDE the parent

**Visual:**
```
┌─────────┐
│ 30px    │  ← Collapsed margin (outside parent)
├─────────┤
│ Parent  │
│ ┌─────┐ │
│ │Child│ │  ← Child has no top margin (it collapsed with parent's)
│ └─────┘ │
└─────────┘
```

#### Scenario 3: Parent and Last Child

Similar to first child, but with bottom margins.

**From spec:** "If the bottom margin of an in-flow child and the bottom margin of its parent are adjoining, they collapse together."

**Conditions:**
- Parent has `height: auto`
- Parent has no `padding-bottom` or `border-bottom`
- Parent doesn't establish BFC

#### Scenario 4: Empty Block

**From spec:** "If the top and bottom margins of a box are adjoining, then it is possible for margins to collapse through it."

**Example:**
```html
<div style="margin-bottom: 20px;">Before</div>
<div style="margin-top: 30px; margin-bottom: 40px; height: 0;"></div>  <!-- Empty -->
<div style="margin-top: 50px;">After</div>
```

**Collapsing:**
1. Empty div's top (30px) and bottom (40px) collapse together → 40px
2. This 40px collapses with "Before"'s bottom (20px) → 40px
3. This 40px collapses with "After"'s top (50px) → 50px

**Final:** Only 50px margin between "Before" and "After"

### Margin Collapsing with Clearance

**From spec:** "Clearance is introduced as spacing above the margin-top of an element. It is used to push the element vertically past the float."

**Effect on collapsing:** Clearance prevents the element's top margin from collapsing with previous margins.

**Algorithm:**
```rust
fn compute_clearance(
    element_margin_top: f32,
    previous_collapsed_margin: CollapsibleMargin,
    lowest_float_bottom: f32,
    hypothetical_position: f32, // Position if clear: none
) -> Option<f32> {

    // Hypothetical position is where element would be without clearance
    // (after collapsing margins)
    let collapsed = previous_collapsed_margin
        .collapse_with(CollapsibleMargin::new(element_margin_top))
        .resolve();

    let hypothetical_border_top = hypothetical_position + collapsed;

    // Does element clear floats at hypothetical position?
    if hypothetical_border_top >= lowest_float_bottom {
        // Already clears, no clearance needed
        return None;
    }

    // Clearance is needed
    // Position element so border edge is at or below float bottom
    let clearance = lowest_float_bottom - hypothetical_position - element_margin_top;

    Some(clearance.max(0.0))
}
```

### Implementation Notes

**Block layout margin collapsing sketch:**

```rust
pub struct MarginCollapseState {
    pending_margin: CollapsibleMargin,
    at_start: bool, // True if we're at the start of the parent (for first-child collapsing)
}

impl MarginCollapseState {
    pub fn new() -> Self {
        Self {
            pending_margin: CollapsibleMargin::new(0.0),
            at_start: true,
        }
    }

    pub fn add_margin(&mut self, margin: f32) {
        self.pending_margin = self.pending_margin.collapse_with(CollapsibleMargin::new(margin));
    }

    pub fn consume(&mut self) -> f32 {
        let value = self.pending_margin.resolve();
        self.pending_margin = CollapsibleMargin::new(0.0);
        self.at_start = false;
        value
    }

    pub fn is_at_start(&self) -> bool {
        self.at_start
    }
}

// Usage in block layout:
fn layout_children_with_margin_collapsing(
    children: &[BoxNode],
    containing_width: f32,
) -> Vec<Fragment> {
    let mut fragments = vec![];
    let mut current_y = 0.0;
    let mut margin_state = MarginCollapseState::new();

    // Handle parent's top margin collapsing with first child
    if parent_top_margin_can_collapse {
        margin_state.add_margin(parent_margin_top);
    }

    for child in children {
        // Add child's top margin
        let child_margin_top = get_margin_top(child);
        margin_state.add_margin(child_margin_top);

        // Consume collapsed margin
        let collapsed = margin_state.consume();
        current_y += collapsed;

        // Layout child
        let child_fragment = layout_child(child, containing_width);

        // Position child
        child_fragment.set_y(current_y);
        current_y += child_fragment.height();

        // Add child's bottom margin to state (for next iteration or parent)
        let child_margin_bottom = get_margin_bottom(child);
        margin_state.add_margin(child_margin_bottom);

        fragments.push(child_fragment);
    }

    // Handle parent's bottom margin collapsing with last child
    if parent_bottom_margin_can_collapse && parent_height_is_auto {
        margin_state.add_margin(parent_margin_bottom);
        // This pending margin will be consumed by the parent's parent
    }

    fragments
}
```

### Edge Cases and Gotchas

1. **Empty div with margins collapses through**
   - Can cause surprising spacing
   - Both margins collapse together, then with adjacent margins

2. **First child margin escapes parent**
   - Common bug: adding margin-top to child adds space above parent instead
   - Fix: add padding-top to parent, or use border, or make parent a BFC

3. **Negative margins can cause overlap**
   - Elements can visually overlap when negative margins collapse
   - Not an error, spec-compliant behavior

4. **Clearance prevents collapsing**
   - Element with `clear` doesn't collapse top margin with previous margins
   - Clearance is inserted ABOVE the margin

5. **BFC boundaries stop collapsing**
   - Margins don't collapse across BFC boundaries
   - Inner margins still collapse within the BFC

### Testing Strategy

**Critical test cases:**

```rust
#[test]
fn test_positive_margins_collapse_to_max() {
    // 20px + 30px = 30px (not 50px)
}

#[test]
fn test_negative_margins_collapse_to_min() {
    // -20px + -30px = -30px (most negative)
}

#[test]
fn test_mixed_margins_sum() {
    // 30px + (-10px) = 20px
    // 40px + (-50px) = -10px
}

#[test]
fn test_parent_first_child_collapsing() {
    // Parent margin-top collapses with first child margin-top
}

#[test]
fn test_parent_first_child_no_collapse_with_padding() {
    // Padding-top prevents collapsing
}

#[test]
fn test_parent_first_child_no_collapse_with_bfc() {
    // overflow: hidden prevents collapsing
}

#[test]
fn test_empty_div_collapses_through() {
    // Empty div's top and bottom collapse, then with siblings
}

#[test]
fn test_clearance_prevents_collapsing() {
    // Element with clear doesn't collapse with previous margins
}
```

---
<a name="ambiguities"></a>
## Ambiguities and Open Questions

These are areas where the CSS 2.1 specification is ambiguous, underspecified, or has known interpretation issues. Each will require testing against real browsers and making implementation decisions.

---

### Ambiguity 1: Percentage Heights with Auto-Height Containing Blocks

**Section:** 10.5

**Specification Quote:** "If the height of the containing block is not specified explicitly (i.e., it depends on content height), and this element is not absolutely positioned, the value computes to 'auto'."

**Ambiguity:** What counts as "specified explicitly"?

**Questions:**
- Does `height: 100%` on the HTML element count as "explicit" for body's children?
- What about `min-height: 100vh` - is that "explicit"?
- For flexbox items, the container height is determined by content - are percentage heights allowed?

**Real-world impact:** Extremely common scenario. Many layouts use `height: 100%` on nested divs.

**Needs testing:** Create test cases with:
```html
<html style="height: 100%;">
  <body style="height: 100%;">
    <div style="height: 50%;">  <!-- Does this work? -->
    </div>
  </body>
</html>
```

**Recommended interpretation:**
- "Explicit" means a length value, not `auto`, not computed from content
- Percentage heights on ancestors can provide an explicit base
- Test against Chrome, Firefox, Safari to ensure compatibility

---

### Ambiguity 2: Clearance Calculation Method

**Section:** 9.5.2

**Specification Quote:** "Clearance is introduced as spacing above the margin-top of an element..."

**Ambiguity:** The spec provides TWO different algorithms for computing clearance and says both are valid "pending further testing."

**Method 1:** Clearance is just enough to position border edge at float bottom
**Method 2:** Clearance can be negative if element would naturally clear

**Questions:**
- Which method do modern browsers use?
- Are there edge cases where they differ significantly?
- Does it matter for practical layouts?

**Needs testing:** Create test with element that has large top margin and `clear: both`.

**Recommended interpretation:**
- Test all major browsers
- Implement the method that matches majority
- Document any deviations

---

### Ambiguity 3: Shrink-to-Fit Width Calculation

**Section:** 10.3.5, 10.3.7, 10.3.9

**Specification Quote:** "Calculation of the shrink-to-fit width is similar to calculating the width of a table cell using the automatic table layout algorithm."

**Ambiguity:** The table auto layout algorithm itself is complex and has implementation variations.

**Questions:**
- How exactly do we compute "preferred minimum width"?
- Do we need to shape text to measure it?
- How do we handle replaced elements with intrinsic aspect ratios?
- What about nested floats or inline-blocks within the shrink-to-fit element?

**Needs testing:**
- Float with long text
- Inline-block with mixed content
- Compare measurements across browsers

**Recommended interpretation:**
- Preferred width: width if no soft breaks (measure all text on one line)
- Preferred minimum: width with all soft breaks taken (break at every opportunity)
- Must actually measure text using font metrics

---

### Ambiguity 4: Margin Collapsing Through Multiple Empty Elements

**Section:** 8.3.1

**Specification Quote:** Margins can collapse "through" an element if its top and bottom margins are adjoining.

**Ambiguity:** What happens with multiple nested empty divs?

**Example:**
```html
<div style="margin-bottom: 20px;">Before</div>
<div style="margin-top: 10px; margin-bottom: 15px;">
  <div style="margin-top: 25px; margin-bottom: 30px;">
    <div style="margin-top: 5px; margin-bottom: 8px;"></div>
  </div>
</div>
<div style="margin-top: 12px;">After</div>
```

**Questions:**
- Do all 8 margins collapse together?
- What's the order of collapsing?
- How does this interact with parent/child collapsing rules?

**Needs testing:** Create nested empty divs with various margin values.

**Recommended interpretation:**
- All margins that are transitively adjoining collapse together
- Use the CollapsibleMargin data structure to accumulate all margins
- Result is max(positives) - max(negatives)

---

### Ambiguity 5: Inline Box Splitting and Border/Padding

**Section:** 9.4.2

**Specification Quote:** "When an inline box is split, margins, borders, and padding have no visual effect where the split occurs."

**Ambiguity:** What exactly is the visual rendering?

**Questions:**
- Are borders drawn as separate rectangles for each line segment?
- Do border-radius corners appear at split points?
- How does box-shadow behave?
- What about background images - do they repeat on each segment or span continuously?

**Example:**
```html
<span style="border: 2px solid black; background: url(pattern.png);">
  Long text that wraps across multiple lines
</span>
```

**Needs testing:** Visual tests with borders, backgrounds, shadows.

**Recommended interpretation:**
- CSS 2.1 doesn't specify box-shadow or border-radius (CSS3 features)
- Borders: draw separate rectangles per line segment, no borders at splits
- Backgrounds: continuous image across all segments (trickier to implement)

---

### Ambiguity 6: Z-Index on Root Element

**Section:** 9.9

**Specification Quote:** "The root element forms the root stacking context. Other stacking contexts are generated by any positioned element... having a computed value of 'z-index' other than 'auto'."

**Ambiguity:** Does the root element's z-index matter? What if it's not 0?

**Questions:**
- Can you set `z-index: 1000` on `<html>`?
- Does it affect anything?
- Is the root stacking context's z-index always 0?

**Needs testing:** Set various z-indexes on html element.

**Recommended interpretation:**
- Root stacking context's z-index is not comparable to anything (it's the root)
- Value is irrelevant for rendering
- Might affect JavaScript APIs that query z-index

---

### Ambiguity 7: Float Positioning with Negative Margins

**Section:** 9.5.1

**Specification:** The 9 positioning rules for floats.

**Ambiguity:** Negative margins can cause the "outer edge" to be on the opposite side of the content edge.

**Example:**
```css
.float {
  float: left;
  margin-left: -50px;  /* Negative margin */
  width: 100px;
}
/* Outer left edge is now INSIDE the float box */
```

**Questions:**
- How do the 9 float rules apply when margins are negative?
- Can floats overlap other floats?
- Can floats extend outside their containing block?

**Needs testing:** Floats with negative margins in various scenarios.

**Recommended interpretation:**
- "Outer edge" includes negative margins (can be to the right of content edge)
- Floats CAN overlap when negative margins are used
- This is spec-compliant behavior

---

### Ambiguity 8: Line Height on Inline-Block Elements

**Section:** 10.8

**Specification:** Line-height applies to inline-level elements.

**Ambiguity:** How does line-height on an inline-block element affect its height?

**Example:**
```html
<span style="display: inline-block; height: auto; line-height: 2.0;">
  <div>Content</div>
</span>
```

**Questions:**
- Does line-height affect the inline-block's height?
- Or only the line box it participates in?
- What if the inline-block contains block-level children?

**Needs testing:** Inline-blocks with various line-height values and content types.

**Recommended interpretation:**
- Line-height applies to inline content INSIDE the inline-block
- Doesn't directly affect the inline-block's height
- Height is determined by BFC rules (it establishes a BFC)

---

### Ambiguity 9: Containing Block for Inline-Level Ancestors (Absolute Positioning)

**Section:** 10.1

**Specification Quote:** "If the ancestor is inline-level, the containing block is the bounding box around the padding boxes of the first and the last inline boxes generated for that element."

**Ambiguity:** What if the inline ancestor wraps across many lines?

**Example:**
```html
<span>
  Line 1 of the span
  Line 2 of the span
  Line 3 of the span
  <div style="position: absolute; top: 0; left: 0;">
    What is my containing block?
  </div>
</span>
```

**Questions:**
- Is the containing block a single rectangle?
- Or a complex multi-rectangle region?
- How do you compute width/height percentages?

**Needs testing:** Absolutely positioned elements inside multi-line inline boxes.

**Recommended interpretation:**
- Containing block is the bounding box (smallest rectangle containing all line segments)
- This can be very wide if lines span the full width
- Rare edge case but must be handled correctly

---

### Ambiguity 10: Auto Heights for Elements with Both In-Flow and Floated Children

**Section:** 10.6.3 vs 10.6.7

**Specification:** Normal blocks ignore floats for height, but BFC roots include them.

**Ambiguity:** What about an element that has BOTH in-flow children and floated children?

**Example:**
```html
<div style="overflow: hidden; height: auto;">  <!-- Establishes BFC -->
  <div>Normal child, height: 100px</div>
  <div style="float: left; height: 200px;">Float child</div>
</div>
```

**Questions:**
- Is the parent's height 100px or 200px?
- Do we take max of in-flow and floats?
- What if the float extends below all in-flow content?

**Needs testing:** BFC with mixed in-flow and floated children.

**Recommended interpretation:**
- For BFC roots, height includes both in-flow and floated children
- Height = max(bottom of last in-flow child, bottom of lowest float)
- This is the typical browser behavior

---

### Ambiguity 11: Relative Positioning and Overflow

**Section:** 9.4.3

**Specification:** Relatively positioned boxes can be offset from their normal position.

**Ambiguity:** Does relative positioning affect overflow calculation?

**Example:**
```html
<div style="width: 100px; height: 100px; overflow: auto;">
  <div style="position: relative; left: 200px; width: 50px; height: 50px;">
    Content
  </div>
</div>
```

**Questions:**
- Does the scrollable area include the offset position?
- Or only the normal flow position?
- Can relative positioning cause scrollbars to appear?

**Needs testing:** Relatively positioned elements in scrollable containers.

**Recommended interpretation:**
- Overflow area includes relatively positioned elements at their offset positions
- This can cause scrollbars even if normal flow wouldn't
- Test against browsers to confirm

---

### Ambiguity 12: Interaction of Min/Max Width with Width: Auto and Margins

**Section:** 10.4 (min-width, max-width)

**Specification:** Min and max constrain the computed width.

**Ambiguity:** How do these interact with the width: auto computation and margin centering?

**Example:**
```css
.box {
  width: auto;
  margin: 0 auto;
  max-width: 500px;
}
/* Does this center when constrained by max-width? */
```

**Questions:**
- Is the constraint applied before or after margin calculation?
- If width would be 800px but max-width is 500px, what happens to auto margins?
- Does the box still center?

**Needs testing:** Various combinations of width, min-width, max-width with auto margins.

**Recommended interpretation:**
- Compute width first
- Apply min/max constraints
- If margins are auto and width is constrained, recalculate margins
- Box should still center

---

### Additional Questions to Investigate

1. **Vertical-align on block-level elements**: Does it do anything? (Spec says no, but worth verifying)
2. **Floats within inline formatting context**: How exactly do they interact with line breaking?
3. **Baseline of empty inline-block**: What is it? (Important for alignment)
4. **Percentage padding on inline elements**: Resolves against containing block width, even for top/bottom - why? (Historical reasons)
5. **Border-collapse on inline tables**: How does it work exactly?

---

### Testing Strategy

For each ambiguity:

1. **Create minimal test case** - Isolate the specific behavior
2. **Test in all major browsers** - Chrome, Firefox, Safari, Edge
3. **Document browser differences** - If they disagree, note it
4. **Choose implementation** - Either majority behavior or most sensible
5. **Add to test suite** - Ensure our implementation matches decision

**Test infrastructure needed:**
- Visual regression tests (screenshot comparison)
- Layout tree dumps (for structure verification)
- Computed style queries (for value resolution)

---
<a name="implementation-recommendations"></a>
## Implementation Recommendations

This section maps spec sections to FastRender V2 implementation areas, providing clear guidance on which parts of this research support each area.

---

### BoxNode Type Definition

**Needs from this research:**

**Box Generation (Section 9.2):**
- Distinction between block-level, inline-level, block container
- List of display values and their box types
- Anonymous box generation rules

**Implementation requirements:**
```rust
pub enum BoxType {
    Block,           // Block-level and block container
    Inline,          // Inline box
    InlineBlock,     // Inline-level block container
    Table,           // Block-level table
    TableCell,       // Block container (not block-level)
    Float,           // Out-of-flow
    Absolute,        // Out-of-flow
    Anonymous,       // Generated by spec rules
}

pub struct BoxNode {
    pub box_type: BoxType,
    pub is_block_level: bool,
    pub is_block_container: bool,
    pub is_inline_level: bool,
    pub establishes_bfc: bool,
    // ...
}
```

**Key spec sections:** 9.2, 9.2.1, 9.2.2

---

### Box Generation Algorithm

**Needs from this research:**

**Box Generation (Section 9.2):**
- When to generate anonymous block boxes
- When to generate anonymous inline boxes
- Rules for mixed content handling

**Algorithm from spec:**
1. Identify element's display value
2. Check for anonymous box requirements
3. Generate principal box
4. Generate any additional boxes (markers for list-items)

**Key spec sections:** 9.2.1.1 (anonymous blocks), 9.2.2.1 (anonymous inlines)

---

### FormattingContext Trait Design

**Needs from this research:**

**Positioning Schemes (Section 9.3):**
- Three schemes: normal flow, floats, absolute
- Each scheme needs different formatting context

**Block FC (Section 9.4.1):**
- Layout rules for BFC

**Inline FC (Section 9.4.2):**
- Layout rules for IFC

**Trait design:**
```rust
pub trait FormattingContext {
    /// Perform layout of this box and its descendants
    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: &LayoutConstraints,
        font_context: &FontContext,
    ) -> Result<FragmentNode>;

    /// Compute intrinsic size (for shrink-to-fit)
    fn compute_intrinsic_size(
        &self,
        box_node: &BoxNode,
        axis: Axis,
        font_context: &FontContext,
    ) -> Result<f32>;

    /// Get the formatting context type
    fn context_type(&self) -> FormattingContextType;
}
```

**Key spec sections:** 9.3, 9.4

---

### Block Layout Implementation

**Needs from this research:**

**Block Formatting Context (Section 9.4.1):**
- Complete BFC layout rules
- Vertical stacking algorithm
- BFC establishment conditions

**Width Computation (Section 10.3.3):**
- Block width algorithm
- Constraint equation
- Margin centering
- Over-constrained handling

**Height Computation (Section 10.6.3):**
- Auto height calculation
- Content-based height

**Margin Collapsing (Section 8.3.1):**
- Complete collapsing algorithm
- All edge cases
- Parent/child collapsing

**Implementation checklist:**
- [ ] Implement width constraint equation
- [ ] Handle width: auto (fill container)
- [ ] Handle margin: auto (centering)
- [ ] Handle over-constrained scenarios
- [ ] Implement auto height (sum of children)
- [ ] Implement margin collapsing state machine
- [ ] Handle first-child collapsing with parent
- [ ] Handle last-child collapsing with parent
- [ ] Handle empty block collapsing through

**Key spec sections:** 9.4.1, 10.3.3, 10.6.3, 8.3.1

---

### Float Layout Implementation

**Needs from this research:**

**Floats (Section 9.5):**
- All 9 positioning rules
- Float placement algorithm
- Interaction with line boxes

**Clear Property (Section 9.5.2):**
- Clearance calculation
- Two possible methods (choose based on browser testing)

**Width Computation (Section 10.3.5):**
- Shrink-to-fit width for floats

**Implementation algorithm:**
```rust
fn position_float(
    float_box: &BoxNode,
    containing_block: Rect,
    existing_floats: &[PositionedFloat],
) -> Point {
    // Apply 9 rules from Section 9.5.1
    // 1. Can't extend past containing block edges
    // 2. Must avoid earlier floats
    // 3. Left and right floats can't overlap
    // 4. Can't be higher than containing block top
    // 5. Can't be higher than earlier block boxes
    // 6. Can't be higher than line boxes from earlier content
    // 7. Must fit within containing block (with earlier floats)
    // 8. As high as possible
    // 9. As far left/right as possible
}
```

**Key spec sections:** 9.5, 9.5.1, 9.5.2, 10.3.5

---

### Inline Layout Implementation

**Needs from this research:**

**Inline Formatting Context (Section 9.4.2):**
- Horizontal layout algorithm
- Line box concept
- Line breaking rules
- Inline box splitting

**Line Height (Section 10.8):**
- Line box height calculation
- Baseline alignment
- Leading and half-leading
- The "strut" concept

**Vertical Align (Section 10.8.1):**
- All vertical-align values
- Baseline, top, bottom, middle
- Aligned subtree concept

**Implementation steps:**
1. **Build inline items** (text runs, inline boxes, atomics)
2. **Break into lines** (line breaking algorithm)
3. **Baseline alignment** (compute line box heights)
4. **Stack lines vertically** (final positioning)

**Key spec sections:** 9.4.2, 10.8, 10.8.1

---

### Positioned Layout Implementation

**Needs from this research:**

**Absolute Positioning (Section 9.6):**
- Fixed vs absolute
- Out-of-flow behavior

**Relative Positioning (Section 9.4.3):**
- Offset application
- Space reservation

**Containing Block (Section 10.1):**
- How containing block is determined
- Padding edge vs content edge

**Width/Height (Sections 10.3.7, 10.6.4):**
- Extended constraint equations with offsets
- Static position concept
- Shrink-to-fit for absolute positioning
- Over-constrained handling

**Implementation requirements:**
```rust
pub struct PositionedLayout;

impl PositionedLayout {
    /// Layout absolutely positioned element
    fn layout_absolute(
        &self,
        box_node: &BoxNode,
        containing_block: Rect,
    ) -> Result<Fragment> {
        // 1. Determine containing block
        // 2. Compute static position (if needed)
        // 3. Resolve constraint equation
        // 4. Handle shrink-to-fit width (if needed)
        // 5. Layout contents
        // 6. Position at final location
    }

    /// Layout relatively positioned element
    fn layout_relative(
        &self,
        box_node: &BoxNode,
        normal_flow_fragment: Fragment,
    ) -> Fragment {
        // 1. Layout in normal flow first
        // 2. Apply top/right/bottom/left offsets
        // 3. Original space is preserved
    }
}
```

**Key spec sections:** 9.4.3, 9.6, 10.1, 10.3.7, 10.6.4

---

### Margin Collapsing Implementation

**Needs from this research:**

**Margin Collapsing (Section 8.3.1):**
- Complete algorithm
- Adjoining margins definition
- When margins do/don't collapse
- Positive, negative, and mixed margins

**Implementation:**
```rust
pub struct CollapsibleMargin {
    pub positive: f32,
    pub negative: f32,
}

impl CollapsibleMargin {
    pub fn collapse_with(self, other: Self) -> Self;
    pub fn resolve(self) -> f32;
}

pub struct MarginCollapseState {
    pending_margin: CollapsibleMargin,
    at_start: bool,
}
```

**Key spec section:** 8.3.1 (entire section critical)

---

### Text Shaping Integration

**Needs from this research:**

**Inline FC (Section 9.4.2):**
- How text participates in inline layout

**Line Height (Section 10.8):**
- Font metrics (ascent, descent)
- Leading calculation
- How line-height affects line box height

**Integration requirements:**
- Must provide font ascent/descent for baseline alignment
- Must provide shaped glyphs with advances for positioning
- Must support line breaking at grapheme clusters
- Must handle complex scripts (via HarfBuzz/rustybuzz)

**Key spec sections:** 9.4.2, 10.8

---

### Paint System and Stacking Contexts

**Needs from this research:**

**Layered Presentation (Section 9.9):**
- What establishes stacking contexts
- Z-index property behavior
- 7-layer painting order
- Stacking context atomicity

**Implementation:**
```rust
pub struct StackingContext {
    pub root: FragmentId,
    pub z_index: i32,
    pub children: Vec<StackingContext>,
}

impl StackingContext {
    pub fn build_from_fragment_tree(root: &Fragment) -> Self;

    pub fn paint(&self, canvas: &mut Canvas) {
        // Layer 1: Background and borders
        // Layer 2: Negative z-index stacking contexts
        // Layer 3: Block-level descendants
        // Layer 4: Floats
        // Layer 5: Inline descendants
        // Layer 6: z-index: 0 and auto
        // Layer 7: Positive z-index stacking contexts
    }
}
```

**Key spec section:** 9.9 (entire section), Appendix E

---

### Computed Style System

**Needs from this research:**

**Display/Position/Float Relationships (Section 9.7):**
- How these properties interact
- Transformation table for display values
- Priority rules

**Implementation:**
```rust
pub fn compute_final_display(
    specified_display: Display,
    position: Position,
    float: Float,
    is_root: bool,
) -> Display {
    // Apply rules from Section 9.7
    if specified_display == Display::None {
        return Display::None;
    }

    if position == Position::Absolute || position == Position::Fixed {
        return transform_display_for_absolute(specified_display);
    }

    if float != Float::None {
        return transform_display_for_float(specified_display);
    }

    if is_root {
        return transform_display_for_root(specified_display);
    }

    specified_display
}
```

**Key spec section:** 9.7

---

### Cross-Area Dependencies

**Dependency graph:**

```
BoxNode Type
    ↓
Box Generation ← FormattingContext trait
    ↓                            ↓
Block Layout ←──────────┘
    ↓
Margin Collapsing
    ↓
Float Layout
    ↓
Positioned Layout
    ↓
Inline Layout (after text shaping)
    ↓
Paint System (depends on all layout components)
```

---

### Critical Paths

**Path 1: Basic block layout**
- Box node definition → Box generation → Formatting context dispatch → Block layout → Margin collapsing
- **Enables:** Simple HTML documents with divs and paragraphs

**Path 2: Text rendering**
- Path 1 + text shaping integration → Inline layout
- **Enables:** Documents with text content

**Path 3: Complex layouts**
- Path 2 + float layout + positioned layout
- **Enables:** Magazine-style layouts, modals, tooltips

**Path 4: Visual rendering**
- Path 3 + paint system
- **Enables:** Complete rendering pipeline

---

### Recommended Implementation Order

1. Box node type, box generation, formatting context trait
2. Block layout (basic) plus margin collapsing
3. Text shaping integration and inline layout
4. Float layout and positioned layout
5. Paint system, then integration and testing

---
<a name="spec-quotes"></a>
## Key Spec Quotes

Critical quotes from the CSS 2.1 specification that should be referenced during implementation:

### On Box Generation

> "Block-level elements are those elements of the source document that are formatted visually as blocks (e.g., paragraphs)."
— Section 9.2

> "This value causes an element to not appear in the formatting structure...the element and its content are removed entirely."
— Section 9.2 (display: none)

### On Positioning

> "An element is out of flow if it is floated, absolutely positioned, or is the root element."
— Section 9.3

> "In the absolute positioning model, a box is removed from the normal flow entirely (it has no impact on later siblings) and assigned a position with respect to a containing block."
— Section 9.3

### On Block Formatting Context

> "In a block formatting context, boxes are laid out one after the other, vertically, beginning at the top of a containing block."
— Section 9.4.1

> "Each box's left outer edge touches the left edge of the containing block (for right-to-left formatting, right edges touch). This is true even in the presence of floats."
— Section 9.4.1

### On Inline Formatting Context

> "The rectangular area that contains the boxes that form a line is called a line box."
— Section 9.4.2

> "When an inline box is split, margins, borders, and padding have no visual effect where the split occurs."
— Section 9.4.2

### On Width Computation

> "'margin-left' + 'border-left-width' + 'padding-left' + 'width' + 'padding-right' + 'border-right-width' + 'margin-right' = width of containing block"
— Section 10.3.3 (The Constraint Equation)

> "If 'width' is 'auto', any other 'auto' values become '0' and 'width' follows from the constraint."
— Section 10.3.3

### On Margin Collapsing

> "In CSS, the adjoining margins of two or more boxes (which might or might not be siblings) can combine to form a single margin. Margins that combine this way are said to collapse."
— Section 8.3.1

> "Horizontal margins never collapse."
— Section 8.3.1

> "When two or more margins collapse, the resulting margin width is the maximum of the collapsing margins' widths."
— Section 8.3.1

### On Line Height

> "The height of the inline box encloses all glyphs and their half-leading on each side and is thus exactly 'line-height'."
— Section 10.8

> "On a block container element whose content is composed of inline-level elements, 'line-height' specifies the minimal height of line boxes within the element."
— Section 10.8

### On Stacking Contexts

> "The root element forms the root stacking context. Other stacking contexts are generated by any positioned element (including relatively positioned elements) having a computed value of 'z-index' other than 'auto'."
— Section 9.9

> "Stacking contexts can contain further stacking contexts. A stacking context is atomic from the point of view of its parent stacking context; boxes in other stacking contexts may not come between any of its boxes."
— Section 9.9

---

<a name="glossary"></a>
## Glossary

### Block-Level Element
An element that generates a block-level principal box. Participates in a block formatting context as a block. Examples: `div`, `p`, `h1`.

### Block Container Box
A box that contains either only block-level boxes or establishes an inline formatting context containing only inline-level boxes.

### Block Box
A box that is both block-level AND a block container. Most common box type in normal flow.

### Inline-Level Element
An element that generates inline-level boxes that participate in an inline formatting context. Examples: `span`, `a`, `em`.

### Inline Box
An inline-level box whose content participates in its containing inline formatting context. Generated by non-replaced inline elements.

### Atomic Inline-Level Box
An inline-level box that participates in its inline formatting context as a single opaque box (inline-block, replaced elements). Cannot be split across lines.

### Anonymous Box
A box generated by the user agent (not directly by an element) to satisfy structural requirements. Can be anonymous block boxes or anonymous inline boxes.

### Normal Flow
Includes block formatting, inline formatting, and relative positioning. The default positioning scheme.

### Out of Flow
An element that is floated, absolutely positioned, or is the root element. Does not participate in normal flow.

### In Flow
An element that is not out of flow. Participates in normal flow layout.

### Containing Block
The rectangle that forms the basis for sizing and positioning a box. Determined differently for static, relative, absolute, and fixed positioning.

### Block Formatting Context (BFC)
A formatting context in which block-level boxes are laid out vertically. Established by various conditions including the root, floats, absolutely positioned elements, and overflow != visible.

### Inline Formatting Context (IFC)
A formatting context in which inline-level boxes are laid out horizontally in line boxes. Text content participates in an IFC.

### Line Box
The rectangular area that contains the boxes that form a single line in an inline formatting context.

### Margin Collapsing
The process by which adjacent vertical margins combine into a single margin. Happens only for block-level boxes in normal flow.

### Adjoining Margins
Two margins that are in contact (no border, padding, or content between them) and belong to boxes in the same block formatting context.

### Principal Box
The main box generated by an element (as opposed to additional boxes like markers for list items).

### Replaced Element
An element whose content is outside the scope of CSS (e.g., `img`, `video`, `iframe`). Has intrinsic dimensions.

### Non-Replaced Element
An element whose content is represented by other boxes in the CSS box tree (e.g., `div`, `span`, `p`).

### Intrinsic Dimensions
The natural width and/or height of a replaced element (e.g., an image's pixel dimensions).

### Shrink-to-Fit
An algorithm for computing width that tries to use as little space as possible while respecting minimum content width. Used for floats, absolutely positioned elements, and inline-blocks with width: auto.

### Preferred Width
The width a box would have if it could use as much space as desired (max-content width).

### Preferred Minimum Width
The narrowest width a box can have without overflow (min-content width).

### Stacking Context
A three-dimensional conceptualization of boxes on the z-axis. Formed by the root element and certain positioned elements with explicit z-index.

### Static Position
The hypothetical position an absolutely positioned element would have if it were statically positioned. Used when offset properties are auto.

### Leading
The extra space added to line-height above and below glyphs. Half-leading is added above and below.

### Strut
An invisible inline box with zero width and the font and line-height of the containing block. Ensures minimum line box height.

### Baseline
The line upon which text sits. Critical for vertical alignment in inline formatting contexts.

### Clearance
Space inserted above an element's top margin to push it below floats. Used with the `clear` property.

---

<a name="references"></a>
## References

### Primary Sources

**CSS 2.1 Specification:**
- Full Specification: https://www.w3.org/TR/CSS21/
- Chapter 8 (Box Model): https://www.w3.org/TR/CSS21/box.html
- Chapter 9 (Visual Formatting Model): https://www.w3.org/TR/CSS21/visuren.html
- Chapter 10 (Visual Formatting Model Details): https://www.w3.org/TR/CSS21/visudet.html
- Appendix E (Elaborate Stacking): https://www.w3.org/TR/CSS21/zindex.html

### Secondary Sources

**MDN Web Docs:**
- Visual Formatting Model: https://developer.mozilla.org/en-US/docs/Web/CSS/Visual_formatting_model
- Block Formatting Context: https://developer.mozilla.org/en-US/docs/Web/Guide/CSS/Block_formatting_context
- Inline Formatting Context: https://developer.mozilla.org/en-US/docs/Web/CSS/Inline_formatting_context

**CSS Specifications (Beyond 2.1):**
- CSS Display Module Level 3: https://www.w3.org/TR/css-display-3/
- CSS Inline Layout Module Level 3: https://www.w3.org/TR/css-inline-3/
- CSS Box Model Module Level 3: https://www.w3.org/TR/css-box-3/

### Browser Implementation References

**Servo (Rust):**
- Repository: https://github.com/servo/servo
- Layout 2020 Code: https://github.com/servo/servo/tree/main/components/layout_2020
- Block Layout: `components/layout_2020/flow/block.rs`
- Inline Layout: `components/layout_2020/flow/inline.rs`

**WebKit (C++):**
- Repository: https://github.com/WebKit/WebKit
- Layout Code: `Source/WebCore/layout/`
- Block Formatting: `Source/WebCore/layout/blockformatting/`
- Inline Formatting: `Source/WebCore/layout/inlineformatting/`

**Chromium/Blink (C++):**
- Repository: https://chromium.googlesource.com/chromium/src/
- Layout Code: `third_party/blink/renderer/core/layout/`

### Academic Papers

**"Cascading Style Sheets (CSS): The Definitive Guide" by Eric A. Meyer**
- Comprehensive explanation of CSS layout algorithms
- Historical context and design decisions

**"The CSS Saga" by Håkon Wium Lie**
- History of CSS development
- Design rationale for key features

### Tools for Testing

**Browser DevTools:**
- Chrome DevTools: Inspect computed styles, layout tree
- Firefox DevTools: Box model visualization, layout debugging
- Safari Web Inspector: Layers view, paint flashing

**Test Suites:**
- CSS WG Test Suite: https://test.csswg.org/
- Web Platform Tests: https://wpt.fyi/

---

## Research Summary

- **Pages of Specification Read:** ~150 pages
- **Ambiguities Identified:** 12
- **Implementation Areas Covered:** 10
- **Key Algorithms Documented:** 15+

### Summary Statistics

**Sections covered:**
- ✅ Box Generation (9.2)
- ✅ Positioning Schemes (9.3)
- ✅ Containing Block (10.1)
- ✅ Block Formatting Context (9.4.1)
- ✅ Inline Formatting Context (9.4.2)
- ✅ Relative Positioning (9.4.3)
- ✅ Floats (9.5)
- ✅ Absolute Positioning (9.6)
- ✅ Display/Position/Float Relationships (9.7)
- ✅ Layered Presentation (9.9)
- ✅ Width Computation (10.3 - all subsections)
- ✅ Height Computation (10.5-10.6)
- ✅ Line Height (10.8)
- ✅ Margin Collapsing (8.3.1)

**Algorithms with pseudocode:**
- [x] Block formatting context layout
- [x] Inline formatting context layout
- [x] Line breaking
- [x] Baseline alignment
- [x] Float positioning (9 rules)
- [x] Clearance calculation
- [x] Width constraint resolution (10.3.3)
- [x] Absolute positioning width resolution (10.3.7)
- [x] Shrink-to-fit width
- [x] Auto height calculation
- [x] Margin collapsing algorithm
- [x] Stacking context painting order

### Validation

All objectives outlined at the start of this research have been achieved:

- ✅ Complete notes file with all required sections
- ✅ Box generation rules documented with examples
- ✅ Each formatting context type explained with pseudocode
- ✅ Width computation algorithms documented for all scenarios
- ✅ Height computation algorithms documented for all scenarios
- ✅ Margin collapsing algorithm documented with edge cases
- ✅ 12 ambiguities/questions identified and documented
- ✅ Key spec quotes captured with section references
- ✅ Cross-references to relevant implementation areas
- ✅ Notes structured for easy reference during implementation

### Next Steps

**For implementation teams:**

1. Define the box node type and box generation rules (see Box Generation section).
2. Implement block layout (use BFC, Width, Height, and Margin Collapsing sections).
3. Implement inline layout after integrating text shaping (see IFC and Line Height sections).
4. Add float and positioned layout support (see Floats and Positioning sections).
5. Build the paint system and finalize integration (see Layered Presentation section).

**For testing:**
- Create test cases for each ambiguity identified
- Verify against real browsers (Chrome, Firefox, Safari)
- Document any deviations from spec

**For future research:**
- Table layout (CSS 2.1 Chapter 17) - separate research recommended
- Flexbox (CSS Flexible Box Module Level 1)
- Grid (CSS Grid Layout Module Level 1)

---

## Acknowledgments

This research is based on the CSS 2.1 Specification, a product of the CSS Working Group at W3C.

**Specification Editors:**
- Bert Bos
- Tantek Çelik
- Ian Hickson
- Håkon Wium Lie

**Contributors:**
- Hundreds of CSS WG members over 20+ years
- Browser implementers who clarified ambiguities through implementation experience
- The web development community who identified edge cases and bugs

---

**End of Research Notes**

*These notes are a living document. As implementation proceeds, new questions and edge cases will be discovered. Document them here and update the ambiguities section accordingly.*
