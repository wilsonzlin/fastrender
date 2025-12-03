# Phase 0: Foundation Research

**Duration:** 2 weeks
**Prerequisites:** None
**Output:** Complete understanding of CSS layout, architecture decisions documented

## Objectives

1. Understand how CSS layout actually works (not how we think it works)
2. Study how real browsers implement layout engines
3. Document architectural decisions with rationale
4. Create mental model for the entire system

## Week 1: Specification Deep Dive

### Day 1-2: CSS 2.1 Visual Formatting Model

**Primary Source:** CSS 2.1 Specification, Chapters 9-10

**Tasks:**
1. Download specification:
   ```bash
   cd /home/user/fastrender/docs/references/specs
   wget https://www.w3.org/TR/CSS21/CSS21.pdf
   ```

2. Read and annotate Chapter 9: Visual Formatting Model
   - 9.1 Introduction to the visual formatting model
   - 9.2 Controlling box generation
   - 9.3 Positioning schemes
   - 9.4 Normal flow
   - 9.5 Floats
   - 9.6 Absolute positioning
   - 9.7 Relationships between 'display', 'position', and 'float'
   - 9.8 Comparison of normal flow, floats, and absolute positioning
   - 9.9 Layered presentation
   - 9.10 Text direction

3. Read Chapter 10: Visual formatting model details
   - 10.1 Definition of "containing block"
   - 10.2 Content width
   - 10.3 Calculating widths and margins
   - 10.4 Minimum and maximum widths
   - 10.5 Content height
   - 10.6 Calculating heights and margins
   - 10.7 Minimum and maximum heights
   - 10.8 Line height calculations

**Deliverable:** Create `/docs/research/visual-formatting-model.md`

Document should include:
- Summary of each section in your own words
- Key algorithms with pseudocode
- Questions/clarifications needed
- Connections to our implementation

**Example format:**
```markdown
## 9.4 Normal Flow

### Summary
Normal flow includes block formatting, inline formatting, and relative positioning.
Boxes in normal flow belong to a formatting context (block or inline).

### Key Concepts
- **Formatting Context**: Environment in which boxes are laid out
- **Block-level boxes**: Generate block-level principal box, participate in BFC
- **Inline-level boxes**: Generate inline-level boxes, participate in IFC

### Algorithm: Block Formatting Context Layout
1. Stack block-level boxes vertically
2. Compute each box's position relative to containing block
3. Handle margin collapsing between adjacent boxes
4. Boxes take full width of containing block (unless floated/positioned)

### Implementation Notes
- We need separate BFC and IFC implementations
- Margin collapsing is complex - separate algorithm needed
- Containing block calculation is critical - affects all sizing

### Questions
- How do we handle percentage heights when parent height is auto?
- What's the exact margin collapsing algorithm?
```

### Day 3: Display and Formatting Contexts

**Primary Source:** CSS Display Module Level 3

**Tasks:**
1. Download specification:
   ```bash
   cd /home/user/fastrender/docs/references/specs
   wget https://www.w3.org/TR/css-display-3/css-display-3.pdf
   ```

2. Read key sections:
   - 2. Box Layout Modes: the `display` property
   - 3. Formatting Contexts
   - 4. Display Types
   - 5. Transformations of `display`

3. Create types diagram:
   ```
   Display Type
   ├── Outer Display Type (how box participates in parent's FC)
   │   ├── block
   │   ├── inline
   │   └── run-in
   └── Inner Display Type (formatting context for children)
       ├── flow (block and inline)
       ├── flow-root (establishes BFC)
       ├── table
       ├── flex
       ├── grid
       └── ruby
   ```

**Deliverable:** Create `/docs/research/formatting-contexts.md`

Must include:
- Complete list of formatting context types
- Rules for establishing each FC type
- Interaction between different FCs
- Anonymous box generation rules

### Day 4: Box Model and Sizing

**Primary Source:** CSS Box Model Module Level 3

**Tasks:**
1. Download specification:
   ```bash
   cd /home/user/fastrender/docs/references/specs
   wget https://www.w3.org/TR/css-box-3/css-box-3.pdf
   ```

2. Study:
   - Box model diagram (content, padding, border, margin)
   - Box-sizing property
   - Margin, border, padding properties
   - Width and height computation

**Deliverable:** Create `/docs/research/box-model.md`

Include:
- Precise definitions of content-box vs border-box
- Calculation order (what depends on what)
- Percentage resolution rules
- Auto value resolution

### Day 5: Table Layout

**Primary Source:** CSS Tables Module Level 3

**Tasks:**
1. Download specification:
   ```bash
   cd /home/user/fastrender/docs/references/specs
   wget https://www.w3.org/TR/css-tables-3/css-tables-3.pdf
   ```

2. Critical sections:
   - 3. Table structure
   - 4. Width computation
   - 5. Height computation
   - 6. Fixed table layout algorithm
   - 7. Auto table layout algorithm

**Deliverable:** Create `/docs/research/table-layout.md`

Must include:
- Complete table structure (wrapper, cells, rows, columns)
- Anonymous table box generation algorithm
- Fixed layout width algorithm (pseudocode)
- Auto layout width algorithm (pseudocode)
- Differences between the two
- Why flexbox cannot emulate tables

**Critical Understanding:**
```markdown
## Why Flexbox Cannot Replace Tables

### Table Features Not in Flexbox:
1. **Column-based sizing**: Table columns are sized based on ALL cells
   in the column, not just parent constraints

2. **Colspan/Rowspan**: Cells can span multiple rows/columns, affecting
   neighboring cells' sizes

3. **Auto layout**: Content in ANY cell can affect width of entire column

4. **Baseline alignment**: Table cells align to row baseline, complex
   algorithm involving all cells

5. **Border collapse**: Borders between cells collapse with specific
   precedence rules

### Example That Breaks in Flexbox:
```html
<table>
  <tr>
    <td>Short</td>
    <td>Very long content that should make this column wider</td>
  </tr>
  <tr>
    <td colspan="2">Spanning cell</td>
  </tr>
</table>
```

In tables: Second column sized by longest content, spanning cell works
In flexbox: Cannot express column constraints, colspan impossible
```

### Day 6-7: Inline Layout

**Primary Source:** CSS Inline Layout Module Level 3

**Tasks:**
1. Download specification:
   ```bash
   cd /home/user/fastrender/docs/references/specs
   wget https://www.w3.org/TR/css-inline-3/css-inline-3.pdf
   ```

2. Study thoroughly (this is complex):
   - Line box model
   - Inline box model
   - Text alignment
   - Vertical alignment (baseline, top, bottom, middle)
   - Line breaking
   - Text fragments

**Deliverable:** Create `/docs/research/inline-layout.md`

This is CRITICAL - we're currently missing inline layout entirely.

Must include:
- Line box construction algorithm
- Inline box stacking algorithm
- Baseline alignment rules
- Vertical-align property effects
- Text-align property effects
- White-space handling

**Example algorithm:**
```markdown
## Line Box Construction Algorithm

Input: Sequence of inline-level boxes
Output: Sequence of line boxes

1. Create empty line box
2. For each inline-level box:
   a. Try to fit box on current line
   b. If fits: add to line, advance position
   c. If doesn't fit:
      - If line is empty: force fit (may overflow)
      - Else: finish current line, start new line
3. Align content within each line (text-align)
4. Align boxes vertically within each line (vertical-align)
5. Position line boxes vertically (stack top-to-bottom)

Key complexity: vertical-align can affect line box height,
which can affect subsequent box positions (iterative)
```

## Week 2: Implementation Study

### Day 1-2: Servo Layout Engine

**Repository:** https://github.com/servo/servo

**Tasks:**
1. Clone Servo:
   ```bash
   cd /home/user/fastrender/docs/references/browsers
   git clone --depth 1 https://github.com/servo/servo.git
   ```

2. Navigate to layout implementation:
   ```bash
   cd servo/components/layout_2020
   ```

3. Study these files in order:
   ```
   layout_2020/
   ├── flow/mod.rs              # Entry point - understand the flow
   ├── formatting_context/      # FC implementations
   │   ├── mod.rs
   │   ├── independent.rs       # BFC, IFC, etc.
   │   └── inline.rs            # IFC implementation
   ├── flow/block.rs            # Block layout
   ├── flow/inline.rs           # Inline layout
   ├── flow/flex.rs             # Flexbox
   ├── table/mod.rs             # Table layout
   ├── fragments/mod.rs         # Fragment tree
   └── display_list/mod.rs      # Display list
   ```

4. For each file, document:
   - Main data structures
   - Key algorithms
   - How it connects to other modules
   - Clever solutions to hard problems
   - Things we should emulate
   - Things we should avoid

**Deliverable:** Create `/docs/research/servo-analysis.md`

**Key things to extract:**
```markdown
## Servo's Fragment Tree

Servo separates Box Tree from Fragment Tree:

### Box Tree (flow/mod.rs)
- Represents CSS boxes
- Built from styled DOM
- Immutable after construction
- Owns ComputedStyles

### Fragment Tree (fragments/mod.rs)
- Represents laid-out boxes
- Built by layout algorithms
- Contains concrete positions/sizes
- Multiple fragments per box (line breaking)

### Why This Separation Matters
- Box tree is reusable across layouts
- Fragment tree is throwaway (rebuilt on resize)
- Clean separation of concerns
- Enables incremental layout (future)

### Our Implementation
We should adopt this separation:
- Box Tree: src/tree/box_tree.rs
- Fragment Tree: src/tree/fragment.rs
```

### Day 3: WebKit Layout

**Repository:** https://github.com/WebKit/WebKit

**Tasks:**
1. Clone WebKit:
   ```bash
   cd /home/user/fastrender/docs/references/browsers
   git clone --depth 1 https://github.com/WebKit/WebKit.git
   ```

2. Study (C++ code, focus on structure not implementation):
   ```
   Source/WebCore/layout/
   ├── LayoutState.cpp          # Main layout state
   ├── blockformatting/         # BFC
   ├── inlineformatting/        # IFC
   ├── formattingContexts/      # FC abstraction
   └── tableformatting/         # Table layout
   ```

3. Compare with Servo's approach

**Deliverable:** Add to `/docs/research/servo-analysis.md` with comparison

### Day 4: Taffy Deep Dive

**Repository:** Already in vendor/taffy

**Tasks:**
1. Study Taffy's architecture:
   ```
   vendor/taffy/src/
   ├── tree/                    # Tree structure
   ├── compute/                 # Layout algorithms
   │   ├── flexbox.rs
   │   ├── grid.rs
   │   └── block.rs
   └── style/                   # Style types
   ```

2. Understand:
   - What Taffy does well (flex, grid)
   - What Taffy doesn't support (table, inline, float)
   - How to integrate Taffy for flex/grid only
   - Taffy's API surface

**Deliverable:** Create `/docs/research/taffy-integration.md`

Must answer:
- What layout modes should use Taffy?
- What layout modes need custom implementation?
- How do we convert Box → Taffy → Fragment?
- Can we use Taffy incrementally?

### Day 5-7: Architecture Decision Record

**Tasks:**
Synthesize all research into concrete decisions.

**Deliverable:** Create `/docs/architecture/decisions.md`

Must include:

```markdown
# Architecture Decisions

## Decision 1: Separate Box Tree from Fragment Tree

**Context:** Need to represent CSS boxes separate from layout results

**Options:**
1. Single tree (current approach) - boxes have position/size
2. Separate trees (Servo/WebKit) - boxes → layout → fragments

**Decision:** Separate trees (Option 2)

**Rationale:**
- Allows box tree reuse across layouts
- Clean separation of concerns
- Enables future optimizations (incremental layout)
- Matches spec model more closely

**Implementation:**
- src/tree/box_tree.rs - immutable box representation
- src/tree/fragment.rs - mutable layout results
- Clear transformation: Box → Layout → Fragment

**Trade-offs:**
- More complex architecture
- More memory (two trees)
- Better long-term maintainability

---

## Decision 2: Independent Layout Algorithm Implementations

**Context:** Different display types need different layout algorithms

**Options:**
1. Unified layout engine (current) - one algorithm for all
2. Independent implementations (Servo) - one per FC type
3. Taffy for everything - force all into flex/grid

**Decision:** Independent implementations (Option 2)

**Rationale:**
- Each FC has different constraints and algorithms
- Table cannot be expressed in flex (colspan, auto-width)
- Inline layout is fundamentally different from block
- Matches browser architecture

**Implementation:**
- src/layout/block.rs - BFC
- src/layout/inline.rs - IFC
- src/layout/flex.rs - FFC (delegates to Taffy)
- src/layout/grid.rs - GFC (delegates to Taffy)
- src/layout/table.rs - TFC (custom implementation)

---

## Decision 3: Taffy Integration Scope

**Context:** Taffy is excellent for flex/grid, lacks table/inline

**Options:**
1. Use Taffy for all layout (current broken approach)
2. Fork Taffy and add table support
3. Use Taffy only for flex/grid, custom for others

**Decision:** Option 3 - Limited Taffy use

**Rationale:**
- Taffy's flex/grid implementation is solid
- Table layout is fundamentally different (cannot fit in Taffy's model)
- Inline layout requires line breaking (not in Taffy)
- Custom table implementation gives us full control

**Implementation:**
- FlexLayout wraps Taffy for FFC
- GridLayout wraps Taffy for GFC
- TableLayout is custom implementation
- InlineLayout is custom implementation
- BlockLayout is simple custom implementation

---

[Continue with more decisions...]
```

## Acceptance Criteria

Phase 0 is complete when:

- [ ] All specifications downloaded and read
- [ ] `/docs/research/` contains complete analysis of:
  - [ ] visual-formatting-model.md
  - [ ] formatting-contexts.md
  - [ ] box-model.md
  - [ ] table-layout.md
  - [ ] inline-layout.md
  - [ ] servo-analysis.md
  - [ ] taffy-integration.md
- [ ] `/docs/architecture/decisions.md` exists with:
  - [ ] Decision on tree structure (box vs fragment)
  - [ ] Decision on FC implementation strategy
  - [ ] Decision on Taffy integration scope
  - [ ] Decision on layout algorithm architecture
  - [ ] Decision on paint architecture
  - [ ] Decision on text shaping architecture
- [ ] All decisions have clear rationale
- [ ] All decisions reference spec sections
- [ ] Implementation approach is clear for Phase 1

## Common Pitfalls for AI Agents

1. **Don't skim specifications** - Read every word, understand every algorithm
2. **Don't assume knowledge** - CSS has subtle rules, verify everything
3. **Don't skip browser code study** - They solved problems we'll face
4. **Don't rush to code** - This research prevents months of rework
5. **Don't skip documentation** - Write down what you learn

## Next Phase

After completing this phase, proceed to:
- [`docs/core/type-system.md`](01-type-system.md)

The architecture decisions made here will guide all subsequent implementation.
