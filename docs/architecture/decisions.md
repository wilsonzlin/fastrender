# Architecture Decisions

**Version:** 1.0
**Last Updated:** 2025-01-19
**Status:** Living Document

## Purpose

This document records the fundamental architectural decisions for FastRender V2. Each decision includes context, options considered, rationale, and implementation guidance.

## Decision Log

### ADR-001: Separate Box Tree from Fragment Tree

**Date:** Phase 0
**Status:** Accepted

**Context:**
Current implementation conflates styled DOM nodes with layout results. This makes it impossible to:
- Reuse layout across different viewports
- Implement incremental layout
- Clearly separate styling from layout concerns

**Options Considered:**

1. **Status Quo**: Single `StyledNode` type with layout info
   - Pro: Simple, less code
   - Con: Violates separation of concerns
   - Con: Cannot reuse boxes across layouts

2. **Servo Model**: Separate Box Tree and Fragment Tree
   - Pro: Clean separation
   - Pro: Matches spec mental model
   - Pro: Enables future optimizations
   - Con: More complexity
   - Con: More memory usage

3. **Hybrid**: Styled nodes create fragments directly
   - Pro: Simpler than option 2
   - Con: Still mixes concerns
   - Con: Hard to optimize later

**Decision:** Option 2 - Separate Box Tree and Fragment Tree

**Rationale:**
- Matches how CSS specs describe layout
- Browsers use this model for good reasons
- Enables clean architecture
- Future-proof for incremental layout
- Memory cost is acceptable (<10% overhead)

**Implementation:**

```rust
// src/tree/box_tree.rs
pub struct BoxTree {
    root: Box<BoxNode>,
}

pub struct BoxNode {
    style: Arc<ComputedStyle>,
    box_type: BoxType,
    children: Vec<Box<BoxNode>>,
}

// src/tree/fragment.rs
pub struct FragmentTree {
    root: Fragment,
}

pub struct Fragment {
    bounds: Rect,
    content: FragmentContent,
    style: Arc<ComputedStyle>,  // Shared with BoxNode
    children: Vec<Fragment>,
}
```

**Verification:**
- Box tree is immutable after construction
- Fragment tree is rebuilt on each layout
- No layout information in BoxNode
- No style resolution in Fragment

---

### ADR-002: Independent Layout Algorithm Implementations

**Date:** Phase 0
**Status:** Accepted

**Context:**
Different CSS display types have fundamentally different layout algorithms:
- Block: stack vertically, margin collapsing
- Inline: line breaking, baseline alignment
- Flex: main/cross axis, flex factors
- Grid: track sizing, area placement
- Table: column sizing, rowspan/colspan

Current implementation tries to force tables into flexbox, which fails for:
- Colspan/rowspan (no equivalent in flex)
- Auto width (requires analyzing all cells)
- Border collapse (table-specific)

**Options Considered:**

1. **Unified Engine**: One algorithm handles all (current approach)
   - Pro: Less code
   - Con: Impossible for table vs flex vs inline
   - Con: Leads to hacks and workarounds

2. **Independent Algorithms**: One implementation per FC type
   - Pro: Each can be spec-compliant
   - Pro: Matches browser architecture
   - Pro: Easier to understand/maintain
   - Con: More code
   - Con: Need coordination between algorithms

3. **Taffy for Everything**: Force all into flex/grid
   - Pro: Leverage existing library
   - Con: Cannot support table/inline properly
   - Con: Not spec-compliant

**Decision:** Option 2 - Independent Implementations

**Rationale:**
- Each layout mode has unique algorithms (per spec)
- Cannot force table into flex without losing correctness
- Inline layout is fundamentally different (line breaking)
- Browser architecture validates this approach
- Code duplication is minimal (each is focused)

**Implementation:**

```rust
// src/layout/mod.rs
pub trait LayoutAlgorithm {
    fn layout(
        &self,
        box_node: &BoxNode,
        constraints: Constraints,
    ) -> Fragment;
}

// src/layout/block.rs
pub struct BlockLayout;
impl LayoutAlgorithm for BlockLayout { /* ... */ }

// src/layout/inline.rs
pub struct InlineLayout;
impl LayoutAlgorithm for InlineLayout { /* ... */ }

// src/layout/flex.rs
pub struct FlexLayout { taffy: TaffyTree }
impl LayoutAlgorithm for FlexLayout { /* delegates to Taffy */ }

// src/layout/grid.rs
pub struct GridLayout { taffy: TaffyTree }
impl LayoutAlgorithm for GridLayout { /* delegates to Taffy */ }

// src/layout/table.rs
pub struct TableLayout;
impl LayoutAlgorithm for TableLayout { /* custom implementation */ }
```

**Verification:**
- Each layout mode passes WPT tests for that mode
- No cross-contamination (table code doesn't touch flex)
- Clear algorithm per CSS spec section

---

### ADR-003: Taffy Integration Scope

**Date:** Phase 0
**Status:** Accepted

**Context:**
Taffy is a high-quality flexbox/grid implementation. Current codebase vendors Taffy but misuses it for table layout.

**Options Considered:**

1. **Taffy for All Layout**: Force table/inline into Taffy
   - Pro: Minimal code
   - Con: Already proven impossible (current codebase)

2. **Fork Taffy**: Add table/inline to Taffy
   - Pro: Everything in one library
   - Con: Table doesn't fit Taffy's architecture
   - Con: Maintenance burden

3. **Limited Taffy**: Use only for flex/grid
   - Pro: Use Taffy's strengths
   - Pro: Full control over table/inline
   - Con: Need integration layer

4. **No Taffy**: Implement everything custom
   - Pro: Full control
   - Con: Reinventing excellent flex/grid code

**Decision:** Option 3 - Limited Taffy Usage

**Rationale:**
- Taffy's flex/grid is production-quality
- Table layout cannot fit Taffy's model (requires column-wise constraints)
- Inline layout needs line-breaking (not in Taffy)
- Integration layer is small cost
- Can still vendor Taffy for stability

**Implementation:**

```rust
// src/layout/flex.rs
pub struct FlexLayout {
    taffy: TaffyTree<()>,
}

impl LayoutAlgorithm for FlexLayout {
    fn layout(&self, box_node: &BoxNode, constraints: Constraints) -> Fragment {
        // 1. Convert BoxNode → Taffy nodes
        let taffy_root = self.build_taffy_tree(box_node);

        // 2. Run Taffy layout
        self.taffy.compute_layout(taffy_root, constraints.into())?;

        // 3. Convert Taffy result → Fragment
        self.extract_fragments(taffy_root, box_node)
    }
}

// Similar for GridLayout
```

**Scope:**
- ✅ Use Taffy for: Flexbox, Grid
- ❌ Don't use Taffy for: Block, Inline, Table, Float, Positioned

**Verification:**
- Flex layout passes all WPT flexbox tests
- Grid layout passes all WPT grid tests
- No Taffy code in table/inline implementations

---

### ADR-004: Formatting Context Abstraction

**Date:** Phase 0
**Status:** Accepted

**Context:**
CSS has distinct formatting context types (BFC, IFC, FFC, GFC, TFC). Each has different rules for how children are laid out.

**Options Considered:**

1. **No Abstraction**: Each layout algorithm is standalone
   - Pro: Simple
   - Con: Miss shared concepts (containing block, etc.)

2. **FormattingContext Trait**: Common interface
   - Pro: Clear abstraction
   - Pro: Enables future FC types (ruby, etc.)
   - Con: Forces common interface (may not fit all)

3. **Enum Dispatch**: FormattingContext enum
   - Pro: Type safety
   - Pro: Exhaustive matching
   - Con: Harder to extend

**Decision:** Option 2 - Trait-based abstraction

**Rationale:**
- Clear interface for all FC types
- Allows different implementations
- Matches CSS spec terminology
- Extensible for future FC types

**Implementation:**

```rust
// src/layout/formatting_context.rs
pub trait FormattingContext {
    fn layout(
        &mut self,
        box_node: &BoxNode,
        containing_block: ContainingBlock,
    ) -> Fragment;
}

pub struct BlockFormattingContext {
    // BFC-specific state
}

impl FormattingContext for BlockFormattingContext {
    fn layout(&mut self, box_node: &BoxNode, cb: ContainingBlock) -> Fragment {
        // BFC layout algorithm
    }
}

// Similar for IFC, FFC, GFC, TFC
```

**Verification:**
- All FC types implement trait
- Each FC passes spec tests
- Clear separation between FC types

---

### ADR-005: Display List Construction

**Date:** Phase 0
**Status:** Accepted

**Context:**
Need to convert fragment tree into rendering operations. Two approaches:
1. Paint directly from fragments
2. Build intermediate display list

**Options Considered:**

1. **Direct Painting**: Fragment → pixels
   - Pro: Simple, fewer steps
   - Con: Hard to optimize (sorting, culling)
   - Con: Mixes layout and rendering

2. **Display List**: Fragment → DisplayList → pixels
   - Pro: Separates concerns
   - Pro: Enables optimizations (layer caching, GPU)
   - Pro: Standard browser architecture
   - Con: Extra data structure

**Decision:** Option 2 - Display List

**Rationale:**
- Enables future optimizations (GPU rendering)
- Separates layout from rasterization
- Standard browser approach
- Allows display list inspection/debugging
- Small memory cost for large benefits

**Implementation:**

```rust
// src/paint/display_list.rs
pub struct DisplayList {
    items: Vec<DisplayItem>,
}

pub enum DisplayItem {
    FillRect { rect: Rect, color: Color },
    DrawText { glyphs: Vec<Glyph>, color: Color },
    DrawImage { image: Arc<Pixmap>, dest: Rect },
    PushClip { rect: Rect },
    PopClip,
    PushTransform { transform: Transform },
    PopTransform,
    PushOpacity { opacity: f32 },
    PopOpacity,
}

// src/paint/display_list_builder.rs
pub fn build_display_list(fragment: &Fragment) -> DisplayList {
    // Walk fragment tree in paint order
    // Emit display items
}

// src/paint/rasterizer.rs
pub fn rasterize(display_list: &DisplayList) -> Pixmap {
    // Execute display items with tiny-skia
}
```

**Verification:**
- Fragment tree is never modified during paint
- Display list can be serialized/inspected
- Rasterizer only knows about DisplayList, not Fragments

---

### ADR-006: Text Shaping Pipeline

**Date:** Phase 0
**Status:** Accepted

**Context:**
Text rendering requires multiple steps: bidi, itemization, shaping, layout.
Need to decide architecture.

**Options Considered:**

1. **All-in-one**: Single function does everything
   - Pro: Simple
   - Con: Hard to test/debug
   - Con: Violates single responsibility

2. **Pipeline**: Separate stages
   - Pro: Testable
   - Pro: Clear stages
   - Con: More complex

**Decision:** Option 2 - Pipeline Architecture

**Rationale:**
- Text is complex, needs clear stages
- Each stage is independently testable
- Matches ICU/HarfBuzz architecture
- Easier to debug (inspect intermediate results)

**Implementation:**

```rust
// src/text/pipeline.rs

// Stage 1: Unicode analysis
pub fn analyze_text(text: &str) -> AnalyzedText {
    // Bidi levels (unicode-bidi)
    // Script runs (unicode-script)
    // Language detection
}

// Stage 2: Font itemization
pub fn itemize_fonts(
    analyzed: &AnalyzedText,
    requested_fonts: &[String],
    font_cache: &FontCache,
) -> Vec<FontRun> {
    // Match script to font
    // Handle font fallback for missing glyphs
}

// Stage 3: Shaping
pub fn shape_runs(
    font_runs: &[FontRun],
    font_cache: &FontCache,
) -> Vec<ShapedRun> {
    // rustybuzz shaping
    // Apply letter-spacing, word-spacing
}

// Stage 4: Line breaking
pub fn break_lines(
    shaped: &[ShapedRun],
    max_width: f32,
) -> Vec<Line> {
    // Find break opportunities (unicode-linebreak)
    // Measure line widths
    // Break at opportunities
}

// Stage 5: Alignment
pub fn align_lines(
    lines: &[Line],
    text_align: TextAlign,
    available_width: f32,
) -> Vec<AlignedLine> {
    // left/right/center/justify
}
```

**Verification:**
- Each stage is pure function
- Each stage has unit tests
- Can inspect intermediate results

---

### ADR-007: Error Handling Strategy

**Date:** Phase 0
**Status:** Accepted

**Context:**
Rendering untrusted HTML/CSS requires robust error handling.

**Options Considered:**

1. **Panic on Error**: Crash on invalid input
   - Pro: Simple
   - Con: DoS vulnerability

2. **Silent Fallback**: Ignore errors, render what we can
   - Pro: Keeps rendering
   - Con: Silent failures hard to debug

3. **Error Propagation**: Return Result<>, report errors
   - Pro: Caller decides
   - Pro: Errors are visible
   - Con: More complex

**Decision:** Option 3 - Error Propagation

**Rationale:**
- Security: Never panic on user input
- Debuggability: Errors must be visible
- Flexibility: Caller can decide (ignore vs abort)
- Industry standard (browsers do this)

**Implementation:**

```rust
// src/error.rs
#[derive(Debug, thiserror::Error)]
pub enum RenderError {
    #[error("HTML parse error: {0}")]
    HtmlParse(String),

    #[error("CSS parse error: {0}")]
    CssParse(String),

    #[error("Layout error: {0}")]
    Layout(String),

    #[error("Font error: {0}")]
    Font(String),

    #[error("Raster error: {0}")]
    Raster(String),
}

pub type Result<T> = std::result::Result<T, RenderError>;

// Usage:
pub fn render(html: &str) -> Result<Pixmap> {
    let dom = parse_html(html)?;  // Propagate error
    let box_tree = build_box_tree(&dom)?;
    let fragments = layout(&box_tree)?;
    let pixmap = rasterize(&fragments)?;
    Ok(pixmap)
}
```

**Error Handling Rules:**
1. Never panic on user input
2. Always return Result for public APIs
3. Use `?` operator for error propagation
4. Include context in error messages
5. Log errors but don't print to stderr (library shouldn't own output)

**Verification:**
- Fuzzing doesn't find panics
- All public functions return Result
- Error messages include useful context

---

### ADR-008: Memory Management Strategy

**Date:** Phase 0
**Status:** Accepted

**Context:**
Large DOM trees and complex layouts can use significant memory.
Need strategy for managing memory usage.

**Options Considered:**

1. **Unlimited**: Keep everything in memory
   - Pro: Simple
   - Con: Can OOM on large pages

2. **Streaming**: Process in chunks
   - Pro: Bounded memory
   - Con: Complex, may not fit layout model

3. **Shared Ownership**: Use Arc for shared data
   - Pro: Reduces duplication
   - Pro: Standard Rust pattern
   - Con: Reference counting overhead

**Decision:** Option 3 - Strategic Arc Usage

**Rationale:**
- ComputedStyle is shared between Box and Fragment
- Fonts are shared across text runs
- Images are shared if repeated
- Memory overhead is acceptable vs duplication

**Implementation:**

```rust
// Share expensive data
pub struct BoxNode {
    style: Arc<ComputedStyle>,  // Shared with Fragment
    // ...
}

pub struct Fragment {
    style: Arc<ComputedStyle>,  // Same Arc as BoxNode
    // ...
}

// Share fonts
pub struct FontCache {
    cache: HashMap<FontKey, Arc<Font>>,
}

// Share images
pub struct ImageCache {
    cache: HashMap<String, Arc<Pixmap>>,
}
```

**Memory Budget:**
- Target: <50MB for typical web page
- Limit: <500MB for complex pages
- Mechanism: Track allocations, warn if exceeding

**Verification:**
- Memory profiling shows shared data
- No unnecessary clones
- Arc usage is intentional and documented

---

### ADR-009: Testing Strategy

**Date:** Phase 0
**Status:** Accepted

**Context:**
Rendering engine correctness is critical. Need comprehensive testing.

**Options Considered:**

1. **Manual Testing**: Visual inspection
   - Pro: Easy to start
   - Con: Not scalable
   - Con: No regression detection

2. **Unit Tests Only**: Test functions
   - Pro: Fast
   - Con: Misses integration issues

3. **Multi-Level**: Unit + Integration + Reference tests
   - Pro: Comprehensive
   - Pro: Catches all bug types
   - Con: Complex test infrastructure

**Decision:** Option 3 - Multi-Level Testing

**Rationale:**
- Unit tests verify algorithms
- Integration tests verify features
- Reference tests verify visual correctness
- WPT tests verify spec compliance
- All are needed for production quality

**Implementation:**

```
tests/
├── unit/                    # Fast, focused tests
│   ├── layout/
│   │   ├── block_test.rs
│   │   ├── inline_test.rs
│   │   └── table_test.rs
│   ├── text/
│   │   └── shaping_test.rs
│   └── paint/
│       └── display_list_test.rs
│
├── integration/             # Feature tests
│   ├── flexbox_test.rs
│   ├── grid_test.rs
│   └── text_test.rs
│
├── reference/               # Pixel-comparison
│   ├── tests/
│   │   ├── block-001.html
│   │   ├── flex-001.html
│   │   └── ...
│   ├── expected/
│   │   ├── block-001.png
│   │   ├── flex-001.png
│   │   └── ...
│   └── harness.rs
│
└── wpt/                     # Web Platform Tests
    └── (git submodule)
```

**Test Types:**

1. **Unit Tests**: Algorithm correctness
2. **Integration Tests**: Feature completeness
3. **Reference Tests**: Visual correctness
4. **WPT Tests**: Spec compliance
5. **Fuzz Tests**: Robustness
6. **Benchmark Tests**: Performance

**Coverage Goals:**
- Unit tests: >80% line coverage
- Integration tests: All major features
- Reference tests: >1000 test cases
- WPT tests: Run official test suite

**Verification:**
- CI runs all test types
- Coverage report generated
- Performance tracked over time

---

## Decision Matrix

| Decision | Impact | Complexity | Priority |
|----------|--------|------------|----------|
| ADR-001: Separate Trees | High | Medium | P0 |
| ADR-002: Independent Layouts | High | High | P0 |
| ADR-003: Limited Taffy | Medium | Low | P0 |
| ADR-004: FC Abstraction | Medium | Medium | P0 |
| ADR-005: Display List | Medium | Medium | P1 |
| ADR-006: Text Pipeline | High | High | P0 |
| ADR-007: Error Handling | High | Low | P0 |
| ADR-008: Memory Strategy | Medium | Low | P1 |
| ADR-009: Testing Strategy | High | High | P0 |

## Implementation Order

Based on dependencies:

1. **Phase 1 Blockers** (must have before coding):
   - ADR-001: Separate Trees
   - ADR-002: Independent Layouts
   - ADR-004: FC Abstraction
   - ADR-007: Error Handling

2. **Phase 1 Nice-to-Have**:
   - ADR-005: Display List (can paint directly initially)
   - ADR-008: Memory Strategy (can optimize later)

3. **Continuous**:
   - ADR-009: Testing (ongoing)

## Revising Decisions

Decisions can be revised if:
- New information contradicts assumptions
- Implementation reveals better approach
- Spec changes require adaptation

Process for revisions:
1. Document new context
2. Re-evaluate options
3. Update this document with "Superseded by ADR-XXX"
4. Create new ADR with revised decision

## Questions & Discussion

Track open questions here:

**Q:** Should we support streaming layout?
**A:** Not in V1. Adds complexity, benefit unclear for our use case.

**Q:** How to handle vendor prefixes (-webkit-, -moz-)?
**A:** Strip them during CSS parsing, don't process.

**Q:** Support CSS preprocessors (SCSS, Less)?
**A:** No. They should compile to CSS before rendering.

---

**Next Steps:**
After understanding all architectural decisions, proceed to:
- [`docs/core/type-system.md`](01-type-system.md)
