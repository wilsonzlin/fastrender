# Implementation Checklist

**Purpose:** Exhaustive, ordered task list for rebuilding FastRender
**Format:** Checkbox list - AI agents should update as tasks complete
**Rule:** Complete tasks in order unless explicitly marked as parallelizable

## Legend

- `[ ]` Not started
- `[~]` In progress
- `[x]` Complete
- `[!]` Blocked (describe blocker in notes)
- `[||]` Can be done in parallel with siblings

## Phase 0: Foundation Research

### Week 1: Specification Study

- [ ] **0.1** Download all specifications
  - [ ] 0.1.1 Download CSS 2.1 spec PDF
  - [ ] 0.1.2 Download CSS Display Module Level 3
  - [ ] 0.1.3 Download CSS Box Model Module Level 3
  - [ ] 0.1.4 Download CSS Tables Module Level 3
  - [ ] 0.1.5 Download CSS Inline Layout Module Level 3
  - [ ] 0.1.6 Download CSS Flexbox Level 1
  - [ ] 0.1.7 Download CSS Grid Level 1
  - [ ] 0.1.8 Download CSS Cascade Level 4
  - [ ] 0.1.9 Store all PDFs in `/docs/references/specs/`

- [ ] **0.2** Read and document CSS 2.1 Visual Formatting Model
  - [ ] 0.2.1 Read Chapter 9.1-9.2 (box generation)
  - [ ] 0.2.2 Read Chapter 9.3-9.4 (positioning, normal flow)
  - [ ] 0.2.3 Read Chapter 9.5-9.6 (floats, absolute positioning)
  - [ ] 0.2.4 Read Chapter 9.7-9.10 (display/position/float relationships)
  - [ ] 0.2.5 Read Chapter 10 (visual formatting details)
  - [ ] 0.2.6 Create `/docs/research/visual-formatting-model.md`
  - [ ] 0.2.7 Document key algorithms in pseudocode
  - [ ] 0.2.8 Document questions for further research

- [ ] **0.3** Read and document Display Module
  - [ ] 0.3.1 Read Section 2 (display property)
  - [ ] 0.3.2 Read Section 3 (formatting contexts)
  - [ ] 0.3.3 Read Section 4 (display types)
  - [ ] 0.3.4 Create formatting context type diagram
  - [ ] 0.3.5 Create `/docs/research/formatting-contexts.md`
  - [ ] 0.3.6 Document FC establishment rules
  - [ ] 0.3.7 Document anonymous box generation rules

- [ ] **0.4** Read and document Box Model
  - [ ] 0.4.1 Read box model definition
  - [ ] 0.4.2 Read box-sizing property
  - [ ] 0.4.3 Read margin/border/padding properties
  - [ ] 0.4.4 Read width/height computation
  - [ ] 0.4.5 Create `/docs/research/box-model.md`
  - [ ] 0.4.6 Document calculation order
  - [ ] 0.4.7 Document percentage resolution rules

- [ ] **0.5** Read and document Table Layout
  - [ ] 0.5.1 Read Section 3 (table structure)
  - [ ] 0.5.2 Read Section 4 (width computation)
  - [ ] 0.5.3 Read Section 5 (height computation)
  - [ ] 0.5.4 Read Section 6 (fixed layout algorithm)
  - [ ] 0.5.5 Read Section 7 (auto layout algorithm)
  - [ ] 0.5.6 Create `/docs/research/table-layout.md`
  - [ ] 0.5.7 Document fixed algorithm in pseudocode
  - [ ] 0.5.8 Document auto algorithm in pseudocode
  - [ ] 0.5.9 Document why flexbox cannot replace tables

- [ ] **0.6** Read and document Inline Layout
  - [ ] 0.6.1 Read line box model
  - [ ] 0.6.2 Read inline box model
  - [ ] 0.6.3 Read text alignment
  - [ ] 0.6.4 Read vertical alignment
  - [ ] 0.6.5 Read line breaking rules
  - [ ] 0.6.6 Create `/docs/research/inline-layout.md`
  - [ ] 0.6.7 Document line box construction algorithm
  - [ ] 0.6.8 Document baseline alignment rules
  - [ ] 0.6.9 Document this is currently missing in codebase

### Week 2: Implementation Study

- [ ] **0.7** Study Servo layout engine
  - [ ] 0.7.1 Clone Servo repository to `/docs/references/browsers/servo/`
  - [ ] 0.7.2 Study `layout_2020/flow/mod.rs` (entry point)
  - [ ] 0.7.3 Study `formatting_context/` directory
  - [ ] 0.7.4 Study `flow/block.rs` (block layout)
  - [ ] 0.7.5 Study `flow/inline.rs` (inline layout)
  - [ ] 0.7.6 Study `table/mod.rs` (table layout)
  - [ ] 0.7.7 Study `fragments/mod.rs` (fragment tree)
  - [ ] 0.7.8 Create `/docs/research/servo-analysis.md`
  - [ ] 0.7.9 Document Box Tree vs Fragment Tree separation
  - [ ] 0.7.10 Document layout algorithm architecture
  - [ ] 0.7.11 Document clever solutions to adopt
  - [ ] 0.7.12 Document anti-patterns to avoid

- [ ] **0.8** Study WebKit layout engine
  - [ ] 0.8.1 Clone WebKit repository to `/docs/references/browsers/webkit/`
  - [ ] 0.8.2 Study `Source/WebCore/layout/LayoutState.cpp`
  - [ ] 0.8.3 Study `blockformatting/` directory
  - [ ] 0.8.4 Study `inlineformatting/` directory
  - [ ] 0.8.5 Study `tableformatting/` directory
  - [ ] 0.8.6 Compare with Servo's approach
  - [ ] 0.8.7 Update `/docs/research/servo-analysis.md` with comparison
  - [ ] 0.8.8 Document architectural differences
  - [ ] 0.8.9 Document which approach suits our needs

- [ ] **0.9** Study Taffy integration
  - [ ] 0.9.1 Review vendored Taffy in `vendor/taffy/`
  - [ ] 0.9.2 Study `tree/` module (tree structure)
  - [ ] 0.9.3 Study `compute/flexbox.rs` (flexbox algorithm)
  - [ ] 0.9.4 Study `compute/grid.rs` (grid algorithm)
  - [ ] 0.9.5 Study API surface
  - [ ] 0.9.6 Create `/docs/research/taffy-integration.md`
  - [ ] 0.9.7 Document what Taffy handles (flex, grid)
  - [ ] 0.9.8 Document what we must implement (table, inline, block, float)
  - [ ] 0.9.9 Document integration strategy (Box → Taffy → Fragment)

- [ ] **0.10** Write Architecture Decision Record
  - [ ] 0.10.1 Create `/docs/plan/00-architecture-decisions.md`
  - [ ] 0.10.2 Document ADR-001: Separate Box and Fragment trees
  - [ ] 0.10.3 Document ADR-002: Independent layout implementations
  - [ ] 0.10.4 Document ADR-003: Limited Taffy usage
  - [ ] 0.10.5 Document ADR-004: Formatting context abstraction
  - [ ] 0.10.6 Document ADR-005: Display list construction
  - [ ] 0.10.7 Document ADR-006: Text shaping pipeline
  - [ ] 0.10.8 Document ADR-007: Error handling strategy
  - [ ] 0.10.9 Document ADR-008: Memory management
  - [ ] 0.10.10 Document ADR-009: Testing strategy
  - [ ] 0.10.11 Add rationale for each decision
  - [ ] 0.10.12 Add implementation guidance
  - [ ] 0.10.13 Add verification criteria

- [ ] **0.11** Phase 0 completion checklist
  - [ ] 0.11.1 All specifications downloaded
  - [ ] 0.11.2 All research documents created
  - [ ] 0.11.3 Architecture decisions documented
  - [ ] 0.11.4 Clear understanding of CSS layout model
  - [ ] 0.11.5 Clear implementation plan for Phase 1
  - [ ] 0.11.6 Review with human stakeholder
  - [ ] 0.11.7 Get approval to proceed to Phase 1

## Phase 1: Core Architecture

### Week 1: Type System (Can parallelize at file level)

- [ ] **1.1** Create geometry types
  - [ ] 1.1.1 Create file `src/geom.rs`
  - [ ] 1.1.2 Implement `Point` struct
  - [ ] 1.1.3 Implement `Size` struct
  - [ ] 1.1.4 Implement `Rect` struct with methods
  - [ ] 1.1.5 Implement `EdgeOffsets` struct
  - [ ] 1.1.6 Write unit tests for `Point`
  - [ ] 1.1.7 Write unit tests for `Size`
  - [ ] 1.1.8 Write unit tests for `Rect`
  - [ ] 1.1.9 Write unit tests for `EdgeOffsets`
  - [ ] 1.1.10 Run `cargo test geom`
  - [ ] 1.1.11 Run `cargo clippy` and fix warnings
  - [ ] 1.1.12 Run `cargo fmt`

- [ ] **1.2** Create Box Tree types
  - [ ] 1.2.1 Create directory `src/tree/`
  - [ ] 1.2.2 Create file `src/tree/box_tree.rs`
  - [ ] 1.2.3 Implement `BoxTree` struct
  - [ ] 1.2.4 Implement `BoxNode` struct
  - [ ] 1.2.5 Implement `BoxType` enum (Block, Inline, Text, Replaced, Anonymous)
  - [ ] 1.2.6 Implement `FormattingContextType` enum
  - [ ] 1.2.7 Implement `DebugInfo` struct
  - [ ] 1.2.8 Implement helper constructors (`new_block`, `new_inline`, etc.)
  - [ ] 1.2.9 Implement query methods (`is_block_level`, `is_inline_level`, etc.)
  - [ ] 1.2.10 Add documentation comments to all public items
  - [ ] 1.2.11 Create `tests/unit/tree/box_tree_test.rs`
  - [ ] 1.2.12 Write test `test_create_block_box`
  - [ ] 1.2.13 Write test `test_create_inline_box`
  - [ ] 1.2.14 Write test `test_create_text_box`
  - [ ] 1.2.15 Write test `test_box_tree_hierarchy`
  - [ ] 1.2.16 Run `cargo test box_tree`
  - [ ] 1.2.17 Verify all tests pass
  - [ ] 1.2.18 Run `cargo clippy` and fix warnings
  - [ ] 1.2.19 Run `cargo fmt`

- [ ] **1.3** Create Fragment Tree types
  - [ ] 1.3.1 Create file `src/tree/fragment.rs`
  - [ ] 1.3.2 Implement `FragmentTree` struct
  - [ ] 1.3.3 Implement `Fragment` struct
  - [ ] 1.3.4 Implement `FragmentContent` enum
  - [ ] 1.3.5 Implement helper constructors
  - [ ] 1.3.6 Implement `position()` and `size()` methods
  - [ ] 1.3.7 Implement `bounding_box()` method (recursive)
  - [ ] 1.3.8 Implement `translate()` method
  - [ ] 1.3.9 Add documentation comments
  - [ ] 1.3.10 Create `tests/unit/tree/fragment_test.rs`
  - [ ] 1.3.11 Write test `test_create_block_fragment`
  - [ ] 1.3.12 Write test `test_fragment_translate`
  - [ ] 1.3.13 Write test `test_fragment_bounding_box`
  - [ ] 1.3.14 Run `cargo test fragment`
  - [ ] 1.3.15 Verify all tests pass
  - [ ] 1.3.16 Run `cargo clippy` and fix warnings
  - [ ] 1.3.17 Run `cargo fmt`

- [ ] **1.4** Create Formatting Context types
  - [ ] 1.4.1 Create directory `src/layout/`
  - [ ] 1.4.2 Create file `src/layout/formatting_context.rs`
  - [ ] 1.4.3 Implement `Constraints` struct
  - [ ] 1.4.4 Implement `AvailableSpace` enum
  - [ ] 1.4.5 Implement `FormattingContext` trait
  - [ ] 1.4.6 Implement helper methods on `Constraints`
  - [ ] 1.4.7 Add documentation comments
  - [ ] 1.4.8 Create `tests/unit/layout/formatting_context_test.rs`
  - [ ] 1.4.9 Write test `test_available_space`
  - [ ] 1.4.10 Write test `test_constraints`
  - [ ] 1.4.11 Run `cargo test formatting_context`
  - [ ] 1.4.12 Verify all tests pass

- [ ] **1.5** Update module structure
  - [ ] 1.5.1 Update `src/lib.rs` to export new modules
  - [ ] 1.5.2 Add `pub mod geom;`
  - [ ] 1.5.3 Add `pub mod tree { pub mod box_tree; pub mod fragment; }`
  - [ ] 1.5.4 Add `pub mod layout { pub mod formatting_context; }`
  - [ ] 1.5.5 Add re-exports for common types
  - [ ] 1.5.6 Run `cargo build`
  - [ ] 1.5.7 Fix any compilation errors
  - [ ] 1.5.8 Run `cargo doc --no-deps`
  - [ ] 1.5.9 Verify documentation renders correctly
  - [ ] 1.5.10 Run `cargo test`
  - [ ] 1.5.11 Verify all tests pass

- [ ] **1.6** Week 1 completion checklist
  - [ ] 1.6.1 All type system files created
  - [ ] 1.6.2 All tests passing
  - [ ] 1.6.3 No clippy warnings
  - [ ] 1.6.4 Code formatted
  - [ ] 1.6.5 Documentation complete
  - [ ] 1.6.6 Review code quality
  - [ ] 1.6.7 Commit changes with message: "Phase 1.1: Core type system"

### Week 2-4: Box Generation

- [ ] **1.7** Plan box generation module
  - [ ] 1.7.1 Read CSS Display spec section on box generation
  - [ ] 1.7.2 Document box generation algorithm in pseudocode
  - [ ] 1.7.3 Identify all cases requiring anonymous boxes
  - [ ] 1.7.4 Create `/docs/research/box-generation-algorithm.md`

- [ ] **1.8** Implement box generation core
  - [ ] 1.8.1 Create file `src/style/box_generation.rs`
  - [ ] 1.8.2 Implement `generate_box_tree(dom: &DomNode) -> BoxTree`
  - [ ] 1.8.3 Implement `generate_box(node: &DomNode) -> Option<BoxNode>`
  - [ ] 1.8.4 Handle `display: none` (return None)
  - [ ] 1.8.5 Handle `display: contents` (skip node, process children)
  - [ ] 1.8.6 Handle block boxes
  - [ ] 1.8.7 Handle inline boxes
  - [ ] 1.8.8 Handle text nodes
  - [ ] 1.8.9 Handle replaced elements (img, video, canvas)
  - [ ] 1.8.10 Add documentation

- [ ] **1.9** Implement anonymous box generation
  - [ ] 1.9.1 Implement `wrap_inline_in_anonymous_block()`
  - [ ] 1.9.2 Implement `wrap_table_parts()`
  - [ ] 1.9.3 Implement `generate_list_item_marker()`
  - [ ] 1.9.4 Write tests for each anonymous box case
  - [ ] 1.9.5 Verify against spec examples

- [ ] **1.10** Implement pseudo-element generation
  - [ ] 1.10.1 Implement `generate_before_box()`
  - [ ] 1.10.2 Implement `generate_after_box()`
  - [ ] 1.10.3 Handle `content` property
  - [ ] 1.10.4 Write tests for pseudo-elements
  - [ ] 1.10.5 Verify ::before and ::after work

- [ ] **1.11** Integration with style system
  - [ ] 1.11.1 Update `src/style/mod.rs` to use box generation
  - [ ] 1.11.2 Replace `StyledNode` → `BoxNode` conversion
  - [ ] 1.11.3 Update renderer to use box generation
  - [ ] 1.11.4 Run integration tests
  - [ ] 1.11.5 Fix any breakage

- [ ] **1.12** Phase 1 completion checklist
  - [ ] 1.12.1 Box tree types implemented
  - [ ] 1.12.2 Fragment tree types implemented
  - [ ] 1.12.3 Box generation working
  - [ ] 1.12.4 Anonymous boxes generated correctly
  - [ ] 1.12.5 Pseudo-elements generated
  - [ ] 1.12.6 All tests passing
  - [ ] 1.12.7 Documentation complete
  - [ ] 1.12.8 Code reviewed
  - [ ] 1.12.9 Commit with message: "Phase 1: Core architecture complete"

## Phase 2: Layout Algorithms

### Block Layout (Week 1-2)

- [ ] **2.1** Study block layout algorithm
  - [ ] 2.1.1 Re-read CSS 2.1 Section 9.4.1 (block formatting context)
  - [ ] 2.1.2 Re-read CSS 2.1 Section 10.3 (width calculation)
  - [ ] 2.1.3 Re-read CSS 2.1 Section 10.6 (height calculation)
  - [ ] 2.1.4 Document algorithm in pseudocode
  - [ ] 2.1.5 Identify edge cases

- [ ] **2.2** Implement block layout core
  - [ ] 2.2.1 Create file `src/layout/block.rs`
  - [ ] 2.2.2 Implement `BlockLayout` struct
  - [ ] 2.2.3 Implement `FormattingContext` trait for `BlockLayout`
  - [ ] 2.2.4 Implement width calculation (spec 10.3.3)
  - [ ] 2.2.5 Implement height calculation (spec 10.6.3)
  - [ ] 2.2.6 Implement positioning (vertical stacking)
  - [ ] 2.2.7 Add documentation comments

- [ ] **2.3** Implement margin collapsing
  - [ ] 2.3.1 Read CSS 2.1 Section 8.3.1 (collapsing margins)
  - [ ] 2.3.2 Create file `src/layout/margin_collapse.rs`
  - [ ] 2.3.3 Implement `compute_collapsed_margin()`
  - [ ] 2.3.4 Handle adjacent sibling margins
  - [ ] 2.3.5 Handle parent-child margins
  - [ ] 2.3.6 Handle empty block margins
  - [ ] 2.3.7 Write comprehensive tests (this is complex!)
  - [ ] 2.3.8 Verify against WPT tests

- [ ] **2.4** Implement percentage resolution
  - [ ] 2.4.1 Implement containing block calculation
  - [ ] 2.4.2 Handle percentage widths
  - [ ] 2.4.3 Handle percentage heights (depends on parent height)
  - [ ] 2.4.4 Handle `auto` values
  - [ ] 2.4.5 Write tests for each case

- [ ] **2.5** Test block layout
  - [ ] 2.5.1 Create `tests/unit/layout/block_test.rs`
  - [ ] 2.5.2 Write test: blocks stack vertically
  - [ ] 2.5.3 Write test: auto width fills containing block
  - [ ] 2.5.4 Write test: specified width is respected
  - [ ] 2.5.5 Write test: margin collapsing works
  - [ ] 2.5.6 Write test: percentage widths resolve
  - [ ] 2.5.7 Write test: min/max width constraints
  - [ ] 2.5.8 Run WPT css/CSS2/normal-flow/* tests
  - [ ] 2.5.9 Fix failures
  - [ ] 2.5.10 Verify >90% WPT pass rate

- [ ] **2.6** Block layout completion
  - [ ] 2.6.1 All tests passing
  - [ ] 2.6.2 WPT tests passing
  - [ ] 2.6.3 Documentation complete
  - [ ] 2.6.4 Code reviewed
  - [ ] 2.6.5 Commit: "Phase 2.1: Block layout"

### Inline Layout (Week 3-4)

- [ ] **2.7** Study inline layout algorithm
  - [ ] 2.7.1 Re-read CSS Inline Layout Module Level 3
  - [ ] 2.7.2 Document line box construction algorithm
  - [ ] 2.7.3 Document baseline alignment rules
  - [ ] 2.7.4 Document vertical-align property effects
  - [ ] 2.7.5 Create `/docs/research/inline-layout-detailed.md`

- [ ] **2.8** Implement inline layout core
  - [ ] 2.8.1 Create file `src/layout/inline.rs`
  - [ ] 2.8.2 Implement `InlineLayout` struct
  - [ ] 2.8.3 Implement `FormattingContext` trait
  - [ ] 2.8.4 Implement line box construction
  - [ ] 2.8.5 Implement text run collection
  - [ ] 2.8.6 Implement inline box nesting

- [ ] **2.9** Implement line breaking
  - [ ] 2.9.1 Integrate unicode-linebreak crate
  - [ ] 2.9.2 Implement `find_break_opportunities()`
  - [ ] 2.9.3 Implement line width measurement
  - [ ] 2.9.4 Implement greedy line breaking
  - [ ] 2.9.5 Handle `white-space` property
  - [ ] 2.9.6 Handle `word-break` property

- [ ] **2.10** Implement alignment
  - [ ] 2.10.1 Implement `text-align` (left, right, center, justify)
  - [ ] 2.10.2 Implement `vertical-align` (baseline, top, bottom, middle)
  - [ ] 2.10.3 Implement baseline calculation
  - [ ] 2.10.4 Handle mixed font sizes
  - [ ] 2.10.5 Handle inline-block elements

- [ ] **2.11** Test inline layout
  - [ ] 2.11.1 Create `tests/unit/layout/inline_test.rs`
  - [ ] 2.11.2 Test: text wraps at line breaks
  - [ ] 2.11.3 Test: text-align works
  - [ ] 2.11.4 Test: vertical-align works
  - [ ] 2.11.5 Test: baseline alignment
  - [ ] 2.11.6 Test: mixed inline and inline-block
  - [ ] 2.11.7 Run WPT css/css-inline/* tests
  - [ ] 2.11.8 Fix failures
  - [ ] 2.11.9 Verify >80% WPT pass rate

- [ ] **2.12** Inline layout completion
  - [ ] 2.12.1 All tests passing
  - [ ] 2.12.2 WPT tests passing
  - [ ] 2.12.3 Documentation complete
  - [ ] 2.12.4 Commit: "Phase 2.2: Inline layout"

### Table Layout (Week 5-6)

See detailed checklist in `docs/plan/02-table-layout.md`

- [ ] **2.13** Implement table structure analysis
  - [ ] 2.13.1 Create `src/layout/table/mod.rs`
  - [ ] 2.13.2 Create `src/layout/table/structure.rs`
  - [ ] 2.13.3 Implement `TableStructure::from_box_tree()`
  - [ ] 2.13.4 Implement anonymous table box generation
  - [ ] 2.13.5 Implement colspan/rowspan tracking
  - [ ] 2.13.6 Write tests for structure analysis
  - [ ] 2.13.7 Verify structure is correct

- [ ] **2.14** Implement column width computation
  - [ ] 2.14.1 Create `src/layout/table/width.rs`
  - [ ] 2.14.2 Implement fixed layout algorithm
  - [ ] 2.14.3 Implement auto layout algorithm
  - [ ] 2.14.4 Implement min-content width calculation
  - [ ] 2.14.5 Implement max-content width calculation
  - [ ] 2.14.6 Implement width distribution algorithm
  - [ ] 2.14.7 Write tests for width computation
  - [ ] 2.14.8 Verify against spec examples

- [ ] **2.15** Implement row height computation
  - [ ] 2.15.1 Create `src/layout/table/height.rs`
  - [ ] 2.15.2 Implement row height calculation
  - [ ] 2.15.3 Implement cell content layout
  - [ ] 2.15.4 Implement baseline alignment
  - [ ] 2.15.5 Handle rowspan height distribution
  - [ ] 2.15.6 Write tests

- [ ] **2.16** Implement border collapsing
  - [ ] 2.16.1 Create `src/layout/table/border_collapse.rs`
  - [ ] 2.16.2 Implement border conflict resolution
  - [ ] 2.16.3 Handle border-collapse property
  - [ ] 2.16.4 Write tests

- [ ] **2.17** Test table layout
  - [ ] 2.17.1 Create `tests/unit/layout/table_test.rs`
  - [ ] 2.17.2 Test: simple 2x2 table
  - [ ] 2.17.3 Test: colspan
  - [ ] 2.17.4 Test: rowspan
  - [ ] 2.17.5 Test: fixed layout
  - [ ] 2.17.6 Test: auto layout
  - [ ] 2.17.7 Test: percentage widths
  - [ ] 2.17.8 Run WPT css/css-tables/* tests
  - [ ] 2.17.9 Fix failures
  - [ ] 2.17.10 Verify >70% WPT pass rate (tables are hard!)

- [ ] **2.18** Table layout completion
  - [ ] 2.18.1 All tests passing
  - [ ] 2.18.2 No element-specific hacks!
  - [ ] 2.18.3 Works on arbitrary tables
  - [ ] 2.18.4 Documentation complete
  - [ ] 2.18.5 Commit: "Phase 2.5: Table layout"

### Flex Layout (Week 7)

- [ ] **2.19** Implement Taffy wrapper for flex
  - [ ] 2.19.1 Create `src/layout/flex.rs`
  - [ ] 2.19.2 Implement `FlexLayout` struct (wraps Taffy)
  - [ ] 2.19.3 Implement Box → Taffy conversion
  - [ ] 2.19.4 Implement Taffy → Fragment conversion
  - [ ] 2.19.5 Handle flex-direction
  - [ ] 2.19.6 Handle flex-wrap
  - [ ] 2.19.7 Handle justify-content
  - [ ] 2.19.8 Handle align-items
  - [ ] 2.19.9 Write tests
  - [ ] 2.19.10 Run WPT css/css-flexbox/* tests
  - [ ] 2.19.11 Verify >95% pass rate (Taffy is good!)

### Grid Layout (Week 8)

- [ ] **2.20** Implement Taffy wrapper for grid
  - [ ] 2.20.1 Create `src/layout/grid.rs`
  - [ ] 2.20.2 Implement `GridLayout` struct (wraps Taffy)
  - [ ] 2.20.3 Implement Box → Taffy conversion
  - [ ] 2.20.4 Implement Taffy → Fragment conversion
  - [ ] 2.20.5 Handle grid-template-columns
  - [ ] 2.20.6 Handle grid-template-rows
  - [ ] 2.20.7 Handle grid-gap
  - [ ] 2.20.8 Handle grid-auto-flow
  - [ ] 2.20.9 Write tests
  - [ ] 2.20.10 Run WPT css/css-grid/* tests
  - [ ] 2.20.11 Verify >90% pass rate

- [ ] **2.21** Phase 2 completion
  - [ ] 2.21.1 All layout modes implemented
  - [ ] 2.21.2 All unit tests passing
  - [ ] 2.21.3 WPT tests for each mode passing
  - [ ] 2.21.4 No hacks, only spec-compliant code
  - [ ] 2.21.5 Performance is acceptable (<100ms typical page)
  - [ ] 2.21.6 Documentation complete
  - [ ] 2.21.7 Code review
  - [ ] 2.21.8 Commit: "Phase 2: All layout algorithms complete"

## Phase 3: Text & Typography

(Detailed checklist - 50+ tasks)

- [ ] **3.1** Refactor font system
- [ ] **3.2** Implement text shaping pipeline
- [ ] **3.3** Implement line breaking
- [ ] **3.4** Test text rendering

## Phase 4: Paint & Rendering

(Detailed checklist - 40+ tasks)

- [ ] **4.1** Implement display list
- [ ] **4.2** Implement stacking contexts
- [ ] **4.3** Implement rasterizer
- [ ] **4.4** Test painting

## Phase 5: CSS Features

(Detailed checklist - 100+ tasks)

- [ ] **5.1** Implement margin collapsing
- [ ] **5.2** Implement float layout
- [ ] **5.3** Implement positioned layout
- [ ] **5.4** Implement z-index
- [ ] **5.5** Implement pseudo-elements
- [ ] **5.6** Implement media queries
- [ ] **5.7** Fix CSS variables

## Phase 6: Testing Infrastructure

(Detailed checklist - 60+ tasks)

- [ ] **6.1** Set up unit test framework
- [ ] **6.2** Set up integration tests
- [ ] **6.3** Set up reference tests
- [ ] **6.4** Set up WPT tests
- [ ] **6.5** Set up fuzz testing
- [ ] **6.6** Set up benchmarking
- [ ] **6.7** Set up CI/CD

## Migration Tasks

### Remove Old Code

- [ ] **M.1** Delete all hacks from old codebase
  - [ ] M.1.1 Delete all `HACK:` comments and associated code
  - [ ] M.1.2 Delete all class-based styling (votearrow, subscribe-btn, etc.)
  - [ ] M.1.3 Delete table→flexbox mapping code
  - [ ] M.1.4 Delete text width estimation heuristics
  - [ ] M.1.5 Delete TOC grid placement hacks
  - [ ] M.1.6 Verify no hardcoded element checks remain

- [ ] **M.2** Remove old LayoutBox type
  - [ ] M.2.1 Find all references to `LayoutBox`
  - [ ] M.2.2 Replace with `Fragment`
  - [ ] M.2.3 Update all callsites
  - [ ] M.2.4 Delete `LayoutBox` struct

- [ ] **M.3** Remove old layout code
  - [ ] M.3.1 Delete old `src/layout.rs`
  - [ ] M.3.2 Verify new layout modules work
  - [ ] M.3.3 Update imports

- [ ] **M.4** Update AGENTS.md
  - [ ] M.4.1 Remove documentation of hacks
  - [ ] M.4.2 Add documentation of new architecture
  - [ ] M.4.3 Explain differences from V1

- [ ] **M.5** Update README.md
  - [ ] M.5.1 Remove false claims ("pixel-perfect")
  - [ ] M.5.2 Add accurate feature list
  - [ ] M.5.3 Add architecture overview
  - [ ] M.5.4 Add testing information

## Final Acceptance

- [ ] **F.1** All phases complete
- [ ] **F.2** All tests passing (unit, integration, reference, WPT)
- [ ] **F.3** No hacks in codebase
- [ ] **F.4** Documentation complete
- [ ] **F.5** Performance benchmarks meet targets
- [ ] **F.6** Code coverage >80%
- [ ] **F.7** Fuzz testing passes 24 hours
- [ ] **F.8** Can render arbitrary HTML without element-specific code
- [ ] **F.9** Human review and approval
- [ ] **F.10** Release v2.0.0

## Progress Tracking

Update this section as phases complete:

- Phase 0: [ ] 0% (0/11 tasks)
- Phase 1: [ ] 0% (0/12 tasks)
- Phase 2: [ ] 0% (0/21 tasks)
- Phase 3: [ ] 0% (0/4 tasks)
- Phase 4: [ ] 0% (0/4 tasks)
- Phase 5: [ ] 0% (0/7 tasks)
- Phase 6: [ ] 0% (0/7 tasks)
- Migration: [ ] 0% (0/5 tasks)
- Overall: [ ] 0% (0/71 major tasks)

## Notes

- Update checkboxes as tasks complete: `[ ]` → `[x]`
- Mark blockers: `[!]` and explain in notes
- Add completion dates: `[x] 2025-01-20`
- Link to relevant commits
- Document any deviations from plan
