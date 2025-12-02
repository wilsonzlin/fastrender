# Phase 1: Integration

**Duration:** Week 4 of Phase 1 (5-7 days)
**Prerequisites:**
- Type system complete (01-type-system.md)
- Formatting contexts defined (01-formatting-contexts.md)
- Box generation implemented (01-box-generation.md)
**Dependencies:** All Phase 1 components
**Output:** Fully integrated Phase 1 architecture with working end-to-end pipeline

## Objectives

Integrate all Phase 1 components into a cohesive system:

1. Wire the new architecture into the existing renderer
2. Create debug visualization tools
3. Add comprehensive integration tests
4. Document the complete pipeline
5. Ensure Phase 2 can build on this foundation

## Context

Phase 1 has created several isolated components:

- **Type System:** BoxTree, FragmentTree, geometry types
- **Formatting Contexts:** Trait and factory (implementations pending)
- **Box Generation:** DOM → BoxTree transformation

Now we need to:

- **Connect them** in the rendering pipeline
- **Test them** end-to-end
- **Debug them** with visualization tools
- **Document them** for Phase 2

## The Current V1 Pipeline

```
HTML → DOM → Taffy Layout Nodes → Taffy Layout → Display List → Paint → Image
     (html5ever)                  (forced mappings)
```

## The New V2 Pipeline (Phase 1 Complete)

```
HTML → DOM → Style → Box Tree → Layout → Fragment Tree → Display List → Paint → Image
     (html5ever)   (computed)  (box gen)  (FCs)     (positioned)
```

**What works after Phase 1:**
- HTML parsing (existing)
- Style computation (existing, with improvements needed)
- Box tree generation (new!)
- Type definitions (new!)
- FC trait (new!)

**What doesn't work yet (Phase 2):**
- Actual layout algorithms (FCs have `todo!()` implementations)
- Display list construction (still using V1)
- Some advanced CSS features

## Step-by-Step Implementation

### Step 1: Refactor Main Renderer (Day 1)

**File: `src/renderer.rs`**

```rust
//! Main rendering pipeline
//!
//! Coordinates all phases: DOM parsing, styling, box generation, layout, paint.

use crate::dom::{DomParser, Node as DomNode};
use crate::style::{StyleEngine, StyleContext};
use crate::tree::{BoxGenerator, BoxNode, FragmentNode};
use crate::layout::LayoutEngine;
use crate::paint::Painter;
use crate::text::FontContext;
use crate::error::Result;

/// Main rendering engine
///
/// This is the public API for rendering HTML/CSS to images.
pub struct Renderer {
    dom_parser: DomParser,
    style_engine: StyleEngine,
    box_generator: BoxGenerator,
    layout_engine: LayoutEngine,
    painter: Painter,
}

impl Renderer {
    /// Creates a new renderer
    pub fn new() -> Self {
        let font_context = FontContext::new();

        Self {
            dom_parser: DomParser::new(),
            style_engine: StyleEngine::new(),
            box_generator: BoxGenerator::new(),
            layout_engine: LayoutEngine::new(font_context.clone()),
            painter: Painter::new(font_context),
        }
    }

    /// Renders HTML/CSS to an image
    ///
    /// # Arguments
    ///
    /// * `html` - HTML source
    /// * `css` - CSS source
    /// * `width` - Viewport width in pixels
    /// * `height` - Viewport height in pixels
    ///
    /// # Returns
    ///
    /// Rendered image as RGBA pixel buffer
    pub fn render(
        &self,
        html: &str,
        css: &str,
        width: u32,
        height: u32,
    ) -> Result<Vec<u8>> {
        // Phase 1: Parse HTML → DOM tree
        let dom_root = self.dom_parser.parse(html)?;

        // Phase 2: Compute styles
        let style_context = StyleContext::new(css)?;
        self.style_engine.compute_styles(&dom_root, &style_context)?;

        // Phase 3: Generate box tree
        let box_root = self.box_generator.generate(&dom_root)?;

        // Phase 4: Layout → fragment tree
        let fragment_root = self.layout_engine.layout(
            &box_root,
            width as f32,
            height as f32,
        )?;

        // Phase 5: Paint → image
        let image = self.painter.paint(&fragment_root, width, height)?;

        Ok(image)
    }

    /// Renders with debug visualization
    ///
    /// Returns additional debug information alongside the image.
    pub fn render_with_debug(
        &self,
        html: &str,
        css: &str,
        width: u32,
        height: u32,
    ) -> Result<RenderDebugInfo> {
        let dom_root = self.dom_parser.parse(html)?;
        let style_context = StyleContext::new(css)?;
        self.style_engine.compute_styles(&dom_root, &style_context)?;

        let box_root = self.box_generator.generate(&dom_root)?;
        let fragment_root = self.layout_engine.layout(
            &box_root,
            width as f32,
            height as f32,
        )?;

        let image = self.painter.paint(&fragment_root, width, height)?;

        Ok(RenderDebugInfo {
            image,
            dom_tree: dom_root.clone(),
            box_tree: box_root.clone(),
            fragment_tree: fragment_root.clone(),
        })
    }
}

impl Default for Renderer {
    fn default() -> Self {
        Self::new()
    }
}

/// Debug information from rendering
#[derive(Debug)]
pub struct RenderDebugInfo {
    /// Rendered image
    pub image: Vec<u8>,

    /// DOM tree after parsing
    pub dom_tree: DomNode,

    /// Box tree after box generation
    pub box_tree: BoxNode,

    /// Fragment tree after layout
    pub fragment_tree: FragmentNode,
}
```

### Step 2: Create Debug Visualization Tools (Day 2)

**File: `src/debug/tree_printer.rs`**

```rust
//! Debug visualization for tree structures
//!
//! Pretty-prints DOM, Box, and Fragment trees for debugging.

use crate::dom::Node as DomNode;
use crate::tree::{BoxNode, FragmentNode, BoxType};
use std::fmt::Write;

/// Prints a DOM tree in a human-readable format
pub fn print_dom_tree(node: &DomNode, indent: usize) -> String {
    let mut output = String::new();
    print_dom_tree_impl(node, indent, &mut output);
    output
}

fn print_dom_tree_impl(node: &DomNode, indent: usize, output: &mut String) {
    let prefix = "  ".repeat(indent);

    match node.node_type() {
        NodeType::Element => {
            let tag = node.tag_name().unwrap_or("unknown");
            let classes = node.classes().join(" ");
            let id = node.id();

            write!(output, "{}<{}", prefix, tag).unwrap();

            if let Some(id) = id {
                write!(output, " id=\"{}\"", id).unwrap();
            }

            if !classes.is_empty() {
                write!(output, " class=\"{}\"", classes).unwrap();
            }

            writeln!(output, ">").unwrap();

            for child in node.children() {
                print_dom_tree_impl(child, indent + 1, output);
            }
        }
        NodeType::Text => {
            let text = node.text_content().unwrap_or("");
            if !text.trim().is_empty() {
                writeln!(output, "{}\"{}\"", prefix, text.trim()).unwrap();
            }
        }
        _ => {}
    }
}

/// Prints a box tree in a human-readable format
pub fn print_box_tree(node: &BoxNode, indent: usize) -> String {
    let mut output = String::new();
    print_box_tree_impl(node, indent, &mut output);
    output
}

fn print_box_tree_impl(node: &BoxNode, indent: usize, output: &mut String) {
    let prefix = "  ".repeat(indent);

    let box_type_str = match &node.box_type {
        BoxType::Block => "Block",
        BoxType::Inline => "Inline",
        BoxType::InlineBlock => "InlineBlock",
        BoxType::Flex => "Flex",
        BoxType::Grid => "Grid",
        BoxType::Table => "Table",
        BoxType::TableRow => "TableRow",
        BoxType::TableCell => "TableCell",
        BoxType::Text(text) => {
            writeln!(output, "{}Text \"{}\"", prefix, text.trim()).unwrap();
            return;
        }
        _ => "Other",
    };

    let anon = if node.is_anonymous { " [anonymous]" } else { "" };
    let debug_name = node.debug_name.as_deref().unwrap_or("");

    writeln!(output, "{}{}{} {}", prefix, box_type_str, anon, debug_name).unwrap();

    for child in &node.children {
        print_box_tree_impl(child, indent + 1, output);
    }
}

/// Prints a fragment tree with layout information
pub fn print_fragment_tree(node: &FragmentNode, indent: usize) -> String {
    let mut output = String::new();
    print_fragment_tree_impl(node, indent, &mut output);
    output
}

fn print_fragment_tree_impl(node: &FragmentNode, indent: usize, output: &mut String) {
    let prefix = "  ".repeat(indent);

    let rect = node.rect();
    let display = &node.style.display;

    writeln!(
        output,
        "{}{:?} @ ({:.1}, {:.1}) size ({:.1} x {:.1})",
        prefix,
        display,
        rect.origin.x,
        rect.origin.y,
        rect.size.width,
        rect.size.height,
    )
    .unwrap();

    for child in &node.children {
        print_fragment_tree_impl(child, indent + 1, output);
    }
}

/// Creates a visual representation of layout with boxes
pub fn visualize_layout(fragment: &FragmentNode, width: u32, height: u32) -> String {
    let mut output = String::new();

    writeln!(output, "Layout Visualization ({}x{}):", width, height).unwrap();
    writeln!(output, "┌{}┐", "─".repeat(width as usize)).unwrap();

    // ASCII art representation (simplified)
    for y in 0..height {
        write!(output, "│").unwrap();
        for x in 0..width {
            // Check if any fragment overlaps this pixel
            if fragment_at_point(fragment, x as f32, y as f32).is_some() {
                write!(output, "█").unwrap();
            } else {
                write!(output, " ").unwrap();
            }
        }
        writeln!(output, "│").unwrap();
    }

    writeln!(output, "└{}┘", "─".repeat(width as usize)).unwrap();

    output
}

fn fragment_at_point(fragment: &FragmentNode, x: f32, y: f32) -> Option<&FragmentNode> {
    if fragment.rect().contains_point(x, y) {
        // Check children first (they're on top)
        for child in &fragment.children {
            if let Some(result) = fragment_at_point(child, x, y) {
                return Some(result);
            }
        }
        return Some(fragment);
    }
    None
}
```

### Step 3: Add Debug CLI Tool (Day 2-3)

**File: `src/bin/debug.rs`**

```rust
//! Debug CLI tool for FastRender
//!
//! Usage:
//!   cargo run --bin debug -- render test.html test.css
//!   cargo run --bin debug -- tree test.html test.css

use fastrender::{Renderer, debug};
use std::fs;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 4 {
        eprintln!("Usage: {} <command> <html-file> <css-file>", args[0]);
        eprintln!("Commands:");
        eprintln!("  render - Render to image");
        eprintln!("  tree   - Print tree structures");
        std::process::exit(1);
    }

    let command = &args[1];
    let html_path = &args[2];
    let css_path = &args[3];

    let html = fs::read_to_string(html_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", html_path, e));

    let css = fs::read_to_string(css_path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", css_path, e));

    let renderer = Renderer::new();

    match command.as_str() {
        "render" => {
            let image = renderer.render(&html, &css, 800, 600).unwrap();
            fs::write("output.png", image).unwrap();
            println!("Rendered to output.png");
        }

        "tree" => {
            let debug_info = renderer.render_with_debug(&html, &css, 800, 600).unwrap();

            println!("=== DOM Tree ===");
            println!("{}", debug::print_dom_tree(&debug_info.dom_tree, 0));

            println!("\n=== Box Tree ===");
            println!("{}", debug::print_box_tree(&debug_info.box_tree, 0));

            println!("\n=== Fragment Tree (Layout) ===");
            println!("{}", debug::print_fragment_tree(&debug_info.fragment_tree, 0));

            println!("\n=== Visual Layout ===");
            println!("{}", debug::visualize_layout(&debug_info.fragment_tree, 80, 40));
        }

        _ => {
            eprintln!("Unknown command: {}", command);
            std::process::exit(1);
        }
    }
}
```

Add to `Cargo.toml`:

```toml
[[bin]]
name = "debug"
path = "src/bin/debug.rs"
```

### Step 4: Write Integration Tests (Day 3-4)

**File: `tests/integration/end_to_end_test.rs`**

```rust
//! End-to-end integration tests for Phase 1
//!
//! These tests verify that the complete pipeline works:
//! HTML → DOM → Box Tree → Layout → Fragment Tree → Paint

use fastrender::Renderer;

#[test]
fn test_simple_block_end_to_end() {
    let html = r#"
        <!DOCTYPE html>
        <html>
        <body>
            <div>Hello World</div>
        </body>
        </html>
    "#;

    let css = r#"
        div {
            width: 200px;
            height: 100px;
            background: blue;
        }
    "#;

    let renderer = Renderer::new();
    let result = renderer.render(html, css, 800, 600);

    // Phase 1: Should successfully create box tree even though layout is todo!()
    // Phase 2: This will actually produce an image
    match result {
        Ok(_image) => {
            // Layout not implemented yet in Phase 1
            panic!("Expected layout to be unimplemented");
        }
        Err(e) => {
            // Expected: layout FCs return todo!()
            assert!(e.to_string().contains("not yet implemented"));
        }
    }
}

#[test]
fn test_box_tree_generation() {
    let html = r#"
        <div>
            <p>Paragraph 1</p>
            <p>Paragraph 2</p>
        </div>
    "#;

    let css = "";

    let renderer = Renderer::new();
    let debug_info = renderer.render_with_debug(html, css, 800, 600);

    // Box tree should be generated even if layout fails
    // We'll catch the layout error but verify box tree was created
    if let Ok(info) = debug_info {
        let box_tree = info.box_tree;

        // Root div
        assert_eq!(box_tree.box_type, BoxType::Block);

        // Two paragraph children
        assert_eq!(box_tree.children.len(), 2);
        assert_eq!(box_tree.children[0].box_type, BoxType::Block);
        assert_eq!(box_tree.children[1].box_type, BoxType::Block);
    }
}

#[test]
fn test_anonymous_box_generation() {
    let html = r#"
        <div>
            Text before
            <p>Block element</p>
            Text after
        </div>
    "#;

    let css = "";

    let renderer = Renderer::new();
    let debug_info = renderer.render_with_debug(html, css, 800, 600);

    if let Ok(info) = debug_info {
        let box_tree = info.box_tree;

        // Should have 3 children: [anon], <p>, [anon]
        assert_eq!(box_tree.children.len(), 3);

        // First and third should be anonymous
        assert!(box_tree.children[0].is_anonymous);
        assert!(box_tree.children[2].is_anonymous);

        // Middle should not be anonymous
        assert!(!box_tree.children[1].is_anonymous);
    }
}

#[test]
fn test_table_structure() {
    let html = r#"
        <table>
            <tr>
                <td>Cell 1</td>
                <td>Cell 2</td>
            </tr>
        </table>
    "#;

    let css = r#"
        table { display: table; }
        tr { display: table-row; }
        td { display: table-cell; }
    "#;

    let renderer = Renderer::new();
    let debug_info = renderer.render_with_debug(html, css, 800, 600);

    if let Ok(info) = debug_info {
        let box_tree = info.box_tree;

        assert_eq!(box_tree.box_type, BoxType::Table);

        // Should have anonymous tbody
        assert_eq!(box_tree.children.len(), 1);
        assert_eq!(box_tree.children[0].box_type, BoxType::TableRowGroup);
        assert!(box_tree.children[0].is_anonymous);
    }
}
```

### Step 5: Update Module Structure (Day 4)

Ensure all new modules are properly exported:

**File: `src/lib.rs`**

```rust
//! FastRender - HTML/CSS Rendering Engine
//!
//! This crate provides a complete implementation of HTML/CSS rendering,
//! from parsing to rasterization.

// Existing modules
pub mod dom;
pub mod style;
pub mod error;
pub mod text;
pub mod paint;

// NEW Phase 1 modules
pub mod tree;
pub mod layout;
pub mod debug;

// Re-exports for convenience
pub use renderer::Renderer;
pub use error::{Result, Error};

mod renderer;
```

**File: `src/tree/mod.rs`**

```rust
//! Tree structures
//!
//! This module contains all tree representations used in rendering:
//! - Box Tree (before layout)
//! - Fragment Tree (after layout)

mod box_tree;
mod fragment_tree;
mod box_generation;
mod geometry;

pub use box_tree::{BoxNode, BoxType};
pub use fragment_tree::{FragmentNode, FragmentType};
pub use box_generation::BoxGenerator;
pub use geometry::{Point, Size, Rect};
```

**File: `src/layout/mod.rs`**

```rust
//! Layout engine
//!
//! This module handles all layout computation using formatting contexts.

pub mod contexts;
mod constraints;
mod engine;

pub use constraints::LayoutConstraints;
pub use engine::LayoutEngine;
```

**File: `src/debug/mod.rs`**

```rust
//! Debug utilities
//!
//! Tools for debugging and visualizing tree structures.

mod tree_printer;

pub use tree_printer::{
    print_dom_tree,
    print_box_tree,
    print_fragment_tree,
    visualize_layout,
};
```

### Step 6: Create Migration Path from V1 (Day 5)

**File: `docs/guides/migration.md`**

```markdown
# Migrating from V1 to V2

## Feature Flag Approach

During Phase 1, we'll maintain both V1 and V2 code paths using a feature flag.

### Cargo.toml

```toml
[features]
default = ["v1"]
v1 = []
v2 = []
```

### Conditional Compilation

```rust
// src/renderer.rs

#[cfg(feature = "v1")]
pub fn render(&self, html: &str, css: &str, width: u32, height: u32) -> Result<Vec<u8>> {
    // Old V1 implementation
    self.render_v1(html, css, width, height)
}

#[cfg(feature = "v2")]
pub fn render(&self, html: &str, css: &str, width: u32, height: u32) -> Result<Vec<u8>> {
    // New V2 implementation
    self.render_v2(html, css, width, height)
}
```

### Testing Both Versions

```bash
# Test V1
cargo test --features v1

# Test V2 (will panic in Phase 1 due to todo!() in layout)
cargo test --features v2 --no-fail-fast

# Compare outputs
cargo run --features v1 --bin compare-versions
```

## Deprecation Timeline

- **Phase 1 (Weeks 1-4):** V2 architecture exists but layout is unimplemented
- **Phase 2 (Weeks 5-12):** V2 layout implemented, both versions work
- **Phase 3-5:** V2 becomes more capable than V1
- **Phase 6:** V1 removed entirely

## Breaking Changes

None during Phase 1. The public API remains the same.
```

### Step 7: Documentation (Day 5)

**File: `docs/architecture/PIPELINE.md`**

```markdown
# Rendering Pipeline

## Overview

FastRender V2 uses a multi-stage pipeline to transform HTML/CSS into rendered images:

```
┌──────┐    ┌─────┐    ┌──────────┐    ┌────────┐    ┌──────────────┐    ┌───────┐
│ HTML │ -> │ DOM │ -> │ Box Tree │ -> │ Layout │ -> │ Fragment Tree│ -> │ Image │
└──────┘    └─────┘    └──────────┘    └────────┘    └──────────────┘    └───────┘
              ↑            ↑               ↑               ↑                  ↑
            parse        style +       formatting      positioned         paint
                         box gen       contexts         geometry
```

## Stage 1: HTML Parsing

**Input:** HTML string
**Output:** DOM tree
**Library:** html5ever

## Stage 2: Style Computation

**Input:** DOM tree + CSS
**Output:** DOM tree with computed styles
**Library:** lightningcss

## Stage 3: Box Generation

**Input:** Styled DOM tree
**Output:** Box tree
**Algorithm:** CSS Box Generation (CSS 2.1 Section 9.2)

Key operations:
- Filter `display: none` elements
- Create anonymous boxes for mixed content
- Fix table structure (implicit tbody, etc.)
- Create pseudo-element boxes

## Stage 4: Layout

**Input:** Box tree + viewport size
**Output:** Fragment tree with positions
**Algorithms:** Formatting contexts (Block, Inline, Table, Flex, Grid)

Each formatting context implements its own layout algorithm:
- **Block FC:** Vertical stacking, margin collapse
- **Inline FC:** Horizontal flow, line breaking
- **Table FC:** Table algorithm (column constraints)
- **Flex FC:** Taffy wrapper
- **Grid FC:** Taffy wrapper

## Stage 5: Paint

**Input:** Fragment tree
**Output:** RGBA pixel buffer
**Library:** tiny-skia

## Debug Points

At each stage, you can inspect the intermediate representation:

```rust
let debug = renderer.render_with_debug(html, css, 800, 600)?;

println!("DOM: {}", debug::print_dom_tree(&debug.dom_tree));
println!("Boxes: {}", debug::print_box_tree(&debug.box_tree));
println!("Layout: {}", debug::print_fragment_tree(&debug.fragment_tree));
```
```

## Acceptance Criteria

- [ ] Renderer integrates all Phase 1 components
- [ ] `render()` method exists and can be called (will panic in layout, OK for Phase 1)
- [ ] `render_with_debug()` returns DOM, Box, and Fragment trees
- [ ] Debug CLI tool can print all tree representations
- [ ] Visual layout ASCII art works
- [ ] All integration tests pass (or fail expectedly at layout stage)
- [ ] V1 code can still be compiled with `--features v1`
- [ ] Documentation explains complete pipeline
- [ ] Code follows 10-code-standards.md

## Common Pitfalls

### Pitfall 1: Trying to Make Everything Work in Phase 1

**Wrong:**
```rust
// Try to implement layout in integration phase
impl FormattingContext for BlockFormattingContext {
    fn layout(...) -> Result<FragmentNode> {
        // Rushed, incomplete implementation
    }
}
```

**Right:**
```rust
impl FormattingContext for BlockFormattingContext {
    fn layout(...) -> Result<FragmentNode> {
        todo!("Block layout not implemented until Phase 2")
    }
}
```

**Why:** Phase 1 is about **architecture**, not implementation. Layout algorithms come in Phase 2.

### Pitfall 2: Not Testing Error Paths

**Wrong:**
```rust
#[test]
fn test_integration() {
    let result = renderer.render(html, css, 800, 600);
    assert!(result.is_ok()); // Will fail!
}
```

**Right:**
```rust
#[test]
fn test_integration_phase1() {
    let result = renderer.render(html, css, 800, 600);

    // In Phase 1, layout is unimplemented
    match result {
        Ok(_) => panic!("Expected layout to be unimplemented"),
        Err(e) => assert!(e.to_string().contains("not yet implemented")),
    }
}
```

**Why:** Phase 1 code will panic at layout. Tests should verify the error is expected.

### Pitfall 3: Skipping Debug Tools

**Wrong:**
```rust
// "We'll add debug tools later"
// (They never get added)
```

**Right:**
```rust
// Add debug tools in Phase 1
pub fn render_with_debug(...) -> RenderDebugInfo { ... }
```

**Why:** Debug tools are essential for implementing Phase 2. Add them now.

## Next Steps

**Phase 1 is now complete!** The architecture is in place:

✅ Type system defined
✅ Formatting context abstraction created
✅ Box generation implemented
✅ Complete pipeline integrated
✅ Debug tools available

**Phase 2 begins:** Implement actual layout algorithms

1. **Start with Block Layout** (02-block-layout.md)
   - Simplest formatting context
   - Good test of the architecture
   - Foundation for other layouts

2. **Then Inline Layout** (02-inline-layout.md)
   - More complex, requires text shaping
   - Line breaking algorithm

3. **Finally Table Layout** (02-table-layout.md)
   - Most critical for fixing V1's original sin
   - Validates the entire architecture

## Phase 1 Retrospective Checklist

Before starting Phase 2, verify:

- [ ] All Phase 1 documents have been implemented
- [ ] Type system tests all pass
- [ ] Box generation tests all pass
- [ ] Integration tests fail at expected points (layout unimplemented)
- [ ] Debug CLI tool works and is documented
- [ ] Code has >80% coverage for implemented parts
- [ ] Documentation is complete and accurate
- [ ] V1 still compiles with feature flag
- [ ] No TODOs except in formatting context implementations
- [ ] Code review complete

## References

- **CSS 2.1 Visual Formatting Model:**
  - https://www.w3.org/TR/CSS21/visuren.html
- **Servo's layout architecture:**
  - `components/layout_2020/`
- **WebKit's rendering pipeline:**
  - `Source/WebCore/rendering/`

## Timeline

- **Day 1:** Refactor renderer to use new components
- **Day 2:** Create debug visualization tools
- **Day 3:** Add debug CLI and integration tests
- **Day 4:** Update module structure and exports
- **Day 5:** Documentation, migration guide, retrospective

**Total: 5-7 days**

---

**Last Updated:** 2025-01-19
**Status:** Ready for Implementation
**Blocking:** Phase 2 (Layout Algorithms)
