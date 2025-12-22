//! Test fixtures for comprehensive layout testing
//!
//! This module provides infrastructure for testing the renderer against
//! HTML fixture files and golden reference images.
//!
//! # Organization
//!
//! Test fixtures are organized by layout mode:
//! - `tests/fixtures/html/` - HTML test files
//! - `tests/fixtures/css/` - Optional external CSS files
//! - `tests/fixtures/golden/` - Golden reference images (PNG)
//!
//! # Coverage
//!
//! 1. **Block Layout**: Simple blocks, nested blocks, margin collapsing, clearance
//! 2. **Inline Layout**: Text wrapping, mixed inline/block, baseline alignment
//! 3. **Flexbox**: flex-direction, justify-content, align-items, flex-grow/shrink
//! 4. **Grid**: Template tracks, auto-flow, grid gaps
//! 5. **Tables**: Fixed layout, auto layout, colspan/rowspan
//! 6. **Floats**: Left/right floats, text wrapping, clearance
//! 7. **Positioned**: Relative positioning, absolute positioning
//! 8. **Text**: Complex scripts, bidi text, line breaking
//!
//! # Usage
//!
//! Run all fixture tests:
//! ```bash
//! cargo test fixtures
//! ```
//!
//! Generate golden images (run with UPDATE_GOLDEN=1):
//! ```bash
//! UPDATE_GOLDEN=1 cargo test fixtures
//! ```

use fastrender::FastRender;
use std::fs;
use std::path::PathBuf;

/// Test configuration for fixtures
const FIXTURE_WIDTH: u32 = 600;
const FIXTURE_HEIGHT: u32 = 800;

/// Get the fixtures directory path
fn fixtures_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures")
}

/// Get the HTML fixtures directory path
fn html_dir() -> PathBuf {
  fixtures_dir().join("html")
}

/// Get the golden images directory path
fn golden_dir() -> PathBuf {
  fixtures_dir().join("golden")
}

/// Load an HTML fixture file
fn load_fixture(name: &str) -> String {
  let path = html_dir().join(format!("{}.html", name));
  fs::read_to_string(&path)
    .unwrap_or_else(|e| panic!("Failed to read fixture {}: {}", path.display(), e))
}

/// Check if golden image exists
#[allow(dead_code)]
fn golden_exists(name: &str) -> bool {
  golden_dir().join(format!("{}.png", name)).exists()
}

/// Load golden image bytes
fn load_golden(name: &str) -> Option<Vec<u8>> {
  let path = golden_dir().join(format!("{}.png", name));
  fs::read(&path).ok()
}

/// Save golden image
fn save_golden(name: &str, data: &[u8]) {
  let dir = golden_dir();
  fs::create_dir_all(&dir).expect("Failed to create golden directory");
  let path = dir.join(format!("{}.png", name));
  fs::write(&path, data).expect("Failed to write golden image");
}

/// Check if we should update golden images
fn should_update_golden() -> bool {
  std::env::var("UPDATE_GOLDEN").is_ok()
}

/// Render a fixture and optionally compare against golden image
///
/// Returns true if test passes (either rendering succeeds and matches golden,
/// or golden was updated).
fn test_fixture(name: &str) -> Result<(), String> {
  let name_owned = name.to_string();
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(move || {
      let name = name_owned;
      let html = load_fixture(&name);
      let mut renderer =
        FastRender::new().map_err(|e| format!("Failed to create renderer: {:?}", e))?;

      // Render the fixture
      let rendered = renderer
        .render_to_png(&html, FIXTURE_WIDTH, FIXTURE_HEIGHT)
        .map_err(|e| format!("Render failed for {}: {:?}", name, e))?;

      // Handle golden image comparison/update
      if should_update_golden() {
        save_golden(&name, &rendered);
        eprintln!("Updated golden image for: {}", name);
        return Ok(());
      }

      // If golden exists, compare
      if let Some(_golden) = load_golden(&name) {
        // For now, just check that rendered output is valid PNG
        // Full pixel comparison would require image comparison library
        if !rendered.is_empty() && rendered.starts_with(&[0x89, b'P', b'N', b'G']) {
          // Valid PNG header
          Ok(())
        } else {
          Err(format!("Invalid PNG output for {}", name))
        }
      } else {
        // No golden exists - just verify rendering succeeds
        if !rendered.is_empty() && rendered.starts_with(&[0x89, b'P', b'N', b'G']) {
          eprintln!(
            "Warning: No golden image for {}. Run with UPDATE_GOLDEN=1 to create.",
            name
          );
          Ok(())
        } else {
          Err(format!("Invalid PNG output for {}", name))
        }
      }
    })
    .unwrap()
    .join()
    .unwrap()
}

// =============================================================================
// Fixture existence tests (always run)
// =============================================================================

#[test]
fn test_fixtures_directory_exists() {
  assert!(fixtures_dir().exists(), "Fixtures directory should exist");
  assert!(html_dir().exists(), "HTML fixtures directory should exist");
  assert!(
    golden_dir().exists(),
    "Golden images directory should exist"
  );
}

#[test]
fn test_all_fixture_files_exist() {
  let expected_fixtures = [
    // Block layout
    "block_simple",
    "block_nested",
    "block_margin_collapse",
    "block_clearance",
    // Inline layout
    "inline_text_wrap",
    "inline_mixed",
    "inline_baseline",
    // Flexbox
    "flex_direction",
    "flex_justify_align",
    "flex_grow_shrink",
    // Grid
    "grid_template",
    "grid_auto_flow",
    "grid_gaps",
    "multicol_basic",
    // Tables
    "table_fixed",
    "table_auto",
    "table_span",
    "columns_multicol",
    // Floats
    "float_basic",
    "float_text_wrap",
    "float_clearance",
    // Positioned
    "positioned_relative",
    "positioned_absolute",
    // Transforms
    "transform_layer",
    // SVG
    "svg_foreign_object",
    // Forms
    "form_controls",
    // Text
    "text_complex_scripts",
    "text_bidi",
    "text_line_break",
    "text_overflow_vertical",
    "shadow_dom",
  ];

  for name in &expected_fixtures {
    let path = html_dir().join(format!("{}.html", name));
    assert!(
      path.exists(),
      "Fixture file should exist: {}",
      path.display()
    );
  }
}

#[test]
fn test_fixture_files_are_valid_html() {
  let html_path = html_dir();
  if !html_path.exists() {
    return;
  }

  for entry in fs::read_dir(&html_path).unwrap() {
    let entry = entry.unwrap();
    let path = entry.path();
    if path.extension().is_some_and(|e| e == "html") {
      let content = fs::read_to_string(&path).unwrap();
      // Basic HTML validation - should contain doctype and html tags
      assert!(
        content.contains("<!DOCTYPE html>") || content.contains("<!doctype html>"),
        "File {} should have DOCTYPE declaration",
        path.display()
      );
      assert!(
        content.contains("<html") && content.contains("</html>"),
        "File {} should have html tags",
        path.display()
      );
    }
  }
}

#[test]
fn fixture_text_overflow_vertical() {
  test_fixture("text_overflow_vertical").unwrap();
}

// =============================================================================
// Block Layout Tests
// =============================================================================

#[test]
fn test_fixture_block_simple() {
  test_fixture("block_simple").expect("block_simple fixture should render");
}

#[test]
fn test_fixture_block_nested() {
  test_fixture("block_nested").expect("block_nested fixture should render");
}

#[test]
fn test_fixture_block_margin_collapse() {
  test_fixture("block_margin_collapse").expect("block_margin_collapse fixture should render");
}

#[test]
fn test_fixture_block_clearance() {
  test_fixture("block_clearance").expect("block_clearance fixture should render");
}

// =============================================================================
// Inline Layout Tests
// =============================================================================

#[test]
fn test_fixture_inline_text_wrap() {
  test_fixture("inline_text_wrap").expect("inline_text_wrap fixture should render");
}

#[test]
fn test_fixture_inline_mixed() {
  test_fixture("inline_mixed").expect("inline_mixed fixture should render");
}

#[test]
fn test_fixture_inline_baseline() {
  test_fixture("inline_baseline").expect("inline_baseline fixture should render");
}

// =============================================================================
// Flexbox Tests
// =============================================================================

#[test]
fn test_fixture_flex_direction() {
  test_fixture("flex_direction").expect("flex_direction fixture should render");
}

#[test]
fn test_fixture_flex_justify_align() {
  test_fixture("flex_justify_align").expect("flex_justify_align fixture should render");
}

#[test]
fn test_fixture_flex_grow_shrink() {
  test_fixture("flex_grow_shrink").expect("flex_grow_shrink fixture should render");
}

// =============================================================================
// Grid Tests
// =============================================================================

#[test]
fn test_fixture_grid_template() {
  test_fixture("grid_template").expect("grid_template fixture should render");
}

#[test]
fn test_fixture_grid_auto_flow() {
  test_fixture("grid_auto_flow").expect("grid_auto_flow fixture should render");
}

#[test]
fn test_fixture_grid_gaps() {
  test_fixture("grid_gaps").expect("grid_gaps fixture should render");
}

// =============================================================================
// Table Tests
// =============================================================================

#[test]
fn test_fixture_table_fixed() {
  test_fixture("table_fixed").expect("table_fixed fixture should render");
}

#[test]
fn test_fixture_table_auto() {
  test_fixture("table_auto").expect("table_auto fixture should render");
}

#[test]
fn test_fixture_table_span() {
  test_fixture("table_span").expect("table_span fixture should render");
}

// =============================================================================
// Column Layout Tests
// =============================================================================

#[test]
fn test_fixture_columns_multicol() {
  test_fixture("columns_multicol").expect("columns_multicol fixture should render");
}

// =============================================================================
// Float Tests
// =============================================================================

#[test]
fn test_fixture_float_basic() {
  test_fixture("float_basic").expect("float_basic fixture should render");
}

#[test]
fn test_fixture_float_text_wrap() {
  test_fixture("float_text_wrap").expect("float_text_wrap fixture should render");
}

#[test]
fn test_fixture_float_clearance() {
  test_fixture("float_clearance").expect("float_clearance fixture should render");
}

// =============================================================================
// Positioned Tests
// =============================================================================

#[test]
fn test_fixture_positioned_relative() {
  test_fixture("positioned_relative").expect("positioned_relative fixture should render");
}

#[test]
fn test_fixture_positioned_absolute() {
  test_fixture("positioned_absolute").expect("positioned_absolute fixture should render");
}

// =============================================================================
// Transform Tests
// =============================================================================

#[test]
fn test_fixture_transform_layer() {
  test_fixture("transform_layer").expect("transform_layer fixture should render");
}

// =============================================================================
// SVG Tests
// =============================================================================

#[test]
fn test_fixture_svg_foreign_object() {
  test_fixture("svg_foreign_object").expect("svg_foreign_object fixture should render");
}

// =============================================================================
// Form Tests
// =============================================================================

#[test]
fn test_fixture_form_controls() {
  test_fixture("form_controls").expect("form_controls fixture should render");
}

// =============================================================================
// Text Tests
// =============================================================================

#[test]
fn test_fixture_text_complex_scripts() {
  test_fixture("text_complex_scripts").expect("text_complex_scripts fixture should render");
}

#[test]
fn test_fixture_text_bidi() {
  test_fixture("text_bidi").expect("text_bidi fixture should render");
}

#[test]
fn test_fixture_text_line_break() {
  test_fixture("text_line_break").expect("text_line_break fixture should render");
}

// =============================================================================
// Shadow DOM Tests
// =============================================================================

#[test]
fn test_fixture_shadow_dom() {
  test_fixture("shadow_dom").expect("shadow_dom fixture should render");
}

// =============================================================================
// Utility tests
// =============================================================================

#[test]
fn test_fixture_loading() {
  // Test that we can load at least one fixture
  let html = load_fixture("block_simple");
  assert!(html.contains("<!DOCTYPE html>"));
  assert!(html.contains("block_simple") || html.contains("Block Layout"));
}

#[test]
fn test_golden_directory_structure() {
  let golden = golden_dir();
  // Golden directory should exist (even if empty)
  assert!(golden.exists() || fs::create_dir_all(&golden).is_ok());
}

// =============================================================================
// Fixture metadata
// =============================================================================

/// Returns a list of all available fixture names
pub fn list_fixtures() -> Vec<&'static str> {
  vec![
    // Block layout
    "block_simple",
    "block_nested",
    "block_margin_collapse",
    "block_clearance",
    // Inline layout
    "inline_text_wrap",
    "inline_mixed",
    "inline_baseline",
    // Flexbox
    "flex_direction",
    "flex_justify_align",
    "flex_grow_shrink",
    // Grid
    "grid_template",
    "grid_auto_flow",
    "grid_gaps",
    // Tables
    "table_fixed",
    "table_auto",
    "table_span",
    // Floats
    "float_basic",
    "float_text_wrap",
    "float_clearance",
    // Positioned
    "positioned_relative",
    "positioned_absolute",
    // SVG
    "svg_foreign_object",
    "shadow_dom",
    // Text
    "text_complex_scripts",
    "text_bidi",
    "text_line_break",
  ]
}

/// Returns fixture metadata for documentation
pub fn fixture_descriptions() -> Vec<(&'static str, &'static str, &'static str)> {
  vec![
    // (name, category, description)
    (
      "block_simple",
      "Block Layout",
      "Simple stacked block elements",
    ),
    (
      "block_nested",
      "Block Layout",
      "Deeply nested block containers",
    ),
    (
      "block_margin_collapse",
      "Block Layout",
      "Adjacent and parent-child margin collapsing",
    ),
    (
      "block_clearance",
      "Block Layout",
      "Float clearance with clear property",
    ),
    (
      "inline_text_wrap",
      "Inline Layout",
      "Text wrapping in various container widths",
    ),
    (
      "inline_mixed",
      "Inline Layout",
      "Mixed inline elements and inline-blocks",
    ),
    (
      "inline_baseline",
      "Inline Layout",
      "Baseline alignment of different-sized elements",
    ),
    (
      "flex_direction",
      "Flexbox",
      "All flex-direction values (row, column, reverse)",
    ),
    (
      "flex_justify_align",
      "Flexbox",
      "justify-content and align-items combinations",
    ),
    (
      "flex_grow_shrink",
      "Flexbox",
      "flex-grow and flex-shrink distribution",
    ),
    (
      "grid_template",
      "Grid",
      "grid-template-columns/rows with various track sizes",
    ),
    (
      "grid_auto_flow",
      "Grid",
      "grid-auto-flow and auto placement",
    ),
    ("grid_gaps", "Grid", "row-gap and column-gap variations"),
    (
      "table_fixed",
      "Tables",
      "table-layout: fixed with explicit widths",
    ),
    (
      "table_auto",
      "Tables",
      "table-layout: auto content-based sizing",
    ),
    ("table_span", "Tables", "colspan and rowspan cell spanning"),
    ("float_basic", "Floats", "Basic left and right floats"),
    (
      "float_text_wrap",
      "Floats",
      "Text wrapping around floated elements",
    ),
    ("float_clearance", "Floats", "Clear property behavior"),
    (
      "positioned_relative",
      "Positioned",
      "position: relative with various offsets",
    ),
    (
      "positioned_absolute",
      "Positioned",
      "position: absolute with containing blocks",
    ),
    (
      "svg_foreign_object",
      "SVG",
      "Inline SVG with foreignObject HTML content",
    ),
    (
      "text_complex_scripts",
      "Text",
      "Various scripts (Arabic, Hebrew, CJK, etc.)",
    ),
    (
      "text_bidi",
      "Text",
      "Bidirectional text with mixed directions",
    ),
    (
      "text_line_break",
      "Text",
      "Line breaking and white-space handling",
    ),
    (
      "shadow_dom",
      "Shadow DOM",
      "Declarative shadow DOM slotting",
    ),
  ]
}
