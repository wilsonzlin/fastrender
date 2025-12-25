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

mod r#ref;

use fastrender::FastRender;
use r#ref::compare::{compare_images, load_png_from_bytes, CompareConfig, ImageDiff};
use std::fs;
use std::path::PathBuf;
use std::sync::Once;

/// Test configuration for fixtures
const FIXTURE_WIDTH: u32 = 600;
const FIXTURE_HEIGHT: u32 = 800;

static SET_BUNDLED_FONTS: Once = Once::new();

fn ensure_bundled_fonts() {
  SET_BUNDLED_FONTS.call_once(|| {
    // Keep fixture goldens deterministic across machines.
    std::env::set_var("FASTR_USE_BUNDLED_FONTS", "1");
  });
}

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

fn fixtures_diff_dir() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target/fixtures_diffs")
}

fn golden_path(name: &str) -> PathBuf {
  golden_dir().join(format!("{}.png", name))
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
  golden_path(name).exists()
}

/// Load golden image bytes
fn load_golden(name: &str) -> Option<Vec<u8>> {
  let path = golden_path(name);
  fs::read(&path).ok()
}

/// Save golden image
fn save_golden(name: &str, data: &[u8]) {
  let dir = golden_dir();
  fs::create_dir_all(&dir).expect("Failed to create golden directory");
  let path = golden_path(name);
  fs::write(&path, data).expect("Failed to write golden image");
}

/// Check if we should update golden images
fn should_update_golden() -> bool {
  std::env::var("UPDATE_GOLDEN").is_ok()
}

fn fixture_compare_config() -> Result<CompareConfig, String> {
  let mut config = if std::env::var("FIXTURE_FUZZY").is_ok() {
    CompareConfig::fuzzy()
  } else {
    CompareConfig::strict()
  };

  if let Ok(tolerance) = std::env::var("FIXTURE_TOLERANCE") {
    let parsed = tolerance
      .parse::<u8>()
      .map_err(|e| format!("Invalid FIXTURE_TOLERANCE '{}': {}", tolerance, e))?;
    config = config.with_channel_tolerance(parsed);
  }

  if let Ok(percent) = std::env::var("FIXTURE_MAX_DIFFERENT_PERCENT") {
    let parsed = percent
      .parse::<f64>()
      .map_err(|e| format!("Invalid FIXTURE_MAX_DIFFERENT_PERCENT '{}': {}", percent, e))?;
    config = config.with_max_different_percent(parsed);
  }

  if std::env::var("FIXTURE_IGNORE_ALPHA").is_ok() {
    config = config.with_compare_alpha(false);
  }

  if let Ok(distance) = std::env::var("FIXTURE_MAX_PERCEPTUAL_DISTANCE") {
    let parsed = distance.parse::<f64>().map_err(|e| {
      format!(
        "Invalid FIXTURE_MAX_PERCEPTUAL_DISTANCE '{}': {}",
        distance, e
      )
    })?;
    config = config.with_max_perceptual_distance(Some(parsed));
  }

  // Always generate diff images for fixture failures to aid debugging
  config.generate_diff_image = true;

  Ok(config)
}

struct ArtifactPaths {
  output_dir: PathBuf,
  actual: PathBuf,
  expected: PathBuf,
  diff: Option<PathBuf>,
}

fn save_fixture_artifacts(
  name: &str,
  rendered_png: &[u8],
  golden_png: &[u8],
  diff: &ImageDiff,
) -> Result<ArtifactPaths, String> {
  let output_dir = fixtures_diff_dir();
  fs::create_dir_all(&output_dir).map_err(|e| {
    format!(
      "Failed to create diff output directory {}: {}",
      output_dir.display(),
      e
    )
  })?;

  let actual_path = output_dir.join(format!("{}_actual.png", name));
  fs::write(&actual_path, rendered_png).map_err(|e| {
    format!(
      "Failed to write actual image to {}: {}",
      actual_path.display(),
      e
    )
  })?;

  let expected_path = output_dir.join(format!("{}_expected.png", name));
  fs::write(&expected_path, golden_png).map_err(|e| {
    format!(
      "Failed to write expected image to {}: {}",
      expected_path.display(),
      e
    )
  })?;

  let diff_path = output_dir.join(format!("{}_diff.png", name));
  let saved_diff_path = if diff.diff_image.is_some() {
    diff.save_diff_image(&diff_path).map_err(|e| {
      format!(
        "Failed to write diff image to {}: {}",
        diff_path.display(),
        e
      )
    })?;
    Some(diff_path)
  } else {
    None
  };

  Ok(ArtifactPaths {
    output_dir,
    actual: actual_path,
    expected: expected_path,
    diff: saved_diff_path,
  })
}

fn compare_rendered_to_golden(
  name: &str,
  rendered_png: &[u8],
  golden_png: &[u8],
  config: &CompareConfig,
) -> Result<(), String> {
  let actual = load_png_from_bytes(rendered_png)
    .map_err(|e| format!("Failed to decode rendered PNG for {}: {}", name, e))?;
  let expected = load_png_from_bytes(golden_png)
    .map_err(|e| format!("Failed to decode golden PNG for {}: {}", name, e))?;

  let image_diff = compare_images(&actual, &expected, config);

  if image_diff.is_match() {
    return Ok(());
  }

  let artifact_result = save_fixture_artifacts(name, rendered_png, golden_png, &image_diff);

  let mut message = format!(
    "Fixture '{}' image mismatch: {}",
    name,
    image_diff.summary()
  );

  match artifact_result {
    Ok(paths) => {
      message.push_str(&format!(
        "\nSaved artifacts to {} (actual: {}, expected: {})",
        paths.output_dir.display(),
        paths.actual.display(),
        paths.expected.display()
      ));

      if let Some(diff_path) = paths.diff {
        message.push_str(&format!("\nDiff image: {}", diff_path.display()));
      } else if !image_diff.dimensions_match {
        message.push_str("\nDiff image not generated due to dimension mismatch");
      }
    }
    Err(e) => {
      message.push_str(&format!("\nFailed to save diff artifacts: {}", e));
    }
  }

  Err(message)
}

/// Render a fixture and optionally compare against golden image
///
/// Returns true if test passes (either rendering succeeds and matches golden,
/// or golden was updated).
fn test_fixture(name: &str) -> Result<(), String> {
  ensure_bundled_fonts();
  let name_owned = name.to_string();
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(move || {
      let compare_config = fixture_compare_config()?;
      let name = name_owned;
      let html = load_fixture(&name);
      let golden = load_golden(&name);
      let (render_width, render_height) = golden
        .as_ref()
        .and_then(|bytes| load_png_from_bytes(bytes).ok())
        .map(|png| (png.width(), png.height()))
        .unwrap_or((FIXTURE_WIDTH, FIXTURE_HEIGHT));
      let mut renderer =
        FastRender::new().map_err(|e| format!("Failed to create renderer: {:?}", e))?;

      // Render the fixture
      let rendered = renderer
        .render_to_png(&html, render_width, render_height)
        .map_err(|e| format!("Render failed for {}: {:?}", name, e))?;

      // Handle golden image comparison/update
      if should_update_golden() {
        save_golden(&name, &rendered);
        eprintln!("Updated golden image for: {}", name);
        return Ok(());
      }

      // If golden exists, compare
      if let Some(golden) = golden {
        compare_rendered_to_golden(&name, &rendered, &golden, &compare_config)
      } else {
        // No golden exists - just verify rendering succeeds
        load_png_from_bytes(&rendered)
          .map_err(|e| format!("Invalid PNG output for {}: {}", name, e))?;
        eprintln!(
          "Warning: No golden image for {}. Run with UPDATE_GOLDEN=1 to create.",
          name
        );
        Ok(())
      }
    })
    .unwrap()
    .join()
    .unwrap()
}

//
// Fixture existence tests (always run)
//

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
    "mask_composite",
    // Forms
    "form_controls",
    // Text
    "text_complex_scripts",
    "text_bidi",
    "text_bidi_mirror",
    "text_line_break",
    "text_overflow_vertical",
    "shadow_dom",
    "svg_foreign_object",
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

//
// Block Layout Tests
//

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

//
// Inline Layout Tests
//

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

//
// Flexbox Tests
//

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

//
// Grid Tests
//

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

//
// Table Tests
//

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

//
// Column Layout Tests
//

#[test]
fn test_fixture_columns_multicol() {
  test_fixture("columns_multicol").expect("columns_multicol fixture should render");
}

//
// Float Tests
//

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

//
// Positioned Tests
//

#[test]
fn test_fixture_positioned_relative() {
  test_fixture("positioned_relative").expect("positioned_relative fixture should render");
}

#[test]
fn test_fixture_positioned_absolute() {
  test_fixture("positioned_absolute").expect("positioned_absolute fixture should render");
}

//
// Transform Tests
//

#[test]
fn test_fixture_transform_layer() {
  test_fixture("transform_layer").expect("transform_layer fixture should render");
}

//
// SVG Tests
//

#[test]
fn test_fixture_svg_foreign_object() {
  test_fixture("svg_foreign_object").expect("svg_foreign_object fixture should render");
}

//
// Mask Tests
//

#[test]
fn test_fixture_mask_composite() {
  test_fixture("mask_composite").expect("mask_composite fixture should render");
}

//
// Form Tests
//

#[test]
fn test_fixture_form_controls() {
  test_fixture("form_controls").expect("form_controls fixture should render");
}

//
// Text Tests
//

#[test]
fn test_fixture_text_complex_scripts() {
  test_fixture("text_complex_scripts").expect("text_complex_scripts fixture should render");
}

#[test]
fn test_fixture_text_bidi() {
  test_fixture("text_bidi").expect("text_bidi fixture should render");
}

#[test]
fn test_fixture_text_bidi_mirror() {
  test_fixture("text_bidi_mirror").expect("text_bidi_mirror fixture should render");
}

#[test]
fn test_fixture_text_line_break() {
  test_fixture("text_line_break").expect("text_line_break fixture should render");
}

//
// Shadow DOM Tests
//

#[test]
fn test_fixture_shadow_dom() {
  test_fixture("shadow_dom").expect("shadow_dom fixture should render");
}

//
// Utility tests
//

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

//
// Fixture metadata
//

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
    // Masks
    "mask_composite",
    // Forms
    "form_controls",
    // Text
    "text_complex_scripts",
    "text_bidi",
    "text_bidi_mirror",
    "text_line_break",
    "text_overflow_vertical",
    // Shadow DOM
    "shadow_dom",
    // SVG
    "svg_foreign_object",
  ]
}

/// Returns fixture metadata for documentation
pub fn fixture_descriptions() -> Vec<(&'static str, &'static str, &'static str)> {
  vec![
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
    ("multicol_basic", "Columns", "Basic multi-column layout"),
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
    (
      "columns_multicol",
      "Columns",
      "Multi-column layout with column span variations",
    ),
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
      "transform_layer",
      "Transforms",
      "Nested transforms and compositing layers",
    ),
    (
      "mask_composite",
      "Masks",
      "Compositing multiple CSS mask layers",
    ),
    ("form_controls", "Forms", "Default form control styling"),
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
      "text_bidi_mirror",
      "Text",
      "Bidirectional text with mirrored punctuation",
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
    (
      "mask_composite",
      "Masks",
      "Compositing multiple CSS mask layers",
    ),
  ]
}
