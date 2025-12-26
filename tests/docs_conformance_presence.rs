//! Guardrail to ensure conformance targets are documented and enforced.

use std::collections::HashSet;
use std::path::Path;

#[test]
fn conformance_doc_is_present_and_non_empty() {
  let conformance = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs/conformance.md");
  assert!(
    conformance.exists(),
    "docs/conformance.md should exist as the conformance source of truth"
  );

  let content = std::fs::read_to_string(&conformance).expect("read docs/conformance.md");
  assert!(
    !content.trim().is_empty(),
    "docs/conformance.md should not be empty"
  );
}

#[test]
fn conformance_doc_links_to_real_code_and_tests() {
  let root = Path::new(env!("CARGO_MANIFEST_DIR"));
  let content = std::fs::read_to_string(root.join("docs/conformance.md"))
    .expect("read docs/conformance.md");

  // Keep links in docs/conformance.md grounded in real modules and tests for each feature area.
  let required_paths = [
    "src/dom.rs",
    "src/html/mod.rs",
    "src/html/encoding.rs",
    "src/html/viewport.rs",
    "src/css/parser.rs",
    "src/css/selectors.rs",
    "src/style/cascade.rs",
    "src/style/media.rs",
    "src/style/values.rs",
    "src/style/color.rs",
    "src/tree/box_generation.rs",
    "src/tree/box_tree.rs",
    "src/tree/table_fixup.rs",
    "src/layout/table.rs",
    "src/layout/contexts/block/mod.rs",
    "src/layout/contexts/inline/mod.rs",
    "src/layout/absolute_positioning.rs",
    "src/layout/contexts/flex.rs",
    "src/layout/contexts/grid.rs",
    "src/layout/taffy_integration.rs",
    "src/layout/fragmentation.rs",
    "src/layout/pagination.rs",
    "src/scroll.rs",
    "src/paint/stacking.rs",
    "src/paint/display_list.rs",
    "src/paint/clip_path.rs",
    "src/paint/display_list_renderer.rs",
    "src/paint/svg_filter.rs",
    "src/paint/text_rasterize.rs",
    "src/paint/text_shadow.rs",
    "src/text/pipeline.rs",
    "src/text/line_break.rs",
    "src/text/hyphenation.rs",
    "src/text/justify.rs",
    "src/animation/mod.rs",
    "src/accessibility.rs",
    "tests/dom_compatibility_test.rs",
    "tests/tree/shadow_dom.rs",
    "tests/css_loader_tests.rs",
    "tests/style/has_selector_test.rs",
    "tests/style/layer_important_test.rs",
    "tests/style/media_test.rs",
    "tests/style/supports_rule_test.rs",
    "tests/style/css_numeric_functions.rs",
    "tests/paint/color_mix_display_list_test.rs",
    "tests/tree/test_anonymous_boxes.rs",
    "tests/tree/form_option_nonrendered.rs",
    "tests/layout/table_columns_test.rs",
    "tests/layout/test_inline_float.rs",
    "tests/layout/test_positioned.rs",
    "tests/layout/flex_box_sizing_test.rs",
    "tests/layout/subgrid.rs",
    "tests/layout/table_anonymous_inheritance.rs",
    "tests/layout/multicol.rs",
    "tests/layout/paged_media.rs",
    "tests/layout/scrollbar_gutter.rs",
    "tests/paint/stacking_test.rs",
    "tests/paint/display_list_test.rs",
    "tests/paint/display_list_renderer_test.rs",
    "tests/paint/text_rasterize_test.rs",
    "tests/text/pipeline_test.rs",
    "tests/text/line_break_test.rs",
    "tests/text/hyphenation_test.rs",
    "tests/text/justify_test.rs",
    "tests/animation_tests.rs",
    "tests/accessibility_test.rs",
    "tests/integration_test.rs",
  ];

  for path in required_paths {
    assert!(
      content.contains(path),
      "docs/conformance.md should mention {path} so the matrix stays tied to the code/tests"
    );
    assert!(
      root.join(path).exists(),
      "Documented path {path} should exist relative to the repo root"
    );
  }

  // Also validate every linked ../src or ../tests path resolves in the repo.
  let mut linked: HashSet<String> = HashSet::new();
  let mut capture_links = |needle: &str| {
    for (idx, _) in content.match_indices(needle) {
      let start = idx + 3; // strip leading ../
      let mut end = start;
      let bytes = content.as_bytes();
      while end < content.len() {
        let ch = bytes[end] as char;
        if ch.is_ascii_alphanumeric() || ch == '/' || ch == '_' || ch == '.' || ch == '-' {
          end += 1;
        } else {
          break;
        }
      }
      if end > start {
        linked.insert(content[start..end].to_string());
      }
    }
  };
  capture_links("../src/");
  capture_links("../tests/");

  assert!(
    linked.iter().any(|p| p.starts_with("src/")),
    "docs/conformance.md should link to at least one source file"
  );
  assert!(
    linked.iter().any(|p| p.starts_with("tests/")),
    "docs/conformance.md should link to at least one test file"
  );

  for path in linked {
    assert!(
      root.join(&path).exists(),
      "Referenced path {path} in docs/conformance.md should exist"
    );
  }
}
