use fastrender::debug::runtime::RuntimeToggles;
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

#[test]
fn layout_profile_timings_are_exposed_in_render_stats() {
  let mut toggles = std::collections::HashMap::new();
  toggles.insert("FASTR_LAYOUT_PROFILE".to_string(), "1".to_string());

  let mut renderer = FastRender::new().expect("renderer should construct");
  let options = RenderOptions::new()
    .with_viewport(200, 100)
    .with_diagnostics_level(DiagnosticsLevel::Basic)
    .with_runtime_toggles(RuntimeToggles::from_map(toggles));

  let result = renderer
    .render_html_with_diagnostics("<div>Hello</div>", options)
    .expect("render should succeed");
  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("stats should be captured");

  let total_calls = stats.layout.layout_block_calls.unwrap_or(0)
    + stats.layout.layout_inline_calls.unwrap_or(0)
    + stats.layout.layout_flex_calls.unwrap_or(0)
    + stats.layout.layout_grid_calls.unwrap_or(0)
    + stats.layout.layout_table_calls.unwrap_or(0)
    + stats.layout.layout_absolute_calls.unwrap_or(0);

  assert!(
    total_calls > 0,
    "expected layout profile call counts to be non-zero with FASTR_LAYOUT_PROFILE=1"
  );
}
