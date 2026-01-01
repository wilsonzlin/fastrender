use fastrender::{
  DiagnosticsLevel, FastRender, FastRenderConfig, FontConfig, FragmentContent, FragmentNode, Point,
  Rect, RenderArtifactRequest, RenderOptions,
};

struct EnvVarGuard {
  key: &'static str,
  previous: Option<String>,
}

impl EnvVarGuard {
  fn set(key: &'static str, value: &str) -> Self {
    let previous = std::env::var(key).ok();
    std::env::set_var(key, value);
    Self { key, previous }
  }
}

impl Drop for EnvVarGuard {
  fn drop(&mut self) {
    match self.previous.take() {
      Some(value) => std::env::set_var(self.key, value),
      None => std::env::remove_var(self.key),
    }
  }
}

fn find_text_fragment_bounds(fragment: &FragmentNode, offset: Point, needle: &str) -> Option<Rect> {
  let abs = Rect::from_xywh(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
    fragment.bounds.width(),
    fragment.bounds.height(),
  );

  if let FragmentContent::Text { text, .. } = &fragment.content {
    if text.as_ref().contains(needle) {
      return Some(abs);
    }
  }

  let next_offset = Point::new(abs.x(), abs.y());
  for child in fragment.children.iter() {
    if let Some(found) = find_text_fragment_bounds(child, next_offset, needle) {
      return Some(found);
    }
  }

  None
}

#[test]
fn taffy_perf_counters_reset_between_diagnostics_renders() {
  let _diag_env = EnvVarGuard::set("FASTR_DIAGNOSTICS_LEVEL", "none");
  let config = FastRenderConfig::default().with_font_sources(FontConfig::bundled_only());
  let mut renderer = FastRender::with_config(config).expect("renderer");

  let options = RenderOptions::default()
    .with_viewport(200, 200)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let html_with_taffy = r#"<!doctype html>
    <html>
      <body>
        <div style="display:flex">
          <div>hello</div>
        </div>
        <div style="display:grid">
          <div>world</div>
        </div>
      </body>
    </html>"#;

  let result = renderer
    .render_html_with_diagnostics(html_with_taffy, options.clone())
    .expect("render with flex/grid");
  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");

  let flex_calls = stats
    .layout
    .taffy_flex_measure_calls
    .expect("expected flex measure calls");
  assert!(flex_calls > 0, "expected flex measure calls > 0");
  assert!(
    stats.layout.taffy_flex_compute_cpu_ms.is_some(),
    "expected flex compute_cpu_ms to be recorded"
  );

  let grid_calls = stats
    .layout
    .taffy_grid_measure_calls
    .expect("expected grid measure calls");
  assert!(grid_calls > 0, "expected grid measure calls > 0");
  assert!(
    stats.layout.taffy_grid_compute_cpu_ms.is_some(),
    "expected grid compute_cpu_ms to be recorded"
  );

  // A second diagnostics-enabled render that does not touch flex/grid should not inherit the prior
  // perf counters (they must be reset per render).
  let html_without_taffy = r#"<!doctype html><html><body><div>plain</div></body></html>"#;
  let result = renderer
    .render_html_with_diagnostics(html_without_taffy, options.clone())
    .expect("render without flex/grid");
  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");

  assert!(
    stats.layout.taffy_flex_compute_cpu_ms.is_none()
      && stats.layout.taffy_flex_measure_calls.is_none(),
    "expected flex perf counters to reset between renders"
  );
  assert!(
    stats.layout.taffy_grid_compute_cpu_ms.is_none()
      && stats.layout.taffy_grid_measure_calls.is_none(),
    "expected grid perf counters to reset between renders"
  );

  // When diagnostics are disabled, stats (including taffy counters) must not be captured.
  let disabled_options = RenderOptions::default()
    .with_viewport(200, 200)
    .with_diagnostics_level(DiagnosticsLevel::None);
  let result = renderer
    .render_html_with_diagnostics(html_with_taffy, disabled_options)
    .expect("render with diagnostics disabled");
  assert!(
    result.diagnostics.stats.is_none(),
    "expected stats to be absent when diagnostics are disabled"
  );
}

#[test]
fn taffy_perf_counters_do_not_reset_between_layout_passes_in_one_render() {
  let _diag_env = EnvVarGuard::set("FASTR_DIAGNOSTICS_LEVEL", "none");
  let config = FastRenderConfig::default().with_font_sources(FontConfig::bundled_only());
  let mut renderer = FastRender::with_config(config).expect("renderer");

  let options = RenderOptions::default()
    .with_viewport(300, 200)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  // This document triggers two layout passes via container queries:
  // 1) First layout runs without container query context, so `.target` uses `display:flex` and
  //    invokes Taffy.
  // 2) After container sizes are resolved, the container query flips `.target` to `display:block`
  //    and the document is laid out a second time (no Taffy usage).
  //
  // We want per-render attribution, so Taffy counters should still reflect pass #1 even though the
  // final pass does not use Taffy.
  let html = r#"<!doctype html>
    <html>
      <head>
        <style>
          .container { width: 600px; container-type: inline-size; }
          .target { display: flex; }
          @container (width >= 500px) {
            .target { display: block; }
          }
        </style>
      </head>
      <body>
        <div class="container">
          <div class="target">
            <div>AAA</div>
            <div>BBB</div>
          </div>
        </div>
      </body>
    </html>"#;

  let report = renderer
    .render_html_with_stylesheets_report(
      html,
      "https://example.test/",
      options,
      RenderArtifactRequest {
        fragment_tree: true,
        ..RenderArtifactRequest::default()
      },
    )
    .expect("render with container query relayout");

  let stats = report
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");
  let flex_calls = stats
    .layout
    .taffy_flex_measure_calls
    .expect("expected flex measure calls from the first layout pass");
  assert!(flex_calls > 0, "expected flex measure calls > 0");

  let fragment_tree = report
    .artifacts
    .fragment_tree
    .as_ref()
    .expect("expected fragment tree artifact");
  let aaa = find_text_fragment_bounds(&fragment_tree.root, Point::ZERO, "AAA")
    .expect("expected text fragment containing AAA");
  let bbb = find_text_fragment_bounds(&fragment_tree.root, Point::ZERO, "BBB")
    .expect("expected text fragment containing BBB");

  assert!(
    bbb.y() > aaa.y() + 1.0,
    "expected container query to flip the final layout to block-flow (BBB should be below AAA); \
     aaa={:?} bbb={:?}",
    aaa,
    bbb
  );
}
