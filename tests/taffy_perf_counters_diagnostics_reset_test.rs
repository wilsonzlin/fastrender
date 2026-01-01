use fastrender::{DiagnosticsLevel, FastRender, FastRenderConfig, FontConfig, RenderOptions};

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
