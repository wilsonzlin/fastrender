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
fn taffy_template_cache_evictions_are_reported_in_diagnostics() {
  let _diag_env = EnvVarGuard::set("FASTR_DIAGNOSTICS_LEVEL", "none");
  // Force the template cache small enough that inserting multiple templates will evict entries.
  let _cache_env = EnvVarGuard::set("FASTR_TAFFY_CACHE_LIMIT", "1");

  let config = FastRenderConfig::default().with_font_sources(FontConfig::bundled_only());
  let mut renderer = FastRender::with_config(config).expect("renderer");

  let options = RenderOptions::default()
    .with_viewport(200, 200)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  // Use nested flex containers so all templates are inserted into the same shared Taffy template
  // cache (the outer flex context's factory is reused when measuring/laying out inner flex items).
  // With `FASTR_TAFFY_CACHE_LIMIT=1`, inserting multiple distinct templates should trigger LRU
  // eviction.
  let html = r#"<!doctype html>
    <html>
      <body>
        <div style="display:flex; flex-direction:column">
          <div style="display:flex">
            <div style="width:10px;height:10px"></div>
          </div>
          <div style="display:flex">
            <div style="width:10px;height:10px"></div>
            <div style="width:10px;height:10px"></div>
          </div>
        </div>
      </body>
    </html>"#;

  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render with flex containers");
  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");

  let flex_built = stats
    .layout
    .taffy_nodes_built
    .expect("expected taffy_nodes_built stats");
  assert!(flex_built > 0, "expected flex layouts to build taffy nodes");

  let flex_evictions = stats
    .layout
    .taffy_flex_template_evictions
    .expect("expected flex template eviction stats");
  assert!(
    flex_evictions > 0,
    "expected flex template evictions when FASTR_TAFFY_CACHE_LIMIT=1; built={:?} reused={:?} style_hits={:?} style_misses={:?} flex_evictions={flex_evictions}",
    stats.layout.taffy_nodes_built,
    stats.layout.taffy_nodes_reused,
    stats.layout.taffy_style_cache_hits,
    stats.layout.taffy_style_cache_misses,
  );

  let grid_evictions = stats
    .layout
    .taffy_grid_template_evictions
    .expect("expected grid template eviction stats");
  assert_eq!(
    grid_evictions, 0,
    "grid template evictions should remain zero without grid containers",
  );
}
