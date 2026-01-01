use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};

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
      Some(val) => std::env::set_var(self.key, val),
      None => std::env::remove_var(self.key),
    }
  }
}

#[test]
fn cascade_profile_env_populates_cascade_diagnostics() {
  let _guard = EnvVarGuard::set("FASTR_CASCADE_PROFILE", "1");
  let mut renderer = FastRender::new().expect("renderer");
  let html = r#"
    <style>
      div.foo { color: red; }
      span.bar { color: blue; }
      .baz .qux { margin: 0; }
    </style>
    <div class="foo baz">
      <span class="bar qux">hi</span>
    </div>
  "#;
  let options = RenderOptions::new()
    .with_viewport(64, 64)
    .with_diagnostics_level(DiagnosticsLevel::Basic);
  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render");
  let stats = result
    .diagnostics
    .stats
    .expect("expected diagnostics stats with DiagnosticsLevel::Basic");

  let nodes = stats
    .cascade
    .nodes
    .expect("expected cascade.nodes when FASTR_CASCADE_PROFILE=1");
  assert!(nodes > 0, "expected cascade.nodes > 0");
  assert!(
    stats.cascade.rule_candidates.is_some(),
    "expected cascade.rule_candidates when FASTR_CASCADE_PROFILE=1"
  );
  assert!(
    stats.cascade.selector_time_ms.is_some(),
    "expected cascade.selector_time_ms when FASTR_CASCADE_PROFILE=1"
  );
}
