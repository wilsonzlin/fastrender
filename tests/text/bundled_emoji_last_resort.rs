use fastrender::api::DiagnosticsLevel;
use fastrender::{FastRender, FontConfig, RenderOptions, ResourcePolicy};

#[test]
fn bundled_emoji_does_not_hit_last_resort_fallbacks() {
  let html = r#"
    <!doctype html>
    <meta charset="utf-8">
    <style>
      body { font-size: 48px; }
    </style>
    <p>⭐ 🌟 🐐 🤠 ☶ ❮ ❯</p>
  "#;

  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let mut renderer = FastRender::builder()
    .font_sources(FontConfig::bundled_only())
    .resource_policy(policy)
    .build()
    .expect("renderer build");

  let options = RenderOptions::new()
    .with_viewport(320, 120)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render emoji snippet");

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");
  assert_eq!(
    stats.counts.last_resort_font_fallbacks,
    Some(0),
    "emoji snippet should not hit last-resort font fallbacks (counts: {:?})",
    stats.counts
  );
}

#[test]
fn bundled_pageset_emoji_regressions_do_not_hit_last_resort_fallbacks() {
  let html = r#"
    <!doctype html>
    <meta charset="utf-8">
    <style>
      body { font-size: 48px; }
    </style>
    <p>🧣 🤙 ⚠️</p>
  "#;

  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let mut renderer = FastRender::builder()
    .font_sources(FontConfig::bundled_only())
    .resource_policy(policy)
    .build()
    .expect("renderer build");

  let options = RenderOptions::new()
    .with_viewport(320, 120)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render emoji snippet");

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");
  assert_eq!(
    stats.counts.last_resort_font_fallbacks,
    Some(0),
    "pageset emoji regressions should not hit last-resort font fallbacks (counts: {:?})",
    stats.counts
  );
}

#[test]
fn bundled_pageset_icon_codepoints_do_not_hit_last_resort_fallbacks() {
  let html = r#"
    <!doctype html>
    <meta charset="utf-8">
    <style>
      body { font-size: 48px; }
    </style>
    <p>
      &#xE909; &#xE021; &#xE022; &#xE031; &#xE083; &#xE085; &#xF301; &#xF8FF; &#x28FE;
    </p>
  "#;

  let policy = ResourcePolicy::default()
    .allow_http(false)
    .allow_https(false)
    .allow_file(true)
    .allow_data(true);

  let mut renderer = FastRender::builder()
    .font_sources(FontConfig::bundled_only())
    .resource_policy(policy)
    .build()
    .expect("renderer build");

  let options = RenderOptions::new()
    .with_viewport(640, 120)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render codepoint snippet");

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("expected diagnostics stats");
  assert_eq!(
    stats.counts.last_resort_font_fallbacks,
    Some(0),
    "codepoint snippet should not hit last-resort font fallbacks (counts: {:?})",
    stats.counts
  );
}
