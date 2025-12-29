use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

fn restore_env(name: &str, previous: Option<std::ffi::OsString>) {
  if let Some(value) = previous {
    std::env::set_var(name, value);
  } else {
    std::env::remove_var(name);
  }
}

#[test]
fn large_blur_tiles_in_segments() {
  let prev_cache_bytes = std::env::var_os("FASTR_SVG_FILTER_CACHE_BYTES");
  std::env::set_var("FASTR_SVG_FILTER_CACHE_BYTES", "65536");

  let html = "<style>body { margin: 0; } #blur { width: 960px; height: 960px; background: rgba(0, 128, 255, 0.8); filter: blur(12px); }</style><div id=\"blur\"></div>";
  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new()
    .with_viewport(800, 800)
    .with_diagnostics_level(DiagnosticsLevel::Basic);
  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render succeeds");
  restore_env("FASTR_SVG_FILTER_CACHE_BYTES", prev_cache_bytes);

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("diagnostics stats");
  let tiles = stats.paint.blur_tiles.unwrap_or(0);
  assert!(
    tiles > 0,
    "expected blur tiling to record tiles for large region (got {tiles})"
  );
}

#[test]
fn svg_filter_results_hit_cache_on_repeat_render() {
  let html = "<svg xmlns='http://www.w3.org/2000/svg' width='0' height='0'><defs><filter id='f'><feGaussianBlur stdDeviation='2'/></filter></defs></svg><style>body { margin: 0; } #target { width: 80px; height: 80px; background: rgb(200, 50, 100); filter: url(#f); }</style><div id=\"target\"></div>";
  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new()
    .with_viewport(120, 120)
    .with_diagnostics_level(DiagnosticsLevel::Basic);

  let first = renderer
    .render_html_with_diagnostics(html, options.clone())
    .expect("first render");
  let first_stats = first
    .diagnostics
    .stats
    .as_ref()
    .expect("stats from first render");
  let first_misses = first_stats.paint.filter_cache_misses.unwrap_or(0);
  assert!(
    first_misses > 0,
    "expected initial render to populate filter cache (misses {first_misses})"
  );

  let second = renderer
    .render_html_with_diagnostics(html, options)
    .expect("second render");
  let second_stats = second
    .diagnostics
    .stats
    .as_ref()
    .expect("stats from second render");
  let second_hits = second_stats.paint.filter_cache_hits.unwrap_or(0);
  assert!(
    second_hits > 0,
    "expected cached filter result on second render (hits {second_hits})"
  );
}

#[test]
fn svg_filter_blur_tiles_large_regions() {
  let prev_cache_bytes = std::env::var_os("FASTR_SVG_FILTER_CACHE_BYTES");
  std::env::set_var("FASTR_SVG_FILTER_CACHE_BYTES", "65536");

  let html = "<svg xmlns='http://www.w3.org/2000/svg' width='0' height='0'><defs><filter id='blur'><feGaussianBlur stdDeviation='8'/></filter></defs></svg><style>body { margin: 0; } #blur { width: 960px; height: 960px; background: rgba(30, 150, 240, 0.7); filter: url(#blur); }</style><div id=\"blur\"></div>";
  let mut renderer = FastRender::new().expect("renderer");
  let options = RenderOptions::new()
    .with_viewport(800, 800)
    .with_diagnostics_level(DiagnosticsLevel::Basic);
  let result = renderer
    .render_html_with_diagnostics(html, options)
    .expect("render succeeds");
  restore_env("FASTR_SVG_FILTER_CACHE_BYTES", prev_cache_bytes);

  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("diagnostics stats");
  let tiles = stats.paint.blur_tiles.unwrap_or(0);
  assert!(
    tiles > 0,
    "expected svg filter blur tiling to record tiles for large region (got {tiles})"
  );
}
