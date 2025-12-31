use fastrender::api::FastRenderConfig;
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions, Rgba};

fn run_with_large_stack(f: impl FnOnce() + Send + 'static) {
  std::thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(f)
    .expect("spawn thread")
    .join()
    .expect("join thread");
}

fn pixmap_has_nontransparent_pixels(pixmap: &tiny_skia::Pixmap) -> bool {
  pixmap.data().chunks_exact(4).any(|px| px[3] != 0)
}

fn pixmap_has_strong_red_pixels(pixmap: &tiny_skia::Pixmap) -> bool {
  pixmap.pixels().iter().any(|px| {
    let a = px.alpha();
    if a == 0 {
      return false;
    }
    let r = px.red();
    let g = px.green();
    let b = px.blue();
    r.saturating_sub(g) > 32 && r.saturating_sub(b) > 32
  })
}

#[test]
fn display_list_text_shadow_populates_glyph_cache() {
  run_with_large_stack(|| {
    std::env::set_var("FASTR_PAINT_BACKEND", "display_list");

    let config = FastRenderConfig::new().with_default_background(Rgba::TRANSPARENT);
    let mut renderer = FastRender::with_config(config).expect("renderer should construct");
    let options = RenderOptions::new()
      .with_viewport(400, 120)
      .with_diagnostics_level(DiagnosticsLevel::Basic);

    // Use a string with distinct glyphs so we can distinguish shadow-induced caching within the
    // first render: shadows are painted before the fill, so the fill should hit the glyph cache
    // even on the initial render if shadows also rasterize through the shared cache.
    let text = "ABCDEFG";
    let html = r#"
      <style>
        html, body { margin: 0; background: transparent; }
        .t {
          font: 48px sans-serif;
          color: black;
          text-shadow: 20px 0 0 red;
        }
      </style>
      <div class="t">ABCDEFG</div>
    "#;

    let first = renderer
      .render_html_with_diagnostics(html, options.clone())
      .expect("first render should succeed");
    assert!(
      pixmap_has_nontransparent_pixels(&first.pixmap),
      "expected first render to paint non-transparent pixels"
    );
    assert!(
      pixmap_has_strong_red_pixels(&first.pixmap),
      "expected first render to include shadow-colored pixels"
    );
    let first_stats = first
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present on first render");
    let first_hits = first_stats.counts.glyph_cache_hits.unwrap_or(0);
    let first_misses = first_stats.counts.glyph_cache_misses.unwrap_or(0);
    let expected_lookups = (text.chars().count() as u64) * 2;
    assert!(
      first_hits + first_misses >= expected_lookups,
      "expected text-shadow+fill to exercise the glyph cache at least {expected_lookups} times (got hits={first_hits}, misses={first_misses})"
    );

    let second = renderer
      .render_html_with_diagnostics(html, options)
      .expect("second render should succeed");
    assert!(
      pixmap_has_nontransparent_pixels(&second.pixmap),
      "expected second render to paint non-transparent pixels"
    );
    assert!(
      pixmap_has_strong_red_pixels(&second.pixmap),
      "expected second render to include shadow-colored pixels"
    );
    let second_stats = second
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present on second render");
    let second_hits = second_stats.counts.glyph_cache_hits.unwrap_or(0);
    assert!(
      second_hits > first_hits,
      "expected glyph cache hits to increase on second render (got first={first_hits}, second={second_hits})"
    );
  });
}
