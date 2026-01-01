use base64::{engine::general_purpose, Engine as _};
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};
use std::sync::{Mutex, OnceLock};

fn run_with_large_stack(f: impl FnOnce() + Send + 'static) {
  std::thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(f)
    .expect("spawn thread")
    .join()
    .expect("join thread");
}

fn run_display_list_test(f: impl FnOnce() + Send + 'static) {
  // Many of these tests depend on process-global state (e.g. `FASTR_PAINT_BACKEND`), so force
  // serialization even when the Rust test harness runs tests in parallel.
  static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
  let _guard = LOCK.get_or_init(|| Mutex::new(())).lock().expect("lock");
  run_with_large_stack(f);
}

#[test]
fn display_list_parallel_reports_text_metrics() {
  run_display_list_test(|| {
    std::env::set_var("FASTR_PAINT_BACKEND", "display_list");

    let mut renderer = FastRender::new().expect("renderer should construct");
    let options = RenderOptions::new()
      .with_viewport(160, 120)
      .with_diagnostics_level(DiagnosticsLevel::Basic)
      .with_paint_parallelism(PaintParallelism {
        tile_size: 32,
        log_timing: false,
        min_display_items: 1,
        min_tiles: 1,
        min_build_fragments: 1,
        build_chunk_size: 1,
        ..PaintParallelism::enabled()
      });
    let warmed_options = options.clone();
    let third_options = options.clone();

    let result = renderer
      .render_html_with_diagnostics("<p>display list text</p>", options)
      .expect("render should succeed");

    let stats = result
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present");
    let shape_ms = stats
      .timings
      .text_shape_cpu_ms
      .expect("text shaping cpu time should be recorded");
    let raster_ms = stats
      .timings
      .text_rasterize_cpu_ms
      .expect("text raster cpu time should be recorded");
    let first_misses = stats.counts.glyph_cache_misses.unwrap_or(0);

    assert!(shape_ms > 0.0);
    assert!(raster_ms > 0.0);
    assert!(stats.counts.shaped_runs.unwrap_or(0) > 0);
    assert!(stats.counts.glyphs.unwrap_or(0) > 0);

    let warmed = renderer
      .render_html_with_diagnostics("<p>display list text</p>", warmed_options)
      .expect("second render should succeed");
    let warmed_stats = warmed
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present on second render");
    let warmed_hits = warmed_stats.counts.glyph_cache_hits.unwrap_or(0);
    let warmed_misses = warmed_stats.counts.glyph_cache_misses.unwrap_or(0);
    assert!(
      warmed_hits > 0,
      "expected glyph cache hits on second render"
    );
    assert!(
      warmed_misses == 0 || warmed_misses <= first_misses,
      "expected second render misses to drop (first={first_misses} second={warmed_misses})"
    );
    let third = renderer
      .render_html_with_diagnostics("<p>display list text</p>", third_options)
      .expect("third render should succeed");
    let third_stats = third
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present on third render");
    let third_hits = third_stats.counts.glyph_cache_hits.unwrap_or(0);
    let third_misses = third_stats.counts.glyph_cache_misses.unwrap_or(0);
    assert!(
      third_hits > 0,
      "expected glyph cache hits on third render"
    );
    assert!(
      third_misses == 0,
      "expected third render to be fully warmed (misses={third_misses})"
    );
    assert!(
      third_stats.counts.glyph_cache_bytes.unwrap_or_default() > 0,
      "cache bytes should be tracked after repeated renders"
    );
  });
}

#[test]
fn display_list_outline_cache_reused_across_font_sizes() {
  run_display_list_test(|| {
    std::env::set_var("FASTR_PAINT_BACKEND", "display_list");

    // Use a test-only web font so the outline cache starts cold even when other tests ran first.
    // The document draws the same glyph at two different font sizes, which should reuse the same
    // cached outline because `FontInstance::glyph_outline` returns paths in design units.
    //
    // Roboto Flex is a variable font with an `opsz` (optical size) axis. The shaping pipeline
    // auto-populates `opsz` based on the font size when `font-optical-sizing` is `auto`, which
    // would (correctly) make outlines differ between sizes. Disable optical sizing so both sizes
    // share the same variation coordinates and can reuse the cached outline.
    let font_base64 = general_purpose::STANDARD.encode(include_bytes!("fonts/RobotoFlex-VF.ttf"));
    let html = format!(
      "<style>@font-face{{font-family:\"RobotoFlex\";src:url(\"data:font/ttf;base64,{font_base64}\") \
       format(\"truetype\");}}body{{margin:0;font-family:\"RobotoFlex\";font-optical-sizing:none;}}</style>\
       <p style=\"margin:0;font-size:16px\">A</p><p style=\"margin:0;font-size:32px\">A</p>"
    );

    let mut renderer = FastRender::new().expect("renderer should construct");
    let options = RenderOptions::new()
      .with_viewport(160, 120)
      .with_diagnostics_level(DiagnosticsLevel::Basic);
    let result = renderer
      .render_html_with_diagnostics(&html, options)
      .expect("render should succeed");

    let stats = result
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present");
    let hits = stats.counts.glyph_cache_hits.unwrap_or(0);
    let misses = stats.counts.glyph_cache_misses.unwrap_or(0);

    // The first glyph outline must be built, and the second (different size) should hit.
    assert!(misses >= 1, "expected at least one outline cache miss");
    assert!(
      hits >= 1,
      "expected outline cache hits when rendering the same glyph at multiple font sizes"
    );
  });
}

#[test]
fn display_list_reports_color_glyph_rasters() {
  run_display_list_test(|| {
    std::env::set_var("FASTR_PAINT_BACKEND", "display_list");

    let font_base64 = general_purpose::STANDARD.encode(include_bytes!("fonts/ColorBitmapTest.ttf"));
    let html = format!(
      r#"
        <style>
          @font-face {{
            font-family: "ColorBitmap";
            src: url("data:font/ttf;base64,{font_base64}") format("truetype");
          }}
          body {{
            margin: 0;
            font: 64px "ColorBitmap";
          }}
        </style>
        <p>A</p>
      "#
    );

    let mut renderer = FastRender::new().expect("renderer should construct");
    let options = RenderOptions::new()
      .with_viewport(128, 96)
      .with_diagnostics_level(DiagnosticsLevel::Basic);
    let result = renderer
      .render_html_with_diagnostics(&html, options)
      .expect("render should succeed");

    let stats = result
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present");
    assert!(
      stats.timings.text_shape_cpu_ms.unwrap_or(0.0) > 0.0,
      "expected text shaping time"
    );
    assert!(
      stats.timings.text_rasterize_cpu_ms.unwrap_or(0.0) > 0.0,
      "expected text rasterization time"
    );
    assert!(
      stats.counts.color_glyph_rasters.unwrap_or(0) > 0,
      "expected color glyph raster count"
    );
    assert!(stats.counts.glyphs.unwrap_or(0) > 0);
  });
}
