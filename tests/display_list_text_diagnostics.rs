use base64::{engine::general_purpose, Engine as _};
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

fn run_with_large_stack(f: impl FnOnce() + Send + 'static) {
  std::thread::Builder::new()
    .stack_size(8 * 1024 * 1024)
    .spawn(f)
    .expect("spawn thread")
    .join()
    .expect("join thread");
}

#[test]
fn display_list_parallel_reports_text_metrics() {
  run_with_large_stack(|| {
    std::env::set_var("FASTR_PAINT_BACKEND", "display_list");

    let mut renderer = FastRender::new().expect("renderer should construct");
    let options = RenderOptions::new()
      .with_viewport(160, 120)
      .with_diagnostics_level(DiagnosticsLevel::Basic)
      .with_paint_parallelism(PaintParallelism {
        enabled: true,
        tile_size: 32,
        log_timing: false,
        min_display_items: 1,
        min_tiles: 1,
        min_build_fragments: 1,
        build_chunk_size: 1,
        ..PaintParallelism::default()
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
      .text_shape_ms
      .expect("text shaping time should be recorded");
    let raster_ms = stats
      .timings
      .text_rasterize_ms
      .expect("text raster time should be recorded");
    let first_hits = stats.counts.glyph_cache_hits.unwrap_or(0);
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
    assert!(
      warmed_hits > first_hits,
      "expected glyph cache hits to increase after warming"
    );
    assert!(
      warmed_stats.counts.glyph_cache_misses.unwrap_or(0) <= first_misses,
      "glyph cache misses should not grow after warming"
    );
    let third = renderer
      .render_html_with_diagnostics("<p>display list text</p>", third_options)
      .expect("third render should succeed");
    let third_stats = third
      .diagnostics
      .stats
      .as_ref()
      .expect("stats should be present on third render");
    assert!(
      third_stats.counts.glyph_cache_hits.unwrap_or(0) >= warmed_hits,
      "cache hits should continue accumulating with reuse"
    );
    assert!(
      third_stats.counts.glyph_cache_bytes.unwrap_or_default() > 0,
      "cache bytes should be tracked after repeated renders"
    );
  });
}

#[test]
fn display_list_reports_color_glyph_rasters() {
  run_with_large_stack(|| {
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
      stats.timings.text_shape_ms.unwrap_or(0.0) > 0.0,
      "expected text shaping time"
    );
    assert!(
      stats.timings.text_rasterize_ms.unwrap_or(0.0) > 0.0,
      "expected text rasterization time"
    );
    assert!(
      stats.counts.color_glyph_rasters.unwrap_or(0) > 0,
      "expected color glyph raster count"
    );
    assert!(stats.counts.glyphs.unwrap_or(0) > 0);
  });
}
