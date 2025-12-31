use fastrender::api::{DiagnosticsLevel, FastRender, RenderOptions};

const PNG_1X1_BASE64: &str =
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR4nGNoaGj4DwAFhAKAjM1mJgAAAABJRU5ErkJggg==";

#[test]
fn legacy_painter_reuses_raster_pixmap_cache() {
  let previous_backend = std::env::var("FASTR_PAINT_BACKEND").ok();
  std::env::set_var("FASTR_PAINT_BACKEND", "legacy");

  let result = std::panic::catch_unwind(|| {
    let mut renderer = FastRender::new().expect("renderer");
    let data_url = format!("data:image/png;base64,{}", PNG_1X1_BASE64);

    let mut html = String::from(
      "<!doctype html><html><head><style>img{width:16px;height:16px;}</style></head><body>",
    );
    for _ in 0..64 {
      html.push_str("<img src=\"");
      html.push_str(&data_url);
      html.push_str("\">");
    }
    html.push_str("</body></html>");

    let options = RenderOptions::new()
      .with_viewport(512, 512)
      .with_diagnostics_level(DiagnosticsLevel::Basic);

    let first = renderer
      .render_html_with_diagnostics(&html, options.clone())
      .expect("first render");
    let stats1 = first
      .diagnostics
      .stats
      .as_ref()
      .expect("first stats should be recorded");
    let hits1 = stats1.resources.raster_pixmap_cache_hits.unwrap_or(0);
    let misses1 = stats1.resources.raster_pixmap_cache_misses.unwrap_or(0);
    let bytes1 = stats1.resources.raster_pixmap_cache_bytes.unwrap_or(0);

    assert!(
      hits1 > 0,
      "expected raster pixmap cache hits (hits={hits1})"
    );
    assert_eq!(
      misses1, 1,
      "expected exactly one raster pixmap cache miss for a repeated image (misses={misses1})"
    );
    assert!(
      bytes1 > 0,
      "expected raster pixmap cache to report non-zero bytes (bytes={bytes1})"
    );

    let second = renderer
      .render_html_with_diagnostics(&html, options)
      .expect("second render");
    let stats2 = second
      .diagnostics
      .stats
      .as_ref()
      .expect("second stats should be recorded");
    let hits2 = stats2.resources.raster_pixmap_cache_hits.unwrap_or(0);
    let misses2 = stats2.resources.raster_pixmap_cache_misses.unwrap_or(0);

    assert!(
      misses2 < misses1,
      "expected second render to have fewer raster pixmap cache misses ({} -> {})",
      misses1,
      misses2
    );
    assert!(
      hits2 >= hits1,
      "expected second render to have at least as many raster pixmap cache hits ({} -> {})",
      hits1,
      hits2
    );
  });

  match previous_backend {
    Some(value) => std::env::set_var("FASTR_PAINT_BACKEND", value),
    None => std::env::remove_var("FASTR_PAINT_BACKEND"),
  }

  if let Err(err) = result {
    std::panic::resume_unwind(err);
  }
}
