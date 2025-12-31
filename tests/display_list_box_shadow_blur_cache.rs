use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::{DiagnosticsLevel, DisplayItem, FastRender, RenderArtifactRequest, RenderOptions};

#[test]
fn display_list_box_shadow_uses_blur_cache() {
  std::env::set_var("FASTR_PAINT_BACKEND", "display_list");
  std::env::set_var("FASTR_SVG_FILTER_CACHE_ITEMS", "256");
  std::env::set_var("FASTR_SVG_FILTER_CACHE_BYTES", "4194304");

  let mut boxes = String::new();
  for _ in 0..100 {
    boxes.push_str("<div class=\"card\"></div>");
  }

  let html = format!(
    r#"<!doctype html>
      <style>
        html, body {{
          margin: 0;
          background: white;
        }}
        .grid {{
          display: grid;
          grid-template-columns: repeat(10, 20px);
          grid-auto-rows: 20px;
          gap: 4px;
          padding: 32px;
        }}
        .card {{
          width: 20px;
          height: 20px;
          background: white;
          border-radius: 6px;
          box-shadow: 0px 0px 8px rgba(0, 0, 0, 0.5);
        }}
      </style>
      <div class="grid">{boxes}</div>
    "#
  );

  let mut renderer = FastRender::new().expect("renderer should construct");
  let options = RenderOptions::new()
    .with_viewport(320, 320)
    .with_diagnostics_level(DiagnosticsLevel::Basic)
    .with_paint_parallelism(PaintParallelism::disabled());
  let report = renderer
    .render_html_with_stylesheets_report(
      &html,
      "https://example.invalid/",
      options,
      RenderArtifactRequest {
        display_list: true,
        ..Default::default()
      },
    )
    .expect("render should succeed");

  let stats = report
    .diagnostics
    .stats
    .as_ref()
    .expect("stats should be present");
  let display_list = report
    .artifacts
    .display_list
    .as_ref()
    .expect("display list should be captured");
  let box_shadows: Vec<_> = display_list
    .items()
    .iter()
    .filter_map(|item| match item {
      DisplayItem::BoxShadow(shadow) => Some(shadow),
      _ => None,
    })
    .collect();
  assert!(
    !box_shadows.is_empty(),
    "expected display list to contain box shadow items (items={})",
    display_list.len()
  );
  assert!(
    box_shadows.iter().any(|s| s.blur_radius > 0.0),
    "expected at least one box shadow with non-zero blur radius"
  );
  let misses = stats.paint.blur_cache_misses.unwrap_or_else(|| {
    panic!(
      "expected blur cache diagnostics to be present (paint={:?})",
      stats.paint
    )
  });
  let hits = stats.paint.blur_cache_hits.unwrap_or_else(|| {
    panic!(
      "expected blur cache diagnostics to be present (paint={:?})",
      stats.paint
    )
  });
  assert!(
    misses >= 1,
    "expected at least one blur cache miss, got misses={misses} hits={hits}"
  );
  assert!(
    hits > 0,
    "expected blur cache hits from repeated box shadows, got misses={misses} hits={hits}"
  );

  let non_white = report
    .pixmap
    .data()
    .chunks_exact(4)
    .filter(|px| px[0] < 250 || px[1] < 250 || px[2] < 250)
    .count();
  assert!(
    non_white > 0,
    "expected some non-white pixels from shadow rendering"
  );
}
