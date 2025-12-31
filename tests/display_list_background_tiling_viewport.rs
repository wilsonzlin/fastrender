use fastrender::{DiagnosticsLevel, FastRender, RenderOptions};

const DATA_URL_1X1_PNG: &str =
  "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/w8AAwMB/6XWZ90AAAAASUVORK5CYII=";

fn render_display_items(renderer: &mut FastRender, height: u32) -> usize {
  let html = format!(
    r#"<!doctype html>
      <style>
        html, body {{ margin: 0; padding: 0; background: transparent; }}
        #big {{
          width: 200px;
          height: {height}px;
          background-image: url("{DATA_URL_1X1_PNG}"), linear-gradient(red, blue);
          background-size: 20px 20px;
          background-repeat: repeat;
        }}
      </style>
      <div id="big"></div>
    "#
  );

  let options = RenderOptions::new()
    .with_viewport(200, 200)
    .with_diagnostics_level(DiagnosticsLevel::Basic);
  let result = renderer
    .render_html_with_diagnostics(&html, options)
    .expect("render should succeed");
  let stats = result
    .diagnostics
    .stats
    .as_ref()
    .expect("diagnostics stats should be present");
  stats
    .paint
    .display_items
    .expect("paint display_items should be present")
}

#[test]
fn display_list_background_tiling_is_clamped_to_viewport() {
  std::env::set_var("FASTR_PAINT_BACKEND", "display_list");

  let mut renderer = FastRender::new().expect("renderer should construct");

  let small = render_display_items(&mut renderer, 2_000);
  let large = render_display_items(&mut renderer, 20_000);

  assert!(
    small < 1_000,
    "expected viewport-clamped background tiling for small page (display_items={small})"
  );
  assert!(
    large < 1_000,
    "expected viewport-clamped background tiling for large page (display_items={large})"
  );
  assert!(
    (large as isize - small as isize).abs() < 50,
    "expected display list size not to scale with page height (small={small}, large={large})"
  );
}

