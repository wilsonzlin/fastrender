use fastrender::{FastRender, Pixmap, RenderOptions};

fn assert_pixmap_eq(left: &Pixmap, right: &Pixmap) {
  assert_eq!(left.width(), right.width());
  assert_eq!(left.height(), right.height());
  assert_eq!(left.data(), right.data());
}

#[test]
fn prepared_document_matches_baseline_render() {
  let html = r#"
    <style>
      body { margin: 0; }
      .swatch { width: 40px; height: 40px; background: rgb(200, 30, 60); }
    </style>
    <div class="swatch"></div>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let expected = renderer.render_html(html, 80, 80).unwrap();

  let mut session_renderer = FastRender::new().unwrap();
  let prepared = session_renderer
    .prepare_html(html, RenderOptions::new().with_viewport(80, 80))
    .unwrap();
  let actual = prepared.paint_default().unwrap();

  assert_pixmap_eq(&expected, &actual);
}

#[test]
fn prepared_document_repaints_scrolled_viewports() {
  let html = r#"
    <style>
      body { margin: 0; }
      .top { height: 80px; background: rgb(255, 0, 0); }
      .bottom { height: 80px; background: rgb(0, 0, 255); }
    </style>
    <div class="top"></div>
    <div class="bottom"></div>
  "#;

  let mut reference = FastRender::new().unwrap();
  let top_options = RenderOptions::new().with_viewport(50, 50);
  let bottom_options = RenderOptions::new()
    .with_viewport(50, 50)
    .with_scroll(0.0, 80.0);

  let expected_top = reference
    .render_html_with_options(html, top_options.clone())
    .unwrap();
  let expected_bottom = reference
    .render_html_with_options(html, bottom_options.clone())
    .unwrap();

  let mut session_renderer = FastRender::new().unwrap();
  let prepared = session_renderer.prepare_html(html, top_options).unwrap();

  let actual_top = prepared.paint_default().unwrap();
  let actual_bottom = prepared.paint(0.0, 80.0, None, None).unwrap();

  assert_pixmap_eq(&expected_top, &actual_top);
  assert_pixmap_eq(&expected_bottom, &actual_bottom);
  assert_ne!(actual_top.data(), actual_bottom.data());
}

#[test]
fn prepared_document_respects_sticky_offsets() {
  let html = r#"
    <style>
      body { margin: 0; }
      header {
        position: sticky;
        top: 0;
        height: 20px;
        background: rgb(255, 0, 0);
      }
      main { height: 140px; background: rgb(0, 255, 0); }
    </style>
    <header></header>
    <main></main>
  "#;

  let mut reference = FastRender::new().unwrap();
  let options = RenderOptions::new()
    .with_viewport(80, 50)
    .with_scroll(0.0, 30.0);
  let expected = reference
    .render_html_with_options(html, options.clone())
    .unwrap();

  let mut session_renderer = FastRender::new().unwrap();
  let prepared = session_renderer.prepare_html(html, options).unwrap();
  let actual = prepared.paint(0.0, 30.0, None, None).unwrap();

  assert_pixmap_eq(&expected, &actual);
}
