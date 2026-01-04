use fastrender::debug::runtime::RuntimeToggles;
use fastrender::{FastRender, FastRenderConfig};
use std::collections::HashMap;
use tempfile::tempdir;
use url::Url;

#[test]
fn display_list_iframe_srcdoc_renders_content() {
  let toggles = RuntimeToggles::from_map(HashMap::from([(
    "FASTR_PAINT_BACKEND".to_string(),
    "display_list".to_string(),
  )]));
  let config = FastRenderConfig::new().with_runtime_toggles(toggles);

  let inner = "<!doctype html><style>html, body { margin: 0; background: rgb(255, 0, 0); }</style>";
  let outer = format!(
    "<!doctype html>\
     <style>html, body {{ margin: 0; background: rgb(0, 200, 0); }}</style>\
     <iframe srcdoc='{inner}' style='width: 16px; height: 16px; border: 0; display: block;'></iframe>"
  );

  let mut inner_renderer = FastRender::with_config(config.clone()).expect("create inner renderer");
  let inner_pixmap = inner_renderer
    .render_html(inner, 16, 16)
    .expect("render inner srcdoc");
  let inner_red = inner_pixmap
    .data()
    .chunks_exact(4)
    .filter(|px| px[0] > 200 && px[1] < 80 && px[2] < 80)
    .count();
  assert!(
    inner_red > 0,
    "inner srcdoc should render red (sample_pixel={:?})",
    inner_pixmap
      .pixel(0, 0)
      .map(|p| (p.red(), p.green(), p.blue(), p.alpha()))
  );

  let mut renderer = FastRender::with_config(config).expect("create renderer");
  let pixmap = renderer
    .render_html(&outer, 32, 32)
    .expect("render display-list iframe");

  let red_pixels = pixmap
    .data()
    .chunks_exact(4)
    .filter(|px| px[0] > 200 && px[1] < 80 && px[2] < 80)
    .count();
  let placeholder_pixels = pixmap
    .data()
    .chunks_exact(4)
    .filter(|px| px[0] == 200 && px[1] == 200 && px[2] == 200 && px[3] == 255)
    .count();
  assert!(
    red_pixels > 0,
    "iframe content should paint red somewhere (red_pixels={}, placeholder_pixels={}, sample_pixel={:?})",
    red_pixels,
    placeholder_pixels,
    pixmap.pixel(4, 4).map(|p| (p.red(), p.green(), p.blue(), p.alpha()))
  );

  let outside = pixmap.pixel(28, 28).unwrap();
  assert!(
    outside.green() > 150 && outside.red() < 120,
    "outer background should remain green, got {:?}",
    (
      outside.red(),
      outside.green(),
      outside.blue(),
      outside.alpha()
    )
  );
}

#[test]
fn display_list_iframe_depth_limit_blocks_nested() {
  let toggles = RuntimeToggles::from_map(HashMap::from([(
    "FASTR_PAINT_BACKEND".to_string(),
    "display_list".to_string(),
  )]));

  let temp = tempdir().expect("tempdir");
  let inner_path = temp.path().join("inner.html");
  std::fs::write(
    &inner_path,
    "<!doctype html><style>html, body { margin: 0; background: rgb(255, 0, 0); }</style>",
  )
  .expect("write inner");

  let inner_url = Url::from_file_path(&inner_path).unwrap();
  let middle_path = temp.path().join("middle.html");
  let middle_html = format!(
    "<!doctype html>\
     <style>html, body {{ margin: 0; background: rgb(0, 0, 255); }}</style>\
     <iframe src=\"{inner}\" style=\"width: 10px; height: 10px; border: 0; display: block;\"></iframe>",
    inner = inner_url
  );
  std::fs::write(&middle_path, middle_html).expect("write middle");
  let middle_url = Url::from_file_path(&middle_path).unwrap();

  let outer_html = format!(
    "<!doctype html>\
     <style>html, body {{ margin: 0; background: rgb(0, 255, 0); }}</style>\
     <iframe src=\"{middle}\" style=\"width: 20px; height: 20px; border: 0; display: block;\"></iframe>",
    middle = middle_url
  );

  let config = FastRenderConfig::new()
    .with_max_iframe_depth(1)
    .with_runtime_toggles(toggles);
  let mut renderer = FastRender::with_config(config).expect("renderer with depth");
  let pixmap = renderer
    .render_html(&outer_html, 24, 24)
    .expect("render nested iframe with depth limit");

  let inner_pixel = pixmap.pixel(5, 5).unwrap();
  assert_eq!(
    (
      inner_pixel.red(),
      inner_pixel.green(),
      inner_pixel.blue(),
      inner_pixel.alpha()
    ),
    (0, 0, 255, 255),
    "inner iframe should be blocked at depth limit, leaving middle background visible"
  );
}
