use fastrender::image_loader::ImageCache;
use fastrender::paint::display_list_renderer::PaintParallelism;
use fastrender::paint::painter::{paint_tree_with_resources_scaled_offset_backend, PaintBackend};
use fastrender::scroll::ScrollState;
use fastrender::{FastRender, Point, Rgba};

fn render_with_backend(
  html: &str,
  width: u32,
  height: u32,
  background: Rgba,
  backend: PaintBackend,
) -> tiny_skia::Pixmap {
  let mut renderer = FastRender::new().expect("renderer");
  let dom = renderer.parse_html(html).expect("parsed");
  let fragment_tree = renderer
    .layout_document(&dom, width, height)
    .expect("laid out");

  let font_ctx = renderer.font_context().clone();
  let image_cache = ImageCache::new();

  paint_tree_with_resources_scaled_offset_backend(
    &fragment_tree,
    width,
    height,
    background,
    font_ctx,
    image_cache,
    1.0,
    Point::ZERO,
    PaintParallelism::default(),
    &ScrollState::default(),
    backend,
  )
  .expect("painted")
}

#[test]
fn contrast_filter_on_half_alpha_pixel_matches_spec() {
  let html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: white; }
      #target {
        width: 1px;
        height: 1px;
        background: rgba(64, 0, 0, 0.5);
        filter: contrast(2);
      }
    </style>
    <div id="target"></div>
  "#;

  let background = Rgba::WHITE;
  let legacy = render_with_backend(html, 1, 1, background, PaintBackend::Legacy);
  let display = render_with_backend(html, 1, 1, background, PaintBackend::DisplayList);

  let expected = (129, 128, 128, 255);
  for (label, pixmap) in [("legacy", &legacy), ("display list", &display)] {
    let px = pixmap.pixels()[0];
    assert_eq!(
      (px.red(), px.green(), px.blue(), px.alpha()),
      expected,
      "{label} backend produced unexpected filtered pixel"
    );
  }

  assert_eq!(
    legacy.data(),
    display.data(),
    "contrast filter output diverged between backends"
  );
}

#[test]
fn baseline_semitransparent_fill_matches_between_backends() {
  let html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: white; }
      #target {
        width: 1px;
        height: 1px;
        background: rgba(64, 0, 0, 0.5);
      }
    </style>
    <div id="target"></div>
  "#;

  let background = Rgba::WHITE;
  let legacy = render_with_backend(html, 1, 1, background, PaintBackend::Legacy);
  let display = render_with_backend(html, 1, 1, background, PaintBackend::DisplayList);

  let expected = (160, 128, 128, 255);
  for (label, pixmap) in [("legacy", &legacy), ("display list", &display)] {
    let px = pixmap.pixels()[0];
    assert_eq!(
      (px.red(), px.green(), px.blue(), px.alpha()),
      expected,
      "{label} backend produced unexpected baseline pixel"
    );
  }
}

#[test]
fn filters_on_semitransparent_pixels_match_legacy_backend() {
  let filters = [
    "brightness(1.5)",
    "contrast(0.25)",
    "grayscale(1)",
    "sepia(1)",
    "saturate(2)",
    "hue-rotate(90deg)",
    "invert(1)",
    "opacity(0.25)",
  ];

  for filter in filters {
    let html = format!(
      r#"
        <!doctype html>
        <style>
          body {{ margin: 0; background: white; }}
          #target {{
            width: 1px;
            height: 1px;
            background: rgba(64, 32, 16, 0.5);
            filter: {filter};
          }}
        </style>
        <div id="target"></div>
      "#
    );

    let background = Rgba::WHITE;
    let legacy = render_with_backend(&html, 1, 1, background, PaintBackend::Legacy);
    let display = render_with_backend(&html, 1, 1, background, PaintBackend::DisplayList);

    assert_eq!(
      legacy.data(),
      display.data(),
      "filter `{filter}` diverged between legacy and display list backends"
    );
  }
}

#[test]
fn backdrop_filter_uses_unpremultiplied_channels() {
  let html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: white; }
      .container { position: relative; width: 1px; height: 1px; }
      .backdrop {
        position: absolute;
        inset: 0;
        background: rgba(64, 0, 0, 0.5);
      }
      .overlay {
        position: absolute;
        inset: 0;
        backdrop-filter: contrast(2);
      }
    </style>
    <div class="container">
      <div class="backdrop"></div>
      <div class="overlay"></div>
    </div>
  "#;

  let background = Rgba::WHITE;
  let legacy = render_with_backend(html, 1, 1, background, PaintBackend::Legacy);
  let display = render_with_backend(html, 1, 1, background, PaintBackend::DisplayList);

  assert_eq!(
    legacy.data(),
    display.data(),
    "backdrop-filter contrast should match across backends"
  );
}

#[test]
fn drop_shadow_spread_preserves_color_ratio() {
  let html = r#"
    <!doctype html>
    <style>
      body { margin: 0; background: transparent; }
      #target {
        position: absolute;
        left: 4px;
        top: 4px;
        width: 1px;
        height: 1px;
        background: rgba(255, 255, 255, 1);
        filter: drop-shadow(0 0 0 2px rgba(20, 40, 80, 0.5));
      }
    </style>
    <div id="target"></div>
  "#;

  let legacy = render_with_backend(html, 10, 10, Rgba::new(0, 0, 0, 0.0), PaintBackend::Legacy);
  let display = render_with_backend(
    html,
    10,
    10,
    Rgba::new(0, 0, 0, 0.0),
    PaintBackend::DisplayList,
  );

  assert_eq!(
    legacy.data(),
    display.data(),
    "drop-shadow spread should be consistent between backends"
  );

  assert_shadow_ratio(&display, (4, 4), [20.0 / 255.0, 40.0 / 255.0, 80.0 / 255.0]);
}

fn assert_shadow_ratio(pixmap: &tiny_skia::Pixmap, source: (u32, u32), expected: [f32; 3]) {
  let mut seen = false;
  for (idx, px) in pixmap.pixels().iter().enumerate() {
    let alpha = px.alpha();
    if alpha == 0 {
      continue;
    }
    let x = (idx as u32) % pixmap.width();
    let y = (idx as u32) / pixmap.width();
    if (x, y) == source {
      continue;
    }
    seen = true;
    let a = alpha as f32;
    let ratios = [
      px.red() as f32 / a,
      px.green() as f32 / a,
      px.blue() as f32 / a,
    ];
    for (ratio, expected_ratio) in ratios.iter().zip(expected) {
      assert!(
        (ratio - expected_ratio).abs() < 0.01,
        "shadow ratio drifted at ({x}, {y}): {ratios:?} vs expected {expected:?}"
      );
    }
  }
  assert!(seen, "no shadow pixels were rendered");
}
