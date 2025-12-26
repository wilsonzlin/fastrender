use fastrender::css::parser::extract_css;
use fastrender::dom;
use fastrender::style::cascade::apply_styles_with_media;
use fastrender::style::media::MediaContext;
use fastrender::tree::box_generation::generate_box_tree;
use fastrender::tree::box_tree::{BoxNode, BoxType, ReplacedType, SvgContent};
use fastrender::FastRender;
use resvg::tiny_skia::Pixmap;

pub(super) fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> [u8; 4] {
  let idx = (y as usize * pixmap.width() as usize + x as usize) * 4;
  let data = pixmap.data();
  [data[idx], data[idx + 1], data[idx + 2], data[idx + 3]]
}

pub(super) fn serialized_inline_svg(html: &str, width: f32, height: f32) -> Option<SvgContent> {
  let dom = dom::parse_html(html).ok()?;
  let stylesheet = extract_css(&dom).ok()?;
  let media = MediaContext::screen(width, height);
  let styled = apply_styles_with_media(&dom, &stylesheet, &media);
  let box_tree = generate_box_tree(&styled);

  fn find_svg(node: &BoxNode) -> Option<SvgContent> {
    if let BoxType::Replaced(repl) = &node.box_type {
      if let ReplacedType::Svg { content } = &repl.replaced_type {
        return Some(content.clone());
      }
    }
    for child in &node.children {
      if let Some(content) = find_svg(child) {
        return Some(content);
      }
    }
    None
  }

  find_svg(&box_tree.root)
}

#[test]
fn inline_svg_applies_document_css_and_current_color() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
      <style>
        body { margin: 0; background: white; }
        svg { color: rgb(0, 128, 0); display: block; }
        svg .shape { fill: currentColor; }
      </style>
      <svg width="20" height="20" viewBox="0 0 20 20">
        <rect class="shape" width="20" height="20"></rect>
        <text class="shape" x="2" y="14">Hi</text>
      </svg>
      "#;

      let pixmap = renderer.render_html(html, 30, 30).expect("render svg");
      assert_eq!(pixel(&pixmap, 10, 10), [0, 128, 0, 255]);
    })
    .unwrap()
    .join()
    .unwrap();
}

#[test]
fn inline_svg_renders_gradients_with_clip_and_mask() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
      <style>
        body { margin: 0; background: white; }
        svg { display: block; }
      </style>
      <svg width="20" height="20" viewBox="0 0 20 20" style="display: block">
        <defs>
          <linearGradient id="grad" x1="0" x2="1" y1="0" y2="0">
            <stop offset="0%" stop-color="red" />
            <stop offset="100%" stop-color="blue" />
          </linearGradient>
          <mask id="fade">
            <rect width="20" height="20" fill="white" />
            <rect width="10" height="20" fill="black" />
          </mask>
        </defs>
        <rect width="20" height="20" fill="url(#grad)" mask="url(#fade)" />
        <rect width="4" height="4" fill="black" transform="translate(12 2)" />
      </svg>
      "#;

      let serialized = serialized_inline_svg(html, 30.0, 30.0).expect("serialize svg");
      assert!(serialized.svg.contains("mask=\"url(#fade)\""));

      let cache = fastrender::image_loader::ImageCache::new();
      let svg_image = cache
        .render_svg(&serialized.svg)
        .expect("render serialized svg");
      let svg_rgba = svg_image.image.to_rgba8();
      let left_alpha = svg_rgba.get_pixel(8, 10)[3];
      let right_alpha = svg_rgba.get_pixel(14, 10)[3];
      assert!(
        left_alpha < right_alpha,
        "mask should reduce alpha in standalone rendering"
      );

      let pixmap = renderer.render_html(html, 30, 30).expect("render svg");
      assert_eq!(
        pixel(&pixmap, 13, 3),
        [0, 0, 0, 255],
        "transforms should offset shapes"
      );
      let masked = pixel(&pixmap, 8, 10);
      let visible = pixel(&pixmap, 14, 10);
      assert_eq!(
        masked,
        [255, 255, 255, 255],
        "masked region should show the page background"
      );
      assert_ne!(
        visible,
        [255, 255, 255, 255],
        "unmasked region should render content"
      );
      assert!(
        visible[2] > visible[0],
        "gradient should shift toward blue on the right"
      );
    })
    .unwrap()
    .join()
    .unwrap();
}

#[test]
fn inline_svg_renders_foreign_object_html() {
  std::thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(|| {
      let mut renderer = FastRender::new().expect("renderer");
      let html = r#"
      <style>
        body { margin: 0; background: white; }
        svg { display: block; }
      </style>
      <svg width="16" height="12" viewBox="0 0 16 12">
        <foreignObject x="0" y="0" width="10" height="12">
          <div xmlns="http://www.w3.org/1999/xhtml" style="width:10px;height:12px;background: rgb(0, 0, 255);"></div>
        </foreignObject>
      </svg>
      "#;

      let pixmap = renderer.render_html(html, 20, 20).expect("render svg");
      assert_eq!(pixel(&pixmap, 5, 6), [0, 0, 255, 255], "foreignObject content should paint");
      // Area outside the foreignObject should stay white.
      assert_eq!(pixel(&pixmap, 14, 6), [255, 255, 255, 255]);
    })
    .unwrap()
    .join()
    .unwrap();
}

#[test]
fn foreign_object_without_dimensions_uses_placeholder_comment() {
  let html = r#"
  <svg width="16" height="12" viewBox="0 0 16 12">
    <foreignObject>
      <div xmlns="http://www.w3.org/1999/xhtml" style="width:10px;height:12px;background: rgb(255, 0, 0);"></div>
    </foreignObject>
  </svg>
  "#;

  let serialized = serialized_inline_svg(html, 20.0, 20.0).expect("serialize svg");
  assert!(
    serialized
      .svg
      .contains("FASTRENDER_FOREIGN_OBJECT_UNRESOLVED"),
    "missing dimensions should keep placeholder path"
  );
}

#[test]
fn foreign_object_with_dimensions_emits_marker() {
  let html = r#"
  <svg width="16" height="12" viewBox="0 0 16 12">
    <foreignObject x="0" y="0" width="10" height="12">
      <div xmlns="http://www.w3.org/1999/xhtml" style="width:10px;height:12px;background: rgb(0, 255, 0);"></div>
    </foreignObject>
  </svg>
  "#;

  let serialized = serialized_inline_svg(html, 20.0, 20.0).expect("serialize svg");
  assert!(
    serialized.svg.contains("FASTRENDER_FOREIGN_OBJECT_0"),
    "foreignObject should be replaced with marker for nested rendering"
  );
  assert!(
    !serialized
      .svg
      .contains("FASTRENDER_FOREIGN_OBJECT_UNRESOLVED"),
    "valid dimensions should avoid unresolved placeholder comments"
  );
}

#[test]
fn foreign_object_accepts_absolute_units_for_dimensions() {
  let html = r#"
  <svg width="2in" height="2in" viewBox="0 0 192 192">
    <foreignObject x="1in" y="0" width="1in" height="1in">
      <div xmlns="http://www.w3.org/1999/xhtml" style="width:96px;height:96px;background: rgb(0, 255, 0);"></div>
    </foreignObject>
  </svg>
  "#;

  let serialized = serialized_inline_svg(html, 200.0, 200.0).expect("serialize svg");
  assert!(
    serialized.svg.contains("FASTRENDER_FOREIGN_OBJECT_0"),
    "absolute units should resolve to a valid foreignObject"
  );
  assert!(
    !serialized
      .svg
      .contains("FASTRENDER_FOREIGN_OBJECT_UNRESOLVED"),
    "converted dimensions should avoid unresolved placeholder"
  );
}
