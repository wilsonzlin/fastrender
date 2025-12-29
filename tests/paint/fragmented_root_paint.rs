use fastrender::api::FastRender;
use fastrender::geometry::{Point, Rect};
use fastrender::paint::painter::paint_tree;
use fastrender::style::color::Rgba;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode, FragmentTree};
use tiny_skia::Pixmap;

#[test]
fn paints_additional_fragment_roots() {
  let html = r#"
    <html>
      <head>
        <style>
          @page { size: 120px 80px; margin: 0; }
          body { margin: 0; }
        </style>
      </head>
      <body>
        <div style="height: 90px"></div>
        <p>Second page text</p>
      </body>
    </html>
  "#;

  let mut renderer = FastRender::new().unwrap();
  let dom = renderer.parse_html(html).unwrap();
  let tree = renderer.layout_document(&dom, 200, 200).unwrap();
  assert!(
    !tree.additional_fragments.is_empty(),
    "expected layout to create multiple fragment roots"
  );

  let text_bounds =
    find_text_bounds(&tree, "Second page text").expect("second page text fragment not found");
  assert!(
    text_bounds.min_y() >= tree.root.bounds.max_y(),
    "text should be placed after the first fragment root"
  );

  let content_bounds = tree.content_size();
  let width = content_bounds.max_x().ceil().max(1.0) as u32;
  let height = content_bounds.max_y().ceil().max(1.0) as u32;
  let background = Rgba::WHITE;
  let pixmap = paint_tree(&tree, width, height, background).expect("paint");

  assert!(
    rect_has_non_background(&pixmap, text_bounds, background),
    "page two text should paint non-background pixels"
  );
}

fn find_text_bounds(tree: &FragmentTree, needle: &str) -> Option<Rect> {
  for root in std::iter::once(&tree.root).chain(tree.additional_fragments.iter()) {
    if let Some(found) = find_text_bounds_inner(root, Point::ZERO, needle) {
      return Some(found);
    }
  }
  None
}

fn find_text_bounds_inner(fragment: &FragmentNode, offset: Point, needle: &str) -> Option<Rect> {
  let origin = Point::new(
    fragment.bounds.x() + offset.x,
    fragment.bounds.y() + offset.y,
  );
  if let FragmentContent::Text { text, .. } = &fragment.content {
    if text.contains(needle) {
      return Some(Rect::from_xywh(
        origin.x,
        origin.y,
        fragment.bounds.width(),
        fragment.bounds.height(),
      ));
    }
  }

  for child in fragment.children.iter() {
    if let Some(found) = find_text_bounds_inner(child, origin, needle) {
      return Some(found);
    }
  }
  None
}

fn rect_has_non_background(pixmap: &Pixmap, rect: Rect, background: Rgba) -> bool {
  let min_x = rect.min_x().floor().max(0.0) as u32;
  let min_y = rect.min_y().floor().max(0.0) as u32;
  let max_x = rect.max_x().ceil().min(pixmap.width() as f32).max(0.0) as u32;
  let max_y = rect.max_y().ceil().min(pixmap.height() as f32).max(0.0) as u32;

  if min_x >= max_x || min_y >= max_y {
    return false;
  }

  let bg = (
    background.r,
    background.g,
    background.b,
    background.alpha_u8(),
  );
  for y in min_y..max_y {
    for x in min_x..max_x {
      let p = pixmap.pixel(x, y).unwrap();
      if (p.red(), p.green(), p.blue(), p.alpha()) != bg {
        return true;
      }
    }
  }
  false
}
