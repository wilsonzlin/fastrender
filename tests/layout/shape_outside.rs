use fastrender::css::types::ColorStop;
use fastrender::geometry::Rect;
use fastrender::geometry::Size;
use fastrender::image_loader::ImageCache;
use fastrender::layout::constraints::LayoutConstraints;
use fastrender::layout::contexts::inline::InlineFormattingContext;
use fastrender::layout::float_context::{FloatContext, FloatSide};
use fastrender::layout::float_shape::build_float_shape;
use fastrender::style::color::Color;
use fastrender::style::color::Rgba;
use fastrender::style::display::FormattingContextType;
use fastrender::style::types::BackgroundImage;
use fastrender::style::types::BackgroundPosition;
use fastrender::style::types::BackgroundPositionComponent;
use fastrender::style::types::BasicShape;
use fastrender::style::types::Direction;
use fastrender::style::types::FillRule;
use fastrender::style::types::LineHeight;
use fastrender::style::types::ShapeOutside;
use fastrender::style::types::ShapeRadius;
use fastrender::style::values::Length;
use fastrender::style::ComputedStyle;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::{FragmentContent, FragmentNode};
use fastrender::BoxNode;
use std::sync::Arc;

fn circle_shape_style() -> ComputedStyle {
  let mut style = ComputedStyle::default();
  style.shape_outside = ShapeOutside::BasicShape(
    Box::new(BasicShape::Circle {
      radius: fastrender::style::types::ShapeRadius::Length(Length::percent(50.0)),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
        y: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
      },
    }),
    None,
  );
  style
}

fn collect_lines(fragment: &FragmentNode) -> Vec<FragmentNode> {
  let mut out = Vec::new();
  let mut stack = vec![fragment];
  while let Some(node) = stack.pop() {
    if matches!(node.content, FragmentContent::Line { .. }) {
      out.push(node.clone());
    }
    for child in node.children.iter() {
      stack.push(child);
    }
  }
  out.sort_by(|a, b| a.bounds.y().partial_cmp(&b.bounds.y()).unwrap());
  out
}

fn layout_lines_with_float(
  float_ctx: &mut FloatContext,
  text: &str,
  container_width: f32,
  line_height: f32,
  direction: Direction,
) -> Vec<FragmentNode> {
  let mut style = ComputedStyle::default();
  style.line_height = LineHeight::Length(Length::px(line_height));
  style.direction = direction;
  let style = Arc::new(style);
  let text_node = BoxNode::new_text(style.clone(), text.to_string());
  let root = BoxNode::new_block(style, FormattingContextType::Block, vec![text_node]);
  let ifc = InlineFormattingContext::new();
  let constraints = LayoutConstraints::definite_width(container_width);
  let fragment = ifc
    .layout_with_floats(&root, &constraints, Some(float_ctx), 0.0)
    .expect("layout with floats");
  collect_lines(&fragment)
}

#[test]
fn circle_shape_shortens_line_space() {
  let mut style = circle_shape_style();
  style.shape_margin = Length::px(0.0);
  let margin_rect = Rect::from_xywh(0.0, 0.0, 80.0, 80.0);
  let border_rect = Rect::from_xywh(0.0, 0.0, 80.0, 80.0);
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(80.0, 80.0),
    Size::new(200.0, 200.0),
    &FontContext::new(),
    &ImageCache::new(),
  )
  .expect("circle shape");

  let mut ctx = FloatContext::new(200.0);
  ctx.add_float_with_shape(
    FloatSide::Left,
    margin_rect.x(),
    margin_rect.y(),
    margin_rect.width(),
    margin_rect.height(),
    Some(shape),
  );

  let (left_edge_top, width_top) = ctx.available_width_at_y(0.0);
  assert!(
    left_edge_top > 45.0 && left_edge_top < 55.0,
    "left_edge_top={left_edge_top}"
  );
  assert!(
    (width_top - (200.0 - left_edge_top)).abs() < 0.5,
    "width_top={width_top} left_edge_top={left_edge_top}"
  );

  let (left_edge_mid, _) = ctx.available_width_at_y(40.0);
  assert!(
    left_edge_mid > 75.0 && left_edge_mid < 85.0,
    "left_edge_mid={left_edge_mid}"
  );
}

#[test]
fn right_float_uses_shape_contour() {
  let style = circle_shape_style();
  let margin_rect = Rect::from_xywh(120.0, 0.0, 80.0, 80.0);
  let border_rect = Rect::from_xywh(120.0, 0.0, 80.0, 80.0);
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(200.0, 80.0),
    Size::new(200.0, 200.0),
    &FontContext::new(),
    &ImageCache::new(),
  )
  .expect("circle shape");

  let mut ctx = FloatContext::new(200.0);
  ctx.add_float_with_shape(
    FloatSide::Right,
    margin_rect.x(),
    margin_rect.y(),
    margin_rect.width(),
    margin_rect.height(),
    Some(shape),
  );

  // Circle contour leaves more room at the top than the full float box.
  let right_edge = ctx.right_edge_at_y(0.0);
  assert!(right_edge > 150.0 && right_edge < 170.0);
}

#[test]
fn image_shape_respects_threshold() {
  let mut style = ComputedStyle::default();
  style.shape_image_threshold = 0.6;
  style.shape_outside = ShapeOutside::Image(BackgroundImage::LinearGradient {
    angle: 270.0,
    stops: vec![
      ColorStop {
        color: Color::Rgba(Rgba::new(0, 0, 0, 0.0)),
        position: Some(0.0),
      },
      ColorStop {
        color: Color::Rgba(Rgba::new(0, 0, 0, 1.0)),
        position: Some(1.0),
      },
    ],
  });
  let margin_rect = Rect::from_xywh(0.0, 0.0, 100.0, 40.0);
  let border_rect = margin_rect;
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(100.0, 40.0),
    Size::new(200.0, 200.0),
    &FontContext::new(),
    &ImageCache::new(),
  )
  .expect("gradient shape");

  let mut ctx = FloatContext::new(200.0);
  ctx.add_float_with_shape(
    FloatSide::Left,
    margin_rect.x(),
    margin_rect.y(),
    margin_rect.width(),
    margin_rect.height(),
    Some(shape),
  );

  let (left_edge, _) = ctx.available_width_at_y(0.0);
  assert!(
    left_edge > 35.0 && left_edge < 55.0,
    "left_edge={left_edge}"
  );
}

#[test]
fn none_falls_back_to_margin_box() {
  let mut style = ComputedStyle::default();
  style.shape_margin = Length::px(20.0);
  let margin_rect = Rect::from_xywh(0.0, 0.0, 60.0, 40.0);
  let border_rect = margin_rect;
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(60.0, 40.0),
    Size::new(200.0, 200.0),
    &FontContext::new(),
    &ImageCache::new(),
  );

  let mut ctx = FloatContext::new(200.0);
  ctx.add_float_with_shape(
    FloatSide::Left,
    margin_rect.x(),
    margin_rect.y(),
    margin_rect.width(),
    margin_rect.height(),
    shape,
  );

  let (left_edge, _) = ctx.available_width_at_y(10.0);
  assert!((left_edge - 60.0).abs() < 0.1);
}

#[test]
fn text_lines_follow_circle_shape() {
  let style = circle_shape_style();
  let margin_rect = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
  let border_rect = margin_rect;
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(100.0, 100.0),
    Size::new(400.0, 400.0),
    &FontContext::new(),
    &ImageCache::new(),
  )
  .expect("circle shape");

  let mut float_ctx = FloatContext::new(200.0);
  float_ctx.add_float_with_shape(
    FloatSide::Left,
    margin_rect.x(),
    margin_rect.y(),
    margin_rect.width(),
    margin_rect.height(),
    Some(shape),
  );

  let text = "wrapping around a float ".repeat(80);
  let lines = layout_lines_with_float(&mut float_ctx, &text, 200.0, 16.0, Direction::Ltr);
  assert!(lines.len() > 3, "expected multiple lines for wrapping");

  let first_x = lines.first().map(|l| l.bounds.x()).unwrap_or(0.0);
  assert!(first_x > 20.0 && first_x < margin_rect.width());

  if let Some(after_float) = lines.iter().find(|l| l.bounds.y() >= margin_rect.height()) {
    assert!(after_float.bounds.x() < 1.0);
  } else {
    panic!("expected a line after the float height");
  }
}

#[test]
fn right_float_ellipse_shortens_line_width() {
  let mut style = ComputedStyle::default();
  style.shape_outside = ShapeOutside::BasicShape(
    Box::new(BasicShape::Ellipse {
      radius_x: ShapeRadius::Length(Length::percent(50.0)),
      radius_y: ShapeRadius::Length(Length::percent(60.0)),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
        y: BackgroundPositionComponent {
          alignment: 0.5,
          offset: Length::px(0.0),
        },
      },
    }),
    None,
  );

  let margin_rect = Rect::from_xywh(120.0, 0.0, 80.0, 120.0);
  let border_rect = margin_rect;
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(80.0, 120.0),
    Size::new(400.0, 400.0),
    &FontContext::new(),
    &ImageCache::new(),
  )
  .expect("ellipse shape");

  let mut float_ctx = FloatContext::new(200.0);
  float_ctx.add_float_with_shape(
    FloatSide::Right,
    margin_rect.x(),
    margin_rect.y(),
    margin_rect.width(),
    margin_rect.height(),
    Some(shape),
  );

  let text = "ellipse wrapping text ".repeat(70);
  let lines = layout_lines_with_float(&mut float_ctx, &text, 200.0, 16.0, Direction::Ltr);
  let first = lines.first().expect("line");
  assert!(first.bounds.width() > 120.0 && first.bounds.width() < 200.0);

  let first_width = first.bounds.width();
  let max_width = lines
    .iter()
    .filter(|l| l.bounds.y() >= margin_rect.height())
    .map(|l| l.bounds.width())
    .fold(0.0, f32::max);
  assert!(
    max_width > first_width,
    "first_width={first_width} max_width={max_width}"
  );
}

#[test]
fn polygon_shape_wraps_rtl_text() {
  let mut style = ComputedStyle::default();
  style.shape_outside = ShapeOutside::BasicShape(
    Box::new(BasicShape::Polygon {
      fill: FillRule::NonZero,
      points: vec![
        (Length::percent(50.0), Length::percent(0.0)),
        (Length::percent(100.0), Length::percent(50.0)),
        (Length::percent(50.0), Length::percent(100.0)),
        (Length::percent(0.0), Length::percent(50.0)),
      ],
    }),
    None,
  );

  let margin_rect = Rect::from_xywh(0.0, 0.0, 100.0, 100.0);
  let border_rect = margin_rect;
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(100.0, 100.0),
    Size::new(400.0, 400.0),
    &FontContext::new(),
    &ImageCache::new(),
  )
  .expect("polygon shape");

  let mut float_ctx = FloatContext::new(220.0);
  float_ctx.add_float_with_shape(
    FloatSide::Left,
    0.0,
    0.0,
    margin_rect.width(),
    margin_rect.height(),
    Some(shape),
  );

  let text = "rtl shape wrapping ".repeat(70);
  let lines = layout_lines_with_float(&mut float_ctx, &text, 220.0, 14.0, Direction::Rtl);
  let first = lines.first().expect("line");
  assert!(first.bounds.x() > 0.0 && first.bounds.x() < margin_rect.width());
}

#[test]
fn image_shape_threshold_wraps_text() {
  let mut style = ComputedStyle::default();
  style.shape_image_threshold = 0.5;
  style.shape_outside = ShapeOutside::Image(BackgroundImage::LinearGradient {
    angle: 270.0,
    stops: vec![
      ColorStop {
        color: Color::Rgba(Rgba::new(0, 0, 0, 0.0)),
        position: Some(0.0),
      },
      ColorStop {
        color: Color::Rgba(Rgba::new(0, 0, 0, 1.0)),
        position: Some(1.0),
      },
    ],
  });

  let margin_rect = Rect::from_xywh(0.0, 0.0, 80.0, 40.0);
  let border_rect = margin_rect;
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(80.0, 40.0),
    Size::new(300.0, 300.0),
    &FontContext::new(),
    &ImageCache::new(),
  )
  .expect("image shape");

  let mut float_ctx = FloatContext::new(200.0);
  float_ctx.add_float_with_shape(
    FloatSide::Left,
    0.0,
    0.0,
    margin_rect.width(),
    margin_rect.height(),
    Some(shape),
  );

  let text = "image threshold wrapping ".repeat(50);
  let lines = layout_lines_with_float(&mut float_ctx, &text, 200.0, 14.0, Direction::Ltr);
  let first_x = lines.first().map(|l| l.bounds.x()).unwrap_or(0.0);
  assert!(first_x > 5.0 && first_x < 60.0, "first_x={first_x}");
}

#[test]
fn shape_none_wraps_to_box_bounds() {
  let mut style = ComputedStyle::default();
  style.shape_margin = Length::px(12.0);
  let margin_rect = Rect::from_xywh(0.0, 0.0, 60.0, 30.0);
  let border_rect = margin_rect;
  let shape = build_float_shape(
    &style,
    margin_rect,
    border_rect,
    Size::new(60.0, 30.0),
    Size::new(200.0, 200.0),
    &FontContext::new(),
    &ImageCache::new(),
  );

  let mut float_ctx = FloatContext::new(180.0);
  float_ctx.add_float_with_shape(
    FloatSide::Left,
    0.0,
    0.0,
    margin_rect.width(),
    margin_rect.height(),
    shape,
  );

  let text = "fallback to box".repeat(30);
  let lines = layout_lines_with_float(&mut float_ctx, &text, 180.0, 14.0, Direction::Ltr);
  let first_x = lines.first().map(|l| l.bounds.x()).unwrap_or(0.0);
  assert!(first_x > 55.0 && first_x < 65.0);
}
