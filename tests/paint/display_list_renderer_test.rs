use super::util::bounding_box_for_color;
use base64::engine::general_purpose::STANDARD;
use base64::Engine;
use fastrender::css::types::ColorStop;
use fastrender::geometry::Rect;
use fastrender::paint::display_list::BlendMode;
use fastrender::paint::display_list::BorderRadii;
use fastrender::paint::display_list::ClipItem;
use fastrender::paint::display_list::ClipShape;
use fastrender::paint::display_list::DecorationPaint;
use fastrender::paint::display_list::DecorationStroke;
use fastrender::paint::display_list::DisplayItem;
use fastrender::paint::display_list::DisplayList;
use fastrender::paint::display_list::FillRectItem;
use fastrender::paint::display_list::OutlineItem;
use fastrender::paint::display_list::ResolvedFilter;
use fastrender::paint::display_list::StackingContextItem;
use fastrender::paint::display_list::TextDecorationItem;
use fastrender::paint::display_list::Transform3D;
use fastrender::paint::display_list_builder::DisplayListBuilder;
use fastrender::paint::display_list_renderer::DisplayListRenderer;
use fastrender::style::color::Color;
use fastrender::style::types::BackfaceVisibility;
use fastrender::style::types::BackgroundImage;
use fastrender::style::types::BackgroundLayer;
use fastrender::style::types::BackgroundPosition;
use fastrender::style::types::BackgroundPositionComponent;
use fastrender::style::types::BackgroundRepeat;
use fastrender::style::types::BackgroundRepeatKeyword;
use fastrender::style::types::BackgroundSize;
use fastrender::style::types::BackgroundSizeComponent;
use fastrender::style::types::BasicShape;
use fastrender::style::types::BorderImage;
use fastrender::style::types::BorderImageSlice;
use fastrender::style::types::BorderImageSliceValue;
use fastrender::style::types::BorderImageSource;
use fastrender::style::types::BorderStyle;
use fastrender::style::types::ClipPath;
use fastrender::style::types::FillRule;
use fastrender::style::types::ImageRendering;
use fastrender::style::types::MixBlendMode;
use fastrender::style::types::ShapeRadius;
use fastrender::style::types::TextDecorationStyle;
use fastrender::style::types::TransformStyle;
use fastrender::style::values::Length;
use fastrender::text::font_loader::FontContext;
use fastrender::tree::fragment_tree::FragmentNode;
use fastrender::ComputedStyle;
use fastrender::Rgba;
use image::codecs::png::PngEncoder;
use image::ExtendedColorType;
use image::ImageEncoder;
use image::RgbaImage;
use std::sync::Arc;
use tiny_skia::Pixmap;

fn rgb_to_hsl(r: u8, g: u8, b: u8) -> (f32, f32, f32) {
  let r = r as f32 / 255.0;
  let g = g as f32 / 255.0;
  let b = b as f32 / 255.0;
  let max = r.max(g).max(b);
  let min = r.min(g).min(b);
  let l = (max + min) / 2.0;

  if (max - min).abs() < f32::EPSILON {
    return (0.0, 0.0, l);
  }

  let d = max - min;
  let s = if l > 0.5 {
    d / (2.0 - max - min)
  } else {
    d / (max + min)
  };
  let h = if (max - r).abs() < f32::EPSILON {
    (g - b) / d + if g < b { 6.0 } else { 0.0 }
  } else if (max - g).abs() < f32::EPSILON {
    (b - r) / d + 2.0
  } else {
    (r - g) / d + 4.0
  } / 6.0;

  (h, s, l)
}

fn hue_to_rgb(p: f32, q: f32, t: f32) -> f32 {
  let mut t = t;
  if t < 0.0 {
    t += 1.0;
  }
  if t > 1.0 {
    t -= 1.0;
  }
  if t < 1.0 / 6.0 {
    return p + (q - p) * 6.0 * t;
  }
  if t < 1.0 / 2.0 {
    return q;
  }
  if t < 2.0 / 3.0 {
    return p + (q - p) * (2.0 / 3.0 - t) * 6.0;
  }
  p
}

fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (u8, u8, u8) {
  if s <= 0.0 {
    let v = (l * 255.0).round().clamp(0.0, 255.0) as u8;
    return (v, v, v);
  }

  let q = if l < 0.5 {
    l * (1.0 + s)
  } else {
    l + s - l * s
  };
  let p = 2.0 * l - q;
  let r = hue_to_rgb(p, q, h + 1.0 / 3.0);
  let g = hue_to_rgb(p, q, h);
  let b = hue_to_rgb(p, q, h - 1.0 / 3.0);
  (
    (r * 255.0).round().clamp(0.0, 255.0) as u8,
    (g * 255.0).round().clamp(0.0, 255.0) as u8,
    (b * 255.0).round().clamp(0.0, 255.0) as u8,
  )
}

fn rgb_to_hsv(r: u8, g: u8, b: u8) -> (f32, f32, f32) {
  let r = r as f32 / 255.0;
  let g = g as f32 / 255.0;
  let b = b as f32 / 255.0;
  let max = r.max(g).max(b);
  let min = r.min(g).min(b);
  let v = max;
  let d = max - min;
  if d.abs() < f32::EPSILON {
    return (0.0, 0.0, v);
  }
  let s = if max > 0.0 { d / max } else { 0.0 };
  let h = if (max - r).abs() < f32::EPSILON {
    (g - b) / d + if g < b { 6.0 } else { 0.0 }
  } else if (max - g).abs() < f32::EPSILON {
    (b - r) / d + 2.0
  } else {
    (r - g) / d + 4.0
  } / 6.0;
  (h, s, v)
}

fn hsv_to_rgb(h: f32, s: f32, v: f32) -> (u8, u8, u8) {
  if s <= 0.0 {
    let v_u8 = (v * 255.0).round().clamp(0.0, 255.0) as u8;
    return (v_u8, v_u8, v_u8);
  }
  let hh = (h * 6.0) % 6.0;
  let i = hh.floor();
  let f = hh - i;
  let p = v * (1.0 - s);
  let q = v * (1.0 - s * f);
  let t = v * (1.0 - s * (1.0 - f));
  let (r, g, b) = match i as i32 {
    0 => (v, t, p),
    1 => (q, v, p),
    2 => (p, v, t),
    3 => (p, q, v),
    4 => (t, p, v),
    _ => (v, p, q),
  };
  (
    (r * 255.0).round().clamp(0.0, 255.0) as u8,
    (g * 255.0).round().clamp(0.0, 255.0) as u8,
    (b * 255.0).round().clamp(0.0, 255.0) as u8,
  )
}

fn srgb_to_linear(v: f32) -> f32 {
  if v <= 0.04045 {
    v / 12.92
  } else {
    ((v + 0.055) / 1.055).powf(2.4)
  }
}

fn linear_to_srgb(v: f32) -> f32 {
  if v <= 0.0031308 {
    v * 12.92
  } else {
    1.055 * v.powf(1.0 / 2.4) - 0.055
  }
}

fn rgb_to_oklch(r: u8, g: u8, b: u8) -> (f32, f32, f32) {
  let r = srgb_to_linear(r as f32 / 255.0);
  let g = srgb_to_linear(g as f32 / 255.0);
  let b = srgb_to_linear(b as f32 / 255.0);
  let l = 0.412_221_46 * r + 0.536_332_54 * g + 0.051_445_996 * b;
  let m = 0.211_903_5 * r + 0.680_699_5 * g + 0.107_396_96 * b;
  let s = 0.088_302_46 * r + 0.281_718_85 * g + 0.629_978_7 * b;

  let l_ = l.cbrt();
  let m_ = m.cbrt();
  let s_ = s.cbrt();

  let l_ok = 0.210_454_26 * l_ + 0.793_617_8 * m_ - 0.004_072_047 * s_;
  let a_ok = 1.977_998_5 * l_ - 2.428_592_2 * m_ + 0.450_593_7 * s_;
  let b_ok = 0.025_904_037 * l_ + 0.782_771_77 * m_ - 0.808_675_77 * s_;
  let c = (a_ok * a_ok + b_ok * b_ok).sqrt();
  let h = b_ok.atan2(a_ok).to_degrees().rem_euclid(360.0);
  (l_ok, c, h)
}

fn oklch_to_rgb(l: f32, c: f32, h: f32) -> (u8, u8, u8) {
  let hr = h.to_radians();
  let a = c * hr.cos();
  let b = c * hr.sin();
  let l_ = l + 0.396_337_78 * a + 0.215_803_76 * b;
  let m_ = l - 0.105_561_35 * a - 0.063_854_17 * b;
  let s_ = l - 0.089_484_18 * a - 1.291_485_5 * b;

  let l3 = l_.powf(3.0);
  let m3 = m_.powf(3.0);
  let s3 = s_.powf(3.0);

  let r = 4.076_741_7 * l3 - 3.307_711_6 * m3 + 0.230_969_94 * s3;
  let g = -1.268_438_ * l3 + 2.609_757_4 * m3 - 0.341_319_38 * s3;
  let b = 0.004_421_97 * l3 - 0.703_418_6 * m3 + 1.698_594_8 * s3;

  (
    (linear_to_srgb(r).clamp(0.0, 1.0) * 255.0)
      .round()
      .clamp(0.0, 255.0) as u8,
    (linear_to_srgb(g).clamp(0.0, 1.0) * 255.0)
      .round()
      .clamp(0.0, 255.0) as u8,
    (linear_to_srgb(b).clamp(0.0, 1.0) * 255.0)
      .round()
      .clamp(0.0, 255.0) as u8,
  )
}

fn blend_hue(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
  let (sh, _, _) = rgb_to_hsl(src.0, src.1, src.2);
  let (_, ds, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
  hsl_to_rgb(sh, ds, dl)
}

fn blend_saturation(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
  let (_, ss, _) = rgb_to_hsl(src.0, src.1, src.2);
  let (dh, _, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
  hsl_to_rgb(dh, ss, dl)
}

fn blend_color(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
  let (sh, ss, _) = rgb_to_hsl(src.0, src.1, src.2);
  let (_, _, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
  hsl_to_rgb(sh, ss, dl)
}

fn blend_luminosity(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
  let (_, _, sl) = rgb_to_hsl(src.0, src.1, src.2);
  let (dh, ds, _) = rgb_to_hsl(dst.0, dst.1, dst.2);
  hsl_to_rgb(dh, ds, sl)
}

fn blend_hue_hsv(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
  let (sh, _, _) = rgb_to_hsv(src.0, src.1, src.2);
  let (_, ds, dv) = rgb_to_hsv(dst.0, dst.1, dst.2);
  hsv_to_rgb(sh, ds, dv)
}

fn blend_color_oklch(src: (u8, u8, u8), dst: (u8, u8, u8)) -> (u8, u8, u8) {
  let (_sl, sc, sh) = rgb_to_oklch(src.0, src.1, src.2);
  let (dl, _, _) = rgb_to_oklch(dst.0, dst.1, dst.2);
  // Hue and chroma from source, lightness from destination.
  oklch_to_rgb(dl, sc, sh)
}

fn hue_delta(a: f32, b: f32) -> f32 {
  let d = (a - b).abs();
  d.min(1.0 - d)
}

fn assert_hsl_components(
  actual: (u8, u8, u8),
  expected_hsl: (f32, f32, f32),
  tol_h: f32,
  tol_s: f32,
  tol_l: f32,
  context: &str,
) {
  let (h, s, l) = rgb_to_hsl(actual.0, actual.1, actual.2);
  assert!(
    hue_delta(h, expected_hsl.0) <= tol_h
      && (s - expected_hsl.1).abs() <= tol_s
      && (l - expected_hsl.2).abs() <= tol_l,
    "{context}: expected hsl {:?}, got hsl ({h:.3},{s:.3},{l:.3})",
    expected_hsl
  );
}

fn assert_oklch_components(
  actual: (u8, u8, u8),
  expected: (f32, f32, f32),
  tol_l: f32,
  tol_c: f32,
  tol_h: f32,
  context: &str,
) {
  let (al, ac, ah) = rgb_to_oklch(actual.0, actual.1, actual.2);
  assert!(
    (al - expected.0).abs() <= tol_l
      && (ac - expected.1).abs() <= tol_c
      && hue_delta(ah / 360.0, expected.2 / 360.0) * 360.0 <= tol_h,
    "{context}: expected oklch {:?}, got oklch ({al:.3},{ac:.3},{ah:.3})",
    expected
  );
}

fn pixel(pixmap: &tiny_skia::Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
  let px = pixmap.pixel(x, y).unwrap();
  (px.red(), px.green(), px.blue(), px.alpha())
}

#[test]
fn mix_blend_mode_multiplies_backdrop_when_not_isolated() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    mix_blend_mode: BlendMode::Multiply,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  let center = pixel(&pixmap, 5, 5);
  assert!(
    center.0 < 10 && center.1 < 10 && center.2 < 10,
    "multiply should darken the red backdrop, got {center:?}"
  );
}

#[test]
fn isolation_blocks_mix_blend_mode_backdrop() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    mix_blend_mode: BlendMode::Multiply,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  let center = pixel(&pixmap, 5, 5);
  assert_eq!(
    center,
    (0, 0, 255, 255),
    "isolated context should composite with source-over, got {center:?}"
  );
}

#[test]
fn backdrop_filter_isolates_blend_mode() {
  let mut list = DisplayList::new();
  // Backdrop starts red.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    color: Rgba::RED,
  }));

  // Apply blur to ensure backdrop filters are present but shouldn't affect blending mode choice.
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    mix_blend_mode: BlendMode::Multiply,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: vec![ResolvedFilter::Blur(0.1)],
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Backdrop filters implicitly isolate the context, so the multiply blend should not darken.
  assert_eq!(pixel(&pixmap, 1, 1), (0, 0, 255, 255));
}

#[test]
fn matrix3d_transforms_translate_content() {
  let renderer = DisplayListRenderer::new(20, 20, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(Transform3D {
      m: [
        1.0, 0.0, 0.0, 0.0, // column 1
        0.0, 1.0, 0.0, 0.0, // column 2
        0.0, 0.0, 1.0, 0.0, // column 3
        5.0, 7.0, 0.0, 1.0, // column 4 (translation)
      ],
    }),
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    color: Rgba::BLACK,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  assert_eq!(pixel(&pixmap, 5, 7), (0, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
}

#[test]
fn perspective_depth_changes_projection_size() {
  let renderer = DisplayListRenderer::new(40, 40, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  let perspective =
    Transform3D::perspective(500.0).multiply(&Transform3D::translate(0.0, 0.0, 200.0));
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(perspective),
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  let Some((min_x, min_y, max_x, max_y)) =
    bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > g && r > b)
  else {
    panic!("expected painted content");
  };
  let width = max_x - min_x + 1;
  let height = max_y - min_y + 1;
  assert!(
    width >= 15 && height >= 15,
    "perspective should expand projected size, got {width}x{height} from bbox ({min_x},{min_y})-({max_x},{max_y})"
  );
}

#[test]
fn backface_hidden_culls_rotated_context() {
  let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    mix_blend_mode: BlendMode::Normal,
    is_isolated: false,
    transform: Some(Transform3D::rotate_x(std::f32::consts::PI)),
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Hidden,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  let bbox = bounding_box_for_color(&pixmap, |(r, g, b, _)| r < 250 || g < 250 || b < 250);
  assert!(
    bbox.is_none(),
    "backface-hidden context should be culled, found {bbox:?}"
  );
}

fn two_color_data_url() -> String {
  let pixels: [u8; 8] = [255, 0, 0, 255, 0, 0, 255, 255];
  let mut buf = Vec::new();
  PngEncoder::new(&mut buf)
    .write_image(&pixels, 2, 1, ExtendedColorType::Rgba8)
    .expect("encode png");
  format!("data:image/png;base64,{}", STANDARD.encode(&buf))
}

#[test]
fn builder_clip_path_masks_rendered_output() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Rgba::RED;
  style.clip_path = ClipPath::BasicShape(
    Box::new(BasicShape::Circle {
      radius: ShapeRadius::Length(Length::px(3.0)),
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

  let fragment = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    vec![],
    Arc::new(style),
  );

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Center pixel should be clipped in, corner should remain the clear background.
  assert_eq!(pixel(&pixmap, 5, 5), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
}

#[test]
fn builder_clip_path_polygon_masks_rendered_output() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Rgba::RED;
  style.clip_path = ClipPath::BasicShape(
    Box::new(BasicShape::Polygon {
      fill: FillRule::NonZero,
      points: vec![
        (Length::px(0.0), Length::px(0.0)),
        (Length::px(0.0), Length::px(10.0)),
        (Length::px(10.0), Length::px(0.0)),
      ],
    }),
    None,
  );

  let fragment = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    vec![],
    Arc::new(style),
  );

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Pixel inside triangle should be filled; bottom-right should stay white.
  assert_eq!(pixel(&pixmap, 2, 2), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 9, 9), (255, 255, 255, 255));
}

#[test]
fn builder_clip_path_inset_masks_rendered_output() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Rgba::RED;
  style.clip_path = ClipPath::BasicShape(
    Box::new(BasicShape::Inset {
      top: Length::px(2.0),
      right: Length::px(2.0),
      bottom: Length::px(2.0),
      left: Length::px(2.0),
      border_radius: Box::new(None),
    }),
    None,
  );

  let fragment = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    vec![],
    Arc::new(style),
  );

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Center pixels should be clipped in; corners remain white per the background.
  assert_eq!(pixel(&pixmap, 5, 5), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 1, 1), (255, 255, 255, 255));
}

#[test]
fn color_mix_srgb_renders_expected_color() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Color::parse("color-mix(in srgb, red 25%, blue 75%)")
    .unwrap()
    .to_rgba(Rgba::BLACK);

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build(&fragment);
  let pixmap = DisplayListRenderer::new(1, 1, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  let expected = Color::parse("color-mix(in srgb, red 25%, blue 75%)")
    .unwrap()
    .to_rgba(Rgba::BLACK);
  assert_eq!(
    pixel(&pixmap, 0, 0),
    (expected.r, expected.g, expected.b, expected.alpha_u8())
  );
}

#[test]
fn color_mix_srgb_linear_matches_resolved_color() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Color::parse("color-mix(in srgb-linear, red 50%, blue 50%)")
    .unwrap()
    .to_rgba(Rgba::BLACK);

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build(&fragment);
  let pixmap = DisplayListRenderer::new(1, 1, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  let expected = Color::parse("color-mix(in srgb-linear, red 50%, blue 50%)")
    .unwrap()
    .to_rgba(Rgba::BLACK);
  assert_eq!(
    pixel(&pixmap, 0, 0),
    (expected.r, expected.g, expected.b, expected.alpha_u8())
  );
}

#[test]
fn color_mix_current_color_resolves_in_renderer() {
  let mut style = fastrender::ComputedStyle::default();
  style.color = Rgba::GREEN;
  style.background_color = Color::parse("color-mix(in srgb, currentColor 50%, blue 50%)")
    .unwrap()
    .to_rgba(style.color);

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build(&fragment);
  let pixmap = DisplayListRenderer::new(1, 1, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  let expected = Color::parse("color-mix(in srgb, currentColor 50%, blue 50%)")
    .unwrap()
    .to_rgba(Rgba::GREEN);
  assert_eq!(
    pixel(&pixmap, 0, 0),
    (expected.r, expected.g, expected.b, expected.alpha_u8())
  );
}

#[test]
fn display_list_renderer_paints_text_decoration_color() {
  let mut list = DisplayList::new();
  let decoration = DecorationPaint {
    style: TextDecorationStyle::Solid,
    color: Rgba::RED,
    underline: Some(DecorationStroke {
      center: 5.0,
      thickness: 2.0,
      segments: None,
    }),
    overline: None,
    line_through: None,
  };

  list.push(DisplayItem::TextDecoration(TextDecorationItem {
    bounds: Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
    line_start: 0.0,
    line_width: 20.0,
    inline_vertical: false,
    decorations: vec![decoration],
  }));

  let renderer = DisplayListRenderer::new(20, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  assert_eq!(pixel(&pixmap, 10, 5), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 10, 0), (255, 255, 255, 255));
}

#[test]
fn text_decoration_segments_offset_by_line_start() {
  // Ensure decoration segments are translated by line_start when rendering (skip-ink segments).
  let mut list = DisplayList::new();
  list.push(DisplayItem::TextDecoration(TextDecorationItem {
    bounds: Rect::from_xywh(0.0, 0.0, 30.0, 10.0),
    line_start: 10.0,
    line_width: 10.0,
    inline_vertical: false,
    decorations: vec![DecorationPaint {
      style: TextDecorationStyle::Solid,
      color: Rgba::BLACK,
      underline: Some(DecorationStroke {
        center: 5.0,
        thickness: 2.0,
        segments: Some(vec![(2.0, 4.0)]),
      }),
      overline: None,
      line_through: None,
    }],
  }));

  let renderer = DisplayListRenderer::new(30, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Painted pixels should appear around x ~12–13 (line_start + segment start/end).
  let mut min_x = u32::MAX;
  let mut max_x = 0;
  for (idx, chunk) in pixmap.data().chunks(4).enumerate() {
    if chunk[0] == 255 && chunk[1] == 255 && chunk[2] == 255 {
      continue;
    }
    let x = (idx as u32) % pixmap.width();
    min_x = min_x.min(x);
    max_x = max_x.max(x);
  }

  assert!(
    (10..=13).contains(&min_x),
    "min_x out of expected range: {}",
    min_x
  );
  assert!(
    (12..=14).contains(&max_x),
    "max_x out of expected range: {}",
    max_x
  );
}

#[test]
fn text_decoration_currentcolor_resolves_in_display_list() {
  // Ensure currentColor on text decorations resolves against the text color when emitting display list items.
  let mut style = fastrender::ComputedStyle::default();
  style.color = Rgba::rgb(10, 20, 30);
  style.text_decoration.lines = fastrender::style::types::TextDecorationLine::UNDERLINE;
  style.text_decoration.color = None; // currentColor
  style.text_decoration.thickness =
    fastrender::style::types::TextDecorationThickness::Length(Length::px(2.0));

  let mut fragment =
    FragmentNode::new_text(Rect::from_xywh(0.0, 0.0, 20.0, 10.0), "hi".to_string(), 0.0);
  fragment.style = Some(Arc::new(style));

  let list = DisplayListBuilder::new().build(&fragment);
  let deco = list
    .items()
    .iter()
    .find_map(|i| match i {
      DisplayItem::TextDecoration(d) => Some(d),
      _ => None,
    })
    .expect("decoration item");

  let underline = deco
    .decorations
    .first()
    .and_then(|d| d.underline.as_ref())
    .expect("underline present");

  // currentColor should resolve to the text color with the specified thickness
  assert_eq!(
    deco.decorations.first().unwrap().color,
    Rgba::rgb(10, 20, 30)
  );
  assert_eq!(underline.thickness, 2.0);

  let pixmap = DisplayListRenderer::new(30, 20, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .expect("render");

  // Verify some pixels are non-white (decoration rendered) and dominated by the text color channels.
  let mut max_r = 0u8;
  let mut max_g = 0u8;
  let mut max_b = 0u8;
  for chunk in pixmap.data().chunks(4) {
    if chunk[0] == 255 && chunk[1] == 255 && chunk[2] == 255 {
      continue;
    }
    max_r = max_r.max(chunk[0]);
    max_g = max_g.max(chunk[1]);
    max_b = max_b.max(chunk[2]);
  }
  assert!(
    max_r >= 10 && max_g >= 20 && max_b >= 30,
    "decoration should resolve currentColor"
  );
}

fn bbox_for_color(
  pixmap: &Pixmap,
  pred: impl Fn((u8, u8, u8, u8)) -> bool,
) -> Option<(u32, u32, u32, u32)> {
  let mut min_x = u32::MAX;
  let mut min_y = u32::MAX;
  let mut max_x = 0u32;
  let mut max_y = 0u32;

  let width = pixmap.width();
  let data = pixmap.data();
  for y in 0..pixmap.height() {
    for x in 0..width {
      let idx = ((y * width + x) * 4) as usize;
      let p = (data[idx], data[idx + 1], data[idx + 2], data[idx + 3]);
      if pred(p) {
        min_x = min_x.min(x);
        min_y = min_y.min(y);
        max_x = max_x.max(x);
        max_y = max_y.max(y);
      }
    }
  }

  if min_x == u32::MAX {
    None
  } else {
    Some((min_x, min_y, max_x, max_y))
  }
}

#[test]
fn marker_text_shadow_is_rendered_in_display_list() {
  let mut style = ComputedStyle::default();
  style.display = fastrender::style::display::Display::Inline;
  style.color = Rgba::BLACK;
  style.font_size = 16.0;
  style.text_shadow = vec![fastrender::css::types::TextShadow {
    offset_x: Length::px(3.0),
    offset_y: Length::px(0.0),
    blur_radius: Length::px(0.0),
    color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
  }];
  let style = Arc::new(style);

  let mut marker = FragmentNode::new_text(
    Rect::from_xywh(10.0, 10.0, 20.0, 20.0),
    "•".to_string(),
    16.0,
  );
  marker.content = fastrender::tree::fragment_tree::FragmentContent::Text {
    text: "•".to_string(),
    box_id: None,
    baseline_offset: 16.0,
    shaped: None,
    is_marker: true,
  };
  marker.style = Some(style);

  let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 40.0, 30.0), vec![marker]);
  let list = DisplayListBuilder::new().build(&root);

  let pixmap = DisplayListRenderer::new(60, 40, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .expect("render");

  let glyph_bbox = bbox_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
    .expect("marker glyph");
  let shadow_bbox = bbox_for_color(&pixmap, |(r, g, b, _)| {
    let (r, g, b) = (r as u16, g as u16, b as u16);
    r > g + 20 && r > b + 20
  })
  .expect("marker shadow");

  assert!(
    shadow_bbox.0 > glyph_bbox.0,
    "shadow should render to the inline end of the marker glyph"
  );
  assert!(
    shadow_bbox.1.abs_diff(glyph_bbox.1) <= 2,
    "shadow should stay vertically aligned with the marker glyph"
  );
}

#[test]
fn display_list_border_image_nine_slice() {
  // Construct a 3x3 image with distinct corners/edges to verify nine-slice placement.
  let mut img = RgbaImage::new(3, 3);
  img.put_pixel(0, 0, image::Rgba([255, 0, 0, 255])); // TL red
  img.put_pixel(2, 0, image::Rgba([0, 0, 255, 255])); // TR blue
  img.put_pixel(0, 2, image::Rgba([0, 255, 0, 255])); // BL green
  img.put_pixel(2, 2, image::Rgba([255, 255, 0, 255])); // BR yellow
  let edge = image::Rgba([0, 255, 255, 255]);
  img.put_pixel(1, 0, edge);
  img.put_pixel(1, 2, edge);
  img.put_pixel(0, 1, edge);
  img.put_pixel(2, 1, edge);
  img.put_pixel(1, 1, image::Rgba([255, 255, 255, 255]));

  let mut buf = Vec::new();
  PngEncoder::new(&mut buf)
    .write_image(img.as_raw(), 3, 3, ExtendedColorType::Rgba8)
    .unwrap();
  let data_url = format!("data:image/png;base64,{}", STANDARD.encode(&buf));

  let mut style = fastrender::ComputedStyle::default();
  style.border_top_width = Length::px(4.0);
  style.border_right_width = Length::px(4.0);
  style.border_bottom_width = Length::px(4.0);
  style.border_left_width = Length::px(4.0);
  style.border_top_style = BorderStyle::Solid;
  style.border_right_style = BorderStyle::Solid;
  style.border_bottom_style = BorderStyle::Solid;
  style.border_left_style = BorderStyle::Solid;
  style.border_image = BorderImage {
    source: BorderImageSource::Image(Box::new(BackgroundImage::Url(data_url))),
    slice: BorderImageSlice {
      top: BorderImageSliceValue::Number(1.0),
      right: BorderImageSliceValue::Number(1.0),
      bottom: BorderImageSliceValue::Number(1.0),
      left: BorderImageSliceValue::Number(1.0),
      fill: false,
    },
    ..BorderImage::default()
  };

  let fragment = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 16.0, 16.0),
    vec![],
    Arc::new(style),
  );

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(16, 16, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  let tl = pixel(&pixmap, 0, 0);
  let tr = pixel(&pixmap, 15, 0);
  let bl = pixel(&pixmap, 0, 15);
  let br = pixel(&pixmap, 15, 15);
  assert_eq!(tl, (255, 0, 0, 255));
  assert_eq!(tr, (0, 0, 255, 255));
  assert_eq!(bl, (0, 255, 0, 255));
  assert_eq!(br, (255, 255, 0, 255));

  let edge_top = pixel(&pixmap, 8, 1);
  let edge_left = pixel(&pixmap, 1, 8);
  assert_eq!(edge_top, (0, 255, 255, 255));
  assert_eq!(edge_left, (0, 255, 255, 255));
}

#[test]
fn background_attachment_local_clips_to_padding_box_in_display_list() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Rgba::TRANSPARENT;
  style.set_background_layers(vec![fastrender::style::types::BackgroundLayer {
    image: Some(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![
        ColorStop {
          color: fastrender::Color::Rgba(Rgba::RED),
          position: Some(0.0),
        },
        ColorStop {
          color: fastrender::Color::Rgba(Rgba::BLUE),
          position: Some(1.0),
        },
      ],
    }),
    position: BackgroundPosition::Position {
      x: BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::px(0.0),
      },
      y: BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::px(0.0),
      },
    },
    size: fastrender::style::types::BackgroundSize::Keyword(
      fastrender::style::types::BackgroundSizeKeyword::Contain,
    ),
    repeat: fastrender::style::types::BackgroundRepeat {
      x: fastrender::style::types::BackgroundRepeatKeyword::NoRepeat,
      y: fastrender::style::types::BackgroundRepeatKeyword::NoRepeat,
    },
    origin: fastrender::style::types::BackgroundBox::PaddingBox,
    clip: fastrender::style::types::BackgroundBox::PaddingBox,
    attachment: fastrender::style::types::BackgroundAttachment::Local,
    ..Default::default()
  }]);
  style.padding_top = Length::px(2.0);
  style.padding_right = Length::px(2.0);
  style.padding_bottom = Length::px(2.0);
  style.padding_left = Length::px(2.0);
  let fragment = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    vec![],
    Arc::new(style),
  );

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(10, 10, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Background should anchor to the padding box; interior should paint.
  assert_ne!(pixel(&pixmap, 3, 3), (255, 255, 255, 255));
}

#[test]
fn display_list_border_image_generated_uniform_color() {
  let mut style = fastrender::ComputedStyle::default();
  style.border_top_width = Length::px(4.0);
  style.border_right_width = Length::px(4.0);
  style.border_bottom_width = Length::px(4.0);
  style.border_left_width = Length::px(4.0);
  style.border_top_style = BorderStyle::Solid;
  style.border_right_style = BorderStyle::Solid;
  style.border_bottom_style = BorderStyle::Solid;
  style.border_left_style = BorderStyle::Solid;
  style.border_image = BorderImage {
    source: BorderImageSource::Image(Box::new(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![
        ColorStop {
          color: fastrender::Color::Rgba(Rgba::RED),
          position: Some(0.0),
        },
        ColorStop {
          color: fastrender::Color::Rgba(Rgba::RED),
          position: Some(1.0),
        },
      ],
    })),
    slice: BorderImageSlice {
      top: BorderImageSliceValue::Number(1.0),
      right: BorderImageSliceValue::Number(1.0),
      bottom: BorderImageSliceValue::Number(1.0),
      left: BorderImageSliceValue::Number(1.0),
      fill: false,
    },
    ..BorderImage::default()
  };

  let fragment = FragmentNode::new_block_styled(
    Rect::from_xywh(0.0, 0.0, 16.0, 16.0),
    vec![],
    Arc::new(style),
  );

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(16, 16, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Uniform gradient should behave like a solid image for border painting.
  let tl = pixel(&pixmap, 0, 0);
  let tr = pixel(&pixmap, 15, 0);
  let bl = pixel(&pixmap, 0, 15);
  let br = pixel(&pixmap, 15, 15);
  assert_eq!(tl, (255, 0, 0, 255));
  assert_eq!(tr, (255, 0, 0, 255));
  assert_eq!(bl, (255, 0, 0, 255));
  assert_eq!(br, (255, 0, 0, 255));
}

#[test]
fn display_list_background_pixelated_uses_nearest_sampling() {
  let url = two_color_data_url();

  let mut style = fastrender::ComputedStyle::default();
  style.image_rendering = ImageRendering::Pixelated;
  style.background_color = Rgba::WHITE;
  style.background_layers = vec![BackgroundLayer {
    image: Some(BackgroundImage::Url(url)),
    size: BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::px(5.0)),
      BackgroundSizeComponent::Length(Length::px(1.0)),
    ),
    repeat: BackgroundRepeat {
      x: BackgroundRepeatKeyword::NoRepeat,
      y: BackgroundRepeatKeyword::NoRepeat,
    },
    ..BackgroundLayer::default()
  }];

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 5.0, 1.0), vec![], Arc::new(style));
  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(5, 1, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Pixelated sampling should keep the left half fully red and the right half blue without purple blending.
  assert_eq!(pixel(&pixmap, 1, 0), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 3, 0), (0, 0, 255, 255));
}

#[test]
fn display_list_background_smooth_blends_when_upscaled() {
  let url = two_color_data_url();

  let mut style = fastrender::ComputedStyle::default();
  style.image_rendering = ImageRendering::Auto;
  style.background_color = Rgba::WHITE;
  style.background_layers = vec![BackgroundLayer {
    image: Some(BackgroundImage::Url(url)),
    size: BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::px(5.0)),
      BackgroundSizeComponent::Length(Length::px(1.0)),
    ),
    repeat: BackgroundRepeat {
      x: BackgroundRepeatKeyword::NoRepeat,
      y: BackgroundRepeatKeyword::NoRepeat,
    },
    ..BackgroundLayer::default()
  }];

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 5.0, 1.0), vec![], Arc::new(style));
  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(5, 1, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  let mid = pixel(&pixmap, 2, 0);
  // Linear sampling should blend the two source pixels, producing a non-primary color.
  assert_ne!(mid, (255, 0, 0, 255));
  assert_ne!(mid, (0, 0, 255, 255));
  assert!(
    mid.0 > 0 && mid.2 > 0,
    "mid pixel should be blended: {:?}",
    mid
  );
}

#[test]
fn display_list_linear_gradient_respects_background_size() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_layers = vec![BackgroundLayer {
    image: Some(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![
        ColorStop {
          color: fastrender::Color::Rgba(Rgba::RED),
          position: Some(0.0),
        },
        ColorStop {
          color: fastrender::Color::Rgba(Rgba::RED),
          position: Some(1.0),
        },
      ],
    }),
    size: BackgroundSize::Explicit(
      BackgroundSizeComponent::Length(Length::px(2.0)),
      BackgroundSizeComponent::Length(Length::px(2.0)),
    ),
    repeat: BackgroundRepeat {
      x: BackgroundRepeatKeyword::NoRepeat,
      y: BackgroundRepeatKeyword::NoRepeat,
    },
    ..Default::default()
  }];

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 4.0, 4.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Gradient tile should be confined to the 2x2 background-size at the top-left.
  assert_eq!(pixel(&pixmap, 1, 1), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 3, 3), (255, 255, 255, 255));
}

#[test]
fn filters_apply_to_stacking_context_layer() {
  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Invert(1.0)],
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  // Inverting blue yields yellow.
  assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 0, 255));
}

#[test]
fn opacity_filter_modulates_alpha() {
  let renderer = DisplayListRenderer::new(2, 2, Rgba::TRANSPARENT, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Opacity(0.5)],
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, a) = pixel(&pixmap, 0, 0);
  assert_eq!((r, g, b), (128, 0, 0));
  assert!(
    (a as i16 - 128).abs() <= 1,
    "expected ~50% alpha, got {}",
    a
  );
}

#[test]
fn zero_opacity_fragments_render_nothing() {
  let mut style = ComputedStyle::default();
  style.background_color = Rgba::RED;
  style.opacity = 0.0;
  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 4.0, 4.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build_with_stacking_tree(&fragment);
  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // All pixels should remain the white background since the fragment is fully transparent.
  for y in 0..4 {
    for x in 0..4 {
      assert_eq!(pixel(&pixmap, x, y), (255, 255, 255, 255));
    }
  }
}

#[test]
fn backdrop_filters_modify_backdrop_region() {
  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: vec![ResolvedFilter::Invert(1.0)],
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  // Inside backdrop-filtered region red should invert to cyan.
  assert_eq!(pixel(&pixmap, 1, 1), (0, 255, 255, 255));
  // Outside region remains red.
  assert_eq!(pixel(&pixmap, 0, 0), (255, 0, 0, 255));
}

#[test]
fn backdrop_filter_respects_clip_path_mask() {
  let renderer = DisplayListRenderer::new(8, 8, Rgba::RED, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  let triangle = fastrender::paint::clip_path::ResolvedClipPath::Polygon {
    points: vec![
      fastrender::geometry::Point::new(1.0, 1.0),
      fastrender::geometry::Point::new(5.0, 1.0),
      fastrender::geometry::Point::new(1.0, 5.0),
    ],
    fill_rule: FillRule::Winding,
  };
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: vec![ResolvedFilter::Invert(1.0)],
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Path { path: triangle },
  }));
  list.push(DisplayItem::PopClip);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).expect("render");
  assert_eq!(pixel(&pixmap, 2, 2), (0, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 4, 4), (255, 0, 0, 255));
}

#[test]
fn backdrop_filter_respects_rounded_clip_rect() {
  let renderer = DisplayListRenderer::new(8, 8, Rgba::RED, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(1.0, 1.0, 6.0, 6.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: vec![ResolvedFilter::Invert(1.0)],
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Rect {
      rect: Rect::from_xywh(1.0, 1.0, 6.0, 6.0),
      radii: Some(BorderRadii::uniform(2.0)),
    },
  }));
  list.push(DisplayItem::PopClip);
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).expect("render");
  assert_eq!(pixel(&pixmap, 3, 3), (0, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 1, 1), (255, 0, 0, 255));
}

#[test]
fn background_blend_mode_combines_multiple_layers() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Rgba::WHITE;

  let top_layer = BackgroundLayer {
    image: Some(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![
        ColorStop {
          color: Color::Rgba(Rgba::GREEN),
          position: Some(0.0),
        },
        ColorStop {
          color: Color::Rgba(Rgba::GREEN),
          position: Some(1.0),
        },
      ],
    }),
    repeat: BackgroundRepeat::no_repeat(),
    blend_mode: MixBlendMode::Multiply,
    ..BackgroundLayer::default()
  };

  let bottom_layer = BackgroundLayer {
    image: Some(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![
        ColorStop {
          color: Color::Rgba(Rgba::RED),
          position: Some(0.0),
        },
        ColorStop {
          color: Color::Rgba(Rgba::RED),
          position: Some(1.0),
        },
      ],
    }),
    repeat: BackgroundRepeat::no_repeat(),
    blend_mode: MixBlendMode::Normal,
    ..BackgroundLayer::default()
  };

  style.set_background_layers(vec![top_layer, bottom_layer]);

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![], Arc::new(style));
  let list = DisplayListBuilder::new().build(&fragment);
  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).expect("renderer");
  let pixmap = renderer.render(&list).expect("render backgrounds");

  // Green multiplied by red should produce black in the covered area.
  assert_eq!(pixel(&pixmap, 1, 1), (0, 0, 0, 255));
}

#[test]
fn drop_shadow_filter_renders_shadow() {
  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::DropShadow {
      offset_x: 1.0,
      offset_y: 0.0,
      blur_radius: 0.0,
      spread: 0.0,
      color: Rgba::BLACK,
    }],
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  // Shadow offset right by 1px should leave black pixel at (2,0).
  assert_eq!(pixel(&pixmap, 2, 0), (0, 0, 0, 255));
  // Original content stays blue.
  assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 255, 255));
}

#[test]
fn blur_filters_arent_clipped_by_border_radii() {
  use fastrender::paint::display_list::BorderRadii;

  let renderer = DisplayListRenderer::new(4, 4, Rgba::TRANSPARENT, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Blur(1.0)],
    backdrop_filters: Vec::new(),
    radii: BorderRadii::uniform(0.5),
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  // Blur should spill outside the original bounds; left neighbor gains alpha.
  let (_, _, _, alpha) = pixel(&pixmap, 0, 1);
  assert!(
    alpha > 0,
    "expected blur outside rounded rect to remain visible; alpha at (0,1) was {alpha}"
  );
  // Farther away should be at most a faint blur, not clipped but low alpha.
  let (_, _, _, corner_alpha) = pixel(&pixmap, 0, 0);
  assert!(
    corner_alpha < 64,
    "expected distant blur to be faint; alpha at (0,0) was {corner_alpha}"
  );
}

#[test]
fn grayscale_filter_converts_to_luma() {
  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Grayscale(1.0)],
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  // Pure blue content becomes luma gray (~18/255 per channel).
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(0, 0, 255, 255),
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, a) = pixel(&pixmap, 0, 0);
  assert_eq!(a, 255);
  assert!(
    r.abs_diff(18) <= 1 && g.abs_diff(18) <= 1 && b.abs_diff(18) <= 1,
    "expected grayscale ~18, got {:?}",
    (r, g, b, a)
  );
}

#[test]
fn color_blend_mode_uses_destination_luminance() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  // Destination: mid-gray.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(128, 128, 128, 255),
  }));
  // Apply color blend with vivid red source: hue/saturation from source, luminance from destination.
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::Color,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, a) = pixel(&pixmap, 0, 0);
  assert_eq!(a, 255);
  let expected = blend_color((255, 0, 0), (128, 128, 128));
  let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
  assert_hsl_components((r, g, b), (eh, es, el), 0.02, 0.05, 0.05, "color blend");
}

#[test]
fn hue_blend_mode_uses_source_hue() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  let dst = (30u8, 120u8, 220u8);
  let src = (200u8, 30u8, 30u8);
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
  }));
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::Hue,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, _) = pixel(&pixmap, 0, 0);
  let expected = blend_hue(src, dst);
  let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
  assert_hsl_components((r, g, b), (eh, es, el), 0.02, 0.05, 0.05, "hue blend");
}

#[test]
fn hue_hsv_blend_mode_uses_source_hue() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  let dst = (255u8, 255u8, 0u8); // yellow
  let src = (0u8, 0u8, 255u8); // blue
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
  }));
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::HueHsv,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, _) = pixel(&pixmap, 0, 0);
  let expected = blend_hue_hsv(src, dst);
  assert_eq!((r, g, b), expected);
}

#[test]
fn saturation_blend_mode_uses_source_saturation() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  let dst = (60u8, 140u8, 200u8);
  let src = (255u8, 40u8, 200u8);
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
  }));
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::Saturation,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, _) = pixel(&pixmap, 0, 0);
  let expected = blend_saturation(src, dst);
  let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
  assert_hsl_components(
    (r, g, b),
    (eh, es, el),
    0.02,
    0.05,
    0.05,
    "saturation blend",
  );
}

#[test]
fn color_oklch_blend_uses_source_chroma_and_hue() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  let dst = (120u8, 120u8, 120u8);
  let src = (0u8, 200u8, 0u8);
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
  }));
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::ColorOklch,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, _) = pixel(&pixmap, 0, 0);
  let expected = blend_color_oklch(src, dst);
  let expected_oklch = rgb_to_oklch(expected.0, expected.1, expected.2);
  assert_oklch_components(
    (r, g, b),
    expected_oklch,
    0.02,
    0.02,
    1.0,
    "color oklch blend",
  );
}

#[test]
fn plus_lighter_blend_adds_colors() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(100, 100, 100, 255),
  }));
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::PlusLighter,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(200, 0, 0, 255),
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  assert_eq!(pixel(&pixmap, 0, 0), (255, 100, 100, 255));
}

#[test]
fn plus_darker_blend_clamps_to_black() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  // Destination: solid red.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::PlusDarker,
  }));
  // Source: solid blue.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 0, 255));
}

#[test]
fn renderer_respects_device_scale() {
  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
    color: Rgba::rgb(255, 0, 0),
  }));

  let renderer =
    DisplayListRenderer::new_scaled(2, 1, Rgba::WHITE, FontContext::new(), 2.0).unwrap();
  let pixmap = renderer.render(&list).unwrap();
  assert_eq!(pixmap.width(), 4);
  assert_eq!(pixmap.height(), 2);

  let red_pixels = pixmap
    .pixels()
    .iter()
    .filter(|p| p.red() == 255 && p.green() == 0 && p.blue() == 0 && p.alpha() == 255)
    .count();
  assert!(
    red_pixels >= 4,
    "expected at least a 2x2 red block after scaling, got {}",
    red_pixels
  );
}

#[test]
fn luminosity_blend_mode_uses_source_luminance() {
  use fastrender::paint::display_list::BlendMode;
  use fastrender::paint::display_list::BlendModeItem;

  let renderer = DisplayListRenderer::new(2, 2, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  let dst = (30u8, 200u8, 60u8);
  let src = (220u8, 40u8, 40u8);
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
  }));
  list.push(DisplayItem::PushBlendMode(BlendModeItem {
    mode: BlendMode::Luminosity,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
  }));
  list.push(DisplayItem::PopBlendMode);

  let pixmap = renderer.render(&list).unwrap();
  let (r, g, b, _) = pixel(&pixmap, 0, 0);
  let expected = blend_luminosity(src, dst);
  let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
  assert_hsl_components(
    (r, g, b),
    (eh, es, el),
    0.02,
    0.05,
    0.05,
    "luminosity blend",
  );
}

#[test]
fn backdrop_blur_samples_outside_bounds() {
  let renderer = DisplayListRenderer::new(6, 1, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  // Backdrop: blue on the left, red on the right.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 3.0, 1.0),
    color: Rgba::BLUE,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(3.0, 0.0, 3.0, 1.0),
    color: Rgba::RED,
  }));
  // Apply a backdrop blur over the red half; the blur should pull in blue from outside the bounds.
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(3.0, 0.0, 3.0, 1.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: vec![ResolvedFilter::Blur(1.0)],
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  // Pixel just inside the blurred region should pick up some blue from the left half.
  let (r, g, b, a) = pixel(&pixmap, 3, 0);
  assert_eq!(a, 255);
  assert!(
    b > 0 && r < 255,
    "expected blur to sample blue neighbor; got ({r},{g},{b})"
  );
}

#[test]
fn filter_blur_not_clipped_to_bounds() {
  let renderer = DisplayListRenderer::new(4, 1, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Blur(1.0)],
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  // Blur should leak outside the original rect into pixel 0.
  let (r, g, b, a) = pixel(&pixmap, 0, 0);
  assert_eq!(a, 255);
  assert!(
    r > g && r > b && (g < 250 || b < 250),
    "expected red-dominant blur outside bounds; got ({r},{g},{b})"
  );
}

#[test]
fn filter_blur_zero_has_no_effect() {
  let renderer = DisplayListRenderer::new(4, 1, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Blur(0.0)],
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  let pixmap = renderer.render(&list).unwrap();
  // No blur: red stays confined to its rect and does not leak to neighbors.
  assert_eq!(pixel(&pixmap, 1, 0), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
}

#[test]
fn clip_path_masks_after_filters() {
  let mut list = DisplayList::new();
  let circle = fastrender::paint::clip_path::ResolvedClipPath::Circle {
    center: fastrender::geometry::Point::new(5.0, 5.0),
    radius: 4.0,
  };
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Path { path: circle },
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopClip);
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Normal,
    is_isolated: true,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: vec![ResolvedFilter::Blur(2.0)],
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(12, 12, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("rendered");
  assert_eq!(pixel(&pixmap, 6, 6), (255, 0, 0, 255));
  assert_eq!(pixel(&pixmap, 0, 0), (255, 255, 255, 255));
  assert_eq!(pixel(&pixmap, 11, 11), (255, 255, 255, 255));
}

#[test]
fn color_mix_background_renders_purple() {
  let mut style = fastrender::ComputedStyle::default();
  style.set_background_layers(vec![BackgroundLayer {
    image: Some(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![ColorStop {
        color: fastrender::Color::parse("color-mix(in srgb, red 50%, blue 50%)").unwrap(),
        position: Some(0.0),
      }],
    }),
    ..Default::default()
  }]);

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 4.0, 4.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build(&fragment);
  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  let (r, g, b, _) = pixel(&pixmap, 2, 2);
  assert!(
    r > 0 && b > 0 && (r as i32 - b as i32).abs() <= 1,
    "expected purple mix, got ({r},{g},{b})"
  );
  assert_eq!(g, 0);
}

#[test]
fn blend_mode_multiply_modulates_destination() {
  // Red source over green destination with multiply should produce black in the overlap.
  let mut list = DisplayList::new();
  // Destination: green rect at the top-left.
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    color: Rgba::GREEN,
  }));
  // Source: red rect overlapping destination at (0,0)-(2,2).
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    mix_blend_mode: fastrender::paint::display_list::BlendMode::Multiply,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: fastrender::paint::display_list::BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
    color: Rgba::RED,
  }));
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  // Top-left pixel should be the multiply of red over green -> black.
  assert_eq!(pixel(&pixmap, 0, 0), (0, 0, 0, 255));
  // Outside the overlap, destination remains green.
  assert_eq!(pixel(&pixmap, 3, 3), (0, 255, 0, 255));
}

#[test]
fn stacking_context_hsl_blend_preserves_backdrop_luminance() {
  use fastrender::paint::display_list::BlendMode;

  let dst = (60u8, 140u8, 200u8);
  let src = (200u8, 40u8, 200u8);

  let mut list = DisplayList::new();
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    color: Rgba::from_rgba8(dst.0, dst.1, dst.2, 255),
  }));
  list.push(DisplayItem::PushStackingContext(StackingContextItem {
    z_index: 0,
    creates_stacking_context: true,
    bounds: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    mix_blend_mode: BlendMode::Hue,
    is_isolated: false,
    transform: None,
    transform_style: TransformStyle::Flat,
    backface_visibility: BackfaceVisibility::Visible,
    filters: Vec::new(),
    backdrop_filters: Vec::new(),
    radii: BorderRadii::ZERO,
    mask: None,
  }));
  list.push(DisplayItem::FillRect(FillRectItem {
    rect: Rect::from_xywh(0.0, 0.0, 4.0, 4.0),
    color: Rgba::from_rgba8(src.0, src.1, src.2, 255),
  }));
  list.push(DisplayItem::PopStackingContext);

  let renderer = DisplayListRenderer::new(4, 4, Rgba::WHITE, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");
  let (r, g, b, _) = pixel(&pixmap, 1, 1);

  let expected = blend_hue(src, dst);
  let (eh, es, el) = rgb_to_hsl(expected.0, expected.1, expected.2);
  assert_hsl_components(
    (r, g, b),
    (eh, es, el),
    0.02,
    0.05,
    0.05,
    "hue stacking blend",
  );
}

#[test]
fn color_mix_oklch_renders_expected_color() {
  let mut style = fastrender::ComputedStyle::default();
  style.background_color = Color::parse("color-mix(in oklch, red 40%, blue 60%)")
    .unwrap()
    .to_rgba(Rgba::BLACK);

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 1.0, 1.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build(&fragment);
  let pixmap = DisplayListRenderer::new(1, 1, Rgba::WHITE, FontContext::new())
    .unwrap()
    .render(&list)
    .unwrap();

  let expected = Color::parse("color-mix(in oklch, red 40%, blue 60%)")
    .unwrap()
    .to_rgba(Rgba::BLACK);
  assert_eq!(
    pixel(&pixmap, 0, 0),
    (expected.r, expected.g, expected.b, expected.alpha_u8())
  );
}

#[test]
fn color_mix_handles_transparent_components() {
  let mut style = ComputedStyle::default();
  style.color = Rgba::BLACK;
  style.background_color = Rgba::TRANSPARENT;
  style.set_background_layers(vec![BackgroundLayer {
    image: Some(BackgroundImage::LinearGradient {
      angle: 0.0,
      stops: vec![
        ColorStop {
          color: Color::parse(
            "color-mix(in srgb, rgba(255, 0, 0, 0.0) 50%, rgba(0, 0, 255, 1) 50%)",
          )
          .unwrap(),
          position: Some(0.0),
        },
        ColorStop {
          color: Color::parse(
            "color-mix(in srgb, rgba(255, 0, 0, 0.0) 50%, rgba(0, 0, 255, 1) 50%)",
          )
          .unwrap(),
          position: Some(1.0),
        },
      ],
    }),
    repeat: BackgroundRepeat::no_repeat(),
    ..BackgroundLayer::default()
  }]);

  let fragment =
    FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![], Arc::new(style));

  let list = DisplayListBuilder::new().build(&fragment);
  let renderer = DisplayListRenderer::new(2, 2, Rgba::TRANSPARENT, FontContext::new()).unwrap();
  let pixmap = renderer.render(&list).expect("render");

  let (r, g, b, a) = pixel(&pixmap, 1, 1);
  assert_eq!((r, g, b, a), (0, 0, 128, 128));
}

#[test]
fn outline_ignores_clip_path() {
  let renderer = DisplayListRenderer::new(40, 40, Rgba::WHITE, FontContext::new()).unwrap();
  let mut list = DisplayList::new();

  let triangle = fastrender::paint::clip_path::ResolvedClipPath::Polygon {
    points: vec![
      fastrender::geometry::Point::new(10.0, 10.0),
      fastrender::geometry::Point::new(14.0, 10.0),
      fastrender::geometry::Point::new(10.0, 14.0),
    ],
    fill_rule: tiny_skia::FillRule::Winding,
  };
  list.push(DisplayItem::PushClip(ClipItem {
    shape: ClipShape::Path { path: triangle },
  }));
  list.push(DisplayItem::Outline(OutlineItem {
    rect: Rect::from_xywh(5.0, 5.0, 20.0, 20.0),
    width: 4.0,
    style: BorderStyle::Solid,
    color: Rgba::RED,
    offset: 0.0,
    invert: false,
  }));
  list.push(DisplayItem::PopClip);

  let pixmap = renderer.render(&list).expect("render");
  // Find any red stroke pixel in the band left of the clip path (x < 8) to verify it isn't clipped.
  let mut found = false;
  for y in 0..40 {
    for x in 0..8 {
      if pixel(&pixmap, x, y) == (255, 0, 0, 255) {
        found = true;
        break;
      }
    }
    if found {
      break;
    }
  }
  assert!(
    found,
    "outline stroke should be visible outside the clip path"
  );
}
