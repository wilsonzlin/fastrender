use super::ColorGlyphRaster;
use crate::style::color::Rgba;
use crate::svg::{svg_root_view_box, svg_view_box_root_transform, SvgViewBox};
use std::sync::Arc;
use tiny_skia::{Pixmap, Transform};

/// Render SVG-in-OpenType glyphs.
pub fn render_svg_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  text_color: Rgba,
) -> Option<ColorGlyphRaster> {
  let svg_doc = face.glyph_svg_image(glyph_id)?;
  let svg_str = std::str::from_utf8(svg_doc.data).ok()?;
  let svg_with_color = inject_current_color(svg_str, text_color)?;

  let options = resvg::usvg::Options::default();

  let tree = resvg::usvg::Tree::from_str(&svg_with_color, &options).ok()?;
  let size = tree.size();
  let source_width = size.width() as f32;
  let source_height = size.height() as f32;
  if source_width <= 0.0 || source_height <= 0.0 {
    return None;
  }

  let view_box = svg_root_view_box(&svg_with_color).unwrap_or(SvgViewBox {
    min_x: 0.0,
    min_y: 0.0,
    width: source_width,
    height: source_height,
  });
  if view_box.width <= 0.0 || view_box.height <= 0.0 {
    return None;
  }

  let units_per_em = face.units_per_em() as f32;
  if units_per_em <= 0.0 {
    return None;
  }

  let scale = font_size / units_per_em;
  let width = (view_box.width * scale).max(1.0).ceil() as u32;
  let height = (view_box.height * scale).max(1.0).ceil() as u32;

  let mut pixmap = Pixmap::new(width, height)?;

  // Map the root SVG viewport into the glyph viewBox while respecting preserveAspectRatio,
  // then flip the Y axis so SVG glyph coordinates (y-up) align with font coordinates.
  let view_box_transform = svg_view_box_root_transform(
    &svg_with_color,
    source_width,
    source_height,
    view_box.width,
    view_box.height,
  )
  .unwrap_or_else(|| {
    Transform::from_scale(
      view_box.width / source_width,
      view_box.height / source_height,
    )
  });

  let max_y = view_box.min_y + view_box.height;
  let glyph_transform = Transform::from_row(
    scale,
    0.0,
    0.0,
    -scale,
    -view_box.min_x * scale,
    max_y * scale,
  );
  let transform = concat_transforms(glyph_transform, view_box_transform);

  resvg::render(&tree, transform, &mut pixmap.as_mut());

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: view_box.min_x * scale,
    top: -max_y * scale,
  })
}

fn inject_current_color(svg: &str, text_color: Rgba) -> Option<String> {
  let alpha = text_color.a.clamp(0.0, 1.0);
  let color_value = format!(
    "rgba({},{},{},{})",
    text_color.r, text_color.g, text_color.b, alpha
  );

  let svg_start = svg.to_ascii_lowercase().find("<svg")?;
  let tag_end = svg[svg_start..].find('>')? + svg_start;

  let mut injected = String::with_capacity(svg.len() + color_value.len() + 16);
  injected.push_str(&svg[..svg_start + 4]);
  injected.push(' ');
  injected.push_str("color=\"");
  injected.push_str(&color_value);
  injected.push('"');
  injected.push_str(&svg[svg_start + 4..=tag_end]);
  injected.push_str(&svg[tag_end + 1..]);
  Some(injected)
}

fn concat_transforms(a: Transform, b: Transform) -> Transform {
  Transform::from_row(
    a.sx * b.sx + a.kx * b.ky,
    a.ky * b.sx + a.sy * b.ky,
    a.sx * b.kx + a.kx * b.sy,
    a.ky * b.kx + a.sy * b.sy,
    a.sx * b.tx + a.kx * b.ty + a.tx,
    a.ky * b.tx + a.sy * b.ty + a.ty,
  )
}
