use super::limits::{log_glyph_limit, round_dimension, GlyphRasterLimits};
use super::ColorGlyphRaster;
use crate::style::color::Rgba;
use crate::svg::{svg_root_view_box, svg_view_box_root_transform, SvgViewBox};
use roxmltree::Document;
use std::sync::Arc;
use tiny_skia::{Pixmap, Transform};

pub const MAX_SVG_GLYPH_BYTES: usize = 256 * 1024;
const MAX_SVG_GLYPH_NODES: usize = 10_000;
const MAX_SVG_GLYPH_DATA_URL_BYTES: usize = 32 * 1024;

/// Render SVG-in-OpenType glyphs.
pub fn render_svg_glyph(
  face: &ttf_parser::Face<'_>,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  text_color: Rgba,
  limits: &GlyphRasterLimits,
) -> Option<ColorGlyphRaster> {
  let svg_doc = face.glyph_svg_image(glyph_id)?;
  let svg_str = sanitize_svg_glyph(svg_doc.data)?;
  let svg_with_color = inject_current_color(svg_str, text_color)?;
  let units_per_em = face.units_per_em() as f32;

  rasterize_svg_with_metrics(
    &svg_with_color,
    glyph_id.0 as u32,
    font_size,
    units_per_em,
    limits,
  )
}

fn rasterize_svg_with_metrics(
  svg_with_color: &str,
  glyph_id: u32,
  font_size: f32,
  units_per_em: f32,
  limits: &GlyphRasterLimits,
) -> Option<ColorGlyphRaster> {
  let mut options = resvg::usvg::Options::default();
  options.resources_dir = None;

  let tree = resvg::usvg::Tree::from_str(svg_with_color, &options).ok()?;
  let size = tree.size();
  let source_width = size.width() as f32;
  let source_height = size.height() as f32;
  if source_width <= 0.0 || source_height <= 0.0 {
    return None;
  }

  let view_box = svg_root_view_box(svg_with_color).unwrap_or(SvgViewBox {
    min_x: 0.0,
    min_y: 0.0,
    width: source_width,
    height: source_height,
  });
  if view_box.width <= 0.0 || view_box.height <= 0.0 {
    return None;
  }

  if units_per_em <= 0.0 {
    return None;
  }

  let scale = font_size / units_per_em;
  if !scale.is_finite() || scale <= 0.0 || !font_size.is_finite() {
    return None;
  }
  let width = round_dimension(view_box.width * scale)?;
  let height = round_dimension(view_box.height * scale)?;
  if let Err(err) = limits.validate(width, height) {
    log_glyph_limit("svg", glyph_id, &err);
    return None;
  }

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

  let top = -max_y * scale;
  if !top.is_finite() {
    return None;
  }

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: view_box.min_x * scale,
    top,
  })
}

pub fn sanitize_svg_glyph_for_tests(svg_bytes: &[u8]) -> Option<&str> {
  sanitize_svg_glyph(svg_bytes)
}

fn sanitize_svg_glyph(svg_bytes: &[u8]) -> Option<&str> {
  if svg_bytes.len() > MAX_SVG_GLYPH_BYTES {
    return None;
  }

  let svg_str = std::str::from_utf8(svg_bytes).ok()?;
  let doc = Document::parse(svg_str).ok()?;
  let mut element_count = 0usize;

  for node in doc.descendants().filter(|n| n.is_element()) {
    element_count += 1;
    if element_count > MAX_SVG_GLYPH_NODES {
      return None;
    }
    if svg_node_has_external_reference(&node) {
      return None;
    }
  }

  Some(svg_str)
}

fn svg_node_has_external_reference(node: &roxmltree::Node<'_, '_>) -> bool {
  if let Some(href) = svg_href_value(node) {
    if is_disallowed_svg_reference(href) {
      return true;
    }
  }

  for attr in node.attributes() {
    if contains_disallowed_url_function(attr.value()) {
      return true;
    }
  }

  if node.tag_name().name().eq_ignore_ascii_case("style") {
    if let Some(text) = node.text() {
      if contains_disallowed_url_function(text) {
        return true;
      }
    }
  }

  false
}

fn svg_href_value(node: &roxmltree::Node<'_, '_>) -> Option<&str> {
  for attr in node.attributes() {
    if attr.name().name().eq_ignore_ascii_case("href") {
      return Some(attr.value());
    }
  }
  None
}

fn contains_disallowed_url_function(value: &str) -> bool {
  let lower = value.to_ascii_lowercase();
  let mut search_start = 0usize;

  while let Some(rel_idx) = lower[search_start..].find("url(") {
    let idx = search_start + rel_idx;
    let after = &value[idx + 4..];
    if let Some(close_idx) = after.find(')') {
      let raw_target = &after[..close_idx];
      let target = raw_target.trim().trim_matches(|c| matches!(c, '"' | '\''));
      if is_disallowed_svg_reference(target) {
        return true;
      }
      search_start = idx + 4 + close_idx + 1;
    } else {
      return true;
    }
  }

  false
}

fn is_disallowed_svg_reference(target: &str) -> bool {
  match classify_svg_reference(target) {
    SvgReference::Empty | SvgReference::Fragment => false,
    SvgReference::DataUrl { too_large } => too_large,
    SvgReference::External => true,
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SvgReference {
  Empty,
  Fragment,
  DataUrl { too_large: bool },
  External,
}

fn classify_svg_reference(target: &str) -> SvgReference {
  let trimmed = target.trim();
  if trimmed.is_empty() {
    return SvgReference::Empty;
  }
  if trimmed.starts_with('#') {
    return SvgReference::Fragment;
  }
  if starts_with_case_insensitive(trimmed, "data:") {
    return SvgReference::DataUrl {
      too_large: trimmed.len() > MAX_SVG_GLYPH_DATA_URL_BYTES,
    };
  }
  SvgReference::External
}

fn starts_with_case_insensitive(value: &str, prefix: &str) -> bool {
  value
    .get(..prefix.len())
    .map(|s| s.eq_ignore_ascii_case(prefix))
    .unwrap_or(false)
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

#[cfg(test)]
mod tests {
  use super::super::limits::GlyphRasterLimits;
  use super::rasterize_svg_with_metrics;

  #[test]
  fn svg_glyph_rasterization_respects_limits() {
    let svg = r#"<svg width="10000" height="10000" viewBox="0 0 10000 10000"></svg>"#;
    let limits = GlyphRasterLimits::new(1024, 1024_u64 * 1024_u64);
    assert!(rasterize_svg_with_metrics(svg, 1, 50.0, 1.0, &limits).is_none());
  }
}
