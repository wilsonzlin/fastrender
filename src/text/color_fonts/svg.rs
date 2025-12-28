use super::limits::{log_glyph_limit, round_dimension, GlyphRasterLimits};
use super::ColorGlyphRaster;
use crate::style::color::Rgba;
use crate::svg::{svg_root_view_box, svg_view_box_root_transform, SvgViewBox};
use regex::Regex;
use roxmltree::Document;
use std::ops::Range;
use std::sync::{Arc, OnceLock};
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
  let svg_with_color = preprocess_svg_markup(svg_str, text_color);
  let units_per_em = face.units_per_em() as f32;

  rasterize_svg_with_metrics(
    svg_with_color.as_deref().unwrap_or(svg_str),
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

fn svg_href_value<'a>(node: &'a roxmltree::Node<'a, 'a>) -> Option<&'a str> {
  for attr in node.attributes() {
    if attr.name().eq_ignore_ascii_case("href") {
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

fn preprocess_svg_markup(svg: &str, text_color: Rgba) -> Option<String> {
  let doc = Document::parse(svg).ok()?;
  let root = doc.root_element();
  let color_css = format_css_color(text_color);

  let mut replacements: Vec<(Range<usize>, String)> = Vec::new();
  let mut root_has_style = false;
  let mut root_has_color_attribute = false;

  for node in doc.descendants().filter(|node| node.is_element()) {
    let is_root = node.id() == root.id();
    for attr in node.attributes() {
      let mut new_value = None;
      let name = attr.name();
      if name.eq_ignore_ascii_case("fill") || name.eq_ignore_ascii_case("stroke") {
        new_value = replace_context_paint(attr.value());
      } else if name.eq_ignore_ascii_case("style") {
        new_value = rewrite_style_attribute(attr.value(), is_root.then_some(color_css.as_str()));
        if is_root {
          root_has_style = true;
        }
      } else if is_root && name.eq_ignore_ascii_case("color") {
        root_has_color_attribute = true;
        if attr.value() != color_css {
          new_value = Some(color_css.clone());
        }
      }

      if let Some(value) = new_value {
        replacements.push((attr.range_value(), value));
      }
    }
  }

  if !root_has_style && !root_has_color_attribute {
    if let Some(insert_at) = find_root_style_insertion(svg, root.range()) {
      replacements.push((
        insert_at..insert_at,
        format!(r#" style="color:{}""#, color_css),
      ));
    }
  }

  if replacements.is_empty() {
    return None;
  }

  replacements.sort_by(|a, b| b.0.start.cmp(&a.0.start));
  let mut output = svg.to_string();
  for (range, value) in replacements {
    output.replace_range(range, &value);
  }
  Some(output)
}

fn rewrite_style_attribute(style: &str, inject_color: Option<&str>) -> Option<String> {
  let mut declarations = Vec::new();
  let mut changed = false;
  let mut saw_color = false;

  for raw in style.split(';') {
    let raw = raw.trim();
    if raw.is_empty() {
      continue;
    }
    if let Some((name, value)) = raw.split_once(':') {
      let name = name.trim();
      let mut value = value.trim().to_string();
      if let Some(replaced) = replace_context_paint(&value) {
        value = replaced;
        changed = true;
      }

      if name.eq_ignore_ascii_case("color") {
        saw_color = true;
        if let Some(color) = inject_color {
          if value != color {
            value = color.to_string();
            changed = true;
          }
        }
      }

      declarations.push(format!("{}:{}", name, value));
    } else {
      declarations.push(raw.to_string());
    }
  }

  if let Some(color) = inject_color {
    if !saw_color {
      declarations.push(format!("color:{}", color));
      changed = true;
    }
  }

  if changed {
    Some(declarations.join(";"))
  } else {
    None
  }
}

fn replace_context_paint(value: &str) -> Option<String> {
  static CONTEXT_RE: OnceLock<Regex> = OnceLock::new();
  let re = CONTEXT_RE.get_or_init(|| Regex::new("(?i)context-(fill|stroke)").unwrap());
  if re.is_match(value) {
    Some(re.replace_all(value, "currentColor").into_owned())
  } else {
    None
  }
}

fn find_root_style_insertion(svg: &str, root_range: Range<usize>) -> Option<usize> {
  let start = root_range.start;
  let slice = svg.get(start..)?;
  let mut end = slice.find('>')? + start;
  if end > start && svg.as_bytes().get(end - 1) == Some(&b'/') {
    end -= 1;
  }
  Some(end)
}

fn format_css_color(color: Rgba) -> String {
  format!(
    "rgba({},{},{},{:.3})",
    color.r,
    color.g,
    color.b,
    color.a.clamp(0.0, 1.0)
  )
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
