use crate::text::font_db::LoadedFont;
use rustybuzz::Variation;
use skrifa::instance::{Location, LocationRef, Size};
use skrifa::outline::{DrawSettings, OutlineGlyph, OutlineGlyphCollection, OutlinePen};
use skrifa::{FontRef, GlyphId, MetadataProvider, Tag};
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};
use tiny_skia::{Path, PathBuilder, Rect, Transform};
use ttf_parser::GlyphId as ParserGlyphId;

/// Hash variations to use in cache keys.
pub fn variation_hash(variations: &[Variation]) -> u64 {
  let mut hasher = DefaultHasher::new();
  let mut ordered: BTreeMap<[u8; 4], f32> = BTreeMap::new();
  for variation in variations {
    ordered.insert(variation.tag.to_bytes(), variation.value);
  }
  for (tag, value) in ordered {
    tag.hash(&mut hasher);
    value.to_bits().hash(&mut hasher);
  }
  hasher.finish()
}

/// Bounding box for a glyph outline in font units (Y-up).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FontBBox {
  pub x_min: f32,
  pub y_min: f32,
  pub x_max: f32,
  pub y_max: f32,
}

impl FontBBox {
  fn from_path_bounds(bounds: Rect) -> Option<Self> {
    let left = bounds.left();
    let right = bounds.right();
    let top = bounds.top();
    let bottom = bounds.bottom();
    if !left.is_finite() || !right.is_finite() || !top.is_finite() || !bottom.is_finite() {
      return None;
    }
    if right <= left || bottom <= top {
      return None;
    }
    Some(Self {
      x_min: left,
      y_min: top,
      x_max: right,
      y_max: bottom,
    })
  }
}

/// Simple metrics captured while building an outline.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct GlyphOutlineMetrics {
  pub verb_count: usize,
  pub point_count: usize,
}

/// Outline and metrics for a single glyph.
#[derive(Debug, Clone)]
pub struct GlyphOutline {
  pub path: Option<Path>,
  pub advance: f32,
  pub bbox: Option<FontBBox>,
  pub metrics: GlyphOutlineMetrics,
}

/// Common outline builder that can accept either ttf-parser or skrifa commands.
pub struct PathOutlineBuilder {
  builder: PathBuilder,
  metrics: GlyphOutlineMetrics,
}

impl PathOutlineBuilder {
  pub fn new() -> Self {
    Self {
      builder: PathBuilder::new(),
      metrics: GlyphOutlineMetrics::default(),
    }
  }

  pub fn finish(self) -> (Option<Path>, GlyphOutlineMetrics) {
    (self.builder.finish(), self.metrics)
  }
}

impl ttf_parser::OutlineBuilder for PathOutlineBuilder {
  fn move_to(&mut self, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 1;
    self.builder.move_to(x, y);
  }

  fn line_to(&mut self, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 1;
    self.builder.line_to(x, y);
  }

  fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 2;
    self.builder.quad_to(x1, y1, x, y);
  }

  fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 3;
    self.builder.cubic_to(x1, y1, x2, y2, x, y);
  }

  fn close(&mut self) {
    self.metrics.verb_count += 1;
    self.builder.close();
  }
}

impl OutlinePen for PathOutlineBuilder {
  fn move_to(&mut self, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 1;
    self.builder.move_to(x, y);
  }

  fn line_to(&mut self, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 1;
    self.builder.line_to(x, y);
  }

  fn quad_to(&mut self, cx0: f32, cy0: f32, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 2;
    self.builder.quad_to(cx0, cy0, x, y);
  }

  fn curve_to(&mut self, cx0: f32, cy0: f32, cx1: f32, cy1: f32, x: f32, y: f32) {
    self.metrics.verb_count += 1;
    self.metrics.point_count += 3;
    self.builder.cubic_to(cx0, cy0, cx1, cy1, x, y);
  }

  fn close(&mut self) {
    self.metrics.verb_count += 1;
    self.builder.close();
  }
}

/// Variation-aware font instance used for outline extraction.
///
/// Static fonts use `ttf-parser` directly, while variable fonts are routed through `skrifa`
/// so gvar/HVAR deltas are applied during outline construction (ttf-parser 0.25 does not apply
/// variations to outlines or advances).
pub struct FontInstance<'a> {
  backend: FontBackend<'a>,
  units_per_em: f32,
  variation_hash: u64,
  is_variable: bool,
}

enum FontBackend<'a> {
  Ttf(ttf_parser::Face<'a>),
  Skrifa {
    font: FontRef<'a>,
    location: Location,
  },
}

impl<'a> FontInstance<'a> {
  pub fn new(font: &'a LoadedFont, variations: &[Variation]) -> Option<Self> {
    let face = font.as_ttf_face().ok()?;
    let units_per_em = face.units_per_em() as f32;
    let variation_hash = variation_hash(variations);

    if face.is_variable() {
      if let Ok(font_ref) = FontRef::from_index(&font.data, font.index) {
        let location = font_ref.axes().location(variations_to_settings(variations));
        return Some(Self {
          units_per_em,
          variation_hash,
          is_variable: true,
          backend: FontBackend::Skrifa {
            font: font_ref,
            location,
          },
        });
      }
    }

    Some(Self {
      units_per_em,
      variation_hash,
      is_variable: false,
      backend: FontBackend::Ttf(face),
    })
  }

  #[inline]
  pub fn units_per_em(&self) -> f32 {
    self.units_per_em
  }

  #[inline]
  pub fn variation_hash(&self) -> u64 {
    self.variation_hash
  }

  #[inline]
  pub fn is_variable(&self) -> bool {
    self.is_variable
  }

  pub fn glyph_outline(&self, glyph_id: u32) -> Option<GlyphOutline> {
    match &self.backend {
      FontBackend::Ttf(face) => build_ttf_outline(face, glyph_id),
      FontBackend::Skrifa { font, location } => build_skrifa_outline(font, location, glyph_id),
    }
  }

  pub fn glyph_bounds(&self, glyph_id: u32) -> Option<FontBBox> {
    self
      .glyph_outline(glyph_id)
      .and_then(|outline| outline.bbox)
  }
}

fn build_ttf_outline(face: &ttf_parser::Face<'_>, glyph_id: u32) -> Option<GlyphOutline> {
  if glyph_id > u16::MAX as u32 {
    return None;
  }

  let parser_glyph = ParserGlyphId(glyph_id as u16);
  let mut builder = PathOutlineBuilder::new();
  let has_outline = face.outline_glyph(parser_glyph, &mut builder).is_some();
  let (path, metrics) = builder.finish();
  let path = if has_outline { path } else { None };
  let bbox = face
    .glyph_bounding_box(parser_glyph)
    .map(|r| FontBBox {
      x_min: r.x_min as f32,
      y_min: r.y_min as f32,
      x_max: r.x_max as f32,
      y_max: r.y_max as f32,
    })
    .or_else(|| {
      path
        .as_ref()
        .and_then(|p| FontBBox::from_path_bounds(p.bounds()))
    });
  let advance = face
    .glyph_hor_advance(parser_glyph)
    .map(|v| v as f32)
    .unwrap_or(0.0);

  Some(GlyphOutline {
    path,
    advance,
    bbox,
    metrics,
  })
}

fn build_skrifa_outline(
  font: &FontRef<'_>,
  location: &Location,
  glyph_id: u32,
) -> Option<GlyphOutline> {
  let glyphs = OutlineGlyphCollection::new(font);
  let outline = glyphs.get(GlyphId::from(glyph_id))?;
  let mut builder = PathOutlineBuilder::new();
  let loc_ref = LocationRef::from(location);
  let metrics = draw_outline_unscaled(&outline, loc_ref, &mut builder).ok();
  let (path, outline_metrics) = builder.finish();
  let bbox = path
    .as_ref()
    .and_then(|p| FontBBox::from_path_bounds(p.bounds()));
  let advance = metrics
    .as_ref()
    .and_then(|m| m.advance_width)
    .or_else(|| {
      let glyph_metrics = font.glyph_metrics(Size::unscaled(), loc_ref);
      glyph_metrics.advance_width(GlyphId::from(glyph_id))
    })
    .unwrap_or(0.0);

  Some(GlyphOutline {
    path,
    advance,
    bbox,
    metrics: outline_metrics,
  })
}

fn draw_outline_unscaled(
  outline: &OutlineGlyph<'_>,
  location: LocationRef<'_>,
  pen: &mut impl OutlinePen,
) -> Result<skrifa::outline::AdjustedMetrics, skrifa::outline::DrawError> {
  outline.draw(DrawSettings::unhinted(Size::unscaled(), location), pen)
}

fn variations_to_settings<'a>(
  variations: impl IntoIterator<Item = &'a Variation>,
) -> Vec<(Tag, f32)> {
  variations
    .into_iter()
    .map(|v| (Tag::from_u32(v.tag.0), v.value))
    .collect()
}

/// Apply a standard font-space to screen-space transform (scale + shear + translate).
#[inline]
pub fn glyph_transform(scale: f32, skew: f32, x: f32, y: f32) -> Transform {
  Transform::from_row(scale, 0.0, skew * scale, -scale, x, y)
}
