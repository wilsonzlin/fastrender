use super::limits::{log_glyph_limit, round_dimension, GlyphRasterLimits};
use super::{cpal, ColorFontCaches, ColorGlyphRaster, FontKey};
use crate::paint::pixmap::new_pixmap;
use crate::style::color::Rgba;
use crate::text::font_db::LoadedFont;
use crate::text::font_instance::{glyph_transform, FontInstance};
use read_fonts::tables::colr::{
  ClipBox, ColorLine, Colr, CompositeMode, Extend, Paint, PaintId, VarColorLine,
};
use read_fonts::tables::variations::{
  DeltaSetIndex, DeltaSetIndexMap, FloatItemDeltaTarget, ItemVariationStore,
};
use read_fonts::types::{F2Dot14, FWord, Fixed, GlyphId};
use read_fonts::{FontRef, TableProvider};
#[cfg(test)]
use std::cell::Cell;
use std::collections::HashSet;
use std::f32::consts::PI;
use std::mem;
use std::sync::{Arc, Mutex};
use tiny_skia::{
  BlendMode, Color, FillRule, GradientStop, Mask, Paint as SkiaPaint, Path, PathBuilder, Pixmap,
  PixmapPaint, Point, PremultipliedColorU8, Rect, SpreadMode, Transform,
};

const RASTER_PAD: f32 = 1.0;

#[derive(Clone)]
pub(super) struct ColrV1RasterPlan {
  pub commands: Arc<Vec<DrawCommand>>,
  pub clip_path: Option<Arc<Path>>,
  pub bounds: (f32, f32, f32, f32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct PaintCacheKey {
  pub font: FontKey,
  pub glyph_id: u16,
  pub font_size_key: u32,
  pub palette_index: u16,
  pub palette_override_hash: u64,
  pub variation_hash: u64,
  pub color_signature: u32,
  pub synthetic_oblique_key: i32,
}

impl PaintCacheKey {
  pub fn new(
    font: FontKey,
    glyph_id: u16,
    font_size: f32,
    palette_index: u16,
    palette_override_hash: u64,
    text_color: Rgba,
    synthetic_oblique: f32,
    variation_hash: u64,
  ) -> Self {
    Self {
      font,
      glyph_id,
      font_size_key: quantize_scale(font_size),
      palette_index,
      palette_override_hash,
      variation_hash,
      color_signature: color_signature(text_color),
      synthetic_oblique_key: quantize_skew(synthetic_oblique),
    }
  }
}

fn quantize_scale(value: f32) -> u32 {
  if !value.is_finite() || value <= 0.0 {
    return 0;
  }
  (value * 100.0).round().clamp(0.0, u32::MAX as f32).trunc() as u32
}

fn quantize_skew(skew: f32) -> i32 {
  if !skew.is_finite() {
    return 0;
  }
  let scaled = (skew * 10_000.0).round();
  scaled.clamp(i32::MIN as f32, i32::MAX as f32).trunc() as i32
}

fn color_signature(color: Rgba) -> u32 {
  ((color.alpha_u8() as u32) << 24)
    | ((color.r as u32) << 16)
    | ((color.g as u32) << 8)
    | color.b as u32
}

#[derive(Clone)]
pub(super) struct ColrV1CacheEntry {
  data: Arc<Vec<u8>>,
  #[allow(dead_code)]
  font_ref: FontRef<'static>,
  pub colr: Colr<'static>,
}

impl std::fmt::Debug for ColrV1CacheEntry {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ColrV1CacheEntry")
      .field("data_len", &self.data.len())
      .finish()
  }
}

pub(super) type CachedBaseGlyph = (Paint<'static>, PaintId);
pub(super) type BaseGlyphCacheValue = Option<(Arc<Vec<u8>>, CachedBaseGlyph)>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct BaseGlyphCacheKey {
  pub font: FontKey,
  pub glyph_id: u16,
}

impl BaseGlyphCacheKey {
  pub fn new(font: FontKey, glyph_id: u16) -> Self {
    Self { font, glyph_id }
  }
}

impl ColrV1CacheEntry {
  pub fn parse(font: &LoadedFont) -> Option<Arc<Self>> {
    let data = Arc::clone(&font.data);
    // SAFETY: the Arc keeps the font data alive for the lifetime of the cached entry.
    let static_data: &'static [u8] = unsafe { mem::transmute::<&[u8], &'static [u8]>(&*data) };
    record_font_ref_parse();
    let font_ref = FontRef::from_index(static_data, font.index).ok()?;
    record_colr_table_access();
    let colr = font_ref.colr().ok()?;
    if colr.version() != 1 {
      return None;
    }
    Some(Arc::new(Self {
      data,
      font_ref,
      colr,
    }))
  }

  pub fn base_glyph(&self, glyph_id: u16) -> BaseGlyphCacheValue {
    self
      .colr
      .v1_base_glyph(GlyphId::from(glyph_id as u32))
      .ok()
      .flatten()
      .map(|(paint, id)| (Arc::clone(&self.data), (paint, id)))
  }
}

/// Render a COLRv1 paint graph for the given glyph.
pub fn render_colr_glyph(
  font: &LoadedFont,
  instance: &FontInstance,
  face: &ttf_parser::Face<'_>,
  font_key: FontKey,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  palette_index: u16,
  overrides: &[(u16, Rgba)],
  palette_override_hash: u64,
  text_color: Rgba,
  synthetic_oblique: f32,
  normalized_coords: &[f32],
  limits: &GlyphRasterLimits,
  caches: &Arc<Mutex<ColorFontCaches>>,
) -> Option<ColorGlyphRaster> {
  let colr_entry = {
    let mut caches = caches.lock().ok()?;
    caches.colr_v1_font(font_key, font)?
  };
  let colr = &colr_entry.colr;

  {
    let mut caches = caches.lock().ok()?;
    let _ = caches.colr_v1_var_store(font_key, face);
  }

  let units_per_em = instance.units_per_em();
  if units_per_em <= 0.0 || !units_per_em.is_finite() || !font_size.is_finite() {
    return None;
  }
  let scale = font_size / units_per_em;
  if !scale.is_finite() {
    return None;
  }
  let cache_key = PaintCacheKey::new(
    font_key,
    glyph_id.0,
    font_size,
    palette_index,
    palette_override_hash,
    text_color,
    synthetic_oblique,
    instance.variation_hash(),
  );

  let cached_plan = {
    let mut caches = caches.lock().ok()?;
    caches.colr_v1_plan(cache_key)
  };

  let plan = if let Some(plan) = cached_plan {
    plan
  } else {
    let (paint, paint_id) = {
      let key = BaseGlyphCacheKey::new(font_key, glyph_id.0);
      let mut caches = caches.lock().ok()?;
      caches.colr_v1_base_glyph(key, colr_entry.as_ref())?
    }?;

    let palette = {
      let mut caches = caches.lock().ok()?;
      caches
        .palette(font_key, face, palette_index)
        .unwrap_or_else(|| Arc::new(cpal::ParsedPalette::default()))
    };
    let mut palette_colors = palette.colors.clone();
    for (idx, color) in overrides {
      if let Some(slot) = palette_colors.get_mut(*idx as usize) {
        *slot = *color;
      }
    }

    let base_transform = glyph_transform(scale, synthetic_oblique, 0.0, 0.0);
    let variations = {
      let coords: Vec<F2Dot14> = normalized_coords
        .iter()
        .copied()
        .map(F2Dot14::from_f32)
        .collect();
      VariationInfo::new(colr.clone(), &coords)
    };

    let clip_path = colr
      .v1_clip_box(GlyphId::from(glyph_id.0 as u32))
      .ok()
      .flatten()
      .and_then(|clip| build_clip_path(clip, &colr, normalized_coords, base_transform));

    let mut commands = Vec::new();
    let mut seen = HashSet::new();
    let renderer = Renderer {
      instance,
      palette: &palette_colors,
      text_color,
      base_transform,
      colr_entry: Arc::clone(&colr_entry),
      caches: Arc::clone(caches),
      font_key,
      variations,
    };

    renderer.walk_paint(
      paint,
      paint_id,
      Transform::identity(),
      BlendMode::SourceOver,
      &mut commands,
      &mut seen,
    )?;

    if commands.is_empty() {
      return None;
    }

    let bounds = raster_bounds(&commands, clip_path.as_ref(), RASTER_PAD)?;
    let plan = Arc::new(ColrV1RasterPlan {
      commands: Arc::new(commands),
      clip_path: clip_path.map(Arc::new),
      bounds,
    });
    if let Ok(mut caches) = caches.lock() {
      caches.put_colr_v1_plan(cache_key, plan.clone());
    }
    plan
  };

  rasterize_plan(&plan, limits, glyph_id.0 as u32)
}

fn rasterize_plan(
  plan: &ColrV1RasterPlan,
  limits: &GlyphRasterLimits,
  glyph_id: u32,
) -> Option<ColorGlyphRaster> {
  let (min_x, min_y, max_x, max_y) = plan.bounds;

  let width = round_dimension(max_x - min_x)?;
  let height = round_dimension(max_y - min_y)?;
  if let Err(err) = limits.validate(width, height) {
    log_glyph_limit("colr", glyph_id, &err);
    return None;
  }
  let mut pixmap = new_pixmap(width, height)?;

  let clip_mask = plan
    .clip_path
    .as_deref()
    .and_then(|clip| build_clip_mask(clip, width, height, min_x, min_y));
  let clip_ref = clip_mask.as_ref();
  let translate = Transform::from_translate(-min_x, -min_y);

  for command in plan.commands.iter() {
    match &command.brush {
      Brush::SweepGradient { .. } => {
        render_sweep_gradient(command, &mut pixmap, min_x, min_y, translate, clip_ref)?;
      }
      Brush::RadialGradient { .. } => {
        render_two_circle_radial_gradient(command, &mut pixmap, min_x, min_y, translate, clip_ref)?;
      }
      _ => {
        let mut paint = brush_to_paint(&command.brush, min_x, min_y)?;
        paint.blend_mode = command.blend_mode;
        paint.anti_alias = true;
        pixmap.fill_path(
          &command.path,
          &paint,
          FillRule::Winding,
          translate,
          clip_ref,
        );
      }
    }
  }

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: min_x,
    top: min_y,
  })
}

/// Compute padded raster bounds from painted commands, intersecting them with a clip path if
/// present so generous base glyph clips don't force oversize pixmaps.
fn raster_bounds(
  commands: &[DrawCommand],
  clip_path: Option<&Path>,
  pad: f32,
) -> Option<(f32, f32, f32, f32)> {
  if commands.is_empty() {
    return None;
  }

  let mut min_x = f32::MAX;
  let mut min_y = f32::MAX;
  let mut max_x = f32::MIN;
  let mut max_y = f32::MIN;

  for command in commands {
    let bounds = command.path.bounds();
    min_x = min_x.min(bounds.left());
    min_y = min_y.min(bounds.top());
    max_x = max_x.max(bounds.right());
    max_y = max_y.max(bounds.bottom());
  }

  if let Some(clip) = clip_path {
    let bounds = clip.bounds();
    min_x = min_x.max(bounds.left());
    min_y = min_y.max(bounds.top());
    max_x = max_x.min(bounds.right());
    max_y = max_y.min(bounds.bottom());
  }

  if !min_x.is_finite()
    || !min_y.is_finite()
    || !max_x.is_finite()
    || !max_y.is_finite()
    || min_x >= max_x
    || min_y >= max_y
  {
    return None;
  }

  let min_x = (min_x - pad).floor();
  let min_y = (min_y - pad).floor();
  let max_x = (max_x + pad).ceil();
  let max_y = (max_y + pad).ceil();

  Some((min_x, min_y, max_x, max_y))
}

struct VariationInfo<'a> {
  coords: Vec<F2Dot14>,
  var_index_map: Option<DeltaSetIndexMap<'a>>,
  item_variation_store: ItemVariationStore<'a>,
}

impl<'a> VariationInfo<'a> {
  fn new(colr: Colr<'a>, coords: &[F2Dot14]) -> Option<Self> {
    if coords.is_empty() {
      return None;
    }
    let item_variation_store = colr.item_variation_store()?.ok()?;
    let var_index_map = colr.var_index_map().and_then(|map| map.ok());
    Some(Self {
      coords: coords.to_vec(),
      var_index_map,
      item_variation_store,
    })
  }

  fn delta(&self, var_index: u32) -> f32 {
    let index = self
      .var_index_map
      .as_ref()
      .and_then(|map| map.get(var_index).ok())
      .unwrap_or_else(|| DeltaSetIndex {
        outer: (var_index >> 16) as u16,
        inner: (var_index & 0xFFFF) as u16,
      });
    self
      .item_variation_store
      .compute_delta(index, &self.coords)
      .ok()
      .map(|delta| delta as f32)
      .unwrap_or(0.0)
  }

  fn fword(&self, base: i16, var_index: u32) -> f32 {
    base as f32 + self.delta(var_index)
  }

  fn ufword(&self, base: u16, var_index: u32) -> f32 {
    base as f32 + self.delta(var_index)
  }

  fn f2dot14(&self, base: F2Dot14, var_index: u32) -> f32 {
    base.to_f32() + self.delta(var_index) / 16384.0
  }
}

struct Renderer<'a, 'p> {
  instance: &'a FontInstance<'a>,
  palette: &'p [Rgba],
  text_color: Rgba,
  base_transform: Transform,
  colr_entry: Arc<ColrV1CacheEntry>,
  caches: Arc<Mutex<ColorFontCaches>>,
  font_key: FontKey,
  variations: Option<VariationInfo<'static>>,
}

#[derive(Clone)]
struct DrawCommand {
  path: Path,
  brush: Brush,
  blend_mode: BlendMode,
}

#[derive(Clone)]
enum Brush {
  Solid(Color),
  LinearGradient {
    p0: Point,
    p1: Point,
    p2: Point,
    stops: Vec<GradientStop>,
    spread: SpreadMode,
  },
  RadialGradient {
    c0: Point,
    r0: f32,
    c1: Point,
    r1: f32,
    stops: Vec<ColorStop>,
    spread: SpreadMode,
    transform: Transform,
  },
  SweepGradient {
    center: Point,
    start_angle: f32,
    end_angle: f32,
    stops: Vec<ColorStop>,
    spread: SpreadMode,
    transform: Transform,
  },
}

#[derive(Clone)]
struct ColorStop {
  offset: f32,
  color: Rgba,
}

impl ResolvedColorLine {
  fn new(spread: SpreadMode, stops: Vec<ColorStop>) -> Self {
    let gradient_stops = stops
      .iter()
      .map(|stop| GradientStop::new(stop.offset, color_from_rgba(stop.color)))
      .collect();
    Self {
      spread,
      stops,
      gradient_stops,
    }
  }
}

struct ResolvedColorLine {
  spread: SpreadMode,
  stops: Vec<ColorStop>,
  gradient_stops: Vec<GradientStop>,
}

impl<'a, 'p> Renderer<'a, 'p> {
  fn vary_fword(&self, value: i16, var_index_base: u32, offset: u32) -> f32 {
    self
      .variations
      .as_ref()
      .map(|variation| variation.fword(value, var_index_base + offset))
      .unwrap_or(value as f32)
  }

  fn vary_ufword(&self, value: u16, var_index_base: u32, offset: u32) -> f32 {
    self
      .variations
      .as_ref()
      .map(|variation| variation.ufword(value, var_index_base + offset))
      .unwrap_or(value as f32)
  }

  fn walk_paint(
    &self,
    paint: Paint<'a>,
    paint_id: PaintId,
    design_transform: Transform,
    blend: BlendMode,
    commands: &mut Vec<DrawCommand>,
    stack: &mut HashSet<PaintId>,
  ) -> Option<()> {
    if !stack.insert(paint_id) {
      return None;
    }

    let colr = &self.colr_entry.colr;
    let result = match paint {
      Paint::ColrLayers(layer) => {
        let start = layer.first_layer_index() as usize;
        let count = layer.num_layers() as usize;
        for idx in start..start + count {
          let (layer_paint, id) = colr.v1_layer(idx).ok()?;
          self.walk_paint(layer_paint, id, design_transform, blend, commands, stack)?;
        }
        Some(())
      }
      Paint::Glyph(paint_glyph) => {
        let child = paint_glyph.paint().ok()?;
        let combined = design_to_device_transform(design_transform, self.base_transform);
        let brush = self.resolve_brush(child, combined)?;
        let path = build_outline(
          self.instance,
          paint_glyph.glyph_id().to_u16(),
          design_transform,
          self.base_transform,
        )?;
        commands.push(DrawCommand {
          path,
          brush,
          blend_mode: blend,
        });
        Some(())
      }
      Paint::ColrGlyph(colr) => {
        let glyph = GlyphId::from(colr.glyph_id().to_u16() as u32);
        let (child_paint, id) = self.cached_base_glyph(glyph)?;
        self.walk_paint(child_paint, id, design_transform, blend, commands, stack)
      }
      Paint::Transform(paint_transform) => {
        let transform = paint_transform.transform().ok()?;
        let m = Transform::from_row(
          fixed_to_f32(transform.xx()),
          fixed_to_f32(transform.yx()),
          fixed_to_f32(transform.xy()),
          fixed_to_f32(transform.yy()),
          fixed_to_f32(transform.dx()),
          fixed_to_f32(transform.dy()),
        );
        let child = paint_transform.paint().ok()?;
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarTransform(paint_transform) => {
        let transform = paint_transform.transform().ok()?;
        let m = Transform::from_row(
          fixed_to_f32(transform.xx()),
          fixed_to_f32(transform.yx()),
          fixed_to_f32(transform.xy()),
          fixed_to_f32(transform.yy()),
          fixed_to_f32(transform.dx()),
          fixed_to_f32(transform.dy()),
        );
        let child = paint_transform.paint().ok()?;
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::Translate(paint_translate) => {
        let child = paint_translate.paint().ok()?;
        let m = Transform::from_translate(
          paint_translate.dx().to_i16() as f32,
          paint_translate.dy().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarTranslate(paint_translate) => {
        let child = paint_translate.paint().ok()?;
        let m = Transform::from_translate(
          paint_translate.dx().to_i16() as f32,
          paint_translate.dy().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::Scale(scale) => {
        let child = scale.paint().ok()?;
        let m = Transform::from_scale(scale.scale_x().to_f32(), scale.scale_y().to_f32());
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarScale(scale) => {
        let child = scale.paint().ok()?;
        let m = Transform::from_scale(scale.scale_x().to_f32(), scale.scale_y().to_f32());
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::ScaleAroundCenter(scale) => {
        let child = scale.paint().ok()?;
        let m = scale_around_center(
          scale.scale_x().to_f32(),
          scale.scale_y().to_f32(),
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarScaleAroundCenter(scale) => {
        let child = scale.paint().ok()?;
        let m = scale_around_center(
          scale.scale_x().to_f32(),
          scale.scale_y().to_f32(),
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::ScaleUniform(scale) => {
        let child = scale.paint().ok()?;
        let s = scale.scale().to_f32();
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(Transform::from_scale(s, s)),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarScaleUniform(scale) => {
        let child = scale.paint().ok()?;
        let s = scale.scale().to_f32();
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(Transform::from_scale(s, s)),
          blend,
          commands,
          stack,
        )
      }
      Paint::ScaleUniformAroundCenter(scale) => {
        let child = scale.paint().ok()?;
        let s = scale.scale().to_f32();
        let m = scale_around_center(
          s,
          s,
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarScaleUniformAroundCenter(scale) => {
        let child = scale.paint().ok()?;
        let s = scale.scale().to_f32();
        let m = scale_around_center(
          s,
          s,
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::Rotate(rotate) => {
        let child = rotate.paint().ok()?;
        let m = Transform::from_rotate(rotate_degrees(rotate.angle()));
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarRotate(rotate) => {
        let child = rotate.paint().ok()?;
        let m = Transform::from_rotate(rotate_degrees(rotate.angle()));
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::RotateAroundCenter(rotate) => {
        let child = rotate.paint().ok()?;
        let m = rotate_around_center(
          rotate_degrees(rotate.angle()),
          rotate.center_x().to_i16() as f32,
          rotate.center_y().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarRotateAroundCenter(rotate) => {
        let child = rotate.paint().ok()?;
        let m = rotate_around_center(
          rotate_degrees(rotate.angle()),
          rotate.center_x().to_i16() as f32,
          rotate.center_y().to_i16() as f32,
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::Skew(skew) => {
        let child = skew.paint().ok()?;
        let m = skew_transform(skew.x_skew_angle(), skew.y_skew_angle(), None);
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarSkew(skew) => {
        let child = skew.paint().ok()?;
        let m = skew_transform(skew.x_skew_angle(), skew.y_skew_angle(), None);
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::SkewAroundCenter(skew) => {
        let child = skew.paint().ok()?;
        let m = skew_transform(
          skew.x_skew_angle(),
          skew.y_skew_angle(),
          Some((
            skew.center_x().to_i16() as f32,
            skew.center_y().to_i16() as f32,
          )),
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::VarSkewAroundCenter(skew) => {
        let child = skew.paint().ok()?;
        let m = skew_transform(
          skew.x_skew_angle(),
          skew.y_skew_angle(),
          Some((
            skew.center_x().to_i16() as f32,
            skew.center_y().to_i16() as f32,
          )),
        );
        self.walk_paint(
          child.clone(),
          paint_identity(&child),
          design_transform.pre_concat(m),
          blend,
          commands,
          stack,
        )
      }
      Paint::Composite(composite) => {
        let mode = map_blend_mode(composite.composite_mode())?;
        let backdrop = composite.backdrop_paint().ok()?;
        let source = composite.source_paint().ok()?;
        let backdrop_id = paint_identity(&backdrop);
        let source_id = paint_identity(&source);
        self.walk_paint(
          backdrop,
          backdrop_id,
          design_transform,
          blend,
          commands,
          stack,
        )?;
        self.walk_paint(source, source_id, design_transform, mode, commands, stack)
      }
      _ => None,
    };

    stack.remove(&paint_id);
    result
  }

  fn resolve_brush(&self, paint: Paint<'a>, combined: Transform) -> Option<Brush> {
    match paint {
      Paint::Solid(solid) => Some(Brush::Solid(resolve_color(
        resolve_palette_color(solid.palette_index(), self.palette, self.text_color),
        solid.alpha(),
      ))),
      Paint::VarSolid(solid) => Some(Brush::Solid(resolve_color(
        resolve_palette_color(solid.palette_index(), self.palette, self.text_color),
        solid.alpha(),
      ))),
      Paint::LinearGradient(gradient) => {
        let color_line = gradient.color_line().ok()?;
        let resolved = resolve_color_line(color_line, self.palette, self.text_color)?;
        let p0 = map_point(
          gradient.x0().to_i16() as f32,
          gradient.y0().to_i16() as f32,
          combined,
        );
        let p1 = map_point(
          gradient.x1().to_i16() as f32,
          gradient.y1().to_i16() as f32,
          combined,
        );
        let p2 = map_point(
          gradient.x2().to_i16() as f32,
          gradient.y2().to_i16() as f32,
          combined,
        );
        Some(Brush::LinearGradient {
          p0,
          p1,
          p2,
          stops: resolved.gradient_stops,
          spread: resolved.spread,
        })
      }
      Paint::VarLinearGradient(gradient) => {
        let color_line = gradient.color_line().ok()?;
        let resolved = resolve_var_color_line(
          color_line,
          self.palette,
          self.text_color,
          self.variations.as_ref(),
        )?;
        let var_index = gradient.var_index_base();
        let p0 = map_point(
          self.vary_fword(gradient.x0().to_i16(), var_index, 0),
          self.vary_fword(gradient.y0().to_i16(), var_index, 1),
          combined,
        );
        let p1 = map_point(
          self.vary_fword(gradient.x1().to_i16(), var_index, 2),
          self.vary_fword(gradient.y1().to_i16(), var_index, 3),
          combined,
        );
        let p2 = map_point(
          self.vary_fword(gradient.x2().to_i16(), var_index, 4),
          self.vary_fword(gradient.y2().to_i16(), var_index, 5),
          combined,
        );
        Some(Brush::LinearGradient {
          p0,
          p1,
          p2,
          stops: resolved.gradient_stops,
          spread: resolved.spread,
        })
      }
      Paint::RadialGradient(radial) => {
        let color_line = radial.color_line().ok()?;
        let resolved = resolve_color_line(color_line, self.palette, self.text_color)?;
        Some(Brush::RadialGradient {
          c0: Point::from_xy(radial.x0().to_i16() as f32, radial.y0().to_i16() as f32),
          r0: fixed_to_f32(radial.radius0().to_fixed()),
          c1: Point::from_xy(radial.x1().to_i16() as f32, radial.y1().to_i16() as f32),
          r1: fixed_to_f32(radial.radius1().to_fixed()),
          stops: resolved.stops,
          spread: resolved.spread,
          transform: combined,
        })
      }
      Paint::VarRadialGradient(radial) => {
        let color_line = radial.color_line().ok()?;
        let resolved = resolve_var_color_line(
          color_line,
          self.palette,
          self.text_color,
          self.variations.as_ref(),
        )?;
        let var_index = radial.var_index_base();
        let c0 = Point::from_xy(
          self.vary_fword(radial.x0().to_i16(), var_index, 0),
          self.vary_fword(radial.y0().to_i16(), var_index, 1),
        );
        let c1 = Point::from_xy(
          self.vary_fword(radial.x1().to_i16(), var_index, 3),
          self.vary_fword(radial.y1().to_i16(), var_index, 4),
        );
        Some(Brush::RadialGradient {
          c0,
          r0: self.vary_ufword(radial.radius0().to_u16(), var_index, 2),
          c1,
          r1: self.vary_ufword(radial.radius1().to_u16(), var_index, 5),
          stops: resolved.stops,
          spread: resolved.spread,
          transform: combined,
        })
      }
      Paint::SweepGradient(sweep) => {
        let color_line = sweep.color_line().ok()?;
        let (spread, stops) = resolve_color_line_rgba(color_line, self.palette, self.text_color)?;
        Some(Brush::SweepGradient {
          center: Point::from_xy(
            sweep.center_x().to_i16() as f32,
            sweep.center_y().to_i16() as f32,
          ),
          start_angle: biased_angle_half_turns(sweep.start_angle()),
          end_angle: biased_angle_half_turns(sweep.end_angle()),
          stops,
          spread,
          transform: combined,
        })
      }
      Paint::VarSweepGradient(sweep) => {
        let color_line = sweep.color_line().ok()?;
        let (spread, stops) = resolve_var_color_line_rgba(
          color_line,
          self.palette,
          self.text_color,
          self.variations.as_ref(),
        )?;
        Some(Brush::SweepGradient {
          center: Point::from_xy(
            sweep.center_x().to_i16() as f32,
            sweep.center_y().to_i16() as f32,
          ),
          start_angle: biased_angle_half_turns(sweep.start_angle()),
          end_angle: biased_angle_half_turns(sweep.end_angle()),
          stops,
          spread,
          transform: combined,
        })
      }
      Paint::Glyph(paint_glyph) => {
        let child = paint_glyph.paint().ok()?;
        self.resolve_brush(child, combined)
      }
      Paint::Transform(transform) => {
        let m = transform.transform().ok()?;
        let matrix = Transform::from_row(
          fixed_to_f32(m.xx()),
          fixed_to_f32(m.yx()),
          fixed_to_f32(m.xy()),
          fixed_to_f32(m.yy()),
          fixed_to_f32(m.dx()),
          fixed_to_f32(m.dy()),
        );
        self.resolve_brush(transform.paint().ok()?, combined.pre_concat(matrix))
      }
      Paint::VarTransform(transform) => {
        let m = transform.transform().ok()?;
        let matrix = Transform::from_row(
          fixed_to_f32(m.xx()),
          fixed_to_f32(m.yx()),
          fixed_to_f32(m.xy()),
          fixed_to_f32(m.yy()),
          fixed_to_f32(m.dx()),
          fixed_to_f32(m.dy()),
        );
        self.resolve_brush(transform.paint().ok()?, combined.pre_concat(matrix))
      }
      Paint::Translate(t) => {
        let m = Transform::from_translate(t.dx().to_i16() as f32, t.dy().to_i16() as f32);
        self.resolve_brush(t.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarTranslate(t) => {
        let m = Transform::from_translate(t.dx().to_i16() as f32, t.dy().to_i16() as f32);
        self.resolve_brush(t.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Scale(scale) => {
        let m = Transform::from_scale(scale.scale_x().to_f32(), scale.scale_y().to_f32());
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarScale(scale) => {
        let m = Transform::from_scale(scale.scale_x().to_f32(), scale.scale_y().to_f32());
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::ScaleAroundCenter(scale) => {
        let m = scale_around_center(
          scale.scale_x().to_f32(),
          scale.scale_y().to_f32(),
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarScaleAroundCenter(scale) => {
        let m = scale_around_center(
          scale.scale_x().to_f32(),
          scale.scale_y().to_f32(),
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::ScaleUniform(scale) => {
        let s = scale.scale().to_f32();
        self.resolve_brush(
          scale.paint().ok()?,
          combined.pre_concat(Transform::from_scale(s, s)),
        )
      }
      Paint::VarScaleUniform(scale) => {
        let s = scale.scale().to_f32();
        self.resolve_brush(
          scale.paint().ok()?,
          combined.pre_concat(Transform::from_scale(s, s)),
        )
      }
      Paint::ScaleUniformAroundCenter(scale) => {
        let s = scale.scale().to_f32();
        let m = scale_around_center(
          s,
          s,
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarScaleUniformAroundCenter(scale) => {
        let s = scale.scale().to_f32();
        let m = scale_around_center(
          s,
          s,
          scale.center_x().to_i16() as f32,
          scale.center_y().to_i16() as f32,
        );
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Rotate(rotate) => {
        let m = Transform::from_rotate(rotate_degrees(rotate.angle()));
        self.resolve_brush(rotate.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarRotate(rotate) => {
        let m = Transform::from_rotate(rotate_degrees(rotate.angle()));
        self.resolve_brush(rotate.paint().ok()?, combined.pre_concat(m))
      }
      Paint::RotateAroundCenter(rotate) => {
        let m = rotate_around_center(
          rotate_degrees(rotate.angle()),
          rotate.center_x().to_i16() as f32,
          rotate.center_y().to_i16() as f32,
        );
        self.resolve_brush(rotate.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarRotateAroundCenter(rotate) => {
        let m = rotate_around_center(
          rotate_degrees(rotate.angle()),
          rotate.center_x().to_i16() as f32,
          rotate.center_y().to_i16() as f32,
        );
        self.resolve_brush(rotate.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Skew(skew) => {
        let m = skew_transform(skew.x_skew_angle(), skew.y_skew_angle(), None);
        self.resolve_brush(skew.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarSkew(skew) => {
        let m = skew_transform(skew.x_skew_angle(), skew.y_skew_angle(), None);
        self.resolve_brush(skew.paint().ok()?, combined.pre_concat(m))
      }
      Paint::SkewAroundCenter(skew) => {
        let m = skew_transform(
          skew.x_skew_angle(),
          skew.y_skew_angle(),
          Some((
            skew.center_x().to_i16() as f32,
            skew.center_y().to_i16() as f32,
          )),
        );
        self.resolve_brush(skew.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarSkewAroundCenter(skew) => {
        let m = skew_transform(
          skew.x_skew_angle(),
          skew.y_skew_angle(),
          Some((
            skew.center_x().to_i16() as f32,
            skew.center_y().to_i16() as f32,
          )),
        );
        self.resolve_brush(skew.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Composite(composite) => self.resolve_brush(composite.source_paint().ok()?, combined),
      Paint::ColrGlyph(colr) => {
        let glyph = GlyphId::from(colr.glyph_id().to_u16() as u32);
        let (paint, _) = self.cached_base_glyph(glyph)?;
        self.resolve_brush(paint, combined)
      }
      Paint::ColrLayers(_) => None,
    }
  }

  fn cached_base_glyph(&self, glyph: GlyphId) -> Option<(Paint<'static>, PaintId)> {
    let key = BaseGlyphCacheKey::new(self.font_key, glyph.to_u32() as u16);
    let mut caches = self.caches.lock().ok()?;
    caches.colr_v1_base_glyph(key, self.colr_entry.as_ref())?
  }
}

/// Apply COLR design-space transforms before mapping from font design units (Y-up) into device
/// space (Y-down) using the base font transform (scale + synthetic oblique + flip).
#[inline]
fn design_to_device_transform(design_transform: Transform, base_transform: Transform) -> Transform {
  // Matrices are left-multiplied, so pre-concatenating the design-space transform ensures glyph
  // space → COLR paint transform → font-to-device transform in that order.
  base_transform.pre_concat(design_transform)
}

fn brush_to_paint(brush: &Brush, offset_x: f32, offset_y: f32) -> Option<SkiaPaint<'static>> {
  let mut paint = SkiaPaint::default();
  match brush {
    Brush::Solid(color) => {
      paint.shader = tiny_skia::Shader::SolidColor(*color);
    }
    Brush::LinearGradient {
      p0,
      p1,
      p2,
      stops,
      spread,
    } => {
      let v1 = Point::from_xy(p1.x - p0.x, p1.y - p0.y);
      let v2 = Point::from_xy(p2.x - p0.x, p2.y - p0.y);
      // COLRv1 maps a linear gradient through three control points. Define the gradient
      // in canonical shader space (start=(0, 0), end=(1, 0)) and use a local transform
      // to span the parallelogram P(u, v) = P0 + u*(P1-P0) + v*(P2-P0), offset into the
      // glyph's raster origin.
      let mut start = Point::from_xy(0.0, 0.0);
      let mut end = Point::from_xy(1.0, 0.0);
      let mut transform =
        Transform::from_row(v1.x, v1.y, v2.x, v2.y, p0.x - offset_x, p0.y - offset_y);

      if transform.invert().is_none() {
        // Degenerate basis; fall back to the simple start/end mapping used previously.
        start = Point::from_xy(p0.x - offset_x, p0.y - offset_y);
        end = Point::from_xy(p1.x - p0.x + start.x, p1.y - p0.y + start.y);
        transform = Transform::identity();
      }

      let shader = tiny_skia::LinearGradient::new(start, end, stops.clone(), *spread, transform)?;
      paint.shader = shader;
    }
    Brush::RadialGradient { .. } => return None,
    Brush::SweepGradient { .. } => return None,
  }
  Some(paint)
}

fn render_two_circle_radial_gradient(
  command: &DrawCommand,
  target: &mut Pixmap,
  min_x: f32,
  min_y: f32,
  translate: Transform,
  clip: Option<&Mask>,
) -> Option<()> {
  let Brush::RadialGradient {
    c0,
    r0,
    c1,
    r1,
    stops,
    spread,
    transform,
  } = &command.brush
  else {
    return None;
  };

  if stops.is_empty() {
    return Some(());
  }

  let inv_transform = transform.invert()?;

  let width = target.width();
  let height = target.height();
  let mut mask = Mask::new(width, height)?;
  mask.fill_path(&command.path, FillRule::Winding, true, translate);

  if let Some(clip_mask) = clip {
    for (coverage, clip_coverage) in mask.data_mut().iter_mut().zip(clip_mask.data()) {
      let combined = (*coverage as u16 * *clip_coverage as u16 + 127) / 255;
      *coverage = combined as u8;
    }
  }

  if mask.data().iter().all(|v| *v == 0) {
    return Some(());
  }

  let mut layer = new_pixmap(width, height)?;
  let mask_data = mask.data();
  let layer_data = layer.pixels_mut();
  for y in 0..height {
    let world_y = min_y + y as f32 + 0.5;
    for x in 0..width {
      let idx = (y * width + x) as usize;
      let coverage = mask_data[idx];
      if coverage == 0 {
        continue;
      }

      let world_x = min_x + x as f32 + 0.5;
      let mut point = Point::from_xy(world_x, world_y);
      inv_transform.map_point(&mut point);

      let mut t = match solve_two_circle_t(point, *c0, *r0, *c1, *r1) {
        Some(value) if value.is_finite() => value,
        _ => continue,
      };
      t = apply_spread_mode(t, *spread);

      let color = sample_color_stops(stops, t);
      let alpha = (color.a * (coverage as f32 / 255.0)).clamp(0.0, 1.0);
      if alpha <= 0.0 {
        continue;
      }
      let pm_a = (alpha * 255.0).round().clamp(0.0, 255.0) as u8;
      let pm_r = ((color.r as f32) * alpha).round().clamp(0.0, 255.0) as u8;
      let pm_g = ((color.g as f32) * alpha).round().clamp(0.0, 255.0) as u8;
      let pm_b = ((color.b as f32) * alpha).round().clamp(0.0, 255.0) as u8;
      layer_data[idx] = PremultipliedColorU8::from_rgba(pm_r, pm_g, pm_b, pm_a)
        .unwrap_or(PremultipliedColorU8::TRANSPARENT);
    }
  }

  let paint = PixmapPaint {
    opacity: 1.0,
    blend_mode: command.blend_mode,
    ..Default::default()
  };
  target.draw_pixmap(0, 0, layer.as_ref(), &paint, Transform::identity(), None);

  Some(())
}

fn render_sweep_gradient(
  command: &DrawCommand,
  target: &mut Pixmap,
  min_x: f32,
  min_y: f32,
  translate: Transform,
  clip: Option<&Mask>,
) -> Option<()> {
  let Brush::SweepGradient {
    center,
    start_angle,
    end_angle,
    stops,
    spread,
    transform,
  } = &command.brush
  else {
    return None;
  };

  if stops.is_empty() {
    return Some(());
  }

  let span = end_angle - start_angle;
  if span.abs() <= f32::EPSILON {
    return None;
  }
  let inv_span = 1.0 / span;
  let inv_transform = transform.invert()?;

  let width = target.width();
  let height = target.height();
  let mut mask = Mask::new(width, height)?;
  mask.fill_path(&command.path, FillRule::Winding, true, translate);

  if let Some(clip_mask) = clip {
    for (coverage, clip_coverage) in mask.data_mut().iter_mut().zip(clip_mask.data()) {
      let combined = (*coverage as u16 * *clip_coverage as u16 + 127) / 255;
      *coverage = combined as u8;
    }
  }

  if mask.data().iter().all(|v| *v == 0) {
    return Some(());
  }

  let mut layer = new_pixmap(width, height)?;
  let mask_data = mask.data();
  let layer_data = layer.pixels_mut();
  let start = *start_angle;
  for y in 0..height {
    for x in 0..width {
      let idx = (y * width + x) as usize;
      let coverage = mask_data[idx];
      if coverage == 0 {
        continue;
      }

      let world_x = min_x + x as f32 + 0.5;
      let world_y = min_y + y as f32 + 0.5;
      let mut point = Point::from_xy(world_x, world_y);
      inv_transform.map_point(&mut point);

      let dx = point.x - center.x;
      let dy = point.y - center.y;
      let angle = dy.atan2(dx) / PI;
      let aligned = (angle - start).rem_euclid(2.0) + start;
      let mut t = (aligned - start) * inv_span;
      t = apply_spread_mode(t, *spread);

      let color = sample_color_stops(stops, t);
      let alpha = (color.a * (coverage as f32 / 255.0)).clamp(0.0, 1.0);
      if alpha <= 0.0 {
        continue;
      }
      let pm_a = (alpha * 255.0).round().clamp(0.0, 255.0) as u8;
      let pm_r = ((color.r as f32) * alpha).round().clamp(0.0, 255.0) as u8;
      let pm_g = ((color.g as f32) * alpha).round().clamp(0.0, 255.0) as u8;
      let pm_b = ((color.b as f32) * alpha).round().clamp(0.0, 255.0) as u8;
      layer_data[idx] = PremultipliedColorU8::from_rgba(pm_r, pm_g, pm_b, pm_a)
        .unwrap_or(PremultipliedColorU8::TRANSPARENT);
    }
  }

  let paint = PixmapPaint {
    opacity: 1.0,
    blend_mode: command.blend_mode,
    ..Default::default()
  };
  target.draw_pixmap(0, 0, layer.as_ref(), &paint, Transform::identity(), None);

  Some(())
}

fn solve_two_circle_t(point: Point, c0: Point, r0: f32, c1: Point, r1: f32) -> Option<f32> {
  // Solve |P - (C0 + t*(C1-C0))| = r0 + t*(r1-r0) for t, i.e. find the
  // parameter where the point lies on the circle interpolated between C0/r0 and
  // C1/r1.
  let dx = c1.x - c0.x;
  let dy = c1.y - c0.y;
  let dr = r1 - r0;
  let px = point.x - c0.x;
  let py = point.y - c0.y;

  let a = dx * dx + dy * dy - dr * dr;
  let b = -2.0 * (px * dx + py * dy + r0 * dr);
  let c = px * px + py * py - r0 * r0;

  if a.abs() < 1e-6 {
    if b.abs() < 1e-6 {
      return None;
    }
    return Some(-c / b);
  }

  let disc = (b * b - 4.0 * a * c).max(0.0);
  let sqrt_disc = disc.sqrt();
  let denom = 2.0 * a;
  if denom.abs() < 1e-6 {
    return None;
  }
  let t0 = (-b - sqrt_disc) / denom;
  let t1 = (-b + sqrt_disc) / denom;
  let t = if t0.abs() <= t1.abs() { t0 } else { t1 };
  Some(t)
}

fn color_from_rgba(color: Rgba) -> Color {
  Color::from_rgba8(
    color.r,
    color.g,
    color.b,
    (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
  )
}

fn resolve_color(color: Rgba, alpha: F2Dot14) -> Color {
  let a = (color.a * alpha.to_f32()).clamp(0.0, 1.0);
  Color::from_rgba8(color.r, color.g, color.b, (a * 255.0).round() as u8)
}

fn resolve_palette_color(idx: u16, palette: &[Rgba], text_color: Rgba) -> Rgba {
  if idx == 0xFFFF {
    return text_color;
  }
  palette.get(idx as usize).copied().unwrap_or(text_color)
}

fn resolve_color_line(
  line: ColorLine<'_>,
  palette: &[Rgba],
  text_color: Rgba,
) -> Option<ResolvedColorLine> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    stops.push(ColorStop {
      offset: stop.stop_offset().to_f32(),
      color: resolve_rgba_color(stop.palette_index(), stop.alpha(), palette, text_color),
    });
  }
  Some(ResolvedColorLine::new(spread, stops))
}

fn resolve_var_color_line(
  line: VarColorLine<'_>,
  palette: &[Rgba],
  text_color: Rgba,
  variations: Option<&VariationInfo<'_>>,
) -> Option<ResolvedColorLine> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    let var_index = stop.var_index_base();
    let offset = variations
      .as_ref()
      .map(|variation| variation.f2dot14(stop.stop_offset(), var_index))
      .unwrap_or_else(|| stop.stop_offset().to_f32());
    let alpha = variations
      .as_ref()
      .map(|variation| F2Dot14::from_f32(variation.f2dot14(stop.alpha(), var_index + 1)))
      .unwrap_or_else(|| stop.alpha());
    stops.push(ColorStop {
      offset,
      color: resolve_rgba_color(stop.palette_index(), alpha, palette, text_color),
    });
  }
  Some(ResolvedColorLine::new(spread, stops))
}

fn resolve_color_line_rgba(
  line: ColorLine<'_>,
  palette: &[Rgba],
  text_color: Rgba,
) -> Option<(SpreadMode, Vec<ColorStop>)> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    stops.push(ColorStop {
      offset: stop.stop_offset().to_f32(),
      color: resolve_rgba_color(stop.palette_index(), stop.alpha(), palette, text_color),
    });
  }
  Some((spread, stops))
}

fn resolve_var_color_line_rgba(
  line: VarColorLine<'_>,
  palette: &[Rgba],
  text_color: Rgba,
  variations: Option<&VariationInfo<'_>>,
) -> Option<(SpreadMode, Vec<ColorStop>)> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    let var_index = stop.var_index_base();
    let offset = variations
      .as_ref()
      .map(|variation| variation.f2dot14(stop.stop_offset(), var_index))
      .unwrap_or_else(|| stop.stop_offset().to_f32());
    let alpha = variations
      .as_ref()
      .map(|variation| F2Dot14::from_f32(variation.f2dot14(stop.alpha(), var_index + 1)))
      .unwrap_or_else(|| stop.alpha());
    stops.push(ColorStop {
      offset,
      color: resolve_rgba_color(stop.palette_index(), alpha, palette, text_color),
    });
  }
  Some((spread, stops))
}

fn resolve_rgba_color(
  palette_index: u16,
  alpha: F2Dot14,
  palette: &[Rgba],
  text_color: Rgba,
) -> Rgba {
  let mut base = resolve_palette_color(palette_index, palette, text_color);
  base.a = (base.a * alpha.to_f32()).clamp(0.0, 1.0);
  base
}

fn biased_angle_half_turns(angle: F2Dot14) -> f32 {
  // Sweep gradients encode start/end angles with a +1.0 bias to allow 360° values.
  angle.to_f32() + 1.0
}

fn rotate_degrees(angle: F2Dot14) -> f32 {
  // F2Dot14 angles are stored in half-turn units; convert to degrees.
  angle.to_f32() * 180.0
}

fn skew_transform(x_angle: F2Dot14, y_angle: F2Dot14, center: Option<(f32, f32)>) -> Transform {
  let x_tan = (rotate_degrees(x_angle)).to_radians().tan();
  let y_tan = (rotate_degrees(y_angle)).to_radians().tan();
  let mut transform = Transform::from_row(1.0, y_tan, x_tan, 1.0, 0.0, 0.0);
  if let Some((cx, cy)) = center {
    transform = Transform::from_translate(cx, cy)
      .pre_concat(transform)
      .pre_concat(Transform::from_translate(-cx, -cy));
  }
  transform
}

fn rotate_around_center(angle: f32, cx: f32, cy: f32) -> Transform {
  Transform::from_translate(cx, cy)
    .pre_concat(Transform::from_rotate(angle))
    .pre_concat(Transform::from_translate(-cx, -cy))
}

fn scale_around_center(scale_x: f32, scale_y: f32, cx: f32, cy: f32) -> Transform {
  Transform::from_translate(cx, cy)
    .pre_concat(Transform::from_scale(scale_x, scale_y))
    .pre_concat(Transform::from_translate(-cx, -cy))
}

fn map_spread(extend: Extend) -> Option<SpreadMode> {
  match extend {
    Extend::Pad => Some(SpreadMode::Pad),
    Extend::Repeat => Some(SpreadMode::Repeat),
    Extend::Reflect => Some(SpreadMode::Reflect),
    Extend::Unknown => None,
  }
}

fn apply_spread_mode(mut t: f32, spread: SpreadMode) -> f32 {
  match spread {
    SpreadMode::Pad => t.clamp(0.0, 1.0),
    SpreadMode::Repeat => t.rem_euclid(1.0),
    SpreadMode::Reflect => {
      t = t.rem_euclid(2.0);
      if t > 1.0 {
        2.0 - t
      } else {
        t
      }
    }
  }
}

fn sample_color_stops(stops: &[ColorStop], position: f32) -> Rgba {
  if stops.is_empty() {
    return Rgba::TRANSPARENT;
  }
  if stops.len() == 1 {
    return stops[0].color;
  }

  let first = &stops[0];
  if position <= first.offset {
    return first.color;
  }
  for pair in stops.windows(2) {
    let (start, end) = (&pair[0], &pair[1]);
    if position < start.offset {
      return start.color;
    }
    if position <= end.offset || (end.offset - start.offset).abs() < f32::EPSILON {
      let span = (end.offset - start.offset).max(1e-6);
      let frac = ((position - start.offset) / span).clamp(0.0, 1.0);
      return Rgba {
        r: lerp_channel(start.color.r, end.color.r, frac),
        g: lerp_channel(start.color.g, end.color.g, frac),
        b: lerp_channel(start.color.b, end.color.b, frac),
        a: start.color.a + (end.color.a - start.color.a) * frac,
      };
    }
  }
  stops
    .last()
    .map(|stop| stop.color)
    .unwrap_or(Rgba::TRANSPARENT)
}

fn lerp_channel(start: u8, end: u8, t: f32) -> u8 {
  ((1.0 - t) * start as f32 + t * end as f32)
    .round()
    .clamp(0.0, 255.0) as u8
}

fn map_blend_mode(mode: CompositeMode) -> Option<BlendMode> {
  match mode {
    CompositeMode::Clear => Some(BlendMode::Clear),
    CompositeMode::Src => Some(BlendMode::Source),
    CompositeMode::Dest => Some(BlendMode::Destination),
    CompositeMode::SrcOver => Some(BlendMode::SourceOver),
    CompositeMode::DestOver => Some(BlendMode::DestinationOver),
    CompositeMode::SrcIn => Some(BlendMode::SourceIn),
    CompositeMode::DestIn => Some(BlendMode::DestinationIn),
    CompositeMode::SrcOut => Some(BlendMode::SourceOut),
    CompositeMode::DestOut => Some(BlendMode::DestinationOut),
    CompositeMode::SrcAtop => Some(BlendMode::SourceAtop),
    CompositeMode::DestAtop => Some(BlendMode::DestinationAtop),
    CompositeMode::Xor => Some(BlendMode::Xor),
    CompositeMode::Plus => Some(BlendMode::Plus),
    CompositeMode::Screen => Some(BlendMode::Screen),
    CompositeMode::Overlay => Some(BlendMode::Overlay),
    CompositeMode::Darken => Some(BlendMode::Darken),
    CompositeMode::Lighten => Some(BlendMode::Lighten),
    CompositeMode::ColorDodge => Some(BlendMode::ColorDodge),
    CompositeMode::ColorBurn => Some(BlendMode::ColorBurn),
    CompositeMode::HardLight => Some(BlendMode::HardLight),
    CompositeMode::SoftLight => Some(BlendMode::SoftLight),
    CompositeMode::Difference => Some(BlendMode::Difference),
    CompositeMode::Exclusion => Some(BlendMode::Exclusion),
    CompositeMode::Multiply => Some(BlendMode::Multiply),
    CompositeMode::HslHue => Some(BlendMode::Hue),
    CompositeMode::HslSaturation => Some(BlendMode::Saturation),
    CompositeMode::HslColor => Some(BlendMode::Color),
    CompositeMode::HslLuminosity => Some(BlendMode::Luminosity),
    CompositeMode::Unknown => None,
  }
}

fn build_outline(
  instance: &FontInstance<'_>,
  glyph_id: u16,
  design_transform: Transform,
  base: Transform,
) -> Option<Path> {
  let outline = instance.glyph_outline(glyph_id as u32)?;
  let path = outline.path?;
  let transform = design_to_device_transform(design_transform, base);
  path.transform(transform)
}

fn clip_box_value(
  base: FWord,
  colr: &Colr<'_>,
  var_index_base: u32,
  offset: u32,
  normalized_coords: &[F2Dot14],
) -> f32 {
  let value = base.to_i16() as f32;
  if normalized_coords.is_empty() {
    return value;
  }
  let Some(Ok(item_variation_store)) = colr.item_variation_store() else {
    return value;
  };

  let mapped_index = match colr.var_index_map() {
    Some(Ok(map)) => map.get(var_index_base + offset).ok(),
    Some(Err(_)) => None,
    None => Some(DeltaSetIndex {
      outer: ((var_index_base + offset) >> 16) as u16,
      inner: (var_index_base + offset) as u16,
    }),
  };
  let Some(mapped_index) = mapped_index else {
    return value;
  };
  let Ok(delta) = item_variation_store.compute_float_delta(mapped_index, normalized_coords) else {
    return value;
  };
  base.apply_float_delta(delta)
}

fn build_clip_path(
  clip: ClipBox<'_>,
  colr: &Colr<'_>,
  normalized_coords: &[f32],
  base_transform: Transform,
) -> Option<Path> {
  let rect = match clip {
    ClipBox::Format1(b) => Rect::from_ltrb(
      b.x_min().to_i16() as f32,
      b.y_min().to_i16() as f32,
      b.x_max().to_i16() as f32,
      b.y_max().to_i16() as f32,
    )?,
    ClipBox::Format2(b) => {
      let var_index_base = b.var_index_base();
      let coords: Vec<F2Dot14> = normalized_coords
        .iter()
        .map(|coord| F2Dot14::from_f32(*coord))
        .collect();
      let x_min = clip_box_value(b.x_min(), colr, var_index_base, 0, &coords);
      let y_min = clip_box_value(b.y_min(), colr, var_index_base, 1, &coords);
      let x_max = clip_box_value(b.x_max(), colr, var_index_base, 2, &coords);
      let y_max = clip_box_value(b.y_max(), colr, var_index_base, 3, &coords);
      Rect::from_ltrb(x_min, y_min, x_max, y_max)?
    }
  };
  let mut builder = PathBuilder::new();
  let points = [
    map_point(rect.left(), rect.top(), base_transform),
    map_point(rect.right(), rect.top(), base_transform),
    map_point(rect.right(), rect.bottom(), base_transform),
    map_point(rect.left(), rect.bottom(), base_transform),
  ];
  builder.move_to(points[0].x, points[0].y);
  for point in &points[1..] {
    builder.line_to(point.x, point.y);
  }
  builder.close();
  builder.finish()
}

fn build_clip_mask(
  path: &Path,
  width: u32,
  height: u32,
  offset_x: f32,
  offset_y: f32,
) -> Option<Mask> {
  let mut mask = Mask::new(width, height)?;
  let transform = Transform::from_translate(-offset_x, -offset_y);
  mask.fill_path(path, FillRule::Winding, true, transform);
  Some(mask)
}

fn map_point(x: f32, y: f32, transform: Transform) -> Point {
  let mut point = Point::from_xy(x, y);
  transform.map_point(&mut point);
  point
}

fn fixed_to_f32(value: Fixed) -> f32 {
  value.to_f32()
}

fn paint_identity(paint: &Paint<'_>) -> PaintId {
  paint.offset_data().as_ref().as_ptr() as usize + paint.format() as usize
}

#[cfg(test)]
thread_local! {
  static TEST_PARSE_COUNTING_ENABLED: Cell<bool> = Cell::new(false);
  static TEST_FONT_REF_PARSE_COUNT: Cell<u64> = Cell::new(0);
  static TEST_COLR_ACCESS_COUNT: Cell<u64> = Cell::new(0);
}

#[cfg(test)]
#[inline]
fn record_font_ref_parse() {
  TEST_PARSE_COUNTING_ENABLED.with(|enabled| {
    if enabled.get() {
      TEST_FONT_REF_PARSE_COUNT.with(|count| count.set(count.get().saturating_add(1)));
    }
  });
}

#[cfg(test)]
#[inline]
fn record_colr_table_access() {
  TEST_PARSE_COUNTING_ENABLED.with(|enabled| {
    if enabled.get() {
      TEST_COLR_ACCESS_COUNT.with(|count| count.set(count.get().saturating_add(1)));
    }
  });
}

#[cfg(test)]
pub(super) struct ColrV1ParseCountGuard {
  _private: (),
}

#[cfg(test)]
impl ColrV1ParseCountGuard {
  pub fn start() -> Self {
    TEST_FONT_REF_PARSE_COUNT.with(|count| count.set(0));
    TEST_COLR_ACCESS_COUNT.with(|count| count.set(0));
    TEST_PARSE_COUNTING_ENABLED.with(|enabled| enabled.set(true));
    Self { _private: () }
  }
}

#[cfg(test)]
impl Drop for ColrV1ParseCountGuard {
  fn drop(&mut self) {
    TEST_PARSE_COUNTING_ENABLED.with(|enabled| enabled.set(false));
  }
}

#[cfg(test)]
pub(super) fn colr_v1_parse_counts() -> (u64, u64) {
  (
    TEST_FONT_REF_PARSE_COUNT.with(|count| count.get()),
    TEST_COLR_ACCESS_COUNT.with(|count| count.get()),
  )
}

#[cfg(not(test))]
#[inline]
fn record_font_ref_parse() {}

#[cfg(not(test))]
#[inline]
fn record_colr_table_access() {}

#[cfg(test)]
mod tests {
  use super::{
    colr_v1_parse_counts, raster_bounds, Brush, ColrV1ParseCountGuard, DrawCommand, RASTER_PAD,
  };
  use crate::style::color::Rgba;
  use crate::text::color_fonts::ColorFontRenderer;
  use crate::text::font_db::{FontStretch, FontStyle, FontWeight, LoadedFont};
  use crate::text::font_instance::FontInstance;
  use std::path::PathBuf;
  use std::sync::Arc;
  use tiny_skia::{BlendMode, Color, Path, PathBuilder};

  fn rect_path(left: f32, top: f32, right: f32, bottom: f32) -> Path {
    let mut builder = PathBuilder::new();
    builder.move_to(left, top);
    builder.line_to(right, top);
    builder.line_to(right, bottom);
    builder.line_to(left, bottom);
    builder.close();
    builder.finish().unwrap()
  }

  #[test]
  fn clip_bounds_intersection_limits_allocation() {
    let commands = vec![DrawCommand {
      path: rect_path(10.0, 10.0, 20.0, 20.0),
      brush: Brush::Solid(Color::from_rgba8(0, 0, 0, 255)),
      blend_mode: BlendMode::SourceOver,
    }];
    let large_clip = rect_path(-1000.0, -1000.0, 1000.0, 1000.0);

    let (min_x, min_y, max_x, max_y) =
      raster_bounds(&commands, Some(&large_clip), RASTER_PAD).expect("bounds");

    assert_eq!(min_x, 9.0);
    assert_eq!(min_y, 9.0);
    assert_eq!(max_x, 21.0);
    assert_eq!(max_y, 21.0);
    assert_eq!(max_x - min_x, 12.0);
    assert_eq!(max_y - min_y, 12.0);
  }

  #[test]
  fn clip_bounds_without_overlap_returns_none() {
    let commands = vec![DrawCommand {
      path: rect_path(0.0, 0.0, 5.0, 5.0),
      brush: Brush::Solid(Color::from_rgba8(0, 0, 0, 255)),
      blend_mode: BlendMode::SourceOver,
    }];
    let disjoint_clip = rect_path(10.0, 10.0, 15.0, 15.0);

    assert!(
      raster_bounds(&commands, Some(&disjoint_clip), RASTER_PAD).is_none(),
      "clip should cull non-overlapping paints"
    );
  }

  fn fixtures_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("tests")
      .join("fixtures")
  }

  fn load_test_font() -> LoadedFont {
    let data = std::fs::read(fixtures_path().join("fonts/colrv1-test.ttf"))
      .expect("failed to read COLRv1 test font");
    LoadedFont {
      id: None,
      data: Arc::new(data),
      index: 0,
      family: "ColrV1Test".into(),
      weight: FontWeight::NORMAL,
      style: FontStyle::Normal,
      stretch: FontStretch::Normal,
    }
  }

  #[test]
  fn colr_v1_table_parsing_is_cached_per_font() {
    let _guard = ColrV1ParseCountGuard::start();
    let font = load_test_font();
    let renderer = ColorFontRenderer::new();
    let face = font.as_ttf_face().unwrap();
    let instance = FontInstance::new(&font, &[]).expect("font instance");
    let text_color = Rgba::from_rgba8(10, 20, 30, 255);

    let renderable_gid = (0..face.number_of_glyphs())
      .find(|gid| {
        renderer
          .render(
            &font,
            &instance,
            *gid as u32,
            64.0,
            0,
            &[],
            0,
            text_color,
            0.0,
            &[],
            None,
          )
          .is_some()
      })
      .expect("font should contain a COLRv1 glyph");

    renderer
      .render(
        &font,
        &instance,
        renderable_gid as u32,
        64.0,
        1,
        &[],
        0,
        text_color,
        0.0,
        &[],
        None,
      )
      .expect("palette render");
    renderer
      .render(
        &font,
        &instance,
        renderable_gid as u32,
        64.0,
        0,
        &[],
        0,
        text_color,
        0.0,
        &[],
        None,
      )
      .expect("cached render");

    let (font_ref_parses, colr_accesses) = colr_v1_parse_counts();
    assert_eq!(font_ref_parses, 1, "FontRef::from_index should be cached");
    assert_eq!(colr_accesses, 1, "COLR table lookup should be cached");
  }
}
