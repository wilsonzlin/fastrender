use super::{cpal, ColorFontCaches, ColorGlyphRaster, FontKey};
use crate::style::color::Rgba;
use crate::text::font_db::LoadedFont;
use crate::text::variations::normalized_coords;
use read_fonts::tables::colr::{
  ClipBox, ColorLine, Colr, CompositeMode, Extend, Paint, PaintId, VarColorLine,
};
use read_fonts::tables::variations::{
  DeltaSetIndex, DeltaSetIndexMap, FloatItemDelta, FloatItemDeltaTarget, ItemVariationStore,
  NO_VARIATION_INDEX,
};
use read_fonts::types::{F2Dot14, Fixed, GlyphId};
use read_fonts::{FontRef, TableProvider};
use rustybuzz::Variation;
use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use tiny_skia::{
  BlendMode, Color, FillRule, GradientStop, Mask, Paint as SkiaPaint, Path, PathBuilder, Pixmap,
  Point, Rect, SpreadMode, Transform,
};

/// Render a COLRv1 paint graph for the given glyph.
pub fn render_colr_glyph(
  font: &LoadedFont,
  face: &ttf_parser::Face<'_>,
  font_key: FontKey,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  palette_index: u16,
  text_color: Rgba,
  synthetic_oblique: f32,
  variations: &[Variation],
  caches: &Arc<Mutex<ColorFontCaches>>,
) -> Option<ColorGlyphRaster> {
  let font_ref = FontRef::from_index(font.data.as_slice(), font.index).ok()?;
  let colr = font_ref.colr().ok()?;
  if colr.version() < 1 {
    return None;
  }

  let (paint, paint_id) = colr
    .v1_base_glyph(GlyphId::from(glyph_id.0 as u32))
    .ok()??;

  let palette = {
    let mut caches = caches.lock().ok()?;
    caches
      .palette(font_key, face, palette_index)
      .unwrap_or_else(|| Arc::new(cpal::ParsedPalette::default()))
  };

  let scale = font_size / face.units_per_em() as f32;
  let base_transform = Transform::from_row(scale, 0.0, synthetic_oblique * scale, -scale, 0.0, 0.0);

  let normalized = normalized_coords(face, variations);
  let normalized_coords: Vec<_> = normalized
    .ordered
    .iter()
    .map(|v| F2Dot14::from_f32(*v))
    .collect();
  let var_index_map = colr.var_index_map().and_then(|map| map.ok());
  let variation_state = colr
    .item_variation_store()
    .and_then(|store| store.ok())
    .map(|store| VariationState::new(store, var_index_map, normalized_coords));

  let clip_path = colr
    .v1_clip_box(GlyphId::from(glyph_id.0 as u32))
    .ok()
    .flatten()
    .and_then(|clip| build_clip_path(clip, base_transform, variation_state.as_ref()));

  let mut commands = Vec::new();
  let mut seen = HashSet::new();
  let renderer = Renderer {
    face,
    colr: &colr,
    palette: &palette.colors,
    text_color,
    base_transform,
    variations: variation_state,
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

  let mut min_x = f32::MAX;
  let mut min_y = f32::MAX;
  let mut max_x = f32::MIN;
  let mut max_y = f32::MIN;

  for command in &commands {
    let bounds = command.path.bounds();
    min_x = min_x.min(bounds.left());
    min_y = min_y.min(bounds.top());
    max_x = max_x.max(bounds.right());
    max_y = max_y.max(bounds.bottom());
  }

  if let Some(clip) = &clip_path {
    let bounds = clip.bounds();
    min_x = min_x.min(bounds.left());
    min_y = min_y.min(bounds.top());
    max_x = max_x.max(bounds.right());
    max_y = max_y.max(bounds.bottom());
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

  let pad = 1.0;
  min_x = (min_x - pad).floor();
  min_y = (min_y - pad).floor();
  max_x = (max_x + pad).ceil();
  max_y = (max_y + pad).ceil();

  let width = (max_x - min_x).max(1.0).round() as u32;
  let height = (max_y - min_y).max(1.0).round() as u32;
  let mut pixmap = Pixmap::new(width, height)?;

  let clip_mask = clip_path
    .as_ref()
    .and_then(|clip| build_clip_mask(clip, width, height, min_x, min_y));
  let clip_ref = clip_mask.as_ref();
  let translate = Transform::from_translate(-min_x, -min_y);

  for command in commands {
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

  Some(ColorGlyphRaster {
    image: Arc::new(pixmap),
    left: min_x,
    top: min_y,
  })
}

#[derive(Clone)]
struct VariationState<'a> {
  coords: Vec<F2Dot14>,
  map: Option<DeltaSetIndexMap<'a>>,
  store: ItemVariationStore<'a>,
}

impl<'a> VariationState<'a> {
  fn new(
    store: ItemVariationStore<'a>,
    map: Option<DeltaSetIndexMap<'a>>,
    coords: Vec<F2Dot14>,
  ) -> Self {
    Self { coords, map, store }
  }

  fn delta_index(&self, base_index: u32, offset: u16) -> Option<DeltaSetIndex> {
    let index = base_index.checked_add(offset as u32)?;
    if index == NO_VARIATION_INDEX {
      return None;
    }
    if let Some(map) = &self.map {
      map.get(index).ok()
    } else {
      Some(DeltaSetIndex {
        outer: (index >> 16) as u16,
        inner: index as u16,
      })
    }
  }

  fn apply<T: FloatItemDeltaTarget>(&self, base: T, base_index: u32, offset: u16) -> f32 {
    let base_value = base.apply_float_delta(FloatItemDelta::ZERO);
    let Some(index) = self.delta_index(base_index, offset) else {
      return base_value;
    };
    match self.store.compute_float_delta(index, &self.coords) {
      Ok(delta) => base.apply_float_delta(delta),
      Err(_) => base_value,
    }
  }
}

struct Renderer<'a, 'b> {
  face: &'a ttf_parser::Face<'a>,
  colr: &'b Colr<'a>,
  palette: &'b [Rgba],
  text_color: Rgba,
  base_transform: Transform,
  variations: Option<VariationState<'a>>,
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
    start: Point,
    end: Point,
    stops: Vec<GradientStop>,
    spread: SpreadMode,
  },
  RadialGradient {
    start: Point,
    end: Point,
    radius: f32,
    stops: Vec<GradientStop>,
    spread: SpreadMode,
  },
}

impl<'a, 'b> Renderer<'a, 'b> {
  fn apply_variation<T: FloatItemDeltaTarget>(&self, base: T, base_index: u32, offset: u16) -> f32 {
    if let Some(variations) = &self.variations {
      variations.apply(base, base_index, offset)
    } else {
      base.apply_float_delta(FloatItemDelta::ZERO)
    }
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

    let result = match paint {
      Paint::ColrLayers(layer) => {
        let start = layer.first_layer_index() as usize;
        let count = layer.num_layers() as usize;
        for idx in start..start + count {
          let (layer_paint, id) = self.colr.v1_layer(idx).ok()?;
          self.walk_paint(layer_paint, id, design_transform, blend, commands, stack)?;
        }
        Some(())
      }
      Paint::Glyph(paint_glyph) => {
        let child = paint_glyph.paint().ok()?;
        let brush = self.resolve_brush(child, self.base_transform.pre_concat(design_transform))?;
        let path = build_outline(
          self.face,
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
        let (child_paint, id) = self.colr.v1_base_glyph(glyph).ok()??;
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
          self.apply_variation(transform.xx(), transform.var_index_base(), 0),
          self.apply_variation(transform.yx(), transform.var_index_base(), 1),
          self.apply_variation(transform.xy(), transform.var_index_base(), 2),
          self.apply_variation(transform.yy(), transform.var_index_base(), 3),
          self.apply_variation(transform.dx(), transform.var_index_base(), 4),
          self.apply_variation(transform.dy(), transform.var_index_base(), 5),
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
          self.apply_variation(paint_translate.dx(), paint_translate.var_index_base(), 0),
          self.apply_variation(paint_translate.dy(), paint_translate.var_index_base(), 1),
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
        let base = scale.var_index_base();
        let m = Transform::from_scale(
          self.apply_variation(scale.scale_x(), base, 0),
          self.apply_variation(scale.scale_y(), base, 1),
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
        let base = scale.var_index_base();
        let m = scale_around_center(
          self.apply_variation(scale.scale_x(), base, 0),
          self.apply_variation(scale.scale_y(), base, 1),
          self.apply_variation(scale.center_x(), base, 2),
          self.apply_variation(scale.center_y(), base, 3),
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
        let s = self.apply_variation(scale.scale(), scale.var_index_base(), 0);
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
        let base = scale.var_index_base();
        let s = self.apply_variation(scale.scale(), base, 0);
        let m = scale_around_center(
          s,
          s,
          self.apply_variation(scale.center_x(), base, 1),
          self.apply_variation(scale.center_y(), base, 2),
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
        let angle = self.apply_variation(rotate.angle(), rotate.var_index_base(), 0);
        let m = Transform::from_rotate(rotate_degrees(F2Dot14::from_f32(angle)));
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
        let base = rotate.var_index_base();
        let m = rotate_around_center(
          rotate_degrees(F2Dot14::from_f32(self.apply_variation(
            rotate.angle(),
            base,
            0,
          ))),
          self.apply_variation(rotate.center_x(), base, 1),
          self.apply_variation(rotate.center_y(), base, 2),
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
        let base = skew.var_index_base();
        let m = skew_transform(
          F2Dot14::from_f32(self.apply_variation(skew.x_skew_angle(), base, 0)),
          F2Dot14::from_f32(self.apply_variation(skew.y_skew_angle(), base, 1)),
          None,
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
        let base = skew.var_index_base();
        let m = skew_transform(
          F2Dot14::from_f32(self.apply_variation(skew.x_skew_angle(), base, 0)),
          F2Dot14::from_f32(self.apply_variation(skew.y_skew_angle(), base, 1)),
          Some((
            self.apply_variation(skew.center_x(), base, 2),
            self.apply_variation(skew.center_y(), base, 3),
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
        F2Dot14::from_f32(self.apply_variation(solid.alpha(), solid.var_index_base(), 0)),
      ))),
      Paint::LinearGradient(gradient) => {
        let color_line = gradient.color_line().ok()?;
        let (spread, stops) = resolve_color_line(color_line, self.palette, self.text_color)?;
        let start = map_point(
          gradient.x0().to_i16() as f32,
          gradient.y0().to_i16() as f32,
          combined,
        );
        let end = map_point(
          gradient.x1().to_i16() as f32,
          gradient.y1().to_i16() as f32,
          combined,
        );
        Some(Brush::LinearGradient {
          start,
          end,
          stops,
          spread,
        })
      }
      Paint::VarLinearGradient(gradient) => {
        let color_line = gradient.color_line().ok()?;
        let (spread, stops) = resolve_var_color_line(
          color_line,
          self.palette,
          self.text_color,
          self.variations.as_ref(),
        )?;
        let base = gradient.var_index_base();
        let start = map_point(
          self.apply_variation(gradient.x0(), base, 0),
          self.apply_variation(gradient.y0(), base, 1),
          combined,
        );
        let end = map_point(
          self.apply_variation(gradient.x1(), base, 2),
          self.apply_variation(gradient.y1(), base, 3),
          combined,
        );
        Some(Brush::LinearGradient {
          start,
          end,
          stops,
          spread,
        })
      }
      Paint::RadialGradient(radial) => {
        let color_line = radial.color_line().ok()?;
        let (spread, stops) = resolve_color_line(color_line, self.palette, self.text_color)?;
        let c0 = map_point(
          radial.x0().to_i16() as f32,
          radial.y0().to_i16() as f32,
          combined,
        );
        let c1 = map_point(
          radial.x1().to_i16() as f32,
          radial.y1().to_i16() as f32,
          combined,
        );
        let r0 = fixed_to_f32(radial.radius0().to_fixed());
        let r1 = fixed_to_f32(radial.radius1().to_fixed());
        Some(Brush::RadialGradient {
          start: c0,
          end: c1,
          radius: (r1 - r0).max(0.0) * combined.sx.hypot(combined.ky),
          stops,
          spread,
        })
      }
      Paint::VarRadialGradient(radial) => {
        let color_line = radial.color_line().ok()?;
        let (spread, stops) = resolve_var_color_line(
          color_line,
          self.palette,
          self.text_color,
          self.variations.as_ref(),
        )?;
        let base = radial.var_index_base();
        let c0 = map_point(
          self.apply_variation(radial.x0(), base, 0),
          self.apply_variation(radial.y0(), base, 1),
          combined,
        );
        let c1 = map_point(
          self.apply_variation(radial.x1(), base, 3),
          self.apply_variation(radial.y1(), base, 4),
          combined,
        );
        let r0 = self.apply_variation(radial.radius0(), base, 2);
        let r1 = self.apply_variation(radial.radius1(), base, 5);
        Some(Brush::RadialGradient {
          start: c0,
          end: c1,
          radius: (r1 - r0).max(0.0) * combined.sx.hypot(combined.ky),
          stops,
          spread,
        })
      }
      Paint::SweepGradient(_) | Paint::VarSweepGradient(_) => None,
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
          self.apply_variation(m.xx(), m.var_index_base(), 0),
          self.apply_variation(m.yx(), m.var_index_base(), 1),
          self.apply_variation(m.xy(), m.var_index_base(), 2),
          self.apply_variation(m.yy(), m.var_index_base(), 3),
          self.apply_variation(m.dx(), m.var_index_base(), 4),
          self.apply_variation(m.dy(), m.var_index_base(), 5),
        );
        self.resolve_brush(transform.paint().ok()?, combined.pre_concat(matrix))
      }
      Paint::Translate(t) => {
        let m = Transform::from_translate(t.dx().to_i16() as f32, t.dy().to_i16() as f32);
        self.resolve_brush(t.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarTranslate(t) => {
        let base = t.var_index_base();
        let m = Transform::from_translate(
          self.apply_variation(t.dx(), base, 0),
          self.apply_variation(t.dy(), base, 1),
        );
        self.resolve_brush(t.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Scale(scale) => {
        let m = Transform::from_scale(scale.scale_x().to_f32(), scale.scale_y().to_f32());
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarScale(scale) => {
        let base = scale.var_index_base();
        let m = Transform::from_scale(
          self.apply_variation(scale.scale_x(), base, 0),
          self.apply_variation(scale.scale_y(), base, 1),
        );
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
        let base = scale.var_index_base();
        let m = scale_around_center(
          self.apply_variation(scale.scale_x(), base, 0),
          self.apply_variation(scale.scale_y(), base, 1),
          self.apply_variation(scale.center_x(), base, 2),
          self.apply_variation(scale.center_y(), base, 3),
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
        let s = self.apply_variation(scale.scale(), scale.var_index_base(), 0);
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
        let base = scale.var_index_base();
        let s = self.apply_variation(scale.scale(), base, 0);
        let m = scale_around_center(
          s,
          s,
          self.apply_variation(scale.center_x(), base, 1),
          self.apply_variation(scale.center_y(), base, 2),
        );
        self.resolve_brush(scale.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Rotate(rotate) => {
        let m = Transform::from_rotate(rotate_degrees(rotate.angle()));
        self.resolve_brush(rotate.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarRotate(rotate) => {
        let m = Transform::from_rotate(rotate_degrees(F2Dot14::from_f32(self.apply_variation(
          rotate.angle(),
          rotate.var_index_base(),
          0,
        ))));
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
        let base = rotate.var_index_base();
        let m = rotate_around_center(
          rotate_degrees(F2Dot14::from_f32(self.apply_variation(
            rotate.angle(),
            base,
            0,
          ))),
          self.apply_variation(rotate.center_x(), base, 1),
          self.apply_variation(rotate.center_y(), base, 2),
        );
        self.resolve_brush(rotate.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Skew(skew) => {
        let m = skew_transform(skew.x_skew_angle(), skew.y_skew_angle(), None);
        self.resolve_brush(skew.paint().ok()?, combined.pre_concat(m))
      }
      Paint::VarSkew(skew) => {
        let base = skew.var_index_base();
        let m = skew_transform(
          F2Dot14::from_f32(self.apply_variation(skew.x_skew_angle(), base, 0)),
          F2Dot14::from_f32(self.apply_variation(skew.y_skew_angle(), base, 1)),
          None,
        );
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
        let base = skew.var_index_base();
        let m = skew_transform(
          F2Dot14::from_f32(self.apply_variation(skew.x_skew_angle(), base, 0)),
          F2Dot14::from_f32(self.apply_variation(skew.y_skew_angle(), base, 1)),
          Some((
            self.apply_variation(skew.center_x(), base, 2),
            self.apply_variation(skew.center_y(), base, 3),
          )),
        );
        self.resolve_brush(skew.paint().ok()?, combined.pre_concat(m))
      }
      Paint::Composite(composite) => self.resolve_brush(composite.source_paint().ok()?, combined),
      Paint::ColrGlyph(colr) => {
        let glyph = GlyphId::from(colr.glyph_id().to_u16() as u32);
        let (paint, _) = self.colr.v1_base_glyph(glyph).ok()??;
        self.resolve_brush(paint, combined)
      }
      Paint::ColrLayers(_) => None,
    }
  }
}

fn brush_to_paint(brush: &Brush, offset_x: f32, offset_y: f32) -> Option<SkiaPaint<'static>> {
  let mut paint = SkiaPaint::default();
  match brush {
    Brush::Solid(color) => {
      paint.shader = tiny_skia::Shader::SolidColor(*color);
    }
    Brush::LinearGradient {
      start,
      end,
      stops,
      spread,
    } => {
      let shader = tiny_skia::LinearGradient::new(
        Point::from_xy(start.x - offset_x, start.y - offset_y),
        Point::from_xy(end.x - offset_x, end.y - offset_y),
        stops.clone(),
        *spread,
        Transform::identity(),
      )?;
      paint.shader = shader;
    }
    Brush::RadialGradient {
      start,
      end,
      radius,
      stops,
      spread,
    } => {
      let shader = tiny_skia::RadialGradient::new(
        Point::from_xy(start.x - offset_x, start.y - offset_y),
        Point::from_xy(end.x - offset_x, end.y - offset_y),
        *radius,
        stops.clone(),
        *spread,
        Transform::identity(),
      )?;
      paint.shader = shader;
    }
  }
  Some(paint)
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
) -> Option<(SpreadMode, Vec<GradientStop>)> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    stops.push(GradientStop::new(
      stop.stop_offset().to_f32(),
      resolve_color(
        resolve_palette_color(stop.palette_index(), palette, text_color),
        stop.alpha(),
      ),
    ));
  }
  Some((spread, stops))
}

fn resolve_var_color_line(
  line: VarColorLine<'_>,
  palette: &[Rgba],
  text_color: Rgba,
  variations: Option<&VariationState<'_>>,
) -> Option<(SpreadMode, Vec<GradientStop>)> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    let base = stop.var_index_base();
    stops.push(GradientStop::new(
      variations
        .map(|v| v.apply(stop.stop_offset(), base, 0))
        .unwrap_or_else(|| stop.stop_offset().to_f32()),
      resolve_color(
        resolve_palette_color(stop.palette_index(), palette, text_color),
        F2Dot14::from_f32(
          variations
            .map(|v| v.apply(stop.alpha(), base, 1))
            .unwrap_or_else(|| stop.alpha().to_f32()),
        ),
      ),
    ));
  }
  Some((spread, stops))
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
  face: &ttf_parser::Face<'_>,
  glyph_id: u16,
  design_transform: Transform,
  base: Transform,
) -> Option<Path> {
  let mut builder = GlyphOutlineBuilder::with_flip(1.0, 0.0, 0.0, 0.0, false);
  face.outline_glyph(ttf_parser::GlyphId(glyph_id), &mut builder)?;
  let path = builder.finish()?;
  let transform = base.pre_concat(design_transform);
  path.transform(transform)
}

fn build_clip_path(
  clip: ClipBox<'_>,
  base_transform: Transform,
  variations: Option<&VariationState<'_>>,
) -> Option<Path> {
  let rect = match clip {
    ClipBox::Format1(b) => Rect::from_ltrb(
      b.x_min().to_i16() as f32,
      b.y_min().to_i16() as f32,
      b.x_max().to_i16() as f32,
      b.y_max().to_i16() as f32,
    )?,
    ClipBox::Format2(b) => {
      let base = b.var_index_base();
      Rect::from_ltrb(
        variations
          .map(|v| v.apply(b.x_min(), base, 0))
          .unwrap_or_else(|| b.x_min().to_i16() as f32),
        variations
          .map(|v| v.apply(b.y_min(), base, 1))
          .unwrap_or_else(|| b.y_min().to_i16() as f32),
        variations
          .map(|v| v.apply(b.x_max(), base, 2))
          .unwrap_or_else(|| b.x_max().to_i16() as f32),
        variations
          .map(|v| v.apply(b.y_max(), base, 3))
          .unwrap_or_else(|| b.y_max().to_i16() as f32),
      )?
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

/// Outline builder reused for COLR layers.
struct GlyphOutlineBuilder {
  builder: PathBuilder,
  scale: f32,
  skew: f32,
  x_offset: f32,
  y_offset: f32,
  flip_y: bool,
}

impl GlyphOutlineBuilder {
  fn new(scale: f32, skew: f32, x_offset: f32, y_offset: f32) -> Self {
    Self::with_flip(scale, skew, x_offset, y_offset, true)
  }

  fn with_flip(scale: f32, skew: f32, x_offset: f32, y_offset: f32, flip_y: bool) -> Self {
    Self {
      builder: PathBuilder::new(),
      scale,
      skew,
      x_offset,
      y_offset,
      flip_y,
    }
  }

  #[inline]
  fn transform_x(&self, x: f32, y: f32) -> f32 {
    (x + self.skew * y) * self.scale + self.x_offset
  }

  #[inline]
  fn transform_y(&self, y: f32) -> f32 {
    if self.flip_y {
      self.y_offset - (y * self.scale)
    } else {
      self.y_offset + (y * self.scale)
    }
  }

  fn finish(self) -> Option<Path> {
    self.builder.finish()
  }
}

impl ttf_parser::OutlineBuilder for GlyphOutlineBuilder {
  fn move_to(&mut self, x: f32, y: f32) {
    self
      .builder
      .move_to(self.transform_x(x, y), self.transform_y(y));
  }

  fn line_to(&mut self, x: f32, y: f32) {
    self
      .builder
      .line_to(self.transform_x(x, y), self.transform_y(y));
  }

  fn quad_to(&mut self, x1: f32, y1: f32, x: f32, y: f32) {
    self.builder.quad_to(
      self.transform_x(x1, y1),
      self.transform_y(y1),
      self.transform_x(x, y),
      self.transform_y(y),
    );
  }

  fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, x: f32, y: f32) {
    self.builder.cubic_to(
      self.transform_x(x1, y1),
      self.transform_y(y1),
      self.transform_x(x2, y2),
      self.transform_y(y2),
      self.transform_x(x, y),
      self.transform_y(y),
    );
  }

  fn close(&mut self) {
    self.builder.close();
  }
}
