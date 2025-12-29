use super::limits::{log_glyph_limit, round_dimension, GlyphRasterLimits};
use super::{cpal, ColorFontCaches, ColorGlyphRaster, FontKey};
use crate::style::color::Rgba;
use crate::text::font_db::LoadedFont;
use read_fonts::tables::colr::{
  ClipBox, ColorLine, Colr, CompositeMode, Extend, Paint, PaintId, VarColorLine,
};
use read_fonts::types::{F2Dot14, Fixed, GlyphId};
use read_fonts::{FontRef, TableProvider};
use std::collections::HashSet;
use std::sync::{Arc, Mutex};
use tiny_skia::{
  BlendMode, Color, FillRule, GradientStop, Mask, Paint as SkiaPaint, Path, PathBuilder, Pixmap,
  PixmapPaint, Point, PremultipliedColorU8, Rect, SpreadMode, Transform,
};

const RASTER_PAD: f32 = 1.0;

/// Render a COLRv1 paint graph for the given glyph.
pub fn render_colr_glyph(
  font: &LoadedFont,
  face: &ttf_parser::Face<'_>,
  font_key: FontKey,
  glyph_id: ttf_parser::GlyphId,
  font_size: f32,
  palette_index: u16,
  overrides: &[(u16, Rgba)],
  text_color: Rgba,
  synthetic_oblique: f32,
  limits: &GlyphRasterLimits,
  caches: &Arc<Mutex<ColorFontCaches>>,
) -> Option<ColorGlyphRaster> {
  let font_ref = FontRef::from_index(font.data.as_slice(), font.index).ok()?;
  let colr = font_ref.colr().ok()?;
  if colr.version() != 1 {
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
  let mut palette_colors = palette.colors.clone();
  for (idx, color) in overrides {
    if let Some(slot) = palette_colors.get_mut(*idx as usize) {
      *slot = *color;
    }
  }

  let units_per_em = face.units_per_em() as f32;
  if units_per_em <= 0.0 || !units_per_em.is_finite() || !font_size.is_finite() {
    return None;
  }
  let scale = font_size / units_per_em;
  if !scale.is_finite() {
    return None;
  }
  let base_transform = Transform::from_row(scale, 0.0, synthetic_oblique * scale, -scale, 0.0, 0.0);

  let clip_path = colr
    .v1_clip_box(GlyphId::from(glyph_id.0 as u32))
    .ok()
    .flatten()
    .and_then(|clip| build_clip_path(clip, base_transform));

  let mut commands = Vec::new();
  let mut seen = HashSet::new();
  let renderer = Renderer {
    face,
    colr: &colr,
    palette: &palette_colors,
    text_color,
    base_transform,
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

  let (min_x, min_y, max_x, max_y) = raster_bounds(&commands, clip_path.as_ref(), RASTER_PAD)?;

  let width = round_dimension(max_x - min_x)?;
  let height = round_dimension(max_y - min_y)?;
  if let Err(err) = limits.validate(width, height) {
    log_glyph_limit("colr", glyph_id.0 as u32, &err);
    return None;
  }
  let mut pixmap = Pixmap::new(width, height)?;

  let clip_mask = clip_path
    .as_ref()
    .and_then(|clip| build_clip_mask(clip, width, height, min_x, min_y));
  let clip_ref = clip_mask.as_ref();
  let translate = Transform::from_translate(-min_x, -min_y);

  for command in commands {
    if matches!(command.brush, Brush::SweepGradient { .. }) {
      render_sweep_gradient_command(&mut pixmap, &command, min_x, min_y, clip_ref)?;
    } else {
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

struct Renderer<'a, 'b> {
  face: &'a ttf_parser::Face<'a>,
  colr: &'b Colr<'a>,
  palette: &'b [Rgba],
  text_color: Rgba,
  base_transform: Transform,
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
  SweepGradient {
    center: Point,
    start_angle: f32,
    end_angle: f32,
    stops: Vec<SweepGradientStop>,
    spread: SpreadMode,
    inv_transform: Option<Transform>,
  },
}

#[derive(Clone)]
struct SweepGradientStop {
  offset: f32,
  color: Rgba,
}

impl<'a, 'b> Renderer<'a, 'b> {
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
        let (spread, stops) = resolve_var_color_line(color_line, self.palette, self.text_color)?;
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
        let (spread, stops) = resolve_var_color_line(color_line, self.palette, self.text_color)?;
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
      Paint::SweepGradient(sweep) => {
        let color_line = sweep.color_line().ok()?;
        let (spread, stops) = resolve_color_line_stops(color_line, self.palette, self.text_color)?;
        let center = Point::from_xy(
          sweep.center_x().to_i16() as f32,
          sweep.center_y().to_i16() as f32,
        );
        Some(Brush::SweepGradient {
          center,
          start_angle: sweep.start_angle().to_f32(),
          end_angle: sweep.end_angle().to_f32(),
          stops,
          spread,
          inv_transform: combined.invert(),
        })
      }
      Paint::VarSweepGradient(sweep) => {
        let color_line = sweep.color_line().ok()?;
        let (spread, stops) =
          resolve_var_color_line_stops(color_line, self.palette, self.text_color)?;
        let center = Point::from_xy(
          sweep.center_x().to_i16() as f32,
          sweep.center_y().to_i16() as f32,
        );
        Some(Brush::SweepGradient {
          center,
          start_angle: sweep.start_angle().to_f32(),
          end_angle: sweep.end_angle().to_f32(),
          stops,
          spread,
          inv_transform: combined.invert(),
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
    Brush::SweepGradient { .. } => return None,
  }
  Some(paint)
}

fn render_sweep_gradient_command(
  dest: &mut Pixmap,
  command: &DrawCommand,
  glyph_left: f32,
  glyph_top: f32,
  clip_mask: Option<&Mask>,
) -> Option<()> {
  let Brush::SweepGradient {
    center,
    start_angle,
    end_angle,
    stops,
    spread,
    inv_transform,
    ..
  } = &command.brush
  else {
    return Some(());
  };

  let sweep = end_angle - start_angle;
  if !sweep.is_finite() || sweep.abs() < 1e-6 {
    return Some(());
  }

  let glyph_bounds = Rect::from_ltrb(
    glyph_left,
    glyph_top,
    glyph_left + dest.width() as f32,
    glyph_top + dest.height() as f32,
  )?;
  let path_bounds = command.path.bounds();
  let left = glyph_bounds.left().max(path_bounds.left());
  let top = glyph_bounds.top().max(path_bounds.top());
  let right = glyph_bounds.right().min(path_bounds.right());
  let bottom = glyph_bounds.bottom().min(path_bounds.bottom());
  if left >= right || top >= bottom {
    return Some(());
  }

  let crop_left = left.floor();
  let crop_top = top.floor();
  let crop_right = right.ceil();
  let crop_bottom = bottom.ceil();
  let crop_width = round_dimension(crop_right - crop_left)?;
  let crop_height = round_dimension(crop_bottom - crop_top)?;
  if crop_width == 0 || crop_height == 0 {
    return Some(());
  }

  let mut path_mask = Mask::new(crop_width, crop_height)?;
  let mask_translate = Transform::from_translate(-crop_left, -crop_top);
  path_mask.fill_path(&command.path, FillRule::Winding, true, mask_translate);

  if let Some(clip) = clip_mask {
    let origin_x = glyph_left as i32;
    let origin_y = glyph_top as i32;
    let clip_stride = clip.width() as usize;
    let mask_stride = path_mask.width() as usize;
    let clip_data = clip.data();
    let mask_data = path_mask.data_mut();
    for y in 0..crop_height as usize {
      let clip_row = (crop_top as i32 - origin_y) as usize + y;
      let clip_offset = clip_row * clip_stride + (crop_left as i32 - origin_x) as usize;
      let mask_offset = y * mask_stride;
      for x in 0..mask_stride {
        let clip_val = clip_data[clip_offset + x] as u16;
        let cov = mask_data[mask_offset + x] as u16;
        mask_data[mask_offset + x] = ((cov * clip_val + 127) / 255) as u8;
      }
    }
  }

  if !path_mask.data().iter().any(|v| *v != 0) {
    return Some(());
  }

  let mut gradient = Pixmap::new(crop_width, crop_height)?;
  let pixels = gradient.pixels_mut();
  let mask_data = path_mask.data();
  let transparent = PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
  let inv = *inv_transform;

  for y in 0..crop_height as usize {
    for x in 0..crop_width as usize {
      let idx = y * crop_width as usize + x;
      let coverage = mask_data[idx];
      if coverage == 0 {
        continue;
      }
      let mut point = Point::from_xy(crop_left + x as f32 + 0.5, crop_top + y as f32 + 0.5);
      if let Some(transform) = inv {
        transform.map_point(&mut point);
      }
      let mut angle = (point.y - center.y).atan2(point.x - center.x).to_degrees();
      angle = if sweep >= 0.0 {
        (angle - start_angle).rem_euclid(360.0)
      } else {
        -((start_angle - angle).rem_euclid(360.0))
      };
      let pos = apply_spread(angle / sweep, *spread);
      let color = sample_sweep_color(stops, pos);
      let alpha = (color.a * (coverage as f32 / 255.0)).clamp(0.0, 1.0);
      pixels[idx] = PremultipliedColorU8::from_rgba(
        color.r,
        color.g,
        color.b,
        (alpha * 255.0).round().clamp(0.0, 255.0) as u8,
      )
      .unwrap_or(transparent);
    }
  }

  let mut paint = PixmapPaint::default();
  paint.blend_mode = command.blend_mode;
  let dest_x = (crop_left - glyph_left) as i32;
  let dest_y = (crop_top - glyph_top) as i32;
  dest.draw_pixmap(
    dest_x,
    dest_y,
    gradient.as_ref(),
    &paint,
    Transform::identity(),
    None,
  );

  Some(())
}

fn apply_spread(mut t: f32, spread: SpreadMode) -> f32 {
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

fn sample_sweep_color(stops: &[SweepGradientStop], pos: f32) -> Rgba {
  if stops.is_empty() {
    return Rgba::TRANSPARENT;
  }
  if stops.len() == 1 {
    return stops[0].color;
  }
  if pos <= stops[0].offset {
    return stops[0].color;
  }
  if pos >= stops.last().unwrap().offset {
    return stops.last().unwrap().color;
  }
  for window in stops.windows(2) {
    let s0 = &window[0];
    let s1 = &window[1];
    if pos < s0.offset {
      return s0.color;
    }
    if pos <= s1.offset || (s1.offset - s0.offset).abs() < f32::EPSILON {
      let span = (s1.offset - s0.offset).max(1e-6);
      let frac = ((pos - s0.offset) / span).clamp(0.0, 1.0);
      return Rgba {
        r: ((1.0 - frac) * s0.color.r as f32 + frac * s1.color.r as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        g: ((1.0 - frac) * s0.color.g as f32 + frac * s1.color.g as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        b: ((1.0 - frac) * s0.color.b as f32 + frac * s1.color.b as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        a: (1.0 - frac) * s0.color.a + frac * s1.color.a,
      };
    }
  }
  stops.last().unwrap().color
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

fn resolve_color_line_stops(
  line: ColorLine<'_>,
  palette: &[Rgba],
  text_color: Rgba,
) -> Option<(SpreadMode, Vec<SweepGradientStop>)> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    let mut color = resolve_palette_color(stop.palette_index(), palette, text_color);
    color.a = (color.a * stop.alpha().to_f32()).clamp(0.0, 1.0);
    stops.push(SweepGradientStop {
      offset: stop.stop_offset().to_f32(),
      color,
    });
  }
  Some((spread, stops))
}

fn resolve_var_color_line_stops(
  line: VarColorLine<'_>,
  palette: &[Rgba],
  text_color: Rgba,
) -> Option<(SpreadMode, Vec<SweepGradientStop>)> {
  let spread = match map_spread(line.extend()) {
    Some(s) => s,
    None => return None,
  };
  let mut stops = Vec::with_capacity(line.num_stops() as usize);
  for stop in line.color_stops() {
    let mut color = resolve_palette_color(stop.palette_index(), palette, text_color);
    color.a = (color.a * stop.alpha().to_f32()).clamp(0.0, 1.0);
    stops.push(SweepGradientStop {
      offset: stop.stop_offset().to_f32(),
      color,
    });
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

fn build_clip_path(clip: ClipBox<'_>, base_transform: Transform) -> Option<Path> {
  let rect = match clip {
    ClipBox::Format1(b) => Rect::from_ltrb(
      b.x_min().to_i16() as f32,
      b.y_min().to_i16() as f32,
      b.x_max().to_i16() as f32,
      b.y_max().to_i16() as f32,
    )?,
    ClipBox::Format2(b) => Rect::from_ltrb(
      b.x_min().to_i16() as f32,
      b.y_min().to_i16() as f32,
      b.x_max().to_i16() as f32,
      b.y_max().to_i16() as f32,
    )?,
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

#[cfg(test)]
mod tests {
  use super::{raster_bounds, Brush, DrawCommand, RASTER_PAD};
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
}
