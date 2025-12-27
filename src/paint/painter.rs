//! Main painter - converts FragmentTree to pixels
//!
//! This module implements the core painting algorithm that transforms
//! the positioned fragment tree into rasterized pixels.
//!
//! # CSS Painting Order
//!
//! Follows CSS 2.1 Appendix E painting order:
//! 1. Background colors and images
//! 2. Borders
//! 3. Child stacking contexts (negative z-index)
//! 4. In-flow non-positioned blocks
//! 5. Floats
//! 6. In-flow inline content
//! 7. Child stacking contexts (z-index: 0 and auto)
//! 8. Positioned descendants (positive z-index)
//!
//! # Architecture
//!
//! The painter walks the fragment tree depth-first, painting each
//! fragment's background, borders, and content. Text is rendered
//! using the system's default font.

use crate::api::render_html_with_shared_resources;
use crate::api::ResourceKind;
use crate::css;
use crate::css::types::ColorStop;
use crate::css::types::RadialGradientShape;
use crate::css::types::RadialGradientSize;
use crate::debug::runtime;
use crate::debug::trace::TraceHandle;
use crate::error::Error;
use crate::error::RenderError;
use crate::error::RenderStage;
use crate::error::Result;
use crate::geometry::Point;
use crate::geometry::Rect;
use crate::geometry::Size;
use crate::html::encoding::decode_html_bytes;
use crate::image_loader::CachedImage;
use crate::image_loader::ImageCache;
use crate::layout::contexts::inline::baseline::compute_line_height_with_metrics_viewport;
use crate::layout::contexts::inline::line_builder::TextItem;
use crate::layout::utils::resolve_font_relative_length;
use crate::paint::blur::apply_gaussian_blur;
use crate::paint::clip_path::resolve_clip_path;
use crate::paint::clip_path::ResolvedClipPath;
use crate::paint::display_list::BorderRadii;
use crate::paint::display_list::Transform2D;
use crate::paint::display_list::Transform3D;
use crate::paint::display_list_builder::DisplayListBuilder;
use crate::paint::display_list_renderer::DisplayListRenderer;
use crate::paint::filter_outset::{compute_filter_outset, FilterOutsetExt};
use crate::paint::homography::{quad_bounds, rect_corners, Homography};
use crate::paint::object_fit::compute_object_fit;
use crate::paint::object_fit::default_object_position;
use crate::paint::optimize::DisplayListOptimizer;
use crate::paint::projective_warp::warp_pixmap;
use crate::paint::rasterize::fill_rounded_rect;
use crate::paint::stacking::creates_stacking_context;
use crate::paint::svg_filter::SvgFilterResolver;
use crate::paint::text_shadow::resolve_text_shadows;
use crate::paint::text_shadow::PathBounds;
use crate::paint::text_shadow::ResolvedTextShadow;
use crate::paint::transform_resolver::{backface_is_hidden, resolve_transform3d};
use crate::render_control::check_active;
use crate::resource::origin_from_url;
use crate::resource::ResourceFetcher;
#[cfg(test)]
use crate::style::color::Color;
use crate::style::color::Rgba;
use crate::style::display::Display;
use crate::style::position::Position;
use crate::style::types::AccentColor;
use crate::style::types::Appearance;
use crate::style::types::BackfaceVisibility;
use crate::style::types::BackgroundAttachment;
use crate::style::types::BackgroundImage;
use crate::style::types::BackgroundLayer;
use crate::style::types::BackgroundPosition;
use crate::style::types::BackgroundRepeatKeyword;
use crate::style::types::BackgroundSize;
use crate::style::types::BackgroundSizeComponent;
use crate::style::types::BackgroundSizeKeyword;
use crate::style::types::BorderImage;
use crate::style::types::BorderImageOutsetValue;
use crate::style::types::BorderImageRepeat;
use crate::style::types::BorderImageSource;
use crate::style::types::BorderImageWidthValue;
use crate::style::types::BorderStyle as CssBorderStyle;
use crate::style::types::ClipComponent;
use crate::style::types::Direction;
use crate::style::types::FilterColor;
use crate::style::types::FilterFunction;
use crate::style::types::FontStyle as CssFontStyle;
use crate::style::types::ImageOrientation;
use crate::style::types::ImageRendering;
use crate::style::types::MaskClip;
use crate::style::types::MaskComposite;
use crate::style::types::MaskMode;
use crate::style::types::MaskOrigin;
use crate::style::types::MixBlendMode;
use crate::style::types::ObjectFit;
use crate::style::types::OrientationTransform;
use crate::style::types::Overflow;
use crate::style::types::TextDecorationLine;
use crate::style::types::TextDecorationSkipInk;
use crate::style::types::TextDecorationStyle;
use crate::style::types::TextDecorationThickness;
use crate::style::types::TransformBox;
use crate::style::types::WritingMode;
use crate::style::values::Length;
use crate::style::values::LengthUnit;
use crate::style::ComputedStyle;
use crate::text::font_db::FontStretch;
use crate::text::font_db::FontStyle;
use crate::text::font_db::ScaledMetrics;
use crate::text::font_loader::FontContext;
use crate::text::pipeline::ShapedRun;
use crate::text::pipeline::ShapingPipeline;
use crate::tree;
use crate::tree::box_tree::ForeignObjectInfo;
use crate::tree::box_tree::ReplacedBox;
use crate::tree::box_tree::ReplacedType;
use crate::tree::box_tree::SvgContent;
use crate::tree::box_tree::{FormControl, FormControlKind};
use crate::tree::fragment_tree::FragmentContent;
use crate::tree::fragment_tree::FragmentNode;
use crate::tree::fragment_tree::FragmentTree;
use base64::Engine;
use encoding_rs::Encoding;
use image::codecs::png::PngEncoder;
use image::ColorType;
use image::ImageEncoder;
use percent_encoding::percent_decode;
use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Write as _;
use std::io::Read;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use std::time::Instant;
use tiny_skia::BlendMode as SkiaBlendMode;
use tiny_skia::FilterQuality;
use tiny_skia::IntSize;
use tiny_skia::LinearGradient;
use tiny_skia::Mask;
use tiny_skia::MaskType;
use tiny_skia::Paint;
use tiny_skia::PathBuilder;
use tiny_skia::Pattern;
use tiny_skia::Pixmap;
use tiny_skia::PixmapPaint;
use tiny_skia::PremultipliedColorU8;
use tiny_skia::RadialGradient;
use tiny_skia::Rect as SkiaRect;
use tiny_skia::SpreadMode;
use tiny_skia::Stroke;
use tiny_skia::Transform;
use url::Url;

/// Main painter that rasterizes a FragmentTree to pixels
pub struct Painter {
  /// The pixmap being painted to
  pixmap: Pixmap,
  /// CSS-to-device scale factor (device pixel ratio)
  scale: f32,
  /// Logical viewport width in CSS px
  css_width: f32,
  /// Logical viewport height in CSS px
  css_height: f32,
  /// Background color
  background: Rgba,
  /// Text shaping pipeline
  shaper: ShapingPipeline,
  /// Font context for resolution
  font_ctx: FontContext,
  /// Image cache for replaced content
  image_cache: ImageCache,
  /// Cache of shaped runs keyed by style and text to avoid reshaping identical content during paint
  text_shape_cache: Arc<Mutex<HashMap<TextCacheKey, Vec<ShapedRun>>>>,
  /// Optional trace collector for Chrome trace output.
  trace: TraceHandle,
}

#[derive(Default)]
struct PaintStats {
  background_ms: f64,
  collect_ms: f64,
  execute_ms: f64,
  commands: usize,
  backgrounds: (usize, f64),
  borders: (usize, f64),
  text: (usize, f64),
  replaced: (usize, f64),
  outline: (usize, f64),
  stacking: (usize, f64),
}

#[derive(Debug, Default, Clone)]
pub struct PaintDiagnosticsSummary {
  pub command_count: usize,
  pub build_ms: f64,
  pub raster_ms: f64,
}

thread_local! {
  static PAINT_DIAGNOSTICS: RefCell<Option<PaintDiagnosticsSummary>> = RefCell::new(None);
}

pub(crate) fn enable_paint_diagnostics() {
  PAINT_DIAGNOSTICS.with(|cell| {
    *cell.borrow_mut() = Some(PaintDiagnosticsSummary::default());
  });
}

pub(crate) fn take_paint_diagnostics() -> Option<PaintDiagnosticsSummary> {
  PAINT_DIAGNOSTICS.with(|cell| cell.borrow_mut().take())
}

fn paint_diagnostics_enabled() -> bool {
  PAINT_DIAGNOSTICS.with(|cell| cell.borrow().is_some())
}

fn with_paint_diagnostics<F: FnOnce(&mut PaintDiagnosticsSummary)>(f: F) {
  PAINT_DIAGNOSTICS.with(|cell| {
    if let Some(stats) = cell.borrow_mut().as_mut() {
      f(stats);
    }
  });
}

#[derive(Copy, Clone)]
struct RootPaintOptions {
  use_root_background: bool,
  extend_background_to_viewport: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TextCacheKey {
  style_ptr: usize,
  font_size_bits: u32,
  text: String,
}

fn dump_stack_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_DUMP_STACK")
}

fn dump_fragments_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_DUMP_FRAGMENTS")
}

fn trace_image_paint_limit() -> Option<usize> {
  runtime::runtime_toggles().usize("FASTR_TRACE_IMAGE_PAINT")
}

static TRACE_IMAGE_PAINT_COUNT: AtomicUsize = AtomicUsize::new(0);

fn dump_counts_enabled() -> bool {
  runtime::runtime_toggles().truthy("FASTR_DUMP_COUNTS")
}

fn stack_profile_threshold_ms() -> Option<f64> {
  runtime::runtime_toggles().f64("FASTR_STACK_PROFILE_MS")
}

fn text_profile_threshold_ms() -> Option<f64> {
  runtime::runtime_toggles().f64("FASTR_TEXT_PROFILE_MS")
}

fn cmd_profile_threshold_ms() -> Option<f64> {
  runtime::runtime_toggles().f64("FASTR_CMD_PROFILE_MS")
}

fn nested_counts(cmds: &[DisplayCommand]) -> (usize, usize) {
  cmds.iter().fold((0, 0), |(total, text), cmd| match cmd {
    DisplayCommand::StackingContext { commands, .. } => {
      let (t, tx) = nested_counts(commands);
      (total + 1 + t, text + tx)
    }
    DisplayCommand::Text { .. } => (total + 1, text + 1),
    _ => (total + 1, text),
  })
}

impl PaintStats {
  fn record(&mut self, cmd: &DisplayCommand, duration: std::time::Duration) {
    let ms = duration.as_secs_f64() * 1000.0;
    self.commands += 1;
    match cmd {
      DisplayCommand::Background { .. } => {
        self.backgrounds.0 += 1;
        self.backgrounds.1 += ms;
      }
      DisplayCommand::Border { .. } => {
        self.borders.0 += 1;
        self.borders.1 += ms;
      }
      DisplayCommand::Text { .. } => {
        self.text.0 += 1;
        self.text.1 += ms;
      }
      DisplayCommand::Replaced { .. } => {
        self.replaced.0 += 1;
        self.replaced.1 += ms;
      }
      DisplayCommand::Outline { .. } => {
        self.outline.0 += 1;
        self.outline.1 += ms;
      }
      DisplayCommand::StackingContext { .. } => {
        self.stacking.0 += 1;
        self.stacking.1 += ms;
      }
    }
    self.execute_ms += ms;
  }

  fn log(&self) {
    eprintln!(
      "paint_stats commands={} background_ms_ms={:.2} collect_ms_ms={:.2} execute_ms_ms={:.2}",
      self.commands, self.background_ms, self.collect_ms, self.execute_ms
    );
    eprintln!(
            " paint_breakdown backgrounds count={} time_ms={:.2} borders count={} time_ms={:.2} text count={} time_ms={:.2} replaced count={} time_ms={:.2} outline count={} time_ms={:.2} stacking count={} time_ms={:.2}",
            self.backgrounds.0,
            self.backgrounds.1,
            self.borders.0,
            self.borders.1,
            self.text.0,
            self.text.1,
            self.replaced.0,
            self.replaced.1,
            self.outline.0,
            self.outline.1,
            self.stacking.0,
            self.stacking.1,
        );
  }
}

#[derive(Debug, Clone)]
enum ResolvedFilter {
  Blur(f32),
  Brightness(f32),
  Contrast(f32),
  Grayscale(f32),
  Sepia(f32),
  Saturate(f32),
  HueRotate(f32),
  Invert(f32),
  Opacity(f32),
  DropShadow {
    offset_x: f32,
    offset_y: f32,
    blur_radius: f32,
    spread: f32,
    color: Rgba,
  },
  SvgFilter(Arc<crate::paint::svg_filter::SvgFilter>),
}

impl FilterOutsetExt for ResolvedFilter {
  fn expand_outset(&self, bbox: Rect, scale: f32, out: &mut (f32, f32, f32, f32)) {
    match self {
      ResolvedFilter::Blur(radius) => {
        let delta = (radius * scale).abs() * 3.0;
        out.0 += delta;
        out.1 += delta;
        out.2 += delta;
        out.3 += delta;
      }
      ResolvedFilter::DropShadow {
        offset_x,
        offset_y,
        blur_radius,
        spread,
        ..
      } => {
        let dx = offset_x * scale;
        let dy = offset_y * scale;
        let blur = blur_radius * scale;
        let spread = spread * scale;
        let delta = (blur.abs() * 3.0 + spread).max(0.0);
        let shadow_left = out.0 + delta - dx;
        let shadow_right = out.2 + delta + dx;
        let shadow_top = out.1 + delta - dy;
        let shadow_bottom = out.3 + delta + dy;
        out.0 = out.0.max(shadow_left);
        out.2 = out.2.max(shadow_right);
        out.1 = out.1.max(shadow_top);
        out.3 = out.3.max(shadow_bottom);
      }
      ResolvedFilter::SvgFilter(filter) => {
        let region = filter.resolve_region(bbox);
        let delta_left = (bbox.min_x() - region.min_x()).max(0.0) * scale;
        let delta_top = (bbox.min_y() - region.min_y()).max(0.0) * scale;
        let delta_right = (region.max_x() - bbox.max_x()).max(0.0) * scale;
        let delta_bottom = (region.max_y() - bbox.max_y()).max(0.0) * scale;
        out.0 = out.0.max(delta_left);
        out.1 = out.1.max(delta_top);
        out.2 = out.2.max(delta_right);
        out.3 = out.3.max(delta_bottom);
      }
      _ => {}
    }
  }
}

#[derive(Debug, Clone)]
enum DisplayCommand {
  Background {
    rect: Rect,
    style: Arc<ComputedStyle>,
  },
  Border {
    rect: Rect,
    style: Arc<ComputedStyle>,
  },
  Text {
    rect: Rect,
    baseline_offset: f32,
    text: String,
    runs: Option<Vec<ShapedRun>>,
    style: Arc<ComputedStyle>,
  },
  Replaced {
    rect: Rect,
    replaced_type: ReplacedType,
    style: Arc<ComputedStyle>,
  },
  Outline {
    rect: Rect,
    style: Arc<ComputedStyle>,
  },
  StackingContext {
    rect: Rect,
    opacity: f32,
    transform: Option<Transform>,
    transform_3d: Option<Transform3D>,
    blend_mode: MixBlendMode,
    isolated: bool,
    mask: Option<Arc<ComputedStyle>>,
    filters: Vec<ResolvedFilter>,
    backdrop_filters: Vec<ResolvedFilter>,
    radii: BorderRadii,
    clip: Option<StackingClip>,
    clip_path: Option<ResolvedClipPath>,
    commands: Vec<DisplayCommand>,
  },
}

#[derive(Debug, Clone, Copy)]
struct StackingClip {
  rect: Rect,
  radii: BorderRadii,
  clip_x: bool,
  clip_y: bool,
  /// Whether this clip should apply to the stacking-context root itself (e.g. CSS `clip`).
  /// Overflow clipping applies only to descendants.
  clip_root: bool,
}

fn is_positioned(style: &ComputedStyle) -> bool {
  !matches!(style.position, Position::Static)
}

fn is_inline_level(style: &ComputedStyle, fragment: &FragmentNode) -> bool {
  let is_inline_display = matches!(
    style.display,
    Display::Inline
      | Display::InlineBlock
      | Display::InlineFlex
      | Display::InlineGrid
      | Display::InlineTable
  );

  let is_inline_content = matches!(
    fragment.content,
    FragmentContent::Inline { .. } | FragmentContent::Text { .. } | FragmentContent::Line { .. }
  );

  is_inline_display || is_inline_content
}

#[derive(Copy, Clone)]
enum EdgeOrientation {
  Horizontal,
  Vertical,
}

#[derive(Copy, Clone)]
enum BorderEdge {
  Top,
  Right,
  Bottom,
  Left,
}

impl BorderEdge {
  fn orientation(self) -> EdgeOrientation {
    match self {
      BorderEdge::Top | BorderEdge::Bottom => EdgeOrientation::Horizontal,
      BorderEdge::Left | BorderEdge::Right => EdgeOrientation::Vertical,
    }
  }

  /// Returns a pair of parallel paths offset by `offset` from the center line.
  fn parallel_lines(
    &self,
    x1: f32,
    y1: f32,
    x2: f32,
    y2: f32,
    offset: f32,
  ) -> (Option<tiny_skia::Path>, Option<tiny_skia::Path>) {
    match self.orientation() {
      EdgeOrientation::Horizontal => {
        let mut first = PathBuilder::new();
        first.move_to(x1, y1 - offset);
        first.line_to(x2, y2 - offset);

        let mut second = PathBuilder::new();
        second.move_to(x1, y1 + offset);
        second.line_to(x2, y2 + offset);

        (first.finish(), second.finish())
      }
      EdgeOrientation::Vertical => {
        let mut first = PathBuilder::new();
        first.move_to(x1 - offset, y1);
        first.line_to(x2 - offset, y2);

        let mut second = PathBuilder::new();
        second.move_to(x1 + offset, y1);
        second.line_to(x2 + offset, y2);

        (first.finish(), second.finish())
      }
    }
  }

  fn groove_ridge_colors(self, base: &Rgba, style: CssBorderStyle) -> (Rgba, Rgba) {
    let lighten = |c: &Rgba| shade_color(c, 1.25);
    let darken = |c: &Rgba| shade_color(c, 0.75);

    let (first_light, second_light) = match style {
      CssBorderStyle::Groove => (false, true),
      CssBorderStyle::Ridge => (true, false),
      _ => (false, false),
    };

    let (first, second) = match self {
      BorderEdge::Top | BorderEdge::Left => (first_light, second_light),
      BorderEdge::Right | BorderEdge::Bottom => (!first_light, !second_light),
    };

    (
      if first { lighten(base) } else { darken(base) },
      if second { lighten(base) } else { darken(base) },
    )
  }

  fn inset_outset_color(self, base: &Rgba, style: CssBorderStyle) -> Rgba {
    match style {
      CssBorderStyle::Inset => match self {
        BorderEdge::Top | BorderEdge::Left => shade_color(base, 0.75),
        BorderEdge::Right | BorderEdge::Bottom => shade_color(base, 1.25),
      },
      CssBorderStyle::Outset => match self {
        BorderEdge::Top | BorderEdge::Left => shade_color(base, 1.25),
        BorderEdge::Right | BorderEdge::Bottom => shade_color(base, 0.75),
      },
      _ => *base,
    }
  }
}

fn shade_color(color: &Rgba, factor: f32) -> Rgba {
  let clamp_to_u8 = |v: f32| v.clamp(0.0, 255.0) as u8;
  let r = clamp_to_u8(color.r as f32 * factor);
  let g = clamp_to_u8(color.g as f32 * factor);
  let b = clamp_to_u8(color.b as f32 * factor);
  Rgba::new(r, g, b, color.a)
}

pub(crate) fn snap_upscale(target: f32, raw: f32) -> Option<(f32, f32)> {
  if target <= 0.0 || raw <= 0.0 || target <= raw {
    return None;
  }
  let scale = target / raw;
  if scale <= 1.0 {
    return None;
  }
  let snapped = (scale.floor().max(1.0)) * raw;
  let snapped = snapped.min(target);
  let offset = (target - snapped) * 0.5;
  Some((snapped, offset))
}

impl Painter {
  fn resolve_scaled_metrics(&self, style: &ComputedStyle) -> Option<ScaledMetrics> {
    let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
    let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
    let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());

    self
      .font_ctx
      .get_font_full(
        &style.font_family,
        style.font_weight.to_u16(),
        if italic {
          FontStyle::Italic
        } else if oblique {
          FontStyle::Oblique
        } else {
          FontStyle::Normal
        },
        stretch,
      )
      .or_else(|| self.font_ctx.get_sans_serif())
      .and_then(|font| font.metrics().ok())
      .map(|m| m.scale(style.font_size))
  }

  fn resolve_scaled_metrics_static(
    style: &ComputedStyle,
    font_ctx: &FontContext,
  ) -> Option<ScaledMetrics> {
    let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
    let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
    let stretch = FontStretch::from_percentage(style.font_stretch.to_percentage());

    font_ctx
      .get_font_full(
        &style.font_family,
        style.font_weight.to_u16(),
        if italic {
          FontStyle::Italic
        } else if oblique {
          FontStyle::Oblique
        } else {
          FontStyle::Normal
        },
        stretch,
      )
      .or_else(|| font_ctx.get_sans_serif())
      .and_then(|font| font.metrics().ok())
      .map(|m| m.scale(style.font_size))
  }

  fn filter_quality_for_image(style: Option<&ComputedStyle>) -> FilterQuality {
    match style.map(|s| s.image_rendering) {
      Some(ImageRendering::CrispEdges) | Some(ImageRendering::Pixelated) => FilterQuality::Nearest,
      _ => FilterQuality::Bilinear,
    }
  }

  fn resolve_replaced_intrinsic_sizes(&self, node: &mut tree::box_tree::BoxNode, viewport: Size) {
    use tree::box_tree::BoxType;
    use tree::box_tree::MarkerContent;
    if let BoxType::Marker(marker_box) = &mut node.box_type {
      if let MarkerContent::Image(replaced) = &mut marker_box.content {
        self.resolve_intrinsic_for_replaced(replaced, node.style.as_ref(), None, viewport);
      }
    }

    if let BoxType::Replaced(replaced_box) = &mut node.box_type {
      let alt = match &replaced_box.replaced_type {
        ReplacedType::Image { alt, .. } => alt.clone(),
        _ => None,
      };
      self.resolve_intrinsic_for_replaced(
        replaced_box,
        node.style.as_ref(),
        alt.as_deref(),
        viewport,
      );
    }

    for child in &mut node.children {
      self.resolve_replaced_intrinsic_sizes(child, viewport);
    }
  }

  fn resolve_intrinsic_for_replaced(
    &self,
    replaced_box: &mut ReplacedBox,
    style: &ComputedStyle,
    alt: Option<&str>,
    viewport: Size,
  ) {
    if let ReplacedType::Math(math) = &mut replaced_box.replaced_type {
      if math.layout.is_none() {
        let layout = crate::math::layout_mathml(&math.root, style, &self.font_ctx);
        math.layout = Some(Arc::new(layout));
      }
      if replaced_box.intrinsic_size.is_none() {
        if let Some(layout) = &math.layout {
          replaced_box.intrinsic_size = Some(layout.size());
          if layout.height > 0.0 {
            replaced_box.aspect_ratio = Some(layout.width / layout.height);
          }
        }
      }
      return;
    }

    let replaced_type_snapshot = replaced_box.replaced_type.clone();
    match replaced_type_snapshot {
      ReplacedType::FormControl(control) => {
        if replaced_box.intrinsic_size.is_none() {
          let metrics = self.resolve_scaled_metrics(style);
          let char_width = metrics
            .as_ref()
            .and_then(|m| m.x_height)
            .unwrap_or(style.font_size * 0.6)
            * 0.6;
          let line_height =
            compute_line_height_with_metrics_viewport(style, metrics.as_ref(), Some(viewport));
          let size = match &control.control {
            FormControlKind::Text { size_attr, .. } => {
              let cols = size_attr.unwrap_or(20) as f32;
              Size::new(char_width * cols.max(1.0), line_height)
            }
            FormControlKind::TextArea { rows, cols, .. } => {
              let row_count = rows.unwrap_or(2) as f32;
              let col_count = cols.unwrap_or(20) as f32;
              Size::new(
                char_width * col_count.max(1.0),
                line_height * row_count.max(1.0),
              )
            }
            FormControlKind::Button { label } => {
              let text_len = label.chars().count().max(1) as f32;
              Size::new(char_width * text_len + char_width * 2.0, line_height)
            }
            FormControlKind::Select { label, .. } => {
              let text_len = label.chars().count().max(4) as f32;
              Size::new(char_width * text_len + 20.0, line_height)
            }
            FormControlKind::Checkbox { .. } => {
              let edge = (style.font_size * 1.1).clamp(12.0, 20.0);
              Size::new(edge, edge)
            }
            FormControlKind::Range { .. } => Size::new(char_width * 12.0, line_height.max(12.0)),
            FormControlKind::Unknown { .. } => Size::new(char_width * 10.0, line_height),
          };
          replaced_box.intrinsic_size = Some(size);
        }

        if replaced_box.aspect_ratio.is_none() {
          if let Some(size) = replaced_box.intrinsic_size {
            if size.height > 0.0 {
              replaced_box.aspect_ratio = Some(size.width / size.height);
            }
          }
        }
      }
      ReplacedType::Image {
        src,
        alt: stored_alt,
        srcset,
        picture_sources,
        ..
      } => {
        let needs_intrinsic = replaced_box.intrinsic_size.is_none();
        let needs_ratio = replaced_box.aspect_ratio.is_none();
        let mut have_resource_dimensions = false;

        let has_source = !src.is_empty() || !srcset.is_empty() || !picture_sources.is_empty();

        let selected = if (needs_intrinsic || needs_ratio) && has_source {
          let media_ctx =
            crate::style::media::MediaContext::screen(viewport.width, viewport.height)
              .with_device_pixel_ratio(self.scale)
              .with_env_overrides();
          let cache_base = self.image_cache.base_url();
          Some(
            replaced_box
              .replaced_type
              .selected_image_source_for_context(crate::tree::box_tree::ImageSelectionContext {
                scale: self.scale,
                slot_width: None,
                viewport: Some(viewport),
                media_context: Some(&media_ctx),
                font_size: Some(style.font_size),
                base_url: cache_base.as_deref(),
              }),
          )
        } else {
          None
        };

        if let Some(selected) = selected {
          if !selected.url.is_empty() {
            if let Ok(img) = self.image_cache.load(selected.url) {
              let orientation = style.image_orientation.resolve(img.orientation, false);
              if let Some((w, h)) = img.css_dimensions(
                orientation,
                &style.image_resolution,
                self.scale,
                selected.resolution,
              ) {
                if needs_intrinsic {
                  replaced_box.intrinsic_size = Some(Size::new(w, h));
                }
                if needs_ratio && h > 0.0 {
                  replaced_box.aspect_ratio = Some(w / h);
                }
                have_resource_dimensions = true;
              }
            }
          }
        }

        if have_resource_dimensions {
          return;
        }

        let inherited_alt = alt.or(stored_alt.as_deref()).unwrap_or("");
        if let Some(text_size) = self.measure_alt_text(inherited_alt, style) {
          if needs_intrinsic && replaced_box.intrinsic_size.is_none() {
            replaced_box.intrinsic_size = Some(text_size);
          }
          if needs_ratio && replaced_box.aspect_ratio.is_none() && text_size.height > 0.0 {
            replaced_box.aspect_ratio = Some(text_size.width / text_size.height);
          }
        }
      }
      ReplacedType::Svg { content } => {
        let needs_intrinsic = replaced_box.intrinsic_size.is_none();
        let needs_ratio = replaced_box.aspect_ratio.is_none();
        if !needs_intrinsic && !needs_ratio {
          return;
        }
        let image = if content.svg.trim_start().starts_with('<') {
          self.image_cache.render_svg(&content.svg)
        } else {
          self.image_cache.load(&content.svg)
        };
        if let Ok(image) = image {
          let orientation = style.image_orientation.resolve(image.orientation, false);
          if let Some((w, h)) =
            image.css_dimensions(orientation, &style.image_resolution, self.scale, None)
          {
            if needs_intrinsic {
              replaced_box.intrinsic_size = Some(Size::new(w, h));
            }
            if needs_ratio && h > 0.0 {
              replaced_box.aspect_ratio = Some(w / h);
            }
          }
        }
      }
      ReplacedType::Embed { src: content } | ReplacedType::Object { data: content } => {
        let needs_intrinsic = replaced_box.intrinsic_size.is_none();
        let needs_ratio = replaced_box.aspect_ratio.is_none();
        if !needs_intrinsic && !needs_ratio {
          return;
        }
        let image = if content.trim_start().starts_with('<') {
          self.image_cache.render_svg(&content)
        } else {
          self.image_cache.load(&content)
        };
        if let Ok(image) = image {
          let orientation = style.image_orientation.resolve(image.orientation, false);
          if let Some((w, h)) =
            image.css_dimensions(orientation, &style.image_resolution, self.scale, None)
          {
            if needs_intrinsic {
              replaced_box.intrinsic_size = Some(Size::new(w, h));
            }
            if needs_ratio && h > 0.0 {
              replaced_box.aspect_ratio = Some(w / h);
            }
          }
        }
      }
      ReplacedType::Video { src: _src, poster } => {
        let needs_intrinsic = replaced_box.intrinsic_size.is_none();
        let needs_ratio = replaced_box.aspect_ratio.is_none();
        if !needs_intrinsic && !needs_ratio {
          return;
        }
        if let Some(poster) = poster {
          if let Ok(img) = self.image_cache.load(&poster) {
            let orientation = style.image_orientation.resolve(img.orientation, false);
            if let Some((w, h)) =
              img.css_dimensions(orientation, &style.image_resolution, self.scale, None)
            {
              if needs_intrinsic {
                replaced_box.intrinsic_size = Some(Size::new(w, h));
              }
              if needs_ratio && h > 0.0 {
                replaced_box.aspect_ratio = Some(w / h);
              }
              return;
            }
          }
        }

        if needs_intrinsic {
          replaced_box.intrinsic_size = Some(Size::new(300.0, 150.0));
        }
        if needs_ratio {
          replaced_box.aspect_ratio = Some(2.0);
        }
      }
      ReplacedType::Canvas => {
        if replaced_box.intrinsic_size.is_none() {
          replaced_box.intrinsic_size = Some(Size::new(300.0, 150.0));
        }
        if replaced_box.aspect_ratio.is_none() {
          replaced_box.aspect_ratio = Some(2.0);
        }
      }
      ReplacedType::Audio { .. } => {
        if replaced_box.intrinsic_size.is_none() {
          replaced_box.intrinsic_size = Some(Size::new(300.0, 32.0));
        }
        if replaced_box.aspect_ratio.is_none() {
          replaced_box.aspect_ratio = Some(300.0 / 32.0);
        }
      }
      ReplacedType::Iframe { .. } => {}
      ReplacedType::Math(_) => {}
    }
  }

  /// Creates a new painter with the given dimensions
  pub fn new(width: u32, height: u32, background: Rgba) -> Result<Self> {
    Self::with_resources_scaled(
      width,
      height,
      background,
      FontContext::new(),
      ImageCache::new(),
      1.0,
    )
  }

  /// Creates a painter with explicit font and image resources
  pub fn with_resources(
    width: u32,
    height: u32,
    background: Rgba,
    font_ctx: FontContext,
    image_cache: ImageCache,
  ) -> Result<Self> {
    Self::with_resources_scaled(width, height, background, font_ctx, image_cache, 1.0)
  }

  /// Creates a painter with explicit font/image resources and a device scale.
  pub fn with_resources_scaled(
    width: u32,
    height: u32,
    background: Rgba,
    font_ctx: FontContext,
    image_cache: ImageCache,
    scale: f32,
  ) -> Result<Self> {
    let scale = if scale.is_finite() && scale > 0.0 {
      scale
    } else {
      1.0
    };
    let device_w = ((width as f32) * scale).round().max(1.0) as u32;
    let device_h = ((height as f32) * scale).round().max(1.0) as u32;
    let pixmap = Pixmap::new(device_w, device_h).ok_or_else(|| RenderError::InvalidParameters {
      message: format!("Failed to create pixmap {}x{}", device_w, device_h),
    })?;

    Ok(Self {
      pixmap,
      scale,
      css_width: width as f32,
      css_height: height as f32,
      background,
      shaper: ShapingPipeline::new(),
      font_ctx,
      image_cache,
      text_shape_cache: Arc::new(Mutex::new(HashMap::new())),
      trace: TraceHandle::disabled(),
    })
  }

  /// Attach a trace handle used for Chrome trace export.
  fn with_trace(mut self, trace: TraceHandle) -> Self {
    self.trace = trace;
    self
  }

  #[inline]
  fn device_length(&self, value: f32) -> f32 {
    value * self.scale
  }

  #[inline]
  fn device_rect(&self, rect: Rect) -> Rect {
    Rect::from_xywh(
      rect.x() * self.scale,
      rect.y() * self.scale,
      rect.width() * self.scale,
      rect.height() * self.scale,
    )
  }

  #[inline]
  fn device_radii(&self, radii: BorderRadii) -> BorderRadii {
    BorderRadii {
      top_left: radii.top_left * self.scale,
      top_right: radii.top_right * self.scale,
      bottom_right: radii.bottom_right * self.scale,
      bottom_left: radii.bottom_left * self.scale,
    }
  }

  #[inline]
  fn device_transform(&self, transform: Option<Transform>) -> Option<Transform> {
    if let Some(mut t) = transform {
      t.tx *= self.scale;
      t.ty *= self.scale;
      Some(t)
    } else {
      None
    }
  }

  #[allow(dead_code)]
  #[inline]
  fn device_dimensions(&self, width: f32, height: f32) -> Option<(u32, u32)> {
    if width <= 0.0 || height <= 0.0 {
      return None;
    }
    let w = ((width * self.scale).round()).max(0.0) as u32;
    let h = ((height * self.scale).round()).max(0.0) as u32;
    Some((w, h))
  }

  /// Paints a fragment tree and returns the resulting pixmap
  pub fn paint(self, tree: &FragmentTree) -> Result<Pixmap> {
    self.paint_with_offset(tree, Point::ZERO)
  }

  /// Paints a fragment tree with an additional offset applied to all fragments.
  pub fn paint_with_offset(mut self, tree: &FragmentTree, offset: Point) -> Result<Pixmap> {
    let profiling = runtime::runtime_toggles().truthy("FASTR_PAINT_STATS");
    let diagnostics_enabled = paint_diagnostics_enabled();
    let mut stats = PaintStats::default();
    check_active(RenderStage::Paint).map_err(Error::Render)?;
    let trace = self.trace.clone();
    let _paint_span = trace.span("paint", "paint");

    if dump_counts_enabled() {
      let (total, text, replaced, lines, inline) = fragment_tree_counts(tree);
      eprintln!(
        "fragment counts total={} text={} lines={} inline={} replaced={}",
        total, text, lines, inline, replaced
      );
    }

    // Fill background
    let start = Instant::now();
    self.fill_background();
    if profiling {
      stats.background_ms = start.elapsed().as_secs_f64() * 1000.0;
    }

    // Build display list in stacking-context order then paint
    let mut items = Vec::new();
    let collect_start = Instant::now();
    let _display_list_span = trace.span("display_list_build", "paint");
    let root_paint = RootPaintOptions {
      use_root_background: tree.has_explicit_viewport(),
      extend_background_to_viewport: tree.has_explicit_viewport()
        && tree.additional_fragments.is_empty(),
    };
    let svg_filter_roots: Vec<&FragmentNode> = std::iter::once(&tree.root)
      .chain(tree.additional_fragments.iter())
      .collect();
    let mut svg_filter_resolver = SvgFilterResolver::new(
      tree.svg_filter_defs.clone(),
      svg_filter_roots.clone(),
      Some(&self.image_cache),
    );
    for root in svg_filter_roots {
      self.collect_stacking_context(
        root,
        offset,
        None,
        true,
        root_paint,
        &mut items,
        &mut svg_filter_resolver,
      );
    }
    drop(_display_list_span);
    check_active(RenderStage::Paint).map_err(Error::Render)?;
    if profiling {
      stats.collect_ms = collect_start.elapsed().as_secs_f64() * 1000.0;
    }
    if diagnostics_enabled {
      let build_ms = collect_start.elapsed().as_secs_f64() * 1000.0;
      let count = items.len();
      with_paint_diagnostics(|diag| {
        diag.build_ms = build_ms;
        diag.command_count = count;
      });
    }
    if dump_stack_enabled() {
      let total_items = items.len();
      let mut stack_items = 0;
      let mut text_items = 0;
      fn nested_counts(cmds: &[DisplayCommand]) -> (usize, usize) {
        cmds.iter().fold((0, 0), |(total, text), cmd| match cmd {
          DisplayCommand::StackingContext { commands, .. } => {
            let (t, tx) = nested_counts(commands);
            (total + 1 + t, text + tx)
          }
          DisplayCommand::Text { .. } => (total + 1, text + 1),
          _ => (total + 1, text),
        })
      }
      for item in &items {
        match item {
          DisplayCommand::StackingContext { commands, .. } => {
            stack_items += 1;
            let (c, t) = nested_counts(commands);
            eprintln!(
              "stack list item: nested_commands={} nested_text={} bounds_not_logged_here",
              c, t
            );
          }
          DisplayCommand::Text { .. } => text_items += 1,
          _ => {}
        }
      }
      eprintln!(
        "stack list summary: total_items={} stack_items={} text_items={}",
        total_items, stack_items, text_items
      );
    }

    // Optional debug: dump a few text commands with positions/colors
    if let Some(limit) = runtime::runtime_toggles().usize("FASTR_DUMP_TEXT_ITEMS") {
      fn collect_text<'a>(cmds: &'a [DisplayCommand], out: &mut Vec<&'a DisplayCommand>) {
        for cmd in cmds {
          match cmd {
            DisplayCommand::Text { .. } => out.push(cmd),
            DisplayCommand::StackingContext { commands, .. } => collect_text(commands, out),
            _ => {}
          }
        }
      }
      let mut texts = Vec::new();
      collect_text(&items, &mut texts);
      eprintln!(
        "dumping first {} text commands ({} total)",
        limit.min(texts.len()),
        texts.len()
      );
      for (idx, cmd) in texts.iter().take(limit).enumerate() {
        if let DisplayCommand::Text {
          rect,
          baseline_offset,
          text,
          style,
          ..
        } = cmd
        {
          let (r, g, b, a) = (style.color.r, style.color.g, style.color.b, style.color.a);
          eprintln!(
                          "  [{idx}] text {:?} rect=({:.1},{:.1},{:.1},{:.1}) baseline_off={:.1} color=rgba({},{},{},{:.2})",
                          text.chars().take(60).collect::<String>(),
                          rect.x(),
                          rect.y(),
                          rect.width(),
                          rect.height(),
                          baseline_offset,
                          r,
                          g,
                          b,
                          a
                      );
        }
      }
    }

    // Optional debug: dump the first N display commands with their rects/types.
    if let Some(limit) = runtime::runtime_toggles().usize("FASTR_DUMP_COMMANDS") {
      fn collect_commands<'a>(
        cmds: &'a [DisplayCommand],
        out: &mut Vec<(&'a DisplayCommand, usize)>,
        depth: usize,
      ) {
        for cmd in cmds {
          out.push((cmd, depth));
          if let DisplayCommand::StackingContext { commands, .. } = cmd {
            collect_commands(commands, out, depth + 1);
          }
        }
      }

      let mut flat = Vec::new();
      collect_commands(&items, &mut flat, 0);
      eprintln!(
        "dumping first {} commands ({} total)",
        limit.min(flat.len()),
        flat.len()
      );

      for (idx, (cmd, depth)) in flat.iter().take(limit).enumerate() {
        match cmd {
          DisplayCommand::Background { rect, style } => eprintln!(
            "  [{idx}] {:indent$}background ({:.1},{:.1},{:.1},{:.1}) color=rgba({},{},{},{:.2})",
            "",
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height(),
            style.background_color.r,
            style.background_color.g,
            style.background_color.b,
            style.background_color.a,
            indent = depth * 2
          ),
          DisplayCommand::Border { rect, .. } => eprintln!(
            "  [{idx}] {:indent$}border ({:.1},{:.1},{:.1},{:.1})",
            "",
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height(),
            indent = depth * 2
          ),
          DisplayCommand::Outline { rect, .. } => eprintln!(
            "  [{idx}] {:indent$}outline ({:.1},{:.1},{:.1},{:.1})",
            "",
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height(),
            indent = depth * 2
          ),
          DisplayCommand::Text { rect, .. } => eprintln!(
            "  [{idx}] {:indent$}text ({:.1},{:.1},{:.1},{:.1})",
            "",
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height(),
            indent = depth * 2
          ),
          DisplayCommand::Replaced { rect, .. } => eprintln!(
            "  [{idx}] {:indent$}replaced ({:.1},{:.1},{:.1},{:.1})",
            "",
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height(),
            indent = depth * 2
          ),
          DisplayCommand::StackingContext { rect, .. } => eprintln!(
            "  [{idx}] {:indent$}stack ({:.1},{:.1},{:.1},{:.1})",
            "",
            rect.x(),
            rect.y(),
            rect.width(),
            rect.height(),
            indent = depth * 2
          ),
        }
      }
    }

    let command_len = items.len();
    let raster_start = diagnostics_enabled.then(Instant::now);
    let _raster_span = trace.span("rasterize", "paint");
    for item in items {
      if let Err(RenderError::Timeout { stage, elapsed }) = check_active(RenderStage::Paint) {
        return Err(Error::Render(RenderError::Timeout { stage, elapsed }));
      }
      if profiling {
        let exec_start = Instant::now();
        self.execute_command(item.clone())?;
        stats.record(&item, exec_start.elapsed());
      } else {
        self.execute_command(item)?;
      }
    }
    drop(_raster_span);
    if let (true, Some(start)) = (diagnostics_enabled, raster_start) {
      let raster_ms = start.elapsed().as_secs_f64() * 1000.0;
      with_paint_diagnostics(|diag| {
        diag.raster_ms = raster_ms;
        if diag.command_count == 0 {
          diag.command_count = command_len;
        }
      });
    }

    if profiling {
      eprintln!("paint_stats enabled");
      stats.log();
    }

    Ok(self.pixmap)
  }

  /// Fills the canvas with the background color
  fn fill_background(&mut self) {
    let color = tiny_skia::Color::from_rgba8(
      self.background.r,
      self.background.g,
      self.background.b,
      self.background.alpha_u8(),
    );
    self.pixmap.fill(color);
  }

  /// Collect display commands respecting stacking-context ordering.
  ///
  /// This follows the simplified CSS painting order for a stacking context:
  /// element background/border → negative z-index stacking contexts →
  /// in-flow/non-positioned content → z-index:auto/0 stacking contexts →
  /// positive z-index stacking contexts.
  fn collect_stacking_context(
    &self,
    fragment: &FragmentNode,
    offset: Point,
    parent_style: Option<&ComputedStyle>,
    is_root_context: bool,
    root_paint: RootPaintOptions,
    items: &mut Vec<DisplayCommand>,
    svg_filters: &mut SvgFilterResolver,
  ) {
    if check_active(RenderStage::Paint).is_err() {
      return;
    }
    let debug_fragments = dump_fragments_enabled();
    let is_root_fragment = is_root_context && parent_style.is_none();
    let root_background = if is_root_fragment && root_paint.use_root_background {
      Some(root_paint.extend_background_to_viewport)
    } else {
      None
    };
    if let Some(style) = fragment.style.as_deref() {
      if !matches!(
        style.visibility,
        crate::style::computed::Visibility::Visible
      ) {
        return;
      }
    }

    let abs_bounds = Rect::from_xywh(
      fragment.bounds.x() + offset.x,
      fragment.bounds.y() + offset.y,
      fragment.bounds.width(),
      fragment.bounds.height(),
    );
    let viewport = (self.css_width, self.css_height);

    if let Some(style) = fragment.style.as_deref() {
      if matches!(style.backface_visibility, BackfaceVisibility::Hidden)
        && (!style.transform.is_empty() || style.perspective.is_some() || style.has_motion_path())
      {
        if let Some(transform) = resolve_transform3d(style, abs_bounds, Some(viewport)) {
          if backface_is_hidden(&transform) {
            return;
          }
        }
      }
    }

    if debug_fragments {
      eprintln!(
        "fragment {:?} bounds=({}, {}, {}, {}) children={} establishes={} display={:?}",
        describe_content(&fragment.content),
        abs_bounds.x(),
        abs_bounds.y(),
        abs_bounds.width(),
        abs_bounds.height(),
        fragment.children.len(),
        fragment
          .style
          .as_deref()
          .map(|s| creates_stacking_context(s, parent_style, is_root_context))
          .unwrap_or(is_root_context),
        fragment.style.as_deref().map(|s| s.display)
      );
    }

    let style_ref = fragment.style.as_deref();
    let establishes_context = style_ref
      .map(|s| creates_stacking_context(s, parent_style, is_root_context))
      .unwrap_or(is_root_context);

    // Collect commands for this subtree locally so we can wrap the context (opacity, etc.)
    let mut local_commands = Vec::new();

    if !establishes_context {
      self.enqueue_background_and_borders(
        fragment,
        abs_bounds,
        root_background,
        &mut local_commands,
      );
      self.enqueue_content(fragment, abs_bounds, &mut local_commands);

      let next_offset = Point::new(abs_bounds.x(), abs_bounds.y());
      let mut negative_contexts = Vec::new();
      let mut zero_contexts = Vec::new();
      let mut positive_contexts = Vec::new();
      let mut blocks_and_floats = Vec::new();
      let mut inlines = Vec::new();
      let mut positioned_auto = Vec::new();

      for (idx, child) in fragment.children.iter().enumerate() {
        if let Some(style) = child.style.as_deref() {
          if creates_stacking_context(style, style_ref, false) {
            let z = style.z_index.unwrap_or(0);
            match z.cmp(&0) {
              std::cmp::Ordering::Less => negative_contexts.push((z, idx)),
              std::cmp::Ordering::Equal => zero_contexts.push((z, idx)),
              std::cmp::Ordering::Greater => positive_contexts.push((z, idx)),
            }
            continue;
          }
          if is_positioned(style) {
            positioned_auto.push(idx);
          } else if is_inline_level(style, child) {
            inlines.push(idx);
          } else {
            blocks_and_floats.push(idx);
          }
        } else if matches!(
          child.content,
          FragmentContent::Inline { .. }
            | FragmentContent::Text { .. }
            | FragmentContent::Line { .. }
        ) {
          inlines.push(idx);
        }
        // Children without style default to block-level when not inline-like.
        else {
          blocks_and_floats.push(idx);
        }
      }

      negative_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
      for (_, idx) in negative_contexts {
        self.collect_stacking_context(
          &fragment.children[idx],
          next_offset,
          style_ref,
          false,
          root_paint,
          &mut local_commands,
          svg_filters,
        );
      }

      for idx in blocks_and_floats {
        self.collect_stacking_context(
          &fragment.children[idx],
          next_offset,
          style_ref,
          false,
          root_paint,
          &mut local_commands,
          svg_filters,
        );
      }
      for idx in inlines {
        self.collect_stacking_context(
          &fragment.children[idx],
          next_offset,
          style_ref,
          false,
          root_paint,
          &mut local_commands,
          svg_filters,
        );
      }
      for idx in positioned_auto {
        self.collect_stacking_context(
          &fragment.children[idx],
          next_offset,
          style_ref,
          false,
          root_paint,
          &mut local_commands,
          svg_filters,
        );
      }

      zero_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
      for (_, idx) in zero_contexts {
        self.collect_stacking_context(
          &fragment.children[idx],
          next_offset,
          style_ref,
          false,
          root_paint,
          &mut local_commands,
          svg_filters,
        );
      }

      positive_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
      for (_, idx) in positive_contexts {
        self.collect_stacking_context(
          &fragment.children[idx],
          next_offset,
          style_ref,
          false,
          root_paint,
          &mut local_commands,
          svg_filters,
        );
      }

      items.extend(local_commands);
      return;
    }

    // Stacking context: paint own background/border first
    self.enqueue_background_and_borders(fragment, abs_bounds, root_background, &mut local_commands);

    // Partition children into stacking-context buckets and normal flow (preserving DOM order)
    let mut negative_contexts = Vec::new();
    let mut zero_contexts = Vec::new();
    let mut positive_contexts = Vec::new();
    // Paint in-flow blocks and floats together in DOM order (CSS2 stacking level 4)
    let mut blocks_and_floats = Vec::new();
    // Paint in-flow inline-level content after blocks/floats (CSS2 stacking level 5)
    let mut inlines = Vec::new();
    // Positioned elements with auto/0 z-index but not establishing a stacking context (CSS2 level 6)
    let mut positioned_auto = Vec::new();

    for (idx, child) in fragment.children.iter().enumerate() {
      if let Some(style) = child.style.as_deref() {
        if creates_stacking_context(style, style_ref, false) {
          let z = style.z_index.unwrap_or(0);
          match z.cmp(&0) {
            std::cmp::Ordering::Less => negative_contexts.push((z, idx)),
            std::cmp::Ordering::Equal => zero_contexts.push((z, idx)),
            std::cmp::Ordering::Greater => positive_contexts.push((z, idx)),
          }
          continue;
        }
        if is_positioned(style) {
          positioned_auto.push(idx);
        } else if is_inline_level(style, child) {
          inlines.push(idx);
        } else {
          blocks_and_floats.push(idx);
        }
      } else if matches!(
        child.content,
        FragmentContent::Inline { .. }
          | FragmentContent::Text { .. }
          | FragmentContent::Line { .. }
      ) {
        inlines.push(idx);
      } else {
        blocks_and_floats.push(idx);
      }
    }

    let child_offset = Point::new(abs_bounds.x(), abs_bounds.y());

    negative_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
    for (_, idx) in negative_contexts {
      self.collect_stacking_context(
        &fragment.children[idx],
        child_offset,
        style_ref,
        false,
        root_paint,
        &mut local_commands,
        svg_filters,
      );
    }

    // In-flow/non-positioned content for this context
    self.enqueue_content(fragment, abs_bounds, &mut local_commands);
    for idx in blocks_and_floats {
      self.collect_stacking_context(
        &fragment.children[idx],
        child_offset,
        style_ref,
        false,
        root_paint,
        &mut local_commands,
        svg_filters,
      );
    }
    for idx in inlines {
      self.collect_stacking_context(
        &fragment.children[idx],
        child_offset,
        style_ref,
        false,
        root_paint,
        &mut local_commands,
        svg_filters,
      );
    }
    for idx in positioned_auto {
      self.collect_stacking_context(
        &fragment.children[idx],
        child_offset,
        style_ref,
        false,
        root_paint,
        &mut local_commands,
        svg_filters,
      );
    }

    zero_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
    for (_, idx) in zero_contexts {
      self.collect_stacking_context(
        &fragment.children[idx],
        child_offset,
        style_ref,
        false,
        root_paint,
        &mut local_commands,
        svg_filters,
      );
    }

    positive_contexts.sort_by(|(z1, i1), (z2, i2)| z1.cmp(z2).then_with(|| i1.cmp(i2)));
    for (_, idx) in positive_contexts {
      self.collect_stacking_context(
        &fragment.children[idx],
        child_offset,
        style_ref,
        false,
        root_paint,
        &mut local_commands,
        svg_filters,
      );
    }

    // Wrap the stacking context if it applies an effect (opacity/transform); otherwise flatten
    let opacity = style_ref.map(|s| s.opacity).unwrap_or(1.0).clamp(0.0, 1.0);
    let transform_3d = build_transform_3d(style_ref, abs_bounds, Some(viewport));
    let transform = transform_3d
      .as_ref()
      .and_then(|t| t.to_2d())
      .map(transform2d_to_skia);
    let blend_mode = style_ref
      .map(|s| s.mix_blend_mode)
      .unwrap_or(MixBlendMode::Normal);
    let isolated = style_ref
      .map(|s| matches!(s.isolation, crate::style::types::Isolation::Isolate))
      .unwrap_or(false);
    let filters = style_ref
      .map(|s| resolve_filters(&s.filter, s, viewport, &self.font_ctx, svg_filters))
      .unwrap_or_default();
    let has_filters = !filters.is_empty();
    let backdrop_filters = style_ref
      .map(|s| resolve_filters(&s.backdrop_filter, s, viewport, &self.font_ctx, svg_filters))
      .unwrap_or_default();
    let has_backdrop = !backdrop_filters.is_empty();
    let clip: Option<StackingClip> = style_ref.and_then(|style| {
      // Honor overflow clipping: when overflow is hidden/scroll/clip, restrict painting to the
      // padding box. This prevents offscreen children from flooding the viewport when their
      // layout positions explode.
      let clip_x = matches!(
        style.overflow_x,
        Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
      ) || style.containment.paint;
      let clip_y = matches!(
        style.overflow_y,
        Overflow::Hidden | Overflow::Scroll | Overflow::Auto | Overflow::Clip
      ) || style.containment.paint;
      let overflow_clip = if clip_x || clip_y {
        let rects = background_rects(
          abs_bounds.x(),
          abs_bounds.y(),
          abs_bounds.width(),
          abs_bounds.height(),
          style,
          Some((self.css_width, self.css_height)),
        );
        let clip_rect = rects.padding;
        if clip_rect.width() <= 0.0 || clip_rect.height() <= 0.0 {
          None
        } else {
          let clip_radii = resolve_clip_radii(
            style,
            &rects,
            crate::style::types::BackgroundBox::PaddingBox,
            Some((self.css_width, self.css_height)),
          );
          Some(StackingClip {
            rect: clip_rect,
            radii: clip_radii,
            clip_x,
            clip_y,
            clip_root: false,
          })
        }
      } else {
        None
      };

      let clip_property = style.clip.as_ref().and_then(|clip| {
        let font_size = style.font_size;
        let root_font = style.root_font_size;
        let viewport = (self.css_width, self.css_height);
        let width = abs_bounds.width();
        let height = abs_bounds.height();

        let left = match &clip.left {
          ClipComponent::Auto => abs_bounds.x(),
          ClipComponent::Length(len) => {
            abs_bounds.x() + resolve_length_for_paint(len, font_size, root_font, width, viewport)
          }
        };
        let top = match &clip.top {
          ClipComponent::Auto => abs_bounds.y(),
          ClipComponent::Length(len) => {
            abs_bounds.y() + resolve_length_for_paint(len, font_size, root_font, height, viewport)
          }
        };
        let right = match &clip.right {
          ClipComponent::Auto => abs_bounds.x() + width,
          ClipComponent::Length(len) => {
            abs_bounds.x() + resolve_length_for_paint(len, font_size, root_font, width, viewport)
          }
        };
        let bottom = match &clip.bottom {
          ClipComponent::Auto => abs_bounds.y() + height,
          ClipComponent::Length(len) => {
            abs_bounds.y() + resolve_length_for_paint(len, font_size, root_font, height, viewport)
          }
        };

        let clip_rect = Rect::from_xywh(left, top, right - left, bottom - top);
        if clip_rect.width() <= 0.0 || clip_rect.height() <= 0.0 {
          None
        } else {
          Some(clip_rect)
        }
      });

      match (overflow_clip, clip_property) {
        (Some(overflow), Some(prop_rect)) => {
          overflow
            .rect
            .intersection(prop_rect)
            .map(|rect| StackingClip {
              rect,
              radii: BorderRadii::ZERO,
              clip_x: true,
              clip_y: true,
              clip_root: true,
            })
        }
        (Some(overflow), None) => Some(overflow),
        (None, Some(prop_rect)) => Some(StackingClip {
          rect: prop_rect,
          radii: BorderRadii::ZERO,
          clip_x: true,
          clip_y: true,
          clip_root: true,
        }),
        (None, None) => None,
      }
    });
    let clip_path = style_ref.and_then(|style| {
      resolve_clip_path(
        style,
        abs_bounds,
        (self.css_width, self.css_height),
        &self.font_ctx,
      )
    });
    let mask = fragment
      .style
      .clone()
      .filter(|s| s.mask_layers.iter().any(|layer| layer.image.is_some()));
    if opacity < 1.0
      || transform.is_some()
      || transform_3d.is_some()
      || !matches!(blend_mode, MixBlendMode::Normal)
      || isolated
      || has_filters
      || has_backdrop
      || clip.is_some()
      || clip_path.is_some()
      || mask.is_some()
    {
      let radii = resolve_border_radii(style_ref, abs_bounds);
      items.push(DisplayCommand::StackingContext {
        rect: abs_bounds,
        opacity,
        transform,
        transform_3d,
        blend_mode,
        isolated,
        mask,
        filters,
        backdrop_filters,
        radii,
        clip,
        clip_path,
        commands: local_commands,
      });
    } else {
      items.extend(local_commands);
    }
  }

  /// Enqueue background and border commands for a fragment (no children).
  fn enqueue_background_and_borders(
    &self,
    fragment: &FragmentNode,
    abs_bounds: Rect,
    root_background: Option<bool>,
    items: &mut Vec<DisplayCommand>,
  ) {
    let style = if root_background.is_some() {
      Self::root_background_style(fragment)
    } else {
      fragment.style.clone()
    };
    let Some(style) = style else { return };

    let has_background = Self::has_paintable_background(&style);
    if has_background {
      let background_rect = if matches!(root_background, Some(true)) {
        Rect::from_xywh(0.0, 0.0, self.css_width, self.css_height)
      } else {
        abs_bounds
      };
      items.push(DisplayCommand::Background {
        rect: background_rect,
        style: style.clone(),
      });
    }

    let has_border = style.border_top_width.to_px() > 0.0
      || style.border_right_width.to_px() > 0.0
      || style.border_bottom_width.to_px() > 0.0
      || style.border_left_width.to_px() > 0.0;
    if has_border {
      items.push(DisplayCommand::Border {
        rect: abs_bounds,
        style: style.clone(),
      });
    }

    if let Some(outline_rect) = Self::outline_bounds(abs_bounds, &style) {
      items.push(DisplayCommand::Outline {
        rect: outline_rect,
        style,
      });
    }
  }

  fn outline_bounds(abs_bounds: Rect, style: &ComputedStyle) -> Option<Rect> {
    let outline_style = style.outline_style.to_border_style();
    let width = style.outline_width.to_px();
    if width <= 0.0 || matches!(outline_style, CssBorderStyle::None | CssBorderStyle::Hidden) {
      return None;
    }
    let expand = style.outline_offset.to_px() + width * 0.5;
    Some(Rect::from_xywh(
      abs_bounds.x() - expand,
      abs_bounds.y() - expand,
      abs_bounds.width() + 2.0 * expand,
      abs_bounds.height() + 2.0 * expand,
    ))
  }

  fn has_paintable_background(style: &ComputedStyle) -> bool {
    style.background_color.alpha_u8() > 0
      || style.background_layers.iter().any(|l| l.image.is_some())
  }

  fn root_background_style(fragment: &FragmentNode) -> Option<Arc<ComputedStyle>> {
    let html = if fragment.children.len() == 1 {
      fragment.children.first().unwrap_or(fragment)
    } else {
      fragment
    };

    // HTML canvas background propagation: prefer a non-transparent body background (usually the
    // first renderable child) over the html element's own background.
    for child in &html.children {
      if let Some(style) = child.style.clone() {
        if Self::has_paintable_background(&style) {
          return Some(style);
        }
      }
    }

    if let Some(style) = html.style.clone() {
      if Self::has_paintable_background(&style) {
        return Some(style);
      }
    }

    fragment.style.clone()
  }

  /// Enqueue paint commands for the fragment's own content (text/replaced).
  fn enqueue_content(
    &self,
    fragment: &FragmentNode,
    abs_bounds: Rect,
    items: &mut Vec<DisplayCommand>,
  ) {
    match &fragment.content {
      FragmentContent::Text {
        text,
        baseline_offset,
        shaped,
        ..
      } => {
        if let Some(style) = fragment.style.clone() {
          items.push(DisplayCommand::Text {
            rect: abs_bounds,
            baseline_offset: *baseline_offset,
            text: text.clone(),
            runs: shaped.clone(),
            style,
          });
        }
      }
      FragmentContent::Replaced { replaced_type, .. } => {
        if let Some(style) = fragment.style.clone() {
          items.push(DisplayCommand::Replaced {
            rect: abs_bounds,
            replaced_type: replaced_type.clone(),
            style,
          });
        }
      }
      _ => {}
    }
  }

  fn execute_command(&mut self, command: DisplayCommand) -> Result<()> {
    let cmd_profile_threshold_ms = cmd_profile_threshold_ms();
    let cmd_profile_enabled = cmd_profile_threshold_ms.is_some();
    let cmd_profile_start = cmd_profile_enabled.then(Instant::now);
    let cmd_profile_summary = cmd_profile_enabled.then(|| match &command {
      DisplayCommand::Background { rect, .. } => format!(
        "background rect=({:.1},{:.1},{:.1},{:.1})",
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height()
      ),
      DisplayCommand::Border { rect, .. } => format!(
        "border rect=({:.1},{:.1},{:.1},{:.1})",
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height()
      ),
      DisplayCommand::Outline { rect, .. } => format!(
        "outline rect=({:.1},{:.1},{:.1},{:.1})",
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height()
      ),
      DisplayCommand::Text { rect, text, style, .. } => {
        let preview: String = text.chars().take(60).collect();
        format!(
          "text font_size={:.2} rect=({:.1},{:.1},{:.1},{:.1}) writing_mode={:?} text=\"{}\"",
          style.font_size,
          rect.x(),
          rect.y(),
          rect.width(),
          rect.height(),
          style.writing_mode,
          preview
        )
      }
      DisplayCommand::Replaced { rect, replaced_type, .. } => format!(
        "replaced {:?} rect=({:.1},{:.1},{:.1},{:.1})",
        replaced_type,
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height()
      ),
      DisplayCommand::StackingContext { rect, commands, .. } => {
        let (nested_cmds, nested_text) = nested_counts(commands);
        format!(
          "stack rect=({:.1},{:.1},{:.1},{:.1}) nested_cmds={nested_cmds} nested_text={nested_text}",
          rect.x(),
          rect.y(),
          rect.width(),
          rect.height()
        )
      }
    });
    match command {
      DisplayCommand::Background { rect, style } => {
        self.paint_background(rect.x(), rect.y(), rect.width(), rect.height(), &style);
      }
      DisplayCommand::Border { rect, style } => {
        self.paint_borders(rect.x(), rect.y(), rect.width(), rect.height(), &style);
      }
      DisplayCommand::Outline { rect, style } => {
        self.paint_outline(rect.x(), rect.y(), rect.width(), rect.height(), &style);
      }
      DisplayCommand::Text {
        rect,
        baseline_offset,
        text,
        runs,
        style,
      } => {
        let text_profile_threshold_ms = text_profile_threshold_ms();
        let text_profile_enabled = text_profile_threshold_ms.is_some();
        let text_total_start = text_profile_enabled.then(Instant::now);
        let shape_start = text_profile_enabled.then(Instant::now);
        let color = style.color;
        let shaped_runs = runs
          .clone()
          .or_else(|| self.shaper.shape(&text, &style, &self.font_ctx).ok());
        let shape_ms = shape_start
          .map(|start| start.elapsed().as_secs_f64() * 1000.0)
          .unwrap_or(0.0);
        let paint_start = text_profile_enabled.then(Instant::now);
        let inline_vertical = matches!(
          style.writing_mode,
          crate::style::types::WritingMode::VerticalRl
            | crate::style::types::WritingMode::VerticalLr
            | crate::style::types::WritingMode::SidewaysRl
            | crate::style::types::WritingMode::SidewaysLr
        );
        let (block_baseline, inline_start, inline_len) = if inline_vertical {
          (rect.x() + baseline_offset, rect.y(), rect.height())
        } else {
          (rect.y() + baseline_offset, rect.x(), rect.width())
        };
        if let Some(ref shaped) = shaped_runs {
          if inline_vertical {
            self.paint_shaped_runs_vertical(
              shaped,
              block_baseline,
              inline_start,
              color,
              Some(&style),
            );
          } else {
            self.paint_shaped_runs(shaped, inline_start, block_baseline, color, Some(&style));
          }
        } else {
          if inline_vertical {
            // Fallback: approximate vertical flow by painting horizontal text at the inline start.
            self.paint_text(
              &text,
              Some(&style),
              inline_start,
              rect.y(),
              style.font_size,
              color,
            );
          } else {
            self.paint_text(
              &text,
              Some(&style),
              inline_start,
              block_baseline,
              style.font_size,
              color,
            );
          }
        }
        self.paint_text_decoration(
          &style,
          shaped_runs.as_deref(),
          inline_start,
          block_baseline,
          inline_len,
          inline_vertical,
        );
        self.paint_text_emphasis(
          &style,
          shaped_runs.as_deref(),
          inline_start,
          block_baseline,
          inline_vertical,
        );
        let paint_ms = paint_start
          .map(|start| start.elapsed().as_secs_f64() * 1000.0)
          .unwrap_or(0.0);
        if let (Some(threshold_ms), Some(total_start)) =
          (text_profile_threshold_ms, text_total_start)
        {
          let total_ms = total_start.elapsed().as_secs_f64() * 1000.0;
          if total_ms >= threshold_ms {
            let (run_count, glyph_count) = shaped_runs
              .as_deref()
              .map(|runs| {
                (
                  runs.len(),
                  runs.iter().map(|r| r.glyphs.len()).sum::<usize>(),
                )
              })
              .unwrap_or((0, 0));
            let preview: String = text.chars().take(80).collect();
            eprintln!(
              "text_profile total_ms={total_ms:.2} shape_ms={shape_ms:.2} paint_ms={paint_ms:.2} runs={run_count} glyphs={glyph_count} font_size={:.2} rect=({:.1},{:.1},{:.1},{:.1}) baseline_off={:.2} writing_mode={:?} text=\"{}\"",
              style.font_size,
              rect.x(),
              rect.y(),
              rect.width(),
              rect.height(),
              baseline_offset,
              style.writing_mode,
              preview
            );
          }
        }
      }
      DisplayCommand::Replaced {
        rect,
        replaced_type,
        style,
      } => self.paint_replaced(
        &replaced_type,
        Some(&style),
        rect.x(),
        rect.y(),
        rect.width(),
        rect.height(),
      ),
      DisplayCommand::StackingContext {
        rect: context_rect,
        opacity,
        transform: _,
        transform_3d,
        blend_mode,
        isolated,
        mask,
        filters,
        backdrop_filters,
        radii,
        clip,
        clip_path,
        commands,
      } => {
        let profile_threshold_ms = stack_profile_threshold_ms();
        let profile_enabled = profile_threshold_ms.is_some();
        let total_start = profile_enabled.then(Instant::now);
        let transform_identity = transform_3d.as_ref().map_or(true, Transform3D::is_identity);
        let mut bounds_ms = 0.0;
        let mut translate_ms = 0.0;
        let mut base_paint_ms = 0.0;
        let mut clip_paint_ms = 0.0;
        let mut overflow_clip_ms = 0.0;
        let mut clip_path_ms = 0.0;
        let mut mask_ms = 0.0;
        let mut outline_ms = 0.0;
        let mut filter_ms = 0.0;
        let mut radii_clip_ms = 0.0;
        let mut backdrop_ms = 0.0;
        let mut composite_ms = 0.0;

        let debug_stack = dump_stack_enabled();
        let nested_info = (debug_stack || profile_enabled).then(|| nested_counts(&commands));
        if debug_stack {
          let (nested_cmds, nested_text) = nested_info.unwrap_or((0, 0));
          eprintln!(
            "stack exec: preclip_bounds=({}, {}, {}, {}) nested_cmds={} nested_text={}",
            context_rect.x(),
            context_rect.y(),
            context_rect.width(),
            context_rect.height(),
            nested_cmds,
            nested_text
          );
        }
        if opacity <= 0.0 {
          return Ok(());
        }

        // Fast path: a "transform: (identity)" stacking context with no other effects can be
        // flattened without affecting pixels. This avoids allocating intermediate layers for
        // common patterns like `translate3d(0, 0, 0)`.
        if opacity >= 1.0 - 1e-6
          && transform_identity
          && matches!(blend_mode, MixBlendMode::Normal)
          && !isolated
          && mask.is_none()
          && filters.is_empty()
          && backdrop_filters.is_empty()
          && clip.is_none()
          && clip_path.is_none()
        {
          let mut outlines = Vec::new();
          for cmd in commands {
            if matches!(cmd, DisplayCommand::Outline { .. }) {
              outlines.push(cmd);
            } else {
              self.execute_command(cmd)?;
            }
          }
          for cmd in outlines {
            self.execute_command(cmd)?;
          }
          return Ok(());
        }

        let bounds_start = profile_enabled.then(Instant::now);
        let Some(mut bounds) = stacking_context_bounds(
          &commands,
          &filters,
          &backdrop_filters,
          context_rect,
          transform_3d.as_ref(),
          clip.as_ref(),
          clip_path.as_ref(),
        ) else {
          return Ok(());
        };
        if let Some(start) = bounds_start {
          bounds_ms = start.elapsed().as_secs_f64() * 1000.0;
        }

        // Constrain the stacking layer to the visible viewport (expanded by filter outsets).
        // If a context is entirely outside the viewport, skip it.
        let (filter_l, filter_t, filter_r, filter_b) = compute_filter_outset(&filters, bounds, 1.0);
        let (back_l, back_t, back_r, back_b) =
          compute_filter_outset(&backdrop_filters, bounds, 1.0);
        let expand_l = filter_l.max(back_l);
        let expand_t = filter_t.max(back_t);
        let expand_r = filter_r.max(back_r);
        let expand_b = filter_b.max(back_b);
        let view_bounds = Rect::from_xywh(
          -expand_l,
          -expand_t,
          self.css_width + expand_l + expand_r,
          self.css_height + expand_t + expand_b,
        );
        match bounds.intersection(view_bounds) {
          Some(clipped) if clipped.width() > 0.0 && clipped.height() > 0.0 => bounds = clipped,
          _ => return Ok(()),
        }
        if debug_stack {
          eprintln!(
            "stack exec: postclip_bounds=({}, {}, {}, {})",
            bounds.x(),
            bounds.y(),
            bounds.width(),
            bounds.height()
          );
        }
        if !bounds.width().is_finite()
          || !bounds.height().is_finite()
          || bounds.width() <= 0.0
          || bounds.height() <= 0.0
        {
          return Ok(());
        }

        // Optional debug: report nested command counts before translating
        if dump_stack_enabled() {
          fn count_commands(cmds: &[DisplayCommand]) -> (usize, usize) {
            cmds.iter().fold((0, 0), |(total, text), cmd| match cmd {
              DisplayCommand::StackingContext { commands, .. } => {
                let (t, tx) = count_commands(commands);
                (total + 1 + t, text + tx)
              }
              DisplayCommand::Text { .. } => (total + 1, text + 1),
              _ => (total + 1, text),
            })
          }
          let (total, text) = count_commands(&commands);
          eprintln!(
            "stacking debug: commands={} text={} bounds=({}, {}, {}, {})",
            total,
            text,
            bounds.min_x(),
            bounds.min_y(),
            bounds.width(),
            bounds.height()
          );
        }

        // Reduce layer size by translating to the top-left of the local bounds.
        let offset = Point::new(bounds.min_x(), bounds.min_y());
        let translate_start = profile_enabled.then(Instant::now);
        let translated = translate_commands(commands, -offset.x, -offset.y);
        if let Some(start) = translate_start {
          translate_ms = start.elapsed().as_secs_f64() * 1000.0;
        }

        let device_bounds = self.device_rect(bounds);
        let width = device_bounds.width().ceil().max(0.0);
        let height = device_bounds.height().ceil().max(0.0);
        if width <= 0.0 || height <= 0.0 {
          return Ok(());
        }

        let root_rect = match context_rect
          .intersection(bounds)
          .map(|r| Rect::from_xywh(r.x() - offset.x, r.y() - offset.y, r.width(), r.height()))
        {
          Some(r) if r.width() > 0.0 && r.height() > 0.0 => r,
          _ => Rect::from_xywh(
            bounds.x() - offset.x,
            bounds.y() - offset.y,
            bounds.width(),
            bounds.height(),
          ),
        };
        let device_root_rect = self.device_rect(root_rect);
        let has_clip = clip.is_some();
        let clip_root = clip.map(|clip| clip.clip_root).unwrap_or(false);
        let mut outline_commands = Vec::new();
        let mut unclipped = Vec::new();
        let mut clipped = Vec::new();
        if has_clip {
          for cmd in translated {
            match cmd {
              DisplayCommand::Outline { .. } => outline_commands.push(cmd),
              DisplayCommand::Background { rect, .. } | DisplayCommand::Border { rect, .. }
                if !clip_root
                  && (rect.x() - root_rect.x()).abs() < f32::EPSILON
                  && (rect.y() - root_rect.y()).abs() < f32::EPSILON
                  && (rect.width() - root_rect.width()).abs() < f32::EPSILON
                  && (rect.height() - root_rect.height()).abs() < f32::EPSILON =>
              {
                unclipped.push(cmd);
              }
              _ => clipped.push(cmd),
            }
          }
        } else {
          // Without overflow clipping there's no need for a secondary clip layer; execute
          // commands directly into the base layer while keeping outlines separate.
          for cmd in translated {
            if matches!(cmd, DisplayCommand::Outline { .. }) {
              outline_commands.push(cmd);
            } else {
              unclipped.push(cmd);
            }
          }
        }

        let layer = match Pixmap::new(width as u32, height as u32) {
          Some(p) => p,
          None => return Ok(()),
        };

        let mut base_painter = Painter {
          pixmap: layer,
          scale: self.scale,
          css_width: self.css_width,
          css_height: self.css_height,
          background: Rgba::new(0, 0, 0, 0.0),
          shaper: ShapingPipeline::new(),
          font_ctx: self.font_ctx.clone(),
          image_cache: self.image_cache.clone(),
          text_shape_cache: Arc::clone(&self.text_shape_cache),
          trace: self.trace.clone(),
        };
        let paint_unclipped_start = profile_enabled.then(Instant::now);
        for cmd in unclipped {
          base_painter.execute_command(cmd)?;
        }
        if let Some(start) = paint_unclipped_start {
          base_paint_ms = start.elapsed().as_secs_f64() * 1000.0;
        }

        if !clipped.is_empty() {
          let clip_layer = match Pixmap::new(width as u32, height as u32) {
            Some(p) => p,
            None => return Ok(()),
          };
          let mut clip_painter = Painter {
            pixmap: clip_layer,
            scale: self.scale,
            css_width: self.css_width,
            css_height: self.css_height,
            background: Rgba::new(0, 0, 0, 0.0),
            shaper: ShapingPipeline::new(),
            font_ctx: self.font_ctx.clone(),
            image_cache: self.image_cache.clone(),
            text_shape_cache: Arc::clone(&self.text_shape_cache),
            trace: self.trace.clone(),
          };
          let paint_clipped_start = profile_enabled.then(Instant::now);
          for cmd in clipped {
            clip_painter.execute_command(cmd)?;
          }
          if let Some(start) = paint_clipped_start {
            clip_paint_ms = start.elapsed().as_secs_f64() * 1000.0;
          }
          let mut clip_pixmap = clip_painter.pixmap;
          if let Some(clip) = clip {
            let mut clip_rect = clip.rect;
            let mut clip_radii = clip.radii;
            let clip_x = clip.clip_x;
            let clip_y = clip.clip_y;
            if !clip_x {
              clip_rect =
                Rect::from_xywh(offset.x, clip_rect.y(), bounds.width(), clip_rect.height());
              clip_radii = BorderRadii::ZERO;
            }
            if !clip_y {
              clip_rect =
                Rect::from_xywh(clip_rect.x(), offset.y, clip_rect.width(), bounds.height());
              clip_radii = BorderRadii::ZERO;
            }
            let mut local_clip = Rect::from_xywh(
              clip_rect.x() - offset.x,
              clip_rect.y() - offset.y,
              clip_rect.width(),
              clip_rect.height(),
            );
            let layer_bounds = Rect::from_xywh(0.0, 0.0, bounds.width(), bounds.height());
            local_clip = clip_rect_axes(local_clip, layer_bounds, true, true);
            if local_clip.width() > 0.0 && local_clip.height() > 0.0 {
              let clip_apply_start = profile_enabled.then(Instant::now);
              apply_clip_mask_rect(
                &mut clip_pixmap,
                self.device_rect(local_clip),
                self.device_radii(clip_radii),
              );
              if let Some(start) = clip_apply_start {
                overflow_clip_ms = start.elapsed().as_secs_f64() * 1000.0;
              }
              base_painter.pixmap.draw_pixmap(
                0,
                0,
                clip_pixmap.as_ref(),
                &PixmapPaint::default(),
                Transform::identity(),
                None,
              );
            }
          } else {
            base_painter.pixmap.draw_pixmap(
              0,
              0,
              clip_pixmap.as_ref(),
              &PixmapPaint::default(),
              Transform::identity(),
              None,
            );
          }
        }

        let mut layer_pixmap = base_painter.pixmap;
        if let Some(ref clip_path) = clip_path {
          let clip_path_start = profile_enabled.then(Instant::now);
          if let Some(size) = IntSize::from_wh(layer_pixmap.width(), layer_pixmap.height()) {
            if let Some(mask) = clip_path.mask(self.scale, size, Transform::identity()) {
              layer_pixmap.apply_mask(&mask);
            }
          }
          if let Some(start) = clip_path_start {
            clip_path_ms = start.elapsed().as_secs_f64() * 1000.0;
          }
        }
        if let Some(ref mask_style) = mask {
          let mask_start = profile_enabled.then(Instant::now);
          let local_bounds = Rect::from_xywh(0.0, 0.0, bounds.width(), bounds.height());
          let local_context_rect = Rect::from_xywh(
            context_rect.x() - offset.x,
            context_rect.y() - offset.y,
            context_rect.width(),
            context_rect.height(),
          );
          if let Some(mask) = self.render_mask(
            mask_style,
            local_context_rect,
            local_bounds,
            (layer_pixmap.width(), layer_pixmap.height()),
          ) {
            layer_pixmap.apply_mask(&mask);
          }
          if let Some(start) = mask_start {
            mask_ms = start.elapsed().as_secs_f64() * 1000.0;
          }
        }
        if !outline_commands.is_empty() {
          let mut outline_painter = Painter {
            pixmap: layer_pixmap,
            scale: self.scale,
            css_width: self.css_width,
            css_height: self.css_height,
            background: Rgba::new(0, 0, 0, 0.0),
            shaper: ShapingPipeline::new(),
            font_ctx: self.font_ctx.clone(),
            image_cache: self.image_cache.clone(),
            text_shape_cache: Arc::clone(&self.text_shape_cache),
            trace: self.trace.clone(),
          };
          let outline_start = profile_enabled.then(Instant::now);
          for cmd in outline_commands {
            outline_painter.execute_command(cmd)?;
          }
          if let Some(start) = outline_start {
            outline_ms = start.elapsed().as_secs_f64() * 1000.0;
          }
          layer_pixmap = outline_painter.pixmap;
        }
        if !filters.is_empty() {
          let filter_start = profile_enabled.then(Instant::now);
          apply_filters(&mut layer_pixmap, &filters, self.scale, device_root_rect);
          if let Some(start) = filter_start {
            filter_ms = start.elapsed().as_secs_f64() * 1000.0;
          }
        }

        let device_radii = self.device_radii(radii);
        if !radii.is_zero() || !filters.is_empty() {
          let (device_out_l, device_out_t, device_out_r, device_out_b) =
            compute_filter_outset(&filters, root_rect, self.scale);
          let clip_rect = Rect::from_xywh(
            device_root_rect.x() - device_out_l,
            device_root_rect.y() - device_out_t,
            device_root_rect.width() + device_out_l + device_out_r,
            device_root_rect.height() + device_out_t + device_out_b,
          );
          let radii_clip_start = profile_enabled.then(Instant::now);
          apply_clip_mask_rect(&mut layer_pixmap, clip_rect, device_radii);
          if let Some(start) = radii_clip_start {
            radii_clip_ms = start.elapsed().as_secs_f64() * 1000.0;
          }
        }

        let combined_transform = transform_3d
          .unwrap_or_else(Transform3D::identity)
          .multiply(&Transform3D::translate(offset.x, offset.y, 0.0));
        let affine_2d = combined_transform.to_2d();

        let src_quad = [
          Point::new(0.0, 0.0),
          Point::new(layer_pixmap.width() as f32, 0.0),
          Point::new(layer_pixmap.width() as f32, layer_pixmap.height() as f32),
          Point::new(0.0, layer_pixmap.height() as f32),
        ];
        let mut dest_quad_device = src_quad;
        let mut projected = true;
        let layer_bounds_css = Rect::from_xywh(0.0, 0.0, bounds.width(), bounds.height());
        for (idx, corner) in rect_corners(layer_bounds_css).iter().enumerate() {
          let (tx, ty, _tz, tw) = combined_transform.transform_point(corner.x, corner.y, 0.0);
          if !tx.is_finite() || !ty.is_finite() || tw.abs() < 1e-6 || !tw.is_finite() {
            projected = false;
            break;
          }
          dest_quad_device[idx] = Point::new(tx / tw * self.scale, ty / tw * self.scale);
        }
        if !projected {
          dest_quad_device = src_quad;
        }
        let dest_bounds_device = quad_bounds(&dest_quad_device);

        if !backdrop_filters.is_empty() {
          let backdrop_start = profile_enabled.then(Instant::now);
          apply_backdrop_filters(
            &mut self.pixmap,
            &dest_bounds_device,
            &backdrop_filters,
            device_radii,
            self.scale,
            root_rect,
          );
          if let Some(start) = backdrop_start {
            backdrop_ms = start.elapsed().as_secs_f64() * 1000.0;
          }
        }
        let composite_start = profile_enabled.then(Instant::now);
        let fallback_blend = if isolated {
          SkiaBlendMode::SourceOver
        } else {
          map_blend_mode(blend_mode)
        };
        if let Some(affine) = affine_2d {
          let mut final_transform = self
            .device_transform(Some(transform2d_to_skia(affine)))
            .unwrap_or_else(Transform::identity);
          if is_hsl_blend(blend_mode) && !isolated {
            if let Some(mut transformed) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
              let mut paint = PixmapPaint::default();
              paint.opacity = 1.0;
              paint.blend_mode = SkiaBlendMode::SourceOver;
              transformed.draw_pixmap(0, 0, layer_pixmap.as_ref(), &paint, final_transform, None);
              composite_hsl_layer(
                &mut self.pixmap,
                &transformed,
                opacity.min(1.0),
                blend_mode,
                Some(dest_bounds_device),
              );
            } else {
              let mut paint = PixmapPaint::default();
              paint.opacity = opacity.min(1.0);
              paint.blend_mode = fallback_blend;
              self
                .pixmap
                .draw_pixmap(0, 0, layer_pixmap.as_ref(), &paint, final_transform, None);
            }
          } else {
            let mut paint = PixmapPaint::default();
            paint.opacity = opacity.min(1.0);
            paint.blend_mode = fallback_blend;
            self
              .pixmap
              .draw_pixmap(0, 0, layer_pixmap.as_ref(), &paint, final_transform, None);
          }
        } else if let Some(homography) = Homography::from_quads(src_quad, dest_quad_device) {
          let dst_quad: [(f32, f32); 4] = dest_quad_device.map(|p| (p.x, p.y));
          let target_size = (self.pixmap.width(), self.pixmap.height());
          if is_hsl_blend(blend_mode) && !isolated {
            if let Some(warped) = warp_pixmap(
              &layer_pixmap,
              &homography,
              &dst_quad,
              target_size,
              None,
            ) {
              if let Some(mut transformed) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
                let mut paint = PixmapPaint::default();
                paint.opacity = 1.0;
                paint.blend_mode = SkiaBlendMode::SourceOver;
                transformed.draw_pixmap(
                  warped.offset.0,
                  warped.offset.1,
                  warped.pixmap.as_ref(),
                  &paint,
                  Transform::identity(),
                  None,
                );
                composite_hsl_layer(
                  &mut self.pixmap,
                  &transformed,
                  opacity.min(1.0),
                  blend_mode,
                  Some(dest_bounds_device),
                );
              } else {
                let mut paint = PixmapPaint::default();
                paint.opacity = opacity.min(1.0);
                paint.blend_mode = fallback_blend;
                self.pixmap.draw_pixmap(
                  warped.offset.0,
                  warped.offset.1,
                  warped.pixmap.as_ref(),
                  &paint,
                  Transform::identity(),
                  None,
                );
              }
            }
          } else if let Some(warped) = warp_pixmap(
            &layer_pixmap,
            &homography,
            &dst_quad,
            target_size,
            None,
          ) {
              let mut paint = PixmapPaint::default();
              paint.opacity = opacity.min(1.0);
              paint.blend_mode = fallback_blend;
              self
                .pixmap
                .draw_pixmap(warped.offset.0, warped.offset.1, warped.pixmap.as_ref(), &paint, Transform::identity(), None);
          }
        }
        if let Some(start) = composite_start {
          composite_ms = start.elapsed().as_secs_f64() * 1000.0;
        }

        if let (Some(threshold_ms), Some(total_start)) = (profile_threshold_ms, total_start) {
          let total_ms = total_start.elapsed().as_secs_f64() * 1000.0;
          if total_ms >= threshold_ms {
            let (nested_cmds, nested_text) = nested_info.unwrap_or((0, 0));
            eprintln!(
              "stack_profile total_ms={total_ms:.2} bounds_ms={bounds_ms:.2} translate_ms={translate_ms:.2} base_paint_ms={base_paint_ms:.2} clip_paint_ms={clip_paint_ms:.2} overflow_clip_ms={overflow_clip_ms:.2} clip_path_ms={clip_path_ms:.2} mask_ms={mask_ms:.2} outline_ms={outline_ms:.2} filter_ms={filter_ms:.2} radii_clip_ms={radii_clip_ms:.2} backdrop_ms={backdrop_ms:.2} composite_ms={composite_ms:.2} layer_px={}x{} nested_cmds={nested_cmds} nested_text={nested_text} filters={} backdrop_filters={} clip={} clip_path={} mask={} opacity={:.2} transform_identity={} blend_mode={:?} isolated={}",
              layer_pixmap.width(),
              layer_pixmap.height(),
              filters.len(),
              backdrop_filters.len(),
              clip.is_some(),
              clip_path.is_some(),
              mask.is_some(),
              opacity,
              transform_identity,
              blend_mode,
              isolated
            );
          }
        }
      }
    }
    if let (Some(threshold_ms), Some(start), Some(summary)) = (
      cmd_profile_threshold_ms,
      cmd_profile_start,
      cmd_profile_summary,
    ) {
      let elapsed_ms = start.elapsed().as_secs_f64() * 1000.0;
      if elapsed_ms >= threshold_ms {
        eprintln!("cmd_profile ms={elapsed_ms:.2} {summary}");
      }
    }
    Ok(())
  }

  fn render_mask(
    &self,
    style: &ComputedStyle,
    css_bounds: Rect,
    layer_bounds: Rect,
    device_size: (u32, u32),
  ) -> Option<Mask> {
    let viewport = (self.css_width, self.css_height);
    let rects = background_rects(
      css_bounds.x(),
      css_bounds.y(),
      css_bounds.width(),
      css_bounds.height(),
      style,
      Some(viewport),
    );
    let mut combined: Option<Mask> = None;
    let canvas_clip = layer_bounds;

    for layer in style.mask_layers.iter().rev() {
      let Some(image) = &layer.image else { continue };

      let origin_rect_css = match layer.origin {
        MaskOrigin::BorderBox => rects.border,
        MaskOrigin::PaddingBox => rects.padding,
        MaskOrigin::ContentBox => rects.content,
      };
      let clip_rect_css = match layer.clip {
        MaskClip::BorderBox => rects.border,
        MaskClip::PaddingBox => rects.padding,
        MaskClip::ContentBox | MaskClip::Text => rects.content,
        MaskClip::NoClip => canvas_clip,
      };
      if origin_rect_css.width() <= 0.0
        || origin_rect_css.height() <= 0.0
        || clip_rect_css.width() <= 0.0
        || clip_rect_css.height() <= 0.0
      {
        continue;
      }

      let mut dummy = BackgroundLayer::default();
      dummy.size = layer.size;
      let (mut tile_w, mut tile_h) = compute_background_size(
        &dummy,
        style.font_size,
        style.root_font_size,
        viewport,
        origin_rect_css.width(),
        origin_rect_css.height(),
        0.0,
        0.0,
      );
      if tile_w <= 0.0 || tile_h <= 0.0 {
        continue;
      }

      let mut rounded_x = false;
      let mut rounded_y = false;
      if layer.repeat.x == BackgroundRepeatKeyword::Round {
        tile_w = round_tile_length(origin_rect_css.width(), tile_w);
        rounded_x = true;
      }
      if layer.repeat.y == BackgroundRepeatKeyword::Round {
        tile_h = round_tile_length(origin_rect_css.height(), tile_h);
        rounded_y = true;
      }
      if rounded_x ^ rounded_y
        && matches!(
          layer.size,
          BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
        )
      {
        let aspect = 1.0;
        if rounded_x {
          tile_h = tile_w / aspect;
        } else {
          tile_w = tile_h * aspect;
        }
      }

      let (offset_x, offset_y) = resolve_background_offset(
        layer.position,
        origin_rect_css.width(),
        origin_rect_css.height(),
        tile_w,
        tile_h,
        style.font_size,
        style.root_font_size,
        viewport,
      );

      let positions_x = tile_positions(
        layer.repeat.x,
        origin_rect_css.x(),
        origin_rect_css.width(),
        tile_w,
        offset_x,
        clip_rect_css.min_x(),
        clip_rect_css.max_x(),
      );
      let positions_y = tile_positions(
        layer.repeat.y,
        origin_rect_css.y(),
        origin_rect_css.height(),
        tile_h,
        offset_y,
        clip_rect_css.min_y(),
        clip_rect_css.max_y(),
      );

      let pixmap_w = tile_w.ceil().max(1.0) as u32;
      let pixmap_h = tile_h.ceil().max(1.0) as u32;
      let tile = match image {
        BackgroundImage::LinearGradient { .. }
        | BackgroundImage::RepeatingLinearGradient { .. }
        | BackgroundImage::RadialGradient { .. }
        | BackgroundImage::RepeatingRadialGradient { .. }
        | BackgroundImage::ConicGradient { .. }
        | BackgroundImage::RepeatingConicGradient { .. } => {
          self.render_generated_image(image, style, pixmap_w, pixmap_h)
        }
        BackgroundImage::Url(_) | BackgroundImage::None => None,
      };
      let Some(tile) = tile else { continue };
      let Some(mask_tile) = mask_tile_from_image(&tile, layer.mode) else {
        continue;
      };

      let mut mask_pixmap = Pixmap::new(device_size.0, device_size.1)?;
      for ty in positions_y.iter().copied() {
        for tx in positions_x.iter().copied() {
          paint_mask_tile(
            &mut mask_pixmap,
            &mask_tile,
            tx,
            ty,
            tile_w,
            tile_h,
            clip_rect_css,
            self.scale,
          );
        }
      }

      let layer_mask = Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha);
      if let Some(dest) = combined.as_mut() {
        apply_mask_composite(dest, &layer_mask, layer.composite);
      } else {
        combined = Some(layer_mask);
      }
    }

    combined
  }

  /// Paints the background of a fragment
  fn paint_background(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
    let rects = background_rects(
      x,
      y,
      width,
      height,
      style,
      Some((self.css_width, self.css_height)),
    );
    let scaled_rects = self.scale_background_rects(&rects);
    let fallback_layer = BackgroundLayer::default();
    let color_clip_layer = style.background_layers.first().unwrap_or(&fallback_layer);
    let color_clip_rect = match color_clip_layer.clip {
      crate::style::types::BackgroundBox::BorderBox => scaled_rects.border,
      crate::style::types::BackgroundBox::PaddingBox => scaled_rects.padding,
      crate::style::types::BackgroundBox::ContentBox => scaled_rects.content,
    };

    if color_clip_rect.width() <= 0.0 || color_clip_rect.height() <= 0.0 {
      return;
    }

    let color_clip_radii = self.device_radii(resolve_clip_radii(
      style,
      &rects,
      color_clip_layer.clip,
      Some((self.css_width, self.css_height)),
    ));

    if style.background_color.alpha_u8() > 0 {
      let _ = fill_rounded_rect(
        &mut self.pixmap,
        color_clip_rect.x(),
        color_clip_rect.y(),
        color_clip_rect.width(),
        color_clip_rect.height(),
        &color_clip_radii,
        style.background_color,
      );
    }

    for layer in style.background_layers.iter().rev() {
      if let Some(image) = &layer.image {
        self.paint_background_image_layer(&rects, style, layer, image);
      }
    }
  }

  fn scale_background_rects(&self, rects: &BackgroundRects) -> BackgroundRects {
    BackgroundRects {
      border: self.device_rect(rects.border),
      padding: self.device_rect(rects.padding),
      content: self.device_rect(rects.content),
    }
  }

  fn paint_background_image_layer(
    &mut self,
    rects: &BackgroundRects,
    style: &ComputedStyle,
    layer: &BackgroundLayer,
    bg: &BackgroundImage,
  ) {
    let is_local = layer.attachment == BackgroundAttachment::Local;
    let clip_box = if is_local {
      match layer.clip {
        crate::style::types::BackgroundBox::ContentBox => {
          crate::style::types::BackgroundBox::ContentBox
        }
        _ => crate::style::types::BackgroundBox::PaddingBox,
      }
    } else {
      layer.clip
    };
    let clip_rect_css = match clip_box {
      crate::style::types::BackgroundBox::BorderBox => rects.border,
      crate::style::types::BackgroundBox::PaddingBox => rects.padding,
      crate::style::types::BackgroundBox::ContentBox => rects.content,
    };
    let clip_radii = resolve_clip_radii(
      style,
      rects,
      clip_box,
      Some((self.css_width, self.css_height)),
    );
    let origin_rect_css = if layer.attachment == BackgroundAttachment::Fixed {
      Rect::from_xywh(0.0, 0.0, self.css_width, self.css_height)
    } else if is_local {
      match layer.origin {
        crate::style::types::BackgroundBox::ContentBox => rects.content,
        _ => rects.padding,
      }
    } else {
      match layer.origin {
        crate::style::types::BackgroundBox::BorderBox => rects.border,
        crate::style::types::BackgroundBox::PaddingBox => rects.padding,
        crate::style::types::BackgroundBox::ContentBox => rects.content,
      }
    };

    if clip_rect_css.width() <= 0.0 || clip_rect_css.height() <= 0.0 {
      return;
    }
    if origin_rect_css.width() <= 0.0 || origin_rect_css.height() <= 0.0 {
      return;
    }

    let clip_rect = self.device_rect(clip_rect_css);
    let clip_radii = self.device_radii(clip_radii);

    let clip_mask = if clip_radii.is_zero() {
      None
    } else {
      build_rounded_rect_mask(
        clip_rect,
        clip_radii,
        self.pixmap.width(),
        self.pixmap.height(),
      )
    };

    match bg {
      BackgroundImage::LinearGradient { .. }
      | BackgroundImage::RepeatingLinearGradient { .. }
      | BackgroundImage::RadialGradient { .. }
      | BackgroundImage::RepeatingRadialGradient { .. }
      | BackgroundImage::ConicGradient { .. }
      | BackgroundImage::RepeatingConicGradient { .. } => {
        let (mut tile_w, mut tile_h) = compute_background_size(
          layer,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
          origin_rect_css.width(),
          origin_rect_css.height(),
          0.0,
          0.0,
        );
        if tile_w <= 0.0 || tile_h <= 0.0 {
          return;
        }

        let mut rounded_x = false;
        let mut rounded_y = false;
        if layer.repeat.x == BackgroundRepeatKeyword::Round {
          tile_w = round_tile_length(origin_rect_css.width(), tile_w);
          rounded_x = true;
        }
        if layer.repeat.y == BackgroundRepeatKeyword::Round {
          tile_h = round_tile_length(origin_rect_css.height(), tile_h);
          rounded_y = true;
        }
        if rounded_x ^ rounded_y
          && matches!(
            layer.size,
            BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
          )
        {
          let aspect = 1.0;
          if rounded_x {
            tile_h = tile_w / aspect;
          } else {
            tile_w = tile_h * aspect;
          }
        }

        let (offset_x, offset_y) = resolve_background_offset(
          layer.position,
          origin_rect_css.width(),
          origin_rect_css.height(),
          tile_w,
          tile_h,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
        );

        let positions_x = tile_positions(
          layer.repeat.x,
          origin_rect_css.x(),
          origin_rect_css.width(),
          tile_w,
          offset_x,
          clip_rect_css.min_x(),
          clip_rect_css.max_x(),
        );
        let positions_y = tile_positions(
          layer.repeat.y,
          origin_rect_css.y(),
          origin_rect_css.height(),
          tile_h,
          offset_y,
          clip_rect_css.min_y(),
          clip_rect_css.max_y(),
        );

        let pixmap_w = tile_w.ceil().max(1.0) as u32;
        let pixmap_h = tile_h.ceil().max(1.0) as u32;
        let Some(pixmap) = self.render_generated_image(bg, style, pixmap_w, pixmap_h) else {
          return;
        };

        let max_x = clip_rect_css.max_x();
        let max_y = clip_rect_css.max_y();
        let quality = Self::filter_quality_for_image(Some(style));

        for ty in positions_y.iter().copied() {
          for tx in positions_x.iter().copied() {
            if tx >= max_x || ty >= max_y {
              continue;
            }
            self.paint_background_tile(
              &pixmap,
              tx,
              ty,
              tile_w,
              tile_h,
              clip_rect_css,
              clip_mask.as_ref(),
              layer.blend_mode,
              quality,
            );
          }
        }
      }
      BackgroundImage::None => {
        return;
      }
      BackgroundImage::Url(src) => {
        let image = match self.image_cache.load(src) {
          Ok(img) => img,
          Err(_) => return,
        };

        let orientation = style.image_orientation.resolve(image.orientation, true);
        let (img_w_raw, img_h_raw) = image.oriented_dimensions(orientation);
        let Some((img_w, img_h)) =
          image.css_dimensions(orientation, &style.image_resolution, self.scale, None)
        else {
          return;
        };
        if img_w <= 0.0 || img_h <= 0.0 || img_w_raw == 0 || img_h_raw == 0 {
          return;
        }

        let pixmap = match Self::dynamic_image_to_pixmap(&image, orientation) {
          Some(p) => p,
          None => return,
        };

        let (mut tile_w, mut tile_h) = compute_background_size(
          layer,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
          origin_rect_css.width(),
          origin_rect_css.height(),
          img_w,
          img_h,
        );
        if tile_w <= 0.0 || tile_h <= 0.0 {
          return;
        }

        let mut rounded_x = false;
        let mut rounded_y = false;
        if layer.repeat.x == BackgroundRepeatKeyword::Round {
          tile_w = round_tile_length(origin_rect_css.width(), tile_w);
          rounded_x = true;
        }
        if layer.repeat.y == BackgroundRepeatKeyword::Round {
          tile_h = round_tile_length(origin_rect_css.height(), tile_h);
          rounded_y = true;
        }
        if rounded_x ^ rounded_y
          && matches!(
            layer.size,
            BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
          )
        {
          let aspect = if img_h != 0.0 { img_w / img_h } else { 1.0 };
          if rounded_x {
            tile_h = tile_w / aspect;
          } else {
            tile_w = tile_h * aspect;
          }
        }

        let (offset_x, offset_y) = resolve_background_offset(
          layer.position,
          origin_rect_css.width(),
          origin_rect_css.height(),
          tile_w,
          tile_h,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
        );

        let positions_x = tile_positions(
          layer.repeat.x,
          origin_rect_css.x(),
          origin_rect_css.width(),
          tile_w,
          offset_x,
          clip_rect_css.min_x(),
          clip_rect_css.max_x(),
        );
        let positions_y = tile_positions(
          layer.repeat.y,
          origin_rect_css.y(),
          origin_rect_css.height(),
          tile_h,
          offset_y,
          clip_rect_css.min_y(),
          clip_rect_css.max_y(),
        );

        let max_x = clip_rect_css.max_x();
        let max_y = clip_rect_css.max_y();
        let quality = Self::filter_quality_for_image(Some(style));

        for ty in positions_y.iter().copied() {
          for tx in positions_x.iter().copied() {
            if tx >= max_x || ty >= max_y {
              continue;
            }
            self.paint_background_tile(
              &pixmap,
              tx,
              ty,
              tile_w,
              tile_h,
              clip_rect_css,
              clip_mask.as_ref(),
              layer.blend_mode,
              quality,
            );
          }
        }
      }
    }
  }

  #[allow(dead_code)]
  fn paint_linear_gradient(
    &mut self,
    gradient_rect: Rect,
    paint_rect: Rect,
    clip_mask: Option<&Mask>,
    angle: f32,
    stops: &[(f32, Rgba)],
    spread: SpreadMode,
    blend_mode: MixBlendMode,
  ) {
    if stops.is_empty() {
      return;
    }

    let gradient_rect = self.device_rect(gradient_rect);
    let paint_rect = self.device_rect(paint_rect);

    let skia_stops = gradient_stops(stops);
    let rad = angle.to_radians();
    let dx = rad.sin();
    let dy = -rad.cos(); // CSS 0deg points up
    let len = 0.5 * (gradient_rect.width() * dx.abs() + gradient_rect.height() * dy.abs());
    let cx = gradient_rect.x() + gradient_rect.width() / 2.0;
    let cy = gradient_rect.y() + gradient_rect.height() / 2.0;

    let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
    let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
    let Some(shader) = LinearGradient::new(start, end, skia_stops, spread, Transform::identity())
    else {
      return;
    };

    let Some(skia_rect) = SkiaRect::from_xywh(
      paint_rect.x(),
      paint_rect.y(),
      paint_rect.width(),
      paint_rect.height(),
    ) else {
      return;
    };
    let path = PathBuilder::from_rect(skia_rect);

    let mut paint = Paint::default();
    paint.shader = shader;
    paint.anti_alias = true;

    if is_hsl_blend(blend_mode) {
      if let Some(mut layer) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
        paint.blend_mode = SkiaBlendMode::SourceOver;
        layer.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          clip_mask,
        );
        composite_hsl_layer(&mut self.pixmap, &layer, 1.0, blend_mode, Some(paint_rect));
      } else {
        paint.blend_mode = map_blend_mode(blend_mode);
        self.pixmap.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          clip_mask,
        );
      }
    } else {
      paint.blend_mode = map_blend_mode(blend_mode);
      self.pixmap.fill_path(
        &path,
        &paint,
        tiny_skia::FillRule::Winding,
        Transform::identity(),
        clip_mask,
      );
    }
  }

  #[allow(dead_code)]
  fn paint_conic_gradient(
    &mut self,
    gradient_rect: Rect,
    paint_rect: Rect,
    clip_mask: Option<&Mask>,
    position: &BackgroundPosition,
    from_angle_deg: f32,
    stops: &[(f32, Rgba)],
    repeating: bool,
    font_size: f32,
    root_font_size: f32,
    blend_mode: MixBlendMode,
  ) {
    if stops.is_empty() {
      return;
    }
    let paint_rect = self.device_rect(paint_rect);
    let gradient_rect = self.device_rect(gradient_rect);
    let width = paint_rect.width().ceil() as u32;
    let height = paint_rect.height().ceil() as u32;
    if width == 0 || height == 0 {
      return;
    }

    let center = resolve_gradient_center(
      gradient_rect,
      position,
      paint_rect,
      font_size,
      root_font_size,
      (self.css_width, self.css_height),
    );
    let mut pix = match Pixmap::new(width, height) {
      Some(p) => p,
      None => return,
    };

    let start_angle = from_angle_deg.to_radians();
    let period = if repeating {
      stops.last().map(|(p, _)| *p).unwrap_or(1.0).max(1e-6)
    } else {
      1.0
    };

    let data = pix.pixels_mut();
    let transparent = PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
    for y in 0..height {
      for x in 0..width {
        let dx = x as f32 + 0.5 - center.x;
        let dy = y as f32 + 0.5 - center.y;
        let angle = dx.atan2(-dy) + start_angle;
        let mut t = (angle / (2.0 * std::f32::consts::PI)).rem_euclid(1.0);
        t *= period;
        let color = sample_stops(stops, t, repeating, period);
        let idx = (y * width + x) as usize;
        data[idx] = PremultipliedColorU8::from_rgba(
          color.r,
          color.g,
          color.b,
          (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
        )
        .unwrap_or(transparent);
      }
    }

    let mut paint = PixmapPaint::default();
    paint.blend_mode = map_blend_mode(blend_mode);
    let transform = Transform::from_translate(paint_rect.x(), paint_rect.y());
    self.pixmap.draw_pixmap(
      0,
      0,
      pix.as_ref(),
      &paint,
      transform,
      clip_mask.cloned().as_ref(),
    );
  }

  #[allow(dead_code)]
  fn paint_radial_gradient(
    &mut self,
    gradient_rect: Rect,
    paint_rect: Rect,
    clip_mask: Option<&Mask>,
    position: &BackgroundPosition,
    size: &RadialGradientSize,
    shape: RadialGradientShape,
    font_size: f32,
    stops: &[(f32, Rgba)],
    spread: SpreadMode,
    blend_mode: MixBlendMode,
  ) {
    if stops.is_empty() {
      return;
    }

    let gradient_rect = self.device_rect(gradient_rect);
    let paint_rect = self.device_rect(paint_rect);
    let skia_stops = gradient_stops(stops);
    let (cx, cy, radius_x, radius_y) = radial_geometry(
      gradient_rect,
      position,
      size,
      shape,
      font_size,
      font_size,
      (self.css_width, self.css_height),
    );
    let transform = Transform::from_translate(cx, cy).pre_scale(radius_x, radius_y);
    let Some(shader) = RadialGradient::new(
      tiny_skia::Point::from_xy(0.0, 0.0),
      tiny_skia::Point::from_xy(0.0, 0.0),
      1.0,
      skia_stops,
      spread,
      transform,
    ) else {
      return;
    };

    let Some(skia_rect) = SkiaRect::from_xywh(
      paint_rect.x(),
      paint_rect.y(),
      paint_rect.width(),
      paint_rect.height(),
    ) else {
      return;
    };
    let path = PathBuilder::from_rect(skia_rect);

    let mut paint = Paint::default();
    paint.shader = shader;
    paint.anti_alias = true;

    if is_hsl_blend(blend_mode) {
      if let Some(mut layer) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
        paint.blend_mode = SkiaBlendMode::SourceOver;
        layer.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          clip_mask,
        );
        composite_hsl_layer(&mut self.pixmap, &layer, 1.0, blend_mode, Some(paint_rect));
      } else {
        paint.blend_mode = map_blend_mode(blend_mode);
        self.pixmap.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          clip_mask,
        );
      }
    } else {
      paint.blend_mode = map_blend_mode(blend_mode);
      self.pixmap.fill_path(
        &path,
        &paint,
        tiny_skia::FillRule::Winding,
        Transform::identity(),
        clip_mask,
      );
    }
  }

  fn paint_background_tile(
    &mut self,
    pixmap: &Pixmap,
    tile_x: f32,
    tile_y: f32,
    tile_w: f32,
    tile_h: f32,
    clip: Rect,
    mask: Option<&Mask>,
    blend_mode: MixBlendMode,
    quality: FilterQuality,
  ) {
    if tile_w <= 0.0 || tile_h <= 0.0 {
      return;
    }

    let mut tile_rect = self.device_rect(Rect::from_xywh(tile_x, tile_y, tile_w, tile_h));
    let clip_rect = self.device_rect(clip);
    let Some(intersection) = tile_rect.intersection(clip_rect) else {
      return;
    };
    if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
      return;
    }

    if quality == FilterQuality::Nearest
      && (tile_rect.width() > pixmap.width() as f32 || tile_rect.height() > pixmap.height() as f32)
    {
      let (snapped_w, offset_x) = snap_upscale(tile_rect.width(), pixmap.width() as f32)
        .unwrap_or_else(|| (tile_rect.width(), 0.0));
      let (snapped_h, offset_y) = snap_upscale(tile_rect.height(), pixmap.height() as f32)
        .unwrap_or_else(|| (tile_rect.height(), 0.0));
      tile_rect = Rect::from_xywh(
        tile_rect.x() + offset_x,
        tile_rect.y() + offset_y,
        snapped_w,
        snapped_h,
      );
    }

    let scale_x = tile_rect.width() / pixmap.width() as f32;
    let scale_y = tile_rect.height() / pixmap.height() as f32;
    if !scale_x.is_finite() || !scale_y.is_finite() {
      return;
    }

    let mut paint = Paint::default();
    paint.shader = Pattern::new(
      pixmap.as_ref(),
      SpreadMode::Pad,
      quality,
      1.0,
      Transform::from_row(scale_x, 0.0, 0.0, scale_y, tile_rect.x(), tile_rect.y()),
    );
    paint.anti_alias = false;

    if let Some(rect) = SkiaRect::from_xywh(
      intersection.x(),
      intersection.y(),
      intersection.width(),
      intersection.height(),
    ) {
      if is_hsl_blend(blend_mode) {
        if let Some(mut layer) = Pixmap::new(self.pixmap.width(), self.pixmap.height()) {
          paint.blend_mode = SkiaBlendMode::SourceOver;
          layer.fill_rect(rect, &paint, Transform::identity(), mask);
          composite_hsl_layer(
            &mut self.pixmap,
            &layer,
            1.0,
            blend_mode,
            Some(intersection),
          );
        } else {
          paint.blend_mode = map_blend_mode(blend_mode);
          self
            .pixmap
            .fill_rect(rect, &paint, Transform::identity(), mask);
        }
      } else {
        paint.blend_mode = map_blend_mode(blend_mode);
        self
          .pixmap
          .fill_rect(rect, &paint, Transform::identity(), mask);
      }
    }
  }

  /// Paints the borders of a fragment
  fn paint_borders(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
    // Only paint if there are borders
    let top_css = style.border_top_width.to_px();
    let right_css = style.border_right_width.to_px();
    let bottom_css = style.border_bottom_width.to_px();
    let left_css = style.border_left_width.to_px();

    if matches!(style.border_image.source, BorderImageSource::Image(_)) {
      if self.paint_border_image(
        x, y, width, height, style, top_css, right_css, bottom_css, left_css,
      ) {
        return;
      }
    }

    let top = top_css * self.scale;
    let right = right_css * self.scale;
    let bottom = bottom_css * self.scale;
    let left = left_css * self.scale;

    let x = self.device_length(x);
    let y = self.device_length(y);
    let width = self.device_length(width);
    let height = self.device_length(height);

    if top <= 0.0 && right <= 0.0 && bottom <= 0.0 && left <= 0.0 {
      return;
    }

    // Center strokes on the border edges so paint remains within the border box.
    let top_center = y + top * 0.5;
    let bottom_center = y + height - bottom * 0.5;
    let left_center = x + left * 0.5;
    let right_center = x + width - right * 0.5;
    let top_start = left_center;
    let top_end = right_center;
    let bottom_start = left_center;
    let bottom_end = right_center;
    let left_start = top_center;
    let left_end = bottom_center;
    let right_start = top_center;
    let right_end = bottom_center;

    // Top border
    if top > 0.0 {
      self.paint_border_edge(
        BorderEdge::Top,
        top_start,
        top_center,
        top_end,
        top_center,
        top,
        style.border_top_style,
        &style.border_top_color,
      );
    }

    // Right border
    if right > 0.0 {
      self.paint_border_edge(
        BorderEdge::Right,
        right_center,
        right_start,
        right_center,
        right_end,
        right,
        style.border_right_style,
        &style.border_right_color,
      );
    }

    // Bottom border
    if bottom > 0.0 {
      self.paint_border_edge(
        BorderEdge::Bottom,
        bottom_start,
        bottom_center,
        bottom_end,
        bottom_center,
        bottom,
        style.border_bottom_style,
        &style.border_bottom_color,
      );
    }

    // Left border
    if left > 0.0 {
      self.paint_border_edge(
        BorderEdge::Left,
        left_center,
        left_start,
        left_center,
        left_end,
        left,
        style.border_left_style,
        &style.border_left_color,
      );
    }
  }

  fn paint_border_image(
    &mut self,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
    style: &ComputedStyle,
    top: f32,
    right: f32,
    bottom: f32,
    left: f32,
  ) -> bool {
    let BorderImage {
      source,
      slice,
      width: img_widths,
      outset,
      repeat,
    } = &style.border_image;

    let border_widths = BorderWidths {
      top,
      right,
      bottom,
      left,
    };

    let viewport = (self.css_width, self.css_height);
    let target_widths = resolve_border_image_widths(
      img_widths,
      border_widths,
      width,
      height,
      style.font_size,
      style.root_font_size,
      viewport,
    );
    let outsets = resolve_border_image_outset(
      outset,
      target_widths,
      style.font_size,
      style.root_font_size,
      viewport,
    );

    let outer_rect = Rect::from_xywh(
      x - outsets.left,
      y - outsets.top,
      width + outsets.left + outsets.right,
      height + outsets.top + outsets.bottom,
    );
    let inner_rect = Rect::from_xywh(
      outer_rect.x() + target_widths.left,
      outer_rect.y() + target_widths.top,
      outer_rect.width() - target_widths.left - target_widths.right,
      outer_rect.height() - target_widths.top - target_widths.bottom,
    );
    if inner_rect.width() <= 0.0 || inner_rect.height() <= 0.0 {
      return true;
    }

    let bg = match source {
      BorderImageSource::Image(img) => img,
      BorderImageSource::None => return false,
    };

    let (pixmap, img_w, img_h) = match bg.as_ref() {
      BackgroundImage::Url(src) => {
        let image = match self.image_cache.load(src) {
          Ok(img) => img,
          Err(_) => return false,
        };
        let orientation = style.image_orientation.resolve(image.orientation, true);
        let Some(pixmap) = Self::dynamic_image_to_pixmap(&image, orientation) else {
          return false;
        };
        let img_w = pixmap.width();
        let img_h = pixmap.height();
        if img_w == 0 || img_h == 0 {
          return false;
        }
        (pixmap, img_w, img_h)
      }
      BackgroundImage::LinearGradient { .. }
      | BackgroundImage::RepeatingLinearGradient { .. }
      | BackgroundImage::RadialGradient { .. }
      | BackgroundImage::RepeatingRadialGradient { .. }
      | BackgroundImage::ConicGradient { .. }
      | BackgroundImage::RepeatingConicGradient { .. } => {
        let img_w = outer_rect.width().max(1.0).round() as u32;
        let img_h = outer_rect.height().max(1.0).round() as u32;
        let Some(pixmap) = self.render_generated_image(bg, style, img_w, img_h) else {
          return false;
        };
        (pixmap, img_w, img_h)
      }
      BackgroundImage::None => return false,
    };

    let slice_top = resolve_slice_value(slice.top, img_h);
    let slice_right = resolve_slice_value(slice.right, img_w);
    let slice_bottom = resolve_slice_value(slice.bottom, img_h);
    let slice_left = resolve_slice_value(slice.left, img_w);

    let sx0 = 0.0;
    let sx1 = slice_left.min(img_w as f32);
    let sx2 = (img_w as f32 - slice_right).max(sx1);
    let sx3 = img_w as f32;

    let sy0 = 0.0;
    let sy1 = slice_top.min(img_h as f32);
    let sy2 = (img_h as f32 - slice_bottom).max(sy1);
    let sy3 = img_h as f32;

    let (repeat_x, repeat_y) = *repeat;

    // Corners
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx0, sy0, sx1 - sx0, sy1 - sy0),
      Rect::from_xywh(
        outer_rect.x(),
        outer_rect.y(),
        target_widths.left,
        target_widths.top,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx2, sy0, sx3 - sx2, sy1 - sy0),
      Rect::from_xywh(
        outer_rect.x() + outer_rect.width() - target_widths.right,
        outer_rect.y(),
        target_widths.right,
        target_widths.top,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx0, sy2, sx1 - sx0, sy3 - sy2),
      Rect::from_xywh(
        outer_rect.x(),
        outer_rect.y() + outer_rect.height() - target_widths.bottom,
        target_widths.left,
        target_widths.bottom,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx2, sy2, sx3 - sx2, sy3 - sy2),
      Rect::from_xywh(
        outer_rect.x() + outer_rect.width() - target_widths.right,
        outer_rect.y() + outer_rect.height() - target_widths.bottom,
        target_widths.right,
        target_widths.bottom,
      ),
      BorderImageRepeat::Stretch,
      BorderImageRepeat::Stretch,
    );

    // Edges
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx1, sy0, sx2 - sx1, sy1 - sy0),
      Rect::from_xywh(
        inner_rect.x(),
        outer_rect.y(),
        inner_rect.width(),
        target_widths.top,
      ),
      repeat_x,
      BorderImageRepeat::Stretch,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx1, sy2, sx2 - sx1, sy3 - sy2),
      Rect::from_xywh(
        inner_rect.x(),
        outer_rect.y() + outer_rect.height() - target_widths.bottom,
        inner_rect.width(),
        target_widths.bottom,
      ),
      repeat_x,
      BorderImageRepeat::Stretch,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx0, sy1, sx1 - sx0, sy2 - sy1),
      Rect::from_xywh(
        outer_rect.x(),
        inner_rect.y(),
        target_widths.left,
        inner_rect.height(),
      ),
      BorderImageRepeat::Stretch,
      repeat_y,
    );
    self.paint_border_patch(
      &pixmap,
      Rect::from_xywh(sx2, sy1, sx3 - sx2, sy2 - sy1),
      Rect::from_xywh(
        outer_rect.x() + outer_rect.width() - target_widths.right,
        inner_rect.y(),
        target_widths.right,
        inner_rect.height(),
      ),
      BorderImageRepeat::Stretch,
      repeat_y,
    );

    if slice.fill {
      self.paint_border_patch(
        &pixmap,
        Rect::from_xywh(sx1, sy1, sx2 - sx1, sy2 - sy1),
        inner_rect,
        repeat_x,
        repeat_y,
      );
    }

    true
  }

  fn paint_border_patch(
    &mut self,
    source: &Pixmap,
    src_rect: Rect,
    dest_rect: Rect,
    repeat_x: BorderImageRepeat,
    repeat_y: BorderImageRepeat,
  ) {
    if src_rect.width() <= 0.0
      || src_rect.height() <= 0.0
      || dest_rect.width() <= 0.0
      || dest_rect.height() <= 0.0
    {
      return;
    }

    let sx0 = src_rect.x().max(0.0).floor() as u32;
    let sy0 = src_rect.y().max(0.0).floor() as u32;
    let sx1 = (src_rect.x() + src_rect.width())
      .ceil()
      .min(source.width() as f32)
      .max(0.0) as u32;
    let sy1 = (src_rect.y() + src_rect.height())
      .ceil()
      .min(source.height() as f32)
      .max(0.0) as u32;
    if sx1 <= sx0 || sy1 <= sy0 {
      return;
    }
    let width = sx1 - sx0;
    let height = sy1 - sy0;

    let data = source.data();
    let mut patch = Vec::with_capacity((width * height * 4) as usize);
    for row in sy0..sy1 {
      let start = ((row * source.width() + sx0) * 4) as usize;
      let end = start + (width * 4) as usize;
      patch.extend_from_slice(&data[start..end]);
    }
    let Some(patch_pixmap) = Pixmap::from_vec(patch, IntSize::from_wh(width, height).unwrap())
    else {
      return;
    };

    let mut tile_w = dest_rect.width();
    let mut tile_h = dest_rect.height();

    let mut scale_x = tile_w / width as f32;
    let mut scale_y = tile_h / height as f32;

    if repeat_x != BorderImageRepeat::Stretch {
      scale_x = scale_y;
      tile_w = width as f32 * scale_x;
    }
    if repeat_y != BorderImageRepeat::Stretch {
      scale_y = scale_x;
      tile_h = height as f32 * scale_y;
    }

    if repeat_x == BorderImageRepeat::Round && tile_w > 0.0 {
      let count = (dest_rect.width() / tile_w).round().max(1.0);
      tile_w = dest_rect.width() / count;
      scale_x = tile_w / width as f32;
    }
    if repeat_y == BorderImageRepeat::Round && tile_h > 0.0 {
      let count = (dest_rect.height() / tile_h).round().max(1.0);
      tile_h = dest_rect.height() / count;
      scale_y = tile_h / height as f32;
    }

    let positions_x = match repeat_x {
      BorderImageRepeat::Stretch => vec![dest_rect.x()],
      BorderImageRepeat::Round => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.x();
        let end = dest_rect.x() + dest_rect.width();
        if tile_w <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_w;
        }
        pos
      }
      BorderImageRepeat::Space => {
        if tile_w <= 0.0 {
          return;
        }
        let count = (dest_rect.width() / tile_w).floor();
        if count < 1.0 {
          vec![dest_rect.x() + (dest_rect.width() - tile_w) * 0.5]
        } else if count < 2.0 {
          vec![dest_rect.x() + (dest_rect.width() - tile_w) * 0.5]
        } else {
          let spacing = (dest_rect.width() - tile_w * count) / (count - 1.0);
          let mut pos = Vec::with_capacity(count as usize);
          let mut cursor = dest_rect.x();
          for _ in 0..(count as usize) {
            pos.push(cursor);
            cursor += tile_w + spacing;
          }
          pos
        }
      }
      BorderImageRepeat::Repeat => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.x();
        let end = dest_rect.x() + dest_rect.width();
        if tile_w <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_w;
        }
        pos
      }
    };

    let positions_y = match repeat_y {
      BorderImageRepeat::Stretch => vec![dest_rect.y()],
      BorderImageRepeat::Round => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.y();
        let end = dest_rect.y() + dest_rect.height();
        if tile_h <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_h;
        }
        pos
      }
      BorderImageRepeat::Space => {
        if tile_h <= 0.0 {
          return;
        }
        let count = (dest_rect.height() / tile_h).floor();
        if count < 1.0 {
          vec![dest_rect.y() + (dest_rect.height() - tile_h) * 0.5]
        } else if count < 2.0 {
          vec![dest_rect.y() + (dest_rect.height() - tile_h) * 0.5]
        } else {
          let spacing = (dest_rect.height() - tile_h * count) / (count - 1.0);
          let mut pos = Vec::with_capacity(count as usize);
          let mut cursor = dest_rect.y();
          for _ in 0..(count as usize) {
            pos.push(cursor);
            cursor += tile_h + spacing;
          }
          pos
        }
      }
      BorderImageRepeat::Repeat => {
        let mut pos = Vec::new();
        let mut cursor = dest_rect.y();
        let end = dest_rect.y() + dest_rect.height();
        if tile_h <= 0.0 {
          return;
        }
        while cursor < end - 1e-3 {
          pos.push(cursor);
          cursor += tile_h;
        }
        pos
      }
    };

    let clip = dest_rect;
    for ty in positions_y.iter().copied() {
      for tx in positions_x.iter().copied() {
        let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
        let Some(intersection) = tile_rect.intersection(clip) else {
          continue;
        };
        if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
          continue;
        }
        let device_clip = self.device_rect(intersection);
        let Some(src_rect) = SkiaRect::from_xywh(
          device_clip.x(),
          device_clip.y(),
          device_clip.width(),
          device_clip.height(),
        ) else {
          continue;
        };
        let mut paint = Paint::default();
        paint.shader = Pattern::new(
          patch_pixmap.as_ref(),
          SpreadMode::Pad,
          FilterQuality::Bilinear,
          1.0,
          Transform::from_row(
            scale_x * self.scale,
            0.0,
            0.0,
            scale_y * self.scale,
            self.device_length(tx),
            self.device_length(ty),
          ),
        );
        paint.anti_alias = false;

        self
          .pixmap
          .fill_rect(src_rect, &paint, Transform::identity(), None);
      }
    }
  }

  fn paint_border_edge(
    &mut self,
    edge: BorderEdge,
    x1: f32,
    y1: f32,
    x2: f32,
    y2: f32,
    width: f32,
    style: CssBorderStyle,
    color: &Rgba,
  ) {
    self.paint_border_edge_with_mode(
      edge,
      x1,
      y1,
      x2,
      y2,
      width,
      style,
      color,
      SkiaBlendMode::SourceOver,
      true,
    );
  }

  fn paint_border_edge_with_mode(
    &mut self,
    edge: BorderEdge,
    x1: f32,
    y1: f32,
    x2: f32,
    y2: f32,
    width: f32,
    style: CssBorderStyle,
    color: &Rgba,
    blend_mode: SkiaBlendMode,
    anti_alias: bool,
  ) {
    if width <= 0.0 || matches!(style, CssBorderStyle::None | CssBorderStyle::Hidden) {
      return;
    }

    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.blend_mode = blend_mode;
    paint.anti_alias = anti_alias;

    let mut stroke = Stroke::default();
    stroke.width = width;
    stroke.line_cap = match style {
      CssBorderStyle::Dotted => tiny_skia::LineCap::Round,
      _ => tiny_skia::LineCap::Butt,
    };

    // Dash patterns per CSS styles
    match style {
      CssBorderStyle::Dotted => {
        stroke.dash = tiny_skia::StrokeDash::new(vec![width, width], 0.0);
      }
      CssBorderStyle::Dashed => {
        stroke.dash = tiny_skia::StrokeDash::new(vec![3.0 * width, width], 0.0);
      }
      _ => {}
    }

    let mut path = PathBuilder::new();
    path.move_to(x1, y1);
    path.line_to(x2, y2);
    let base_path = match path.finish() {
      Some(p) => p,
      None => return,
    };

    match style {
      CssBorderStyle::Double => {
        // When too thin to draw two strokes and a gap, fall back to a solid line.
        if width < 3.0 {
          self
            .pixmap
            .stroke_path(&base_path, &paint, &stroke, Transform::identity(), None);
          return;
        }

        let third = width / 3.0;
        let offset = third + third * 0.5;

        let (outer_path, inner_path) = edge.parallel_lines(x1, y1, x2, y2, offset);

        let mut inner_stroke = stroke.clone();
        inner_stroke.width = third;
        stroke.width = third;

        if let Some(outer) = outer_path {
          self
            .pixmap
            .stroke_path(&outer, &paint, &stroke, Transform::identity(), None);
        }
        if let Some(inner) = inner_path {
          self
            .pixmap
            .stroke_path(&inner, &paint, &inner_stroke, Transform::identity(), None);
        }
      }
      CssBorderStyle::Groove | CssBorderStyle::Ridge => {
        let half = width / 2.0;
        let offset = half * 0.5;
        let (first_path, second_path) = edge.parallel_lines(x1, y1, x2, y2, offset);

        let mut first_paint = paint.clone();
        let mut second_paint = paint.clone();
        let mut first_stroke = stroke.clone();
        let mut second_stroke = stroke.clone();
        first_stroke.width = half;
        second_stroke.width = half;

        let (first_color, second_color) = edge.groove_ridge_colors(color, style);
        first_paint.set_color_rgba8(
          first_color.r,
          first_color.g,
          first_color.b,
          first_color.alpha_u8(),
        );
        second_paint.set_color_rgba8(
          second_color.r,
          second_color.g,
          second_color.b,
          second_color.alpha_u8(),
        );

        if let Some(first) = first_path {
          self.pixmap.stroke_path(
            &first,
            &first_paint,
            &first_stroke,
            Transform::identity(),
            None,
          );
        }
        if let Some(second) = second_path {
          self.pixmap.stroke_path(
            &second,
            &second_paint,
            &second_stroke,
            Transform::identity(),
            None,
          );
        }
      }
      CssBorderStyle::Inset | CssBorderStyle::Outset => {
        let shaded = edge.inset_outset_color(color, style);
        paint.set_color_rgba8(shaded.r, shaded.g, shaded.b, shaded.alpha_u8());
        self
          .pixmap
          .stroke_path(&base_path, &paint, &stroke, Transform::identity(), None);
      }
      _ => {
        self
          .pixmap
          .stroke_path(&base_path, &paint, &stroke, Transform::identity(), None);
      }
    }
  }

  fn paint_outline(&mut self, x: f32, y: f32, width: f32, height: f32, style: &ComputedStyle) {
    let ow = style.outline_width.to_px() * self.scale;
    let outline_style = style.outline_style.to_border_style();
    if ow <= 0.0 || matches!(outline_style, CssBorderStyle::None | CssBorderStyle::Hidden) {
      return;
    }
    let outer_x = self.device_length(x);
    let outer_y = self.device_length(y);
    let outer_w = self.device_length(width);
    let outer_h = self.device_length(height);
    let (color, invert) = style.outline_color.resolve(style.color);
    let blend_mode = if invert {
      SkiaBlendMode::Difference
    } else {
      SkiaBlendMode::SourceOver
    };

    let top_center = outer_y + ow * 0.5;
    let bottom_center = outer_y + outer_h - ow * 0.5;
    let left_center = outer_x + ow * 0.5;
    let right_center = outer_x + outer_w - ow * 0.5;

    self.paint_border_edge_with_mode(
      BorderEdge::Top,
      left_center,
      top_center,
      right_center,
      top_center,
      ow,
      outline_style,
      &color,
      blend_mode,
      false,
    );
    self.paint_border_edge_with_mode(
      BorderEdge::Bottom,
      left_center,
      bottom_center,
      right_center,
      bottom_center,
      ow,
      outline_style,
      &color,
      blend_mode,
      false,
    );
    self.paint_border_edge_with_mode(
      BorderEdge::Left,
      left_center,
      top_center,
      left_center,
      bottom_center,
      ow,
      outline_style,
      &color,
      blend_mode,
      false,
    );
    self.paint_border_edge_with_mode(
      BorderEdge::Right,
      right_center,
      top_center,
      right_center,
      bottom_center,
      ow,
      outline_style,
      &color,
      blend_mode,
      false,
    );
  }

  /// Paints text by shaping and rasterizing glyph outlines.
  fn paint_text(
    &mut self,
    text: &str,
    style: Option<&ComputedStyle>,
    x: f32,
    baseline_y: f32,
    font_size: f32,
    color: Rgba,
  ) {
    if text.is_empty() {
      return;
    }

    // Use computed style when available; otherwise construct a minimal fallback
    let style_for_shaping: Cow<ComputedStyle> = match style {
      Some(s) => Cow::Borrowed(s),
      None => {
        let mut s = ComputedStyle::default();
        s.font_size = font_size;
        Cow::Owned(s)
      }
    };

    // Shape text with the full pipeline (bidi, script, fallback fonts)
    let style_ptr = style_for_shaping.as_ref() as *const ComputedStyle as usize;
    let font_size_bits = style_for_shaping.font_size.to_bits();
    let key = TextCacheKey {
      style_ptr,
      font_size_bits,
      text: text.to_string(),
    };

    let shaped_runs = if let Ok(cache) = self.text_shape_cache.lock() {
      cache.get(&key).cloned()
    } else {
      None
    }
    .unwrap_or_else(|| {
      let runs = match self.shaper.shape(text, &style_for_shaping, &self.font_ctx) {
        Ok(runs) => runs,
        Err(_) => return Vec::new(),
      };
      if let Ok(mut cache) = self.text_shape_cache.lock() {
        cache.insert(key, runs.clone());
      }
      runs
    });

    if shaped_runs.is_empty() {
      return;
    }

    self.paint_shaped_runs(&shaped_runs, x, baseline_y, color, style);
  }

  fn paint_shaped_runs(
    &mut self,
    runs: &[ShapedRun],
    origin_x: f32,
    baseline_y: f32,
    color: Rgba,
    style: Option<&ComputedStyle>,
  ) {
    let mut pen_x = origin_x;

    for run in runs {
      let run_origin = if run.direction.is_rtl() {
        pen_x + run.advance
      } else {
        pen_x
      };
      self.paint_shaped_run(run, run_origin, baseline_y, color, style);
      pen_x += run.advance;
    }
  }

  fn paint_shaped_runs_vertical(
    &mut self,
    runs: &[ShapedRun],
    block_origin: f32,
    inline_origin: f32,
    color: Rgba,
    style: Option<&ComputedStyle>,
  ) {
    let mut pen_inline = inline_origin;
    for run in runs {
      let run_origin_inline = if run.direction.is_rtl() {
        pen_inline + run.advance
      } else {
        pen_inline
      };
      self.paint_shaped_run_vertical(run, block_origin, run_origin_inline, color, style);
      pen_inline += run.advance;
    }
  }

  fn paint_shaped_run(
    &mut self,
    run: &ShapedRun,
    origin_x: f32,
    baseline_y: f32,
    color: Rgba,
    style: Option<&ComputedStyle>,
  ) {
    let origin_x = origin_x * self.scale;
    let baseline_y = baseline_y * self.scale;
    // ttf_parser face
    let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
      Ok(f) => f,
      Err(_) => return,
    };
    let units_per_em = face.units_per_em() as f32;
    let mut scale = run.font_size / units_per_em;
    scale *= run.scale * self.scale;

    let mut glyph_paths = Vec::with_capacity(run.glyphs.len());
    let mut bounds = PathBounds::new();

    for glyph in &run.glyphs {
      let glyph_x = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => origin_x - glyph.x_offset * self.scale,
        crate::text::pipeline::Direction::LeftToRight => origin_x + glyph.x_offset * self.scale,
      };
      let glyph_y = baseline_y - glyph.y_offset * self.scale;

      let glyph_id: u16 = glyph.glyph_id as u16;

      if let Some(path) = Self::build_glyph_path(&face, glyph_id, glyph_x, glyph_y, scale) {
        bounds.include(&path.bounds());
        glyph_paths.push(path);
      }
    }

    if glyph_paths.is_empty() || !bounds.is_valid() {
      return;
    }

    // Rotate sideways runs for vertical text-orientation.
    if !matches!(run.rotation, crate::text::pipeline::RunRotation::None) {
      let angle = match run.rotation {
        crate::text::pipeline::RunRotation::Ccw90 => -90.0_f32.to_radians(),
        crate::text::pipeline::RunRotation::Cw90 => 90.0_f32.to_radians(),
        crate::text::pipeline::RunRotation::None => 0.0,
      };
      let (sin, cos) = angle.sin_cos();
      let tx = origin_x - origin_x * cos + baseline_y * sin;
      let ty = baseline_y - origin_x * sin - baseline_y * cos;
      let rotate = tiny_skia::Transform::from_row(cos, sin, -sin, cos, tx, ty);

      let mut rotated_paths = Vec::with_capacity(glyph_paths.len());
      let mut rotated_bounds = PathBounds::new();
      for path in glyph_paths {
        if let Some(rotated) = path.clone().transform(rotate) {
          let rect = path.bounds();
          let corners = [
            (rect.left(), rect.top()),
            (rect.right(), rect.top()),
            (rect.right(), rect.bottom()),
            (rect.left(), rect.bottom()),
          ];
          let mut min_x = f32::INFINITY;
          let mut min_y = f32::INFINITY;
          let mut max_x = f32::NEG_INFINITY;
          let mut max_y = f32::NEG_INFINITY;
          for (x, y) in corners {
            let mapped_x = x * cos + y * -sin + tx;
            let mapped_y = x * sin + y * cos + ty;
            min_x = min_x.min(mapped_x);
            min_y = min_y.min(mapped_y);
            max_x = max_x.max(mapped_x);
            max_y = max_y.max(mapped_y);
          }
          if let Some(mapped) = tiny_skia::Rect::from_ltrb(min_x, min_y, max_x, max_y) {
            rotated_bounds.include(&mapped);
          }
          rotated_paths.push(rotated);
        }
      }
      if rotated_bounds.is_valid() && !rotated_paths.is_empty() {
        glyph_paths = rotated_paths;
        bounds = rotated_bounds;
      } else {
        glyph_paths = rotated_paths;
      }
    }

    if let Some(style) = style {
      if !style.text_shadow.is_empty() {
        let shadows = resolve_text_shadows(style);
        if !shadows.is_empty() {
          self.paint_text_shadows(&glyph_paths, &bounds, &shadows);
        }
      }
    }

    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;

    for path in &glyph_paths {
      self.pixmap.fill_path(
        path,
        &paint,
        tiny_skia::FillRule::EvenOdd,
        Transform::identity(),
        None,
      );
    }
  }

  fn paint_shaped_run_vertical(
    &mut self,
    run: &ShapedRun,
    block_origin: f32,
    inline_origin: f32,
    color: Rgba,
    style: Option<&ComputedStyle>,
  ) {
    let block_origin = block_origin * self.scale;
    let inline_origin = inline_origin * self.scale;

    let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
      Ok(f) => f,
      Err(_) => return,
    };
    let units_per_em = face.units_per_em() as f32;
    let mut scale = run.font_size / units_per_em;
    scale *= run.scale * self.scale;

    let mut glyph_paths = Vec::with_capacity(run.glyphs.len());
    let mut bounds = PathBounds::new();

    for glyph in &run.glyphs {
      let inline_pos = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => {
          inline_origin - glyph.x_offset * self.scale
        }
        crate::text::pipeline::Direction::LeftToRight => {
          inline_origin + glyph.x_offset * self.scale
        }
      };
      let block_pos = block_origin - glyph.y_offset * self.scale;
      let glyph_id: u16 = glyph.glyph_id as u16;

      if let Some(path) = Self::build_glyph_path(&face, glyph_id, block_pos, inline_pos, scale) {
        bounds.include(&path.bounds());
        glyph_paths.push(path);
      }
    }

    if glyph_paths.is_empty() || !bounds.is_valid() {
      return;
    }

    if !matches!(run.rotation, crate::text::pipeline::RunRotation::None) {
      let angle = match run.rotation {
        crate::text::pipeline::RunRotation::Ccw90 => -90.0_f32.to_radians(),
        crate::text::pipeline::RunRotation::Cw90 => 90.0_f32.to_radians(),
        crate::text::pipeline::RunRotation::None => 0.0,
      };
      let (sin, cos) = angle.sin_cos();
      let tx = block_origin - block_origin * cos + inline_origin * sin;
      let ty = inline_origin - block_origin * sin - inline_origin * cos;
      let rotate = tiny_skia::Transform::from_row(cos, sin, -sin, cos, tx, ty);

      let mut rotated_paths = Vec::with_capacity(glyph_paths.len());
      let mut rotated_bounds = PathBounds::new();
      for path in glyph_paths {
        if let Some(rotated) = path.clone().transform(rotate) {
          let mapped = rotated.bounds();
          rotated_bounds.include(&mapped);
          rotated_paths.push(rotated);
        }
      }
      if rotated_bounds.is_valid() && !rotated_paths.is_empty() {
        glyph_paths = rotated_paths;
        bounds = rotated_bounds;
      } else {
        glyph_paths = rotated_paths;
      }
    }

    if let Some(style) = style {
      if !style.text_shadow.is_empty() {
        let shadows = resolve_text_shadows(style);
        if !shadows.is_empty() {
          self.paint_text_shadows(&glyph_paths, &bounds, &shadows);
        }
      }
    }

    let mut paint = Paint::default();
    paint.set_color_rgba8(color.r, color.g, color.b, color.alpha_u8());
    paint.anti_alias = true;

    for path in &glyph_paths {
      self.pixmap.fill_path(
        path,
        &paint,
        tiny_skia::FillRule::EvenOdd,
        Transform::identity(),
        None,
      );
    }
  }

  fn build_glyph_path(
    face: &ttf_parser::Face,
    glyph_id: u16,
    x: f32,
    baseline_y: f32,
    scale: f32,
  ) -> Option<tiny_skia::Path> {
    use ttf_parser::OutlineBuilder;

    struct PathConverter {
      builder: PathBuilder,
      scale: f32,
      x: f32,
      y: f32,
    }

    impl OutlineBuilder for PathConverter {
      fn move_to(&mut self, px: f32, py: f32) {
        self
          .builder
          .move_to(self.x + px * self.scale, self.y - py * self.scale);
      }

      fn line_to(&mut self, px: f32, py: f32) {
        self
          .builder
          .line_to(self.x + px * self.scale, self.y - py * self.scale);
      }

      fn quad_to(&mut self, x1: f32, y1: f32, px: f32, py: f32) {
        self.builder.quad_to(
          self.x + x1 * self.scale,
          self.y - y1 * self.scale,
          self.x + px * self.scale,
          self.y - py * self.scale,
        );
      }

      fn curve_to(&mut self, x1: f32, y1: f32, x2: f32, y2: f32, px: f32, py: f32) {
        self.builder.cubic_to(
          self.x + x1 * self.scale,
          self.y - y1 * self.scale,
          self.x + x2 * self.scale,
          self.y - y2 * self.scale,
          self.x + px * self.scale,
          self.y - py * self.scale,
        );
      }

      fn close(&mut self) {
        self.builder.close();
      }
    }

    let mut converter = PathConverter {
      builder: PathBuilder::new(),
      scale,
      x,
      y: baseline_y,
    };

    face.outline_glyph(ttf_parser::GlyphId(glyph_id), &mut converter)?;
    converter.builder.finish()
  }

  fn paint_text_shadows(
    &mut self,
    paths: &[tiny_skia::Path],
    bounds: &PathBounds,
    shadows: &[ResolvedTextShadow],
  ) {
    for shadow in shadows {
      let offset_x = shadow.offset_x * self.scale;
      let offset_y = shadow.offset_y * self.scale;
      let blur = shadow.blur_radius * self.scale;

      let blur_margin = (blur.abs() * 3.0).ceil();
      let shadow_min_x = bounds.min_x + offset_x - blur_margin;
      let shadow_max_x = bounds.max_x + offset_x + blur_margin;
      let shadow_min_y = bounds.min_y + offset_y - blur_margin;
      let shadow_max_y = bounds.max_y + offset_y + blur_margin;

      let shadow_width = (shadow_max_x - shadow_min_x).ceil().max(0.0) as u32;
      let shadow_height = (shadow_max_y - shadow_min_y).ceil().max(0.0) as u32;
      if shadow_width == 0 || shadow_height == 0 {
        continue;
      }

      let Some(mut shadow_pixmap) = Pixmap::new(shadow_width, shadow_height) else {
        continue;
      };

      let mut paint = Paint::default();
      paint.set_color_rgba8(
        shadow.color.r,
        shadow.color.g,
        shadow.color.b,
        shadow.color.alpha_u8(),
      );
      paint.anti_alias = true;

      let translate_x = -bounds.min_x + blur_margin;
      let translate_y = -bounds.min_y + blur_margin;
      let transform = Transform::from_translate(translate_x, translate_y);
      for path in paths {
        shadow_pixmap.fill_path(path, &paint, tiny_skia::FillRule::EvenOdd, transform, None);
      }

      if blur > 0.0 {
        apply_gaussian_blur(&mut shadow_pixmap, blur);
      }

      let dest_x = shadow_min_x.floor() as i32;
      let dest_y = shadow_min_y.floor() as i32;
      let frac_x = shadow_min_x - dest_x as f32;
      let frac_y = shadow_min_y - dest_y as f32;
      let pixmap_paint = PixmapPaint {
        opacity: 1.0,
        blend_mode: SkiaBlendMode::SourceOver,
        ..Default::default()
      };
      self.pixmap.draw_pixmap(
        dest_x,
        dest_y,
        shadow_pixmap.as_ref(),
        &pixmap_paint,
        Transform::from_translate(frac_x, frac_y),
        None,
      );
    }
  }

  /// Paints a replaced element (image, etc.)
  fn paint_replaced(
    &mut self,
    replaced_type: &ReplacedType,
    style: Option<&ComputedStyle>,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
  ) {
    if width <= 0.0 || height <= 0.0 {
      return;
    }

    let content_rect = if let Some(style) = style {
      background_rects(
        x,
        y,
        width,
        height,
        style,
        Some((self.css_width, self.css_height)),
      )
      .content
    } else {
      Rect::from_xywh(x, y, width, height)
    };
    if content_rect.width() <= 0.0 || content_rect.height() <= 0.0 {
      return;
    }

    // Try to render actual content for images and SVG
    match replaced_type {
      ReplacedType::FormControl(control) => {
        if let Some(style) = style {
          if self.paint_form_control(control, style, content_rect) {
            return;
          }
        }
      }
      ReplacedType::Image { alt, .. } => {
        let media_ctx = crate::style::media::MediaContext::screen(self.css_width, self.css_height)
          .with_device_pixel_ratio(self.scale)
          .with_env_overrides();
        let cache_base = self.image_cache.base_url();
        let sources =
          replaced_type.image_sources_with_fallback(crate::tree::box_tree::ImageSelectionContext {
            scale: self.scale,
            slot_width: Some(content_rect.width()),
            viewport: Some(Size::new(self.css_width, self.css_height)),
            media_context: Some(&media_ctx),
            font_size: style.map(|s| s.font_size),
            base_url: cache_base.as_deref(),
          });
        for candidate in sources {
          if self.paint_image_from_src(
            &candidate,
            style,
            content_rect.x(),
            content_rect.y(),
            content_rect.width(),
            content_rect.height(),
          ) {
            return;
          }
        }
        if let (Some(style), Some(alt_text)) = (style, alt.as_deref()) {
          if self.paint_alt_text(alt_text, style, content_rect) {
            return;
          }
        }
      }
      ReplacedType::Iframe {
        srcdoc: Some(html),
        src,
      } => {
        if let Some(pixmap) = self.render_iframe_srcdoc(html, content_rect, style) {
          let device_x = self.device_length(content_rect.x());
          let device_y = self.device_length(content_rect.y());
          let paint = PixmapPaint::default();
          self.pixmap.draw_pixmap(
            device_x as i32,
            device_y as i32,
            pixmap.as_ref(),
            &paint,
            Transform::identity(),
            None,
          );
          return;
        }

        if self.paint_svg(
          src,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
        if self.paint_image_from_src(
          src,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
      }
      ReplacedType::Svg { content } => {
        if self.paint_inline_svg(
          content,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
        if self.paint_svg(
          &content.fallback_svg,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
        if self.paint_image_from_src(
          &content.fallback_svg,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
      }
      ReplacedType::Iframe {
        src: content,
        srcdoc: None,
      } => {
        if let Some(pixmap) = self.render_iframe_src(content, content_rect, style) {
          let device_x = self.device_length(content_rect.x());
          let device_y = self.device_length(content_rect.y());
          let paint = PixmapPaint::default();
          self.pixmap.draw_pixmap(
            device_x as i32,
            device_y as i32,
            pixmap.as_ref(),
            &paint,
            Transform::identity(),
            None,
          );
          return;
        }

        if self.paint_svg(
          content,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
        if self.paint_image_from_src(
          content,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
      }
      ReplacedType::Embed { src: content } | ReplacedType::Object { data: content } => {
        if self.paint_svg(
          content,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
        if self.paint_image_from_src(
          content,
          style,
          content_rect.x(),
          content_rect.y(),
          content_rect.width(),
          content_rect.height(),
        ) {
          return;
        }
      }
      ReplacedType::Video { .. } => {
        let media_ctx = crate::style::media::MediaContext::screen(self.css_width, self.css_height)
          .with_device_pixel_ratio(self.scale)
          .with_env_overrides();
        let cache_base = self.image_cache.base_url();
        let sources =
          replaced_type.image_sources_with_fallback(crate::tree::box_tree::ImageSelectionContext {
            scale: self.scale,
            slot_width: Some(content_rect.width()),
            viewport: Some(Size::new(self.css_width, self.css_height)),
            media_context: Some(&media_ctx),
            font_size: style.map(|s| s.font_size),
            base_url: cache_base.as_deref(),
          });
        for candidate in sources {
          if self.paint_image_from_src(
            &candidate,
            style,
            content_rect.x(),
            content_rect.y(),
            content_rect.width(),
            content_rect.height(),
          ) {
            return;
          }
        }
      }
      _ => {}
    }

    self.paint_replaced_placeholder(replaced_type, style, content_rect);
  }

  fn paint_inline_svg(
    &mut self,
    content: &SvgContent,
    style: Option<&ComputedStyle>,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
  ) -> bool {
    let svg_markup = if content.foreign_objects.is_empty() {
      Some(content.svg.clone())
    } else {
      self.inline_svg_with_foreign_objects(content)
    };

    if let Some(svg) = svg_markup {
      return self.paint_svg(&svg, style, x, y, width, height);
    }

    false
  }

  fn inline_svg_with_foreign_objects(&mut self, content: &SvgContent) -> Option<String> {
    let mut svg = content.svg.clone();
    for (idx, foreign) in content.foreign_objects.iter().enumerate() {
      let data_url = self.render_foreign_object_data_url(foreign, &content.shared_css)?;
      let replacement = self.foreign_object_image_tag(foreign, &data_url, idx);
      let placeholder = if foreign.placeholder.is_empty() {
        format!("<!--FASTRENDER_FOREIGN_OBJECT_{}-->", idx)
      } else {
        foreign.placeholder.clone()
      };

      if let Some(pos) = svg.find(&placeholder) {
        let end = pos + placeholder.len();
        svg.replace_range(pos..end, &replacement);
      } else {
        svg.push_str(&replacement);
      }
    }

    Some(svg)
  }

  fn foreign_object_image_tag(
    &self,
    info: &ForeignObjectInfo,
    data_url: &str,
    idx: usize,
  ) -> String {
    let mut parts: Vec<String> = Vec::new();
    parts.push(format!("x=\"{:.6}\"", info.x));
    parts.push(format!("y=\"{:.6}\"", info.y));
    parts.push(format!("width=\"{:.6}\"", info.width));
    parts.push(format!("height=\"{:.6}\"", info.height));
    if info.opacity < 1.0 {
      parts.push(format!("opacity=\"{:.3}\"", info.opacity.clamp(0.0, 1.0)));
    }

    for (name, value) in &info.attributes {
      if matches!(name.as_str(), "x" | "y" | "width" | "height") {
        continue;
      }
      parts.push(format!("{}=\"{}\"", name, escape_attr_value(value)));
    }

    parts.push("preserveAspectRatio=\"none\"".to_string());
    parts.push(format!("href=\"{}\"", escape_attr_value(data_url)));

    let clip_id = format!("fastr-fo-{}", idx);
    let clip = format!(
      "<clipPath id=\"{}\"><rect x=\"{:.6}\" y=\"{:.6}\" width=\"{:.6}\" height=\"{:.6}\"/></clipPath>",
      clip_id,
      info.x,
      info.y,
      info.width,
      info.height
    );

    format!(
      "<g>{clip}<image clip-path=\"url(#{clip_id})\" {attrs}/></g>",
      clip = clip,
      clip_id = clip_id,
      attrs = parts.join(" ")
    )
  }

  fn render_foreign_object_data_url(
    &mut self,
    info: &ForeignObjectInfo,
    shared_css: &str,
  ) -> Option<String> {
    let width = info.width.max(1.0).round() as u32;
    let height = info.height.max(1.0).round() as u32;
    if width == 0 || height == 0 {
      return None;
    }

    let html = build_foreign_object_document(info, shared_css);
    let background = info.background.unwrap_or(Rgba::TRANSPARENT);
    let context = self.image_cache.resource_context();
    let policy = context
      .as_ref()
      .map(|c| c.policy.clone())
      .unwrap_or_default();
    let pixmap = render_html_with_shared_resources(
      &html,
      width,
      height,
      background,
      &self.font_ctx,
      &self.image_cache,
      Arc::clone(self.image_cache.fetcher()),
      self.image_cache.base_url(),
      1.0,
      policy,
      context,
      crate::api::DEFAULT_MAX_IFRAME_DEPTH,
    )
    .ok()?;

    pixmap_to_data_url(pixmap)
  }

  fn resolved_accent_color(style: &ComputedStyle) -> Rgba {
    match style.accent_color {
      AccentColor::Color(c) => c,
      AccentColor::Auto => style.color,
    }
  }

  fn paint_form_control(
    &mut self,
    control: &FormControl,
    style: &ComputedStyle,
    content_rect: Rect,
  ) -> bool {
    if content_rect.width() <= 0.0 || content_rect.height() <= 0.0 {
      return true;
    }

    let accent = Self::resolved_accent_color(style);
    let muted_accent = if control.disabled {
      accent.with_alpha((accent.a * 0.7).clamp(0.0, 1.0))
    } else {
      accent
    };
    let inset_rect = |rect: Rect, inset: f32| {
      Rect::from_xywh(
        rect.x() + inset,
        rect.y() + inset,
        (rect.width() - 2.0 * inset).max(0.0),
        (rect.height() - 2.0 * inset).max(0.0),
      )
    };

    match &control.control {
      FormControlKind::Text {
        value, placeholder, ..
      } => {
        let value_trimmed = value.trim();
        let (text, color) = if !value_trimmed.is_empty() {
          (value_trimmed, style.color)
        } else if let Some(ph) = placeholder.as_deref() {
          (ph.trim(), style.color.with_alpha(0.6))
        } else {
          return true;
        };

        let mut text_style = style.clone();
        text_style.color = color;
        let rect = inset_rect(content_rect, 2.0);
        let _ = self.paint_alt_text(text, &text_style, rect);
        true
      }
      FormControlKind::TextArea { value, .. } => {
        if value.is_empty() {
          return true;
        }
        let rect = inset_rect(content_rect, 2.0);
        let metrics = self.resolve_scaled_metrics(style);
        let line_height = compute_line_height_with_metrics_viewport(
          style,
          metrics.as_ref(),
          Some(Size::new(self.css_width, self.css_height)),
        );
        let mut y = rect.y();
        for line in value.split('\n') {
          if y > rect.y() + rect.height() {
            break;
          }
          let line_rect = Rect::from_xywh(
            rect.x(),
            y,
            rect.width(),
            (rect.height() - (y - rect.y())).max(0.0),
          );
          let _ = self.paint_alt_text(line.trim_end(), style, line_rect);
          y += line_height;
        }
        true
      }
      FormControlKind::Select { label, .. } => {
        let rect = inset_rect(content_rect, 2.0);
        let arrow_space = if matches!(control.appearance, Appearance::None) {
          0.0
        } else {
          14.0
        };
        let text_rect = Rect::from_xywh(
          rect.x(),
          rect.y(),
          (rect.width() - arrow_space).max(0.0),
          rect.height(),
        );
        let _ = self.paint_alt_text(label, style, text_rect);

        if arrow_space > 0.0 {
          let mut arrow_style = style.clone();
          arrow_style.color = muted_accent;
          let arrow_rect = Rect::from_xywh(
            rect.x() + rect.width() - arrow_space,
            rect.y(),
            arrow_space,
            rect.height(),
          );
          let _ = self.paint_alt_text("▾", &arrow_style, arrow_rect);
        }
        true
      }
      FormControlKind::Button { label } => {
        if label.trim().is_empty() {
          return true;
        }

        let label_rect = if let Some(size) = self.measure_alt_text(label, style) {
          let start_x = content_rect.x() + ((content_rect.width() - size.width).max(0.0) / 2.0);
          Rect::from_xywh(
            start_x,
            content_rect.y(),
            size.width.min(content_rect.width()),
            content_rect.height(),
          )
        } else {
          content_rect
        };
        let _ = self.paint_alt_text(label, style, label_rect);
        true
      }
      FormControlKind::Checkbox { is_radio, checked } => {
        if !*checked || matches!(control.appearance, Appearance::None) {
          return true;
        }
        let mut mark_style = style.clone();
        mark_style.color = muted_accent;
        let rect = inset_rect(content_rect, 2.0);
        let glyph = if *is_radio { "●" } else { "✓" };
        let _ = self.paint_alt_text(glyph, &mark_style, rect);
        true
      }
      FormControlKind::Range { value, min, max } => {
        let track_height = 4.0_f32.min(content_rect.height());
        let track_y = content_rect.y() + (content_rect.height() - track_height) / 2.0;
        let radii = BorderRadii::uniform(track_height / 2.0);
        let track_color = style
          .background_color
          .with_alpha((style.background_color.a * 0.8).max(0.1));
        let _ = fill_rounded_rect(
          &mut self.pixmap,
          content_rect.x(),
          track_y,
          content_rect.width(),
          track_height,
          &radii,
          track_color,
        );

        let min_val = min.unwrap_or(0.0);
        let max_val = max.unwrap_or(100.0);
        let span = (max_val - min_val).abs().max(0.0001);
        let clamped = ((*value - min_val) / span).clamp(0.0, 1.0);
        let knob_radius = (content_rect.height().min(16.0)) / 2.0;
        let knob_center_x =
          content_rect.x() + knob_radius + clamped * (content_rect.width() - 2.0 * knob_radius);
        let knob_center_y = content_rect.y() + content_rect.height() / 2.0;
        if let Some(path) = PathBuilder::from_circle(knob_center_x, knob_center_y, knob_radius) {
          let mut paint = Paint::default();
          paint.set_color(
            tiny_skia::Color::from_rgba(
              muted_accent.r as f32 / 255.0,
              muted_accent.g as f32 / 255.0,
              muted_accent.b as f32 / 255.0,
              muted_accent.a,
            )
            .unwrap_or(tiny_skia::Color::BLACK),
          );
          paint.anti_alias = true;
          self.pixmap.fill_path(
            &path,
            &paint,
            tiny_skia::FillRule::Winding,
            Transform::identity(),
            None,
          );
        }
        true
      }
      FormControlKind::Unknown { label } => {
        if let Some(text) = label {
          let rect = inset_rect(content_rect, 2.0);
          let _ = self.paint_alt_text(text, style, rect);
        }
        true
      }
    }
  }

  fn render_iframe_srcdoc(
    &self,
    html: &str,
    content_rect: Rect,
    style: Option<&ComputedStyle>,
  ) -> Option<Pixmap> {
    let width = content_rect.width().ceil() as u32;
    let height = content_rect.height().ceil() as u32;
    if width == 0 || height == 0 {
      return None;
    }

    let background = style.map(|s| s.background_color).unwrap_or(Rgba::WHITE);
    let base_url = self.image_cache.base_url();
    let mut image_cache = self.image_cache.clone();
    if let Some(base_url) = base_url.clone() {
      image_cache.set_base_url(base_url);
    }
    let context = image_cache.resource_context();
    let policy = context
      .as_ref()
      .map(|c| c.policy.clone())
      .unwrap_or_default();
    render_html_with_shared_resources(
      html,
      width,
      height,
      background,
      &self.font_ctx,
      &image_cache,
      Arc::clone(image_cache.fetcher()),
      base_url,
      self.scale,
      policy,
      context,
      crate::api::DEFAULT_MAX_IFRAME_DEPTH,
    )
    .ok()
  }

  fn render_iframe_src(
    &self,
    src: &str,
    content_rect: Rect,
    style: Option<&ComputedStyle>,
  ) -> Option<Pixmap> {
    if src.is_empty() {
      return None;
    }
    let width = content_rect.width().ceil() as u32;
    let height = content_rect.height().ceil() as u32;
    if width == 0 || height == 0 {
      return None;
    }

    let context = self.image_cache.resource_context();
    let resolved = self.image_cache.resolve_url(src);
    let fetcher = Arc::clone(self.image_cache.fetcher());
    if let Some(ctx) = context.as_ref() {
      if ctx
        .check_allowed(ResourceKind::Document, &resolved)
        .is_err()
      {
        return None;
      }
    }
    let resource = fetcher.fetch(&resolved).ok()?;
    let content_type = resource.content_type.as_deref();
    let is_html = content_type
      .map(|ct| {
        let ct = ct.to_ascii_lowercase();
        ct.starts_with("text/html")
          || ct.starts_with("application/xhtml+xml")
          || ct.starts_with("application/html")
          || ct.contains("+html")
      })
      .unwrap_or_else(|| {
        let lower = resolved.to_ascii_lowercase();
        lower.ends_with(".html") || lower.ends_with(".htm") || lower.ends_with(".xhtml")
      });
    if !is_html {
      return None;
    }

    let html = decode_html_bytes(&resource.bytes, content_type);
    let background = style.map(|s| s.background_color).unwrap_or(Rgba::WHITE);
    let mut image_cache = self.image_cache.clone();
    image_cache.set_base_url(resolved.clone());
    let nested_origin = origin_from_url(resource.final_url.as_deref().unwrap_or(&resolved));
    let nested_context = context.as_ref().map(|ctx| ctx.for_origin(nested_origin));
    let policy = nested_context
      .as_ref()
      .map(|ctx| ctx.policy.clone())
      .or_else(|| context.as_ref().map(|ctx| ctx.policy.clone()))
      .unwrap_or_default();

    render_html_with_shared_resources(
      &html,
      width,
      height,
      background,
      &self.font_ctx,
      &image_cache,
      fetcher,
      Some(resolved),
      self.scale,
      policy,
      nested_context,
      crate::api::DEFAULT_MAX_IFRAME_DEPTH,
    )
    .ok()
  }

  fn paint_image_from_src(
    &mut self,
    src: &str,
    style: Option<&ComputedStyle>,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
  ) -> bool {
    if src.is_empty() {
      return false;
    }

    if let Some(limit) = trace_image_paint_limit() {
      let idx = TRACE_IMAGE_PAINT_COUNT.fetch_add(1, Ordering::Relaxed);
      if idx < limit {
        let resolved = self.image_cache.resolve_url(src);
        eprintln!(
          "[image-paint] #{idx} src={} resolved={} rect=({:.1},{:.1},{:.1},{:.1})",
          src, resolved, x, y, width, height
        );
      }
    }

    let log_image_fail = runtime::runtime_toggles().truthy("FASTR_LOG_IMAGE_FAIL");

    let image = match self.image_cache.load(src) {
      Ok(img) => img,
      Err(e) => {
        if log_image_fail {
          eprintln!("[image-load-fail] src={} stage=load err={}", src, e);
        }
        if src.trim_start().starts_with('<') {
          match self.image_cache.render_svg(src) {
            Ok(img) => img,
            Err(_) => return false,
          }
        } else {
          return false;
        }
      }
    };

    if let Some(limit) = trace_image_paint_limit() {
      let seen = TRACE_IMAGE_PAINT_COUNT.load(Ordering::Relaxed);
      if seen <= limit {
        eprintln!(
          " [image-paint-loaded] src={} cached_image_ptr={:p} dyn_ptr={:p} dims={}x{}",
          src,
          Arc::as_ptr(&image),
          Arc::as_ptr(&image.image),
          image.width(),
          image.height()
        );
      }
    }

    let image_resolution = style.map(|s| s.image_resolution).unwrap_or_default();
    let orientation = style
      .map(|s| s.image_orientation.resolve(image.orientation, false))
      .unwrap_or_else(|| ImageOrientation::default().resolve(image.orientation, false));
    let (img_w_raw, img_h_raw) = image.oriented_dimensions(orientation);
    if img_w_raw == 0 || img_h_raw == 0 {
      if log_image_fail {
        eprintln!(
          "[image-load-fail] src={} stage=oriented-dimensions w={} h={}",
          src, img_w_raw, img_h_raw
        );
      }
      return false;
    }
    let Some((img_w_css, img_h_css)) =
      image.css_dimensions(orientation, &image_resolution, self.scale, None)
    else {
      if log_image_fail {
        eprintln!(
          "[image-load-fail] src={} stage=css-dimensions orientation={:?} resolution={:?}",
          src, orientation, image_resolution
        );
      }
      return false;
    };

    let pixmap = match Self::dynamic_image_to_pixmap(&image, orientation) {
      Some(pixmap) => pixmap,
      None => {
        if log_image_fail {
          eprintln!("[image-load-fail] src={} stage=pixmap", src);
        }
        return false;
      }
    };
    let fit = style.map(|s| s.object_fit).unwrap_or(ObjectFit::Fill);
    let pos = style
      .map(|s| s.object_position)
      .unwrap_or_else(default_object_position);

    let (dest_x, dest_y, mut dest_w, mut dest_h) = match compute_object_fit(
      fit,
      pos,
      width,
      height,
      img_w_css,
      img_h_css,
      style.map(|s| s.font_size).unwrap_or(16.0),
      Some((self.css_width, self.css_height)),
    ) {
      Some(v) => v,
      None => return false,
    };

    if matches!(
      style.map(|s| s.image_rendering),
      Some(ImageRendering::Pixelated | ImageRendering::CrispEdges)
    ) && (dest_w > img_w_raw as f32 || dest_h > img_h_raw as f32)
    {
      let (snapped_w, offset_x) = snap_upscale(dest_w, img_w_raw as f32).unwrap_or((dest_w, 0.0));
      let (snapped_h, offset_y) = snap_upscale(dest_h, img_h_raw as f32).unwrap_or((dest_h, 0.0));
      dest_w = snapped_w;
      dest_h = snapped_h;
      let dest_x = dest_x + offset_x;
      let dest_y = dest_y + offset_y;
      let dest_x = self.device_length(dest_x);
      let dest_y = self.device_length(dest_y);
      let dest_w = self.device_length(dest_w);
      let dest_h = self.device_length(dest_h);

      let scale_x = dest_w / img_w_raw as f32;
      let scale_y = dest_h / img_h_raw as f32;
      if !scale_x.is_finite() || !scale_y.is_finite() || dest_w <= 0.0 || dest_h <= 0.0 {
        return false;
      }

      let mut paint = PixmapPaint::default();
      paint.quality = Self::filter_quality_for_image(style);

      let transform = Transform::from_row(
        scale_x,
        0.0,
        0.0,
        scale_y,
        self.device_length(x) + dest_x,
        self.device_length(y) + dest_y,
      );
      self
        .pixmap
        .draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
      return true;
    }

    let dest_x = self.device_length(dest_x);
    let dest_y = self.device_length(dest_y);
    let dest_w = self.device_length(dest_w);
    let dest_h = self.device_length(dest_h);

    let scale_x = dest_w / img_w_raw as f32;
    let scale_y = dest_h / img_h_raw as f32;
    if !scale_x.is_finite() || !scale_y.is_finite() {
      return false;
    }

    let mut paint = PixmapPaint::default();
    paint.quality = Self::filter_quality_for_image(style);

    let transform = Transform::from_row(
      scale_x,
      0.0,
      0.0,
      scale_y,
      self.device_length(x) + dest_x,
      self.device_length(y) + dest_y,
    );
    self
      .pixmap
      .draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
    true
  }

  fn paint_svg(
    &mut self,
    content: &str,
    style: Option<&ComputedStyle>,
    x: f32,
    y: f32,
    width: f32,
    height: f32,
  ) -> bool {
    if content.is_empty() {
      return false;
    }

    let trimmed = content.trim_start();
    let inline_svg = trimmed.starts_with("<svg") || trimmed.starts_with("<?xml");

    if inline_svg {
      let meta = match self.image_cache.probe_svg_content(content, "inline-svg") {
        Ok(meta) => meta,
        Err(_) => return false,
      };

      let image_resolution = style.map(|s| s.image_resolution).unwrap_or_default();
      let orientation = style
        .map(|s| s.image_orientation.resolve(meta.orientation, false))
        .unwrap_or_else(|| ImageOrientation::default().resolve(meta.orientation, false));
      let (img_w_raw, img_h_raw) = meta.oriented_dimensions(orientation);
      if img_w_raw == 0 || img_h_raw == 0 {
        return false;
      }
      let Some((img_w_css, img_h_css)) =
        meta.css_dimensions(orientation, &image_resolution, self.scale, None)
      else {
        return false;
      };

      let fit = style.map(|s| s.object_fit).unwrap_or(ObjectFit::Fill);
      let pos = style
        .map(|s| s.object_position)
        .unwrap_or_else(default_object_position);

      let (dest_x, dest_y, dest_w, dest_h) = match compute_object_fit(
        fit,
        pos,
        width,
        height,
        img_w_css,
        img_h_css,
        style.map(|s| s.font_size).unwrap_or(16.0),
        Some((self.css_width, self.css_height)),
      ) {
        Some(v) => v,
        None => return false,
      };

      let dest_x_device = self.device_length(dest_x);
      let dest_y_device = self.device_length(dest_y);
      let dest_w_device = self.device_length(dest_w);
      let dest_h_device = self.device_length(dest_h);
      if dest_w_device <= 0.0 || dest_h_device <= 0.0 {
        return false;
      }

      let render_w = dest_w_device.ceil().max(1.0) as u32;
      let render_h = dest_h_device.ceil().max(1.0) as u32;
      let pixmap =
        match self
          .image_cache
          .render_svg_pixmap_at_size(content, render_w, render_h, "inline-svg")
        {
          Ok(pixmap) => pixmap,
          Err(_) => return false,
        };

      let scale_x = dest_w_device / render_w as f32;
      let scale_y = dest_h_device / render_h as f32;
      if !scale_x.is_finite() || !scale_y.is_finite() {
        return false;
      }

      let mut paint = PixmapPaint::default();
      paint.quality = Self::filter_quality_for_image(style);

      let transform = Transform::from_row(
        scale_x,
        0.0,
        0.0,
        scale_y,
        self.device_length(x) + dest_x_device,
        self.device_length(y) + dest_y_device,
      );
      self
        .pixmap
        .draw_pixmap(0, 0, pixmap.as_ref().as_ref(), &paint, transform, None);
      return true;
    }

    let image = match self.image_cache.load(content) {
      Ok(img) => img,
      Err(_) => return false,
    };

    let image_resolution = style.map(|s| s.image_resolution).unwrap_or_default();
    let orientation = style
      .map(|s| s.image_orientation.resolve(image.orientation, false))
      .unwrap_or_else(|| ImageOrientation::default().resolve(image.orientation, false));
    let (img_w_raw, img_h_raw) = image.oriented_dimensions(orientation);
    if img_w_raw == 0 || img_h_raw == 0 {
      return false;
    }
    let Some((img_w_css, img_h_css)) =
      image.css_dimensions(orientation, &image_resolution, self.scale, None)
    else {
      return false;
    };

    let pixmap = match Self::dynamic_image_to_pixmap(&image, orientation) {
      Some(pixmap) => pixmap,
      None => return false,
    };
    let fit = style.map(|s| s.object_fit).unwrap_or(ObjectFit::Fill);
    let pos = style
      .map(|s| s.object_position)
      .unwrap_or_else(default_object_position);

    let (dest_x, dest_y, mut dest_w, mut dest_h) = match compute_object_fit(
      fit,
      pos,
      width,
      height,
      img_w_css,
      img_h_css,
      style.map(|s| s.font_size).unwrap_or(16.0),
      Some((self.css_width, self.css_height)),
    ) {
      Some(v) => v,
      None => return false,
    };

    if matches!(
      style.map(|s| s.image_rendering),
      Some(ImageRendering::Pixelated | ImageRendering::CrispEdges)
    ) && (dest_w > img_w_raw as f32 || dest_h > img_h_raw as f32)
    {
      let (snapped_w, offset_x) = snap_upscale(dest_w, img_w_raw as f32).unwrap_or((dest_w, 0.0));
      let (snapped_h, offset_y) = snap_upscale(dest_h, img_h_raw as f32).unwrap_or((dest_h, 0.0));
      dest_w = snapped_w;
      dest_h = snapped_h;
      let dest_x = dest_x + offset_x;
      let dest_y = dest_y + offset_y;
      let dest_x = self.device_length(dest_x);
      let dest_y = self.device_length(dest_y);
      let dest_w = self.device_length(dest_w);
      let dest_h = self.device_length(dest_h);

      let scale_x = dest_w / img_w_raw as f32;
      let scale_y = dest_h / img_h_raw as f32;
      if !scale_x.is_finite() || !scale_y.is_finite() || dest_w <= 0.0 || dest_h <= 0.0 {
        return false;
      }

      let mut paint = PixmapPaint::default();
      paint.quality = Self::filter_quality_for_image(style);

      let transform = Transform::from_row(
        scale_x,
        0.0,
        0.0,
        scale_y,
        self.device_length(x) + dest_x,
        self.device_length(y) + dest_y,
      );
      self
        .pixmap
        .draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
      return true;
    }

    let dest_x = self.device_length(dest_x);
    let dest_y = self.device_length(dest_y);
    let dest_w = self.device_length(dest_w);
    let dest_h = self.device_length(dest_h);

    let scale_x = dest_w / img_w_raw as f32;
    let scale_y = dest_h / img_h_raw as f32;
    if !scale_x.is_finite() || !scale_y.is_finite() {
      return false;
    }

    let mut paint = PixmapPaint::default();
    paint.quality = Self::filter_quality_for_image(style);

    let transform = Transform::from_row(
      scale_x,
      0.0,
      0.0,
      scale_y,
      self.device_length(x) + dest_x,
      self.device_length(y) + dest_y,
    );
    self
      .pixmap
      .draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
    true
  }

  fn paint_replaced_placeholder(
    &mut self,
    replaced_type: &ReplacedType,
    style: Option<&ComputedStyle>,
    rect: Rect,
  ) {
    let log_placeholder = runtime::runtime_toggles().truthy("FASTR_LOG_IMAGE_FAIL");
    if log_placeholder {
      if let ReplacedType::Image { src, .. } = replaced_type {
        eprintln!("[image-placeholder] src={}", src);
      }
    }

    let mut paint = Paint::default();
    paint.set_color_rgba8(200, 200, 200, 255); // Light gray
    paint.anti_alias = true;

    let device_rect = self.device_rect(rect);

    if let Some(sk_rect) = SkiaRect::from_xywh(
      device_rect.x(),
      device_rect.y(),
      device_rect.width(),
      device_rect.height(),
    ) {
      let path = PathBuilder::from_rect(sk_rect);
      self.pixmap.fill_path(
        &path,
        &paint,
        tiny_skia::FillRule::Winding,
        Transform::identity(),
        None,
      );

      // Border around the placeholder
      let mut stroke_paint = Paint::default();
      stroke_paint.set_color_rgba8(150, 150, 150, 255);
      stroke_paint.anti_alias = true;

      let stroke = tiny_skia::Stroke {
        width: 1.0 * self.scale,
        ..Default::default()
      };
      self
        .pixmap
        .stroke_path(&path, &stroke_paint, &stroke, Transform::identity(), None);
    }

    // Optional label to hint the missing resource type
    let label = replaced_type.placeholder_label();

    if let (Some(style), Some(label_text)) = (style, label) {
      let mut label_style = style.clone();
      label_style.color = Rgba::rgb(120, 120, 120);
      // Use a small inset to avoid clipping against the placeholder edges
      let inset = 2.0;
      let label_rect = Rect::from_xywh(
        rect.x() + inset,
        rect.y() + inset,
        (rect.width() - 2.0 * inset).max(0.0),
        (rect.height() - 2.0 * inset).max(0.0),
      );
      let _ = self.paint_alt_text(label_text, &label_style, label_rect);
    }
  }

  fn paint_alt_text(&mut self, alt: &str, style: &ComputedStyle, rect: Rect) -> bool {
    let text = alt.trim();
    if text.is_empty() {
      return false;
    }

    let mut runs = match self.shaper.shape(text, style, &self.font_ctx) {
      Ok(runs) => runs,
      Err(_) => return false,
    };
    if runs.is_empty() {
      return false;
    }

    TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);

    let metrics_scaled = self.resolve_scaled_metrics(style);
    let line_height = compute_line_height_with_metrics_viewport(
      style,
      metrics_scaled.as_ref(),
      Some(Size::new(self.css_width, self.css_height)),
    );
    let metrics = TextItem::metrics_from_runs(&runs, line_height, style.font_size);
    let half_leading = (metrics.line_height - (metrics.ascent + metrics.descent)) / 2.0;
    let baseline_y = rect.y() + half_leading + metrics.baseline_offset;

    self.paint_shaped_runs(&runs, rect.x(), baseline_y, style.color, Some(style));
    true
  }

  fn measure_alt_text(&self, alt: &str, style: &ComputedStyle) -> Option<Size> {
    let text = alt.trim();
    if text.is_empty() {
      return None;
    }
    let mut runs = self.shaper.shape(text, style, &self.font_ctx).ok()?;
    TextItem::apply_spacing_to_runs(&mut runs, text, style.letter_spacing, style.word_spacing);
    let metrics_scaled = Self::resolve_scaled_metrics_static(style, &self.font_ctx);
    let line_height = compute_line_height_with_metrics_viewport(
      style,
      metrics_scaled.as_ref(),
      Some(Size::new(self.css_width, self.css_height)),
    );
    let width: f32 = runs.iter().map(|r| r.advance).sum();
    Some(Size::new(width, line_height))
  }

  fn decoration_metrics<'a>(
    &self,
    runs: Option<&'a [ShapedRun]>,
    style: &ComputedStyle,
  ) -> Option<DecorationMetrics> {
    let mut metrics_source = runs.and_then(|rs| {
      rs.iter()
        .find_map(|run| run.font.metrics().ok().map(|m| (m, run.font_size)))
    });

    if metrics_source.is_none() {
      let italic = matches!(style.font_style, crate::style::types::FontStyle::Italic);
      let oblique = matches!(style.font_style, crate::style::types::FontStyle::Oblique(_));
      let stretch =
        crate::text::font_db::FontStretch::from_percentage(style.font_stretch.to_percentage());
      metrics_source = self
        .font_ctx
        .get_font_full(
          &style.font_family,
          style.font_weight.to_u16(),
          if italic {
            crate::text::font_db::FontStyle::Italic
          } else if oblique {
            crate::text::font_db::FontStyle::Oblique
          } else {
            crate::text::font_db::FontStyle::Normal
          },
          stretch,
        )
        .or_else(|| self.font_ctx.get_sans_serif())
        .and_then(|font| font.metrics().ok().map(|m| (m, style.font_size)));
    }

    if let Some((metrics, size)) = metrics_source {
      let scale = size / (metrics.units_per_em as f32);

      let underline_pos = metrics.underline_position as f32 * scale;
      let underline_thickness = (metrics.underline_thickness as f32 * scale).max(1.0);
      let descent = (metrics.descent as f32 * scale).abs();
      let strike_pos = metrics
        .strikeout_position
        .map(|p| p as f32 * scale)
        .unwrap_or_else(|| metrics.ascent as f32 * scale * 0.3);
      let strike_thickness = metrics
        .strikeout_thickness
        .map(|t| t as f32 * scale)
        .unwrap_or(underline_thickness);
      let ascent = metrics.ascent as f32 * scale;

      Some(DecorationMetrics {
        underline_pos,
        underline_thickness,
        strike_pos,
        strike_thickness,
        ascent,
        descent,
      })
    } else {
      // Fallback heuristic metrics when we cannot obtain font metrics.
      let size = style.font_size.max(1.0);
      let ascent = size * 0.8;
      let descent = size - ascent;
      let underline_thickness = (size * 0.05).max(1.0);
      let underline_pos = descent * 0.5;
      let strike_pos = ascent * 0.4;

      Some(DecorationMetrics {
        underline_pos,
        underline_thickness,
        strike_pos,
        strike_thickness: underline_thickness,
        ascent,
        descent,
      })
    }
  }

  #[allow(dead_code)]
  fn resolve_underline_offset(&self, style: &ComputedStyle) -> f32 {
    self.resolve_underline_offset_value(style.text_underline_offset, style)
  }

  fn resolve_underline_offset_value(
    &self,
    offset: crate::style::types::TextUnderlineOffset,
    style: &ComputedStyle,
  ) -> f32 {
    let resolved = match offset {
      crate::style::types::TextUnderlineOffset::Auto => 0.0,
      crate::style::types::TextUnderlineOffset::Length(l) => {
        if l.unit == LengthUnit::Percent {
          l.resolve_against(style.font_size).unwrap_or(0.0)
        } else if l.unit.is_viewport_relative() {
          l.resolve_with_viewport(self.css_width, self.css_height)
            .unwrap_or_else(|| l.to_px())
        } else {
          resolve_font_relative_length(l, style, &self.font_ctx)
        }
      }
    };
    resolved * self.scale
  }

  fn resolve_decoration_thickness_value(
    &self,
    thickness: TextDecorationThickness,
    style: &ComputedStyle,
  ) -> Option<f32> {
    match thickness {
      TextDecorationThickness::Auto | TextDecorationThickness::FromFont => None,
      TextDecorationThickness::Length(l) => {
        if l.unit == LengthUnit::Percent {
          Some(l.resolve_against(style.font_size).unwrap_or(0.0) * self.scale)
        } else if l.unit.is_viewport_relative() {
          l.resolve_with_viewport(self.css_width, self.css_height)
            .map(|v| v * self.scale)
        } else {
          Some(resolve_font_relative_length(l, style, &self.font_ctx) * self.scale)
        }
      }
    }
  }

  fn underline_position(
    &self,
    metrics: &DecorationMetrics,
    position: crate::style::types::TextUnderlinePosition,
    offset: f32,
    thickness: f32,
  ) -> f32 {
    let under_base = -metrics.descent - thickness * 0.5;
    let base = match position {
      crate::style::types::TextUnderlinePosition::Auto
      | crate::style::types::TextUnderlinePosition::FromFont => metrics.underline_pos,
      crate::style::types::TextUnderlinePosition::Under
      | crate::style::types::TextUnderlinePosition::UnderLeft
      | crate::style::types::TextUnderlinePosition::UnderRight => {
        metrics.underline_pos.min(under_base)
      }
      crate::style::types::TextUnderlinePosition::Left
      | crate::style::types::TextUnderlinePosition::Right => metrics.underline_pos,
    };

    metrics.underline_position_with_offset(base, offset)
  }

  fn dynamic_image_to_pixmap(
    image: &CachedImage,
    orientation: OrientationTransform,
  ) -> Option<Pixmap> {
    let rgba = image.to_oriented_rgba(orientation);
    let (width, height) = rgba.dimensions();
    if width == 0 || height == 0 {
      return None;
    }

    // tiny-skia expects premultiplied RGBA
    let mut data = Vec::with_capacity((width * height * 4) as usize);
    for pixel in rgba.pixels() {
      let [r, g, b, a] = pixel.0;
      let alpha = a as f32 / 255.0;
      data.push((r as f32 * alpha).round() as u8);
      data.push((g as f32 * alpha).round() as u8);
      data.push((b as f32 * alpha).round() as u8);
      data.push(a);
    }

    let size = IntSize::from_wh(width, height)?;
    Pixmap::from_vec(data, size)
  }

  fn render_generated_image(
    &self,
    bg: &BackgroundImage,
    style: &ComputedStyle,
    width: u32,
    height: u32,
  ) -> Option<Pixmap> {
    if width == 0 || height == 0 {
      return None;
    }

    let rect = Rect::from_xywh(0.0, 0.0, width as f32, height as f32);
    match bg {
      BackgroundImage::LinearGradient { angle, stops } => {
        let resolved = normalize_color_stops(stops, style.color);
        if resolved.is_empty() {
          return None;
        }
        let gradient_rect = rect;
        let skia_stops = gradient_stops(&resolved);
        let rad = angle.to_radians();
        let dx = rad.sin();
        let dy = -rad.cos();
        let len = 0.5 * (gradient_rect.width() * dx.abs() + gradient_rect.height() * dy.abs());
        let cx = gradient_rect.x() + gradient_rect.width() / 2.0;
        let cy = gradient_rect.y() + gradient_rect.height() / 2.0;

        let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
        let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
        let shader = LinearGradient::new(
          start,
          end,
          skia_stops,
          SpreadMode::Pad,
          Transform::identity(),
        )?;

        let mut pixmap = Pixmap::new(width, height)?;
        let skia_rect = SkiaRect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
        let path = PathBuilder::from_rect(skia_rect);

        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        pixmap.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          None,
        );
        Some(pixmap)
      }
      BackgroundImage::RepeatingLinearGradient { angle, stops } => {
        let resolved = normalize_color_stops(stops, style.color);
        if resolved.is_empty() {
          return None;
        }
        let gradient_rect = rect;
        let skia_stops = gradient_stops(&resolved);
        let rad = angle.to_radians();
        let dx = rad.sin();
        let dy = -rad.cos();
        let len = 0.5 * (gradient_rect.width() * dx.abs() + gradient_rect.height() * dy.abs());
        let cx = gradient_rect.x() + gradient_rect.width() / 2.0;
        let cy = gradient_rect.y() + gradient_rect.height() / 2.0;

        let start = tiny_skia::Point::from_xy(cx - dx * len, cy - dy * len);
        let end = tiny_skia::Point::from_xy(cx + dx * len, cy + dy * len);
        let shader = LinearGradient::new(
          start,
          end,
          skia_stops,
          SpreadMode::Repeat,
          Transform::identity(),
        )?;

        let mut pixmap = Pixmap::new(width, height)?;
        let skia_rect = SkiaRect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
        let path = PathBuilder::from_rect(skia_rect);

        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        pixmap.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          None,
        );
        Some(pixmap)
      }
      BackgroundImage::RadialGradient {
        shape,
        size,
        position,
        stops,
      } => {
        let resolved = normalize_color_stops(stops, style.color);
        if resolved.is_empty() {
          return None;
        }
        let skia_stops = gradient_stops(&resolved);
        let (cx, cy, radius_x, radius_y) = radial_geometry(
          rect,
          position,
          size,
          *shape,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
        );
        let transform = Transform::from_translate(cx, cy).pre_scale(radius_x, radius_y);
        let shader = RadialGradient::new(
          tiny_skia::Point::from_xy(0.0, 0.0),
          tiny_skia::Point::from_xy(0.0, 0.0),
          1.0,
          skia_stops,
          SpreadMode::Pad,
          transform,
        )?;

        let mut pixmap = Pixmap::new(width, height)?;
        let skia_rect = SkiaRect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
        let path = PathBuilder::from_rect(skia_rect);
        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        pixmap.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          None,
        );
        Some(pixmap)
      }
      BackgroundImage::RepeatingRadialGradient {
        shape,
        size,
        position,
        stops,
      } => {
        let resolved = normalize_color_stops(stops, style.color);
        if resolved.is_empty() {
          return None;
        }
        let skia_stops = gradient_stops(&resolved);
        let (cx, cy, radius_x, radius_y) = radial_geometry(
          rect,
          position,
          size,
          *shape,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
        );
        let transform = Transform::from_translate(cx, cy).pre_scale(radius_x, radius_y);
        let shader = RadialGradient::new(
          tiny_skia::Point::from_xy(0.0, 0.0),
          tiny_skia::Point::from_xy(0.0, 0.0),
          1.0,
          skia_stops,
          SpreadMode::Repeat,
          transform,
        )?;

        let mut pixmap = Pixmap::new(width, height)?;
        let skia_rect = SkiaRect::from_xywh(0.0, 0.0, width as f32, height as f32)?;
        let path = PathBuilder::from_rect(skia_rect);
        let mut paint = Paint::default();
        paint.shader = shader;
        paint.anti_alias = true;
        pixmap.fill_path(
          &path,
          &paint,
          tiny_skia::FillRule::Winding,
          Transform::identity(),
          None,
        );
        Some(pixmap)
      }
      BackgroundImage::ConicGradient {
        from_angle,
        position,
        stops,
      } => {
        let resolved = normalize_color_stops_unclamped(stops, style.color);
        if resolved.is_empty() {
          return None;
        }
        let mut pixmap = Pixmap::new(width, height)?;
        let center = resolve_gradient_center(
          rect,
          position,
          rect,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
        );
        let start_angle = from_angle.to_radians();
        let period = 1.0;
        let data = pixmap.pixels_mut();
        let transparent = PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
        for y in 0..height {
          for x in 0..width {
            let dx = x as f32 + 0.5 - center.x;
            let dy = y as f32 + 0.5 - center.y;
            let angle = dx.atan2(-dy) + start_angle;
            let mut t = (angle / (2.0 * std::f32::consts::PI)).rem_euclid(1.0);
            t *= period;
            let color = sample_stops(&resolved, t, false, period);
            let idx = (y * width + x) as usize;
            data[idx] = PremultipliedColorU8::from_rgba(
              color.r,
              color.g,
              color.b,
              (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
            )
            .unwrap_or(transparent);
          }
        }
        Some(pixmap)
      }
      BackgroundImage::RepeatingConicGradient {
        from_angle,
        position,
        stops,
      } => {
        let resolved = normalize_color_stops_unclamped(stops, style.color);
        if resolved.is_empty() {
          return None;
        }
        let mut pixmap = Pixmap::new(width, height)?;
        let center = resolve_gradient_center(
          rect,
          position,
          rect,
          style.font_size,
          style.root_font_size,
          (self.css_width, self.css_height),
        );
        let start_angle = from_angle.to_radians();
        let period = resolved.last().map(|(p, _)| *p).unwrap_or(1.0).max(1e-6);
        let data = pixmap.pixels_mut();
        let transparent = PremultipliedColorU8::from_rgba(0, 0, 0, 0).unwrap();
        for y in 0..height {
          for x in 0..width {
            let dx = x as f32 + 0.5 - center.x;
            let dy = y as f32 + 0.5 - center.y;
            let angle = dx.atan2(-dy) + start_angle;
            let mut t = (angle / (2.0 * std::f32::consts::PI)).rem_euclid(1.0);
            t *= period;
            let color = sample_stops(&resolved, t, true, period);
            let idx = (y * width + x) as usize;
            data[idx] = PremultipliedColorU8::from_rgba(
              color.r,
              color.g,
              color.b,
              (color.a * 255.0).round().clamp(0.0, 255.0) as u8,
            )
            .unwrap_or(transparent);
          }
        }
        Some(pixmap)
      }
      BackgroundImage::Url(_) | BackgroundImage::None => None,
    }
  }

  fn paint_text_decoration(
    &mut self,
    style: &ComputedStyle,
    runs: Option<&[ShapedRun]>,
    inline_start: f32,
    block_baseline: f32,
    inline_len: f32,
    inline_vertical: bool,
  ) {
    let has_any_decoration =
      !style.applied_text_decorations.is_empty() || !style.text_decoration.lines.is_empty();
    if !has_any_decoration || inline_len <= 0.0 {
      return;
    }

    let inline_start = self.device_length(inline_start);
    let block_baseline = self.device_length(block_baseline);
    let inline_len = self.device_length(inline_len);

    let Some(metrics) = self.decoration_metrics(runs, style) else {
      return;
    };
    let metrics = metrics.scaled(self.scale);

    let draw_solid_line =
      |pixmap: &mut Pixmap, paint: &Paint, start: f32, len: f32, center: f32, thickness: f32| {
        if thickness <= 0.0 || len <= 0.0 {
          return;
        }

        let rect = if inline_vertical {
          SkiaRect::from_xywh(center - thickness * 0.5, start, thickness, len)
        } else {
          SkiaRect::from_xywh(start, center - thickness * 0.5, len, thickness)
        };
        if let Some(rect) = rect {
          let path = PathBuilder::from_rect(rect);
          pixmap.fill_path(
            &path,
            &paint,
            tiny_skia::FillRule::Winding,
            Transform::identity(),
            None,
          );
        }
      };

    let draw_stroked_line = |pixmap: &mut Pixmap,
                             paint: &Paint,
                             start: f32,
                             len: f32,
                             center: f32,
                             thickness: f32,
                             dash: Option<Vec<f32>>,
                             round: bool| {
      let mut path = PathBuilder::new();
      if inline_vertical {
        path.move_to(center, start);
        path.line_to(center, start + len);
      } else {
        path.move_to(start, center);
        path.line_to(start + len, center);
      }
      let Some(path) = path.finish() else { return };

      let mut stroke = Stroke::default();
      stroke.width = thickness;
      stroke.line_cap = if round {
        tiny_skia::LineCap::Round
      } else {
        tiny_skia::LineCap::Butt
      };
      if let Some(arr) = dash {
        stroke.dash = tiny_skia::StrokeDash::new(arr, 0.0);
      }

      pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
    };

    let draw_wavy_line =
      |pixmap: &mut Pixmap, paint: &Paint, start: f32, len: f32, center: f32, thickness: f32| {
        if thickness <= 0.0 || len <= 0.0 {
          return;
        }
        let wavelength = (thickness * 4.0).max(6.0);
        let amplitude = (thickness * 0.75).max(thickness * 0.5);

        let mut path = PathBuilder::new();
        if inline_vertical {
          path.move_to(center, start);
        } else {
          path.move_to(start, center);
        }
        let mut cursor = start;
        let mut up = true;
        while cursor < start + len {
          let end = (cursor + wavelength).min(start + len);
          let mid = cursor + (end - cursor) * 0.5;
          if inline_vertical {
            let control_x = if up {
              center - amplitude
            } else {
              center + amplitude
            };
            path.quad_to(control_x, mid, center, end);
          } else {
            let control_y = if up {
              center - amplitude
            } else {
              center + amplitude
            };
            path.quad_to(mid, control_y, end, center);
          }
          cursor = end;
          up = !up;
        }

        if let Some(path) = path.finish() {
          let mut stroke = Stroke::default();
          stroke.width = thickness.max(0.5);
          stroke.line_cap = tiny_skia::LineCap::Round;
          pixmap.stroke_path(&path, &paint, &stroke, Transform::identity(), None);
        }
      };

    let decorations = if !style.applied_text_decorations.is_empty() {
      Cow::Borrowed(style.applied_text_decorations.as_slice())
    } else {
      Cow::Owned(vec![crate::style::types::ResolvedTextDecoration {
        decoration: style.text_decoration.clone(),
        skip_ink: style.text_decoration_skip_ink,
        underline_offset: style.text_underline_offset,
        underline_position: style.text_underline_position,
      }])
    };

    for deco in decorations.iter() {
      let mut paint = Paint::default();
      paint.anti_alias = true;
      let decoration_color = deco.decoration.color.unwrap_or(style.color);
      paint.set_color(color_to_skia(decoration_color));

      let used_thickness =
        self.resolve_decoration_thickness_value(deco.decoration.thickness, style);

      let painter_style = deco.decoration.style;
      let render_line = |pixmap: &mut Pixmap, center: f32, thickness: f32| match painter_style {
        TextDecorationStyle::Solid => {
          draw_solid_line(pixmap, &paint, inline_start, inline_len, center, thickness);
        }
        TextDecorationStyle::Double => {
          let line_thickness = (thickness * 0.7).max(0.5);
          let gap = line_thickness.max(thickness * 0.6);
          draw_solid_line(
            pixmap,
            &paint,
            inline_start,
            inline_len,
            center - (gap * 0.5),
            line_thickness,
          );
          draw_solid_line(
            pixmap,
            &paint,
            inline_start,
            inline_len,
            center + (gap * 0.5),
            line_thickness,
          );
        }
        TextDecorationStyle::Dotted => {
          draw_stroked_line(
            pixmap,
            &paint,
            inline_start,
            inline_len,
            center,
            thickness,
            Some(vec![thickness, thickness]),
            true,
          );
        }
        TextDecorationStyle::Dashed => {
          draw_stroked_line(
            pixmap,
            &paint,
            inline_start,
            inline_len,
            center,
            thickness,
            Some(vec![3.0 * thickness, thickness]),
            false,
          );
        }
        TextDecorationStyle::Wavy => {
          draw_wavy_line(pixmap, &paint, inline_start, inline_len, center, thickness);
        }
      };

      let render_line_segment = |pixmap: &mut Pixmap,
                                 start: f32,
                                 len: f32,
                                 center: f32,
                                 thickness: f32| match painter_style
      {
        TextDecorationStyle::Solid => {
          draw_solid_line(pixmap, &paint, start, len, center, thickness);
        }
        TextDecorationStyle::Double => {
          let line_thickness = (thickness * 0.7).max(0.5);
          let gap = line_thickness.max(thickness * 0.6);
          draw_solid_line(
            pixmap,
            &paint,
            start,
            len,
            center - (gap * 0.5),
            line_thickness,
          );
          draw_solid_line(
            pixmap,
            &paint,
            start,
            len,
            center + (gap * 0.5),
            line_thickness,
          );
        }
        TextDecorationStyle::Dotted => {
          draw_stroked_line(
            pixmap,
            &paint,
            start,
            len,
            center,
            thickness,
            Some(vec![thickness, thickness]),
            true,
          );
        }
        TextDecorationStyle::Dashed => {
          draw_stroked_line(
            pixmap,
            &paint,
            start,
            len,
            center,
            thickness,
            Some(vec![3.0 * thickness, thickness]),
            false,
          );
        }
        TextDecorationStyle::Wavy => {
          draw_wavy_line(pixmap, &paint, start, len, center, thickness);
        }
      };

      let underline_offset = self.resolve_underline_offset_value(deco.underline_offset, style);
      if deco
        .decoration
        .lines
        .contains(TextDecorationLine::UNDERLINE)
      {
        let thickness = used_thickness.unwrap_or(metrics.underline_thickness);
        let adjusted_pos = self.underline_position(
          &metrics,
          deco.underline_position,
          underline_offset,
          thickness,
        );
        let center = block_baseline - adjusted_pos;
        if inline_vertical {
          render_line(&mut self.pixmap, center, thickness);
        } else if matches!(
          deco.skip_ink,
          crate::style::types::TextDecorationSkipInk::Auto
            | crate::style::types::TextDecorationSkipInk::All
        ) {
          if let Some(runs) = runs {
            let segments = self.build_underline_segments(
              runs,
              inline_start,
              inline_len,
              center,
              thickness,
              block_baseline,
              inline_vertical,
              deco.skip_ink,
            );
            for (seg_start, seg_end) in segments {
              let seg_width = seg_end - seg_start;
              if seg_width <= 0.0 {
                continue;
              }
              render_line_segment(&mut self.pixmap, seg_start, seg_width, center, thickness);
            }
          } else {
            render_line(&mut self.pixmap, center, thickness);
          }
        } else {
          render_line(&mut self.pixmap, center, thickness);
        }
      }
      if deco.decoration.lines.contains(TextDecorationLine::OVERLINE) {
        render_line(
          &mut self.pixmap,
          block_baseline - metrics.ascent,
          used_thickness.unwrap_or(metrics.underline_thickness),
        );
      }
      if deco
        .decoration
        .lines
        .contains(TextDecorationLine::LINE_THROUGH)
      {
        render_line(
          &mut self.pixmap,
          block_baseline - metrics.strike_pos,
          used_thickness.unwrap_or(metrics.strike_thickness),
        );
      }
    }
  }

  fn build_underline_segments(
    &self,
    runs: &[ShapedRun],
    line_start: f32,
    line_width: f32,
    center: f32,
    thickness: f32,
    baseline_y: f32,
    inline_vertical: bool,
    skip_ink: TextDecorationSkipInk,
  ) -> Vec<(f32, f32)> {
    if line_width <= 0.0 {
      return Vec::new();
    }

    let band_half = (thickness * 0.5).abs();
    let mut exclusions = if inline_vertical {
      let band_left = center - band_half;
      let band_right = center + band_half;
      collect_underline_exclusions_vertical(
        runs,
        line_start,
        baseline_y,
        band_left,
        band_right,
        skip_ink == TextDecorationSkipInk::All,
        self.scale,
      )
    } else {
      let band_top = center - band_half;
      let band_bottom = center + band_half;
      collect_underline_exclusions(
        runs,
        line_start,
        baseline_y,
        band_top,
        band_bottom,
        skip_ink == TextDecorationSkipInk::All,
        self.scale,
      )
    };

    let mut segments = subtract_intervals(
      (line_start, line_start + line_width),
      exclusions.as_mut_slice(),
    );
    if segments.is_empty() && skip_ink != TextDecorationSkipInk::All {
      // Never drop the underline entirely when skipping ink; fall back to a full span.
      segments.push((line_start, line_start + line_width));
    }
    segments
  }

  fn paint_text_emphasis(
    &mut self,
    style: &ComputedStyle,
    runs: Option<&[ShapedRun]>,
    inline_origin: f32,
    block_baseline: f32,
    inline_vertical: bool,
  ) {
    let runs = match runs {
      Some(r) => r,
      None => return,
    };
    if style.text_emphasis_style.is_none() {
      return;
    }

    let inline_origin = self.device_length(inline_origin);
    let block_baseline = self.device_length(block_baseline);

    let Some(metrics) = self.decoration_metrics(Some(runs), style) else {
      return;
    };
    let metrics = metrics.scaled(self.scale);

    let resolved_position = match style.text_emphasis_position {
      crate::style::types::TextEmphasisPosition::Auto => {
        crate::style::types::TextEmphasisPosition::Over
      }
      other => other,
    };
    let emphasis_color = style.text_emphasis_color.unwrap_or(style.color);
    let mark_size = (style.font_size * 0.5 * self.scale).max(1.0);
    let gap = mark_size * 0.3;

    let block_center = if inline_vertical {
      let offset = gap + mark_size * 0.5;
      match resolved_position {
        crate::style::types::TextEmphasisPosition::Over
        | crate::style::types::TextEmphasisPosition::OverLeft
        | crate::style::types::TextEmphasisPosition::OverRight => block_baseline + offset,
        crate::style::types::TextEmphasisPosition::Under
        | crate::style::types::TextEmphasisPosition::UnderLeft
        | crate::style::types::TextEmphasisPosition::UnderRight => block_baseline - offset,
        crate::style::types::TextEmphasisPosition::Auto => block_baseline + offset,
      }
    } else {
      match resolved_position {
        crate::style::types::TextEmphasisPosition::Over
        | crate::style::types::TextEmphasisPosition::OverLeft
        | crate::style::types::TextEmphasisPosition::OverRight => {
          block_baseline - metrics.ascent - gap - mark_size * 0.5
        }
        crate::style::types::TextEmphasisPosition::Under
        | crate::style::types::TextEmphasisPosition::UnderLeft
        | crate::style::types::TextEmphasisPosition::UnderRight => {
          block_baseline + metrics.descent + gap + mark_size * 0.5
        }
        crate::style::types::TextEmphasisPosition::Auto => {
          block_baseline - metrics.ascent - gap - mark_size * 0.5
        }
      }
    };

    // Precompute mark painter for string emphasis.
    let mut string_mark: Option<(Vec<tiny_skia::Path>, f32, f32)> = None;
    if let crate::style::types::TextEmphasisStyle::String(ref s) = style.text_emphasis_style {
      if !s.is_empty() {
        let mut mark_style = style.clone();
        mark_style.font_size = style.font_size * 0.5;
        let Ok(mark_runs) = self.shaper.shape(s, &mark_style, &self.font_ctx) else {
          return;
        };
        let mark_width: f32 = mark_runs.iter().map(|r| r.advance * self.scale).sum();
        let mark_metrics =
          crate::layout::contexts::inline::line_builder::TextItem::metrics_from_runs(
            &mark_runs,
            mark_style.font_size,
            mark_style.font_size,
          );
        let mut paths = Vec::new();
        let mut pen_x = 0.0;
        for run in &mark_runs {
          let advance = run.advance * self.scale;
          let run_origin = if run.direction.is_rtl() {
            pen_x + advance
          } else {
            pen_x
          };
          let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
            Ok(f) => f,
            Err(_) => continue,
          };
          let units_per_em = face.units_per_em() as f32;
          let scale = (run.font_size / units_per_em) * self.scale;
          for glyph in &run.glyphs {
            let glyph_x = match run.direction {
              crate::text::pipeline::Direction::RightToLeft => {
                run_origin - glyph.x_offset * self.scale
              }
              crate::text::pipeline::Direction::LeftToRight => {
                run_origin + glyph.x_offset * self.scale
              }
            };
            let glyph_y = mark_metrics.baseline_offset * self.scale;
            if let Some(path) =
              Self::build_glyph_path(&face, glyph.glyph_id as u16, glyph_x, glyph_y, scale)
            {
              paths.push(path);
            }
          }
          pen_x += advance;
        }
        if !paths.is_empty() {
          string_mark = Some((
            paths,
            mark_width,
            (mark_metrics.ascent + mark_metrics.descent) * self.scale,
          ));
        }
      }
    }

    let mut pen_inline = inline_origin;
    for run in runs {
      let advance = run.advance * self.scale;
      let run_origin_inline = if run.direction.is_rtl() {
        pen_inline + advance
      } else {
        pen_inline
      };

      let mut seen_clusters = std::collections::HashSet::new();
      for glyph in &run.glyphs {
        if !seen_clusters.insert(glyph.cluster) {
          continue;
        }
        let text_byte = glyph.cluster as usize;
        if text_byte < run.text.len() {
          if let Some(ch) = run.text[text_byte..].chars().next() {
            if ch.is_whitespace() || ch.is_control() {
              continue;
            }
          }
        }
        let inline_center = match run.direction {
          crate::text::pipeline::Direction::RightToLeft => {
            run_origin_inline - (glyph.x_offset + glyph.x_advance * 0.5) * self.scale
          }
          crate::text::pipeline::Direction::LeftToRight => {
            run_origin_inline + (glyph.x_offset + glyph.x_advance * 0.5) * self.scale
          }
        };
        let (mark_center_x, mark_center_y) = if inline_vertical {
          (block_center, inline_center)
        } else {
          (inline_center, block_center)
        };

        match style.text_emphasis_style {
          crate::style::types::TextEmphasisStyle::Mark { fill, shape } => {
            self.draw_emphasis_mark(
              mark_center_x,
              mark_center_y,
              mark_size,
              fill,
              shape,
              emphasis_color,
              resolved_position,
              inline_vertical,
            );
          }
          crate::style::types::TextEmphasisStyle::String(_) => {
            if let Some((ref paths, width, height)) = string_mark {
              let mut paint = Paint::default();
              paint.anti_alias = true;
              paint.set_color(color_to_skia(emphasis_color));

              let (draw_w, draw_h) = if inline_vertical {
                (height, width)
              } else {
                (width, height)
              };
              let offset_x = mark_center_x - draw_w * 0.5;
              let offset_y = mark_center_y - draw_h * 0.5;
              for path in paths {
                let translated = path
                  .clone()
                  .transform(Transform::from_translate(offset_x, offset_y));
                if let Some(p) = translated {
                  self.pixmap.fill_path(
                    &p,
                    &paint,
                    tiny_skia::FillRule::EvenOdd,
                    Transform::identity(),
                    None,
                  );
                }
              }
            }
          }
          crate::style::types::TextEmphasisStyle::None => {}
        }
      }

      pen_inline += advance;
    }
  }

  fn draw_emphasis_mark(
    &mut self,
    center_x: f32,
    center_y: f32,
    size: f32,
    fill: crate::style::types::TextEmphasisFill,
    shape: crate::style::types::TextEmphasisShape,
    color: Rgba,
    position: crate::style::types::TextEmphasisPosition,
    inline_vertical: bool,
  ) {
    let mut paint = Paint::default();
    paint.anti_alias = true;
    paint.set_color(color_to_skia(color));

    match shape {
      crate::style::types::TextEmphasisShape::Dot => {
        let radius = size * 0.5;
        if let Some(path) = PathBuilder::from_circle(center_x, center_y, radius) {
          match fill {
            crate::style::types::TextEmphasisFill::Filled => self.pixmap.fill_path(
              &path,
              &paint,
              tiny_skia::FillRule::EvenOdd,
              Transform::identity(),
              None,
            ),
            crate::style::types::TextEmphasisFill::Open => {
              let mut stroke = Stroke::default();
              stroke.width = (size * 0.18).max(0.5);
              self
                .pixmap
                .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
            }
          }
        }
      }
      crate::style::types::TextEmphasisShape::Circle => {
        let radius = size * 0.5;
        if let Some(path) = PathBuilder::from_circle(center_x, center_y, radius) {
          match fill {
            crate::style::types::TextEmphasisFill::Filled => self.pixmap.fill_path(
              &path,
              &paint,
              tiny_skia::FillRule::EvenOdd,
              Transform::identity(),
              None,
            ),
            crate::style::types::TextEmphasisFill::Open => {
              let mut stroke = Stroke::default();
              stroke.width = (size * 0.18).max(0.5);
              self
                .pixmap
                .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
            }
          }
        }
      }
      crate::style::types::TextEmphasisShape::DoubleCircle => {
        let mut stroke = Stroke::default();
        stroke.width = (size * 0.14).max(0.5);
        let radii = [size * 0.5, size * 0.33];
        for radius in radii {
          if let Some(path) = PathBuilder::from_circle(center_x, center_y, radius) {
            self
              .pixmap
              .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
          }
        }
      }
      crate::style::types::TextEmphasisShape::Triangle => {
        let half = size * 0.5;
        let height = size * 0.9;
        let direction = matches!(
          position,
          crate::style::types::TextEmphasisPosition::Over
            | crate::style::types::TextEmphasisPosition::OverLeft
            | crate::style::types::TextEmphasisPosition::OverRight
        );
        let mut builder = PathBuilder::new();
        if inline_vertical {
          let apex_x = if direction {
            center_x - height * 0.5
          } else {
            center_x + height * 0.5
          };
          let base_x = if direction {
            center_x + height * 0.5
          } else {
            center_x - height * 0.5
          };
          builder.move_to(apex_x, center_y);
          builder.line_to(base_x, center_y - half);
          builder.line_to(base_x, center_y + half);
        } else {
          let apex_y = if direction {
            center_y - height * 0.5
          } else {
            center_y + height * 0.5
          };
          let base_y = if direction {
            center_y + height * 0.5
          } else {
            center_y - height * 0.5
          };
          builder.move_to(center_x, apex_y);
          builder.line_to(center_x - half, base_y);
          builder.line_to(center_x + half, base_y);
        }
        builder.close();
        if let Some(path) = builder.finish() {
          match fill {
            crate::style::types::TextEmphasisFill::Filled => {
              self.pixmap.fill_path(
                &path,
                &paint,
                tiny_skia::FillRule::EvenOdd,
                Transform::identity(),
                None,
              );
            }
            crate::style::types::TextEmphasisFill::Open => {
              let mut stroke = Stroke::default();
              stroke.width = (size * 0.18).max(0.5);
              self
                .pixmap
                .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
            }
          }
        }
      }
      crate::style::types::TextEmphasisShape::Sesame => {
        let len = size * 0.75;
        let angle = 20.0_f32.to_radians();
        let dx = (angle.cos() * len * 0.5, angle.sin() * len * 0.5);
        let mut builder = PathBuilder::new();
        if inline_vertical {
          // Rotate sesame strokes to slant along the block axis for vertical writing.
          builder.move_to(center_x - dx.1, center_y - dx.0);
          builder.line_to(center_x + dx.1, center_y + dx.0);
        } else {
          builder.move_to(center_x - dx.0, center_y - dx.1);
          builder.line_to(center_x + dx.0, center_y + dx.1);
        }
        if let Some(path) = builder.finish() {
          let mut stroke = Stroke::default();
          stroke.width = (size * 0.2).max(0.6);
          stroke.line_cap = tiny_skia::LineCap::Round;
          self
            .pixmap
            .stroke_path(&path, &paint, &stroke, Transform::identity(), None);
        }
      }
    }
  }
}

#[derive(Debug, Clone, Copy)]
struct DecorationMetrics {
  underline_pos: f32,
  underline_thickness: f32,
  strike_pos: f32,
  strike_thickness: f32,
  ascent: f32,
  descent: f32,
}

impl DecorationMetrics {
  fn underline_position_with_offset(&self, base: f32, offset: f32) -> f32 {
    let direction = if base >= 0.0 { 1.0 } else { -1.0 };
    base + offset * direction
  }

  fn scaled(self, scale: f32) -> Self {
    Self {
      underline_pos: self.underline_pos * scale,
      underline_thickness: self.underline_thickness * scale,
      strike_pos: self.strike_pos * scale,
      strike_thickness: self.strike_thickness * scale,
      ascent: self.ascent * scale,
      descent: self.descent * scale,
    }
  }
}

fn collect_underline_exclusions(
  runs: &[ShapedRun],
  line_start: f32,
  baseline_y: f32,
  band_top: f32,
  band_bottom: f32,
  skip_all: bool,
  device_scale: f32,
) -> Vec<(f32, f32)> {
  let mut intervals = Vec::new();
  // Small inflation to account for antialiasing without swallowing the entire line.
  let tolerance = 0.5 * device_scale;

  let mut pen_x = line_start * device_scale;
  for run in runs {
    let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
      Ok(f) => f,
      Err(_) => continue,
    };
    let units_per_em = face.units_per_em() as f32;
    if units_per_em == 0.0 {
      continue;
    }
    let mut scale = run.font_size / units_per_em;
    scale *= run.scale * device_scale;
    let advance = run.advance * device_scale;
    let run_origin = if run.direction.is_rtl() {
      pen_x + advance
    } else {
      pen_x
    };

    for glyph in &run.glyphs {
      let glyph_x = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => run_origin - glyph.x_offset * device_scale,
        crate::text::pipeline::Direction::LeftToRight => run_origin + glyph.x_offset * device_scale,
      };
      let glyph_y = baseline_y - glyph.y_offset * device_scale;
      if let Some(bbox) = face.glyph_bounding_box(ttf_parser::GlyphId(glyph.glyph_id as u16)) {
        let left = glyph_x + bbox.x_min as f32 * scale - tolerance;
        let right = glyph_x + bbox.x_max as f32 * scale + tolerance;
        let top = glyph_y - bbox.y_max as f32 * scale - tolerance;
        let bottom = glyph_y - bbox.y_min as f32 * scale + tolerance;

        if skip_all || (bottom >= band_top && top <= band_bottom) {
          intervals.push((left, right));
        }
      }
    }

    pen_x += advance;
  }

  intervals
}

fn collect_underline_exclusions_vertical(
  runs: &[ShapedRun],
  inline_start: f32,
  block_baseline: f32,
  band_left: f32,
  band_right: f32,
  skip_all: bool,
  device_scale: f32,
) -> Vec<(f32, f32)> {
  let mut intervals = Vec::new();
  let tolerance = 0.5 * device_scale;

  let mut pen_inline = inline_start * device_scale;
  for run in runs {
    let face = match ttf_parser::Face::parse(&run.font.data, run.font.index) {
      Ok(f) => f,
      Err(_) => continue,
    };
    let units_per_em = face.units_per_em() as f32;
    if units_per_em == 0.0 {
      continue;
    }
    let mut scale = run.font_size / units_per_em;
    scale *= run.scale * device_scale;
    let advance = run.advance * device_scale;
    let run_origin = if run.direction.is_rtl() {
      pen_inline + advance
    } else {
      pen_inline
    };

    for glyph in &run.glyphs {
      let inline_pos = match run.direction {
        crate::text::pipeline::Direction::RightToLeft => run_origin - glyph.x_offset * device_scale,
        crate::text::pipeline::Direction::LeftToRight => run_origin + glyph.x_offset * device_scale,
      };
      let block_pos = block_baseline - glyph.y_offset * device_scale;
      if let Some(bbox) = face.glyph_bounding_box(ttf_parser::GlyphId(glyph.glyph_id as u16)) {
        let inline_left = inline_pos + bbox.x_min as f32 * scale - tolerance;
        let inline_right = inline_pos + bbox.x_max as f32 * scale + tolerance;
        let block_top = block_pos - bbox.y_max as f32 * scale - tolerance;
        let block_bottom = block_pos - bbox.y_min as f32 * scale + tolerance;

        if skip_all || (block_bottom >= band_left && block_top <= band_right) {
          intervals.push((inline_left, inline_right));
        }
      }
    }

    pen_inline += advance;
  }

  intervals
}

fn subtract_intervals(total: (f32, f32), exclusions: &mut [(f32, f32)]) -> Vec<(f32, f32)> {
  exclusions.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap_or(std::cmp::Ordering::Equal));
  let mut start = total.0;
  let mut allowed = Vec::new();

  for &(ex_start, ex_end) in exclusions.iter() {
    if ex_end <= start {
      continue;
    }
    if ex_start > total.1 {
      break;
    }
    let seg_end = ex_start.min(total.1);
    if seg_end > start {
      allowed.push((start, seg_end));
    }
    start = ex_end.max(start);
    if start >= total.1 {
      break;
    }
  }

  if start < total.1 {
    allowed.push((start, total.1));
  }

  allowed
}

fn color_to_skia(color: Rgba) -> tiny_skia::Color {
  let alpha = (color.a * 255.0).clamp(0.0, 255.0).round() as u8;
  tiny_skia::Color::from_rgba8(color.r, color.g, color.b, alpha)
}

fn build_transform_3d(
  style: Option<&ComputedStyle>,
  bounds: Rect,
  viewport: Option<(f32, f32)>,
) -> Option<Transform3D> {
  style.and_then(|style| DisplayListBuilder::debug_resolve_transform(style, bounds, viewport))
}

fn transform2d_to_skia(transform: Transform2D) -> Transform {
  Transform::from_row(
    transform.a,
    transform.b,
    transform.c,
    transform.d,
    transform.e,
    transform.f,
  )
}

fn transform_rect(rect: Rect, ts: &Transform) -> Rect {
  let corners = [
    (rect.min_x(), rect.min_y()),
    (rect.max_x(), rect.min_y()),
    (rect.max_x(), rect.max_y()),
    (rect.min_x(), rect.max_y()),
  ];
  let mut min_x = f32::INFINITY;
  let mut min_y = f32::INFINITY;
  let mut max_x = f32::NEG_INFINITY;
  let mut max_y = f32::NEG_INFINITY;

  for (x, y) in corners {
    let tx = x * ts.sx + y * ts.kx + ts.tx;
    let ty = x * ts.ky + y * ts.sy + ts.ty;
    min_x = min_x.min(tx);
    min_y = min_y.min(ty);
    max_x = max_x.max(tx);
    max_y = max_y.max(ty);
  }

  Rect::from_xywh(min_x, min_y, max_x - min_x, max_y - min_y)
}

fn approx_same_rect(a: Rect, b: Rect) -> bool {
  let eps = 0.001;
  (a.min_x() - b.min_x()).abs() <= eps
    && (a.min_y() - b.min_y()).abs() <= eps
    && (a.width() - b.width()).abs() <= eps
    && (a.height() - b.height()).abs() <= eps
}

fn decode_data_url_to_string(data_url: &str) -> Result<String> {
  let mut parts = data_url.splitn(2, ',');
  let header = parts.next().ok_or_else(|| RenderError::InvalidParameters {
    message: "Invalid data URL".to_string(),
  })?;
  let data = parts.next().ok_or_else(|| RenderError::InvalidParameters {
    message: "Invalid data URL".to_string(),
  })?;

  let is_base64 = header.to_ascii_lowercase().ends_with(";base64");
  let decoded = if is_base64 {
    base64::engine::general_purpose::STANDARD
      .decode(data.as_bytes())
      .map_err(|_| RenderError::InvalidParameters {
        message: "Invalid base64 in data URL".to_string(),
      })?
  } else {
    percent_decode(data.as_bytes()).collect()
  };

  if let Some((enc, bom_len)) = Encoding::for_bom(&decoded) {
    return Ok(
      enc
        .decode_without_bom_handling(&decoded[bom_len..])
        .0
        .into_owned(),
    );
  }

  Ok(String::from_utf8_lossy(&decoded).into_owned())
}

struct EmbeddedImportFetcher {
  base_url: Option<String>,
}

impl EmbeddedImportFetcher {
  fn resolve_url(&self, href: &str) -> Option<Url> {
    if href.starts_with("data:") {
      return None;
    }

    if let Ok(abs) = Url::parse(href) {
      return Some(abs);
    }

    let base = self.base_url.as_ref()?;
    let mut base_candidate = base.clone();
    if base_candidate.starts_with("file://") {
      let path = &base_candidate["file://".len()..];
      if std::path::Path::new(path).is_dir() && !base_candidate.ends_with('/') {
        base_candidate.push('/');
      }
    }

    Url::parse(&base_candidate)
      .or_else(|_| {
        Url::from_file_path(&base_candidate).map_err(|()| url::ParseError::RelativeUrlWithoutBase)
      })
      .ok()
      .and_then(|base_url| base_url.join(href).ok())
  }
}

impl css::types::CssImportLoader for EmbeddedImportFetcher {
  fn load(&self, url: &str) -> Result<String> {
    if url.starts_with("data:") {
      return decode_data_url_to_string(url);
    }

    let resolved = self
      .resolve_url(url)
      .or_else(|| Url::parse(url).ok())
      .ok_or_else(|| RenderError::InvalidParameters {
        message: format!("Cannot resolve @import URL '{}'", url),
      })?;

    match resolved.scheme() {
      "file" => {
        let path = resolved
          .to_file_path()
          .map_err(|()| RenderError::InvalidParameters {
            message: format!("Invalid file URL for @import: {}", resolved),
          })?;
        let bytes = std::fs::read(&path)?;
        Ok(css::encoding::decode_css_bytes(&bytes, None))
      }
      "http" | "https" => {
        let agent = ureq::Agent::config_builder()
          .timeout_global(Some(std::time::Duration::from_secs(30)))
          .build();
        let agent: ureq::Agent = agent.into();
        let response = agent
          .get(resolved.as_str())
          .call()
          .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;
        let content_type = response
          .headers()
          .get("content-type")
          .and_then(|h| h.to_str().ok())
          .map(|s| s.to_string());
        let mut body = Vec::new();
        let mut reader = response.into_body().into_reader();
        reader.read_to_end(&mut body)?;
        Ok(css::encoding::decode_css_bytes(
          &body,
          content_type.as_deref(),
        ))
      }
      _ => Err(
        RenderError::InvalidParameters {
          message: format!("Unsupported URL scheme for @import: {}", resolved),
        }
        .into(),
      ),
    }
  }
}

fn clip_rect_axes(mut bounds: Rect, clip: Rect, clip_x: bool, clip_y: bool) -> Rect {
  if clip_x {
    let min_x = bounds.min_x().max(clip.min_x());
    let max_x = bounds.max_x().min(clip.max_x());
    bounds.origin.x = min_x;
    bounds.size.width = (max_x - min_x).max(0.0);
  }
  if clip_y {
    let min_y = bounds.min_y().max(clip.min_y());
    let max_y = bounds.max_y().min(clip.max_y());
    bounds.origin.y = min_y;
    bounds.size.height = (max_y - min_y).max(0.0);
  }
  bounds
}

fn compute_descendant_bounds(commands: &[DisplayCommand], root_rect: Rect) -> Option<Rect> {
  let mut current: Option<Rect> = None;
  for cmd in commands {
    if matches!(cmd, DisplayCommand::Background { rect, .. } | DisplayCommand::Border { rect, .. } if approx_same_rect(*rect, root_rect))
    {
      continue;
    }
    if let Some(r) = command_bounds(cmd) {
      current = Some(match current {
        Some(acc) => acc.union(r),
        None => r,
      });
    }
  }
  current
}

fn compute_outline_bounds(commands: &[DisplayCommand]) -> Option<Rect> {
  let mut current: Option<Rect> = None;

  for cmd in commands {
    match cmd {
      DisplayCommand::Outline { rect, .. } => {
        current = Some(current.map(|acc| acc.union(*rect)).unwrap_or(*rect));
      }
      DisplayCommand::StackingContext { commands, .. } => {
        if let Some(r) = compute_outline_bounds(commands) {
          current = Some(current.map(|acc| acc.union(r)).unwrap_or(r));
        }
      }
      _ => {}
    }
  }

  current
}

fn stacking_context_bounds(
  commands: &[DisplayCommand],
  filters: &[ResolvedFilter],
  backdrop_filters: &[ResolvedFilter],
  rect: Rect,
  transform: Option<&Transform3D>,
  clip: Option<&StackingClip>,
  clip_path: Option<&ResolvedClipPath>,
) -> Option<Rect> {
  let project_rect_bounds = |rect: Rect, transform: &Transform3D| -> Option<Rect> {
    let corners = rect_corners(rect);
    let mut projected = [Point::ZERO; 4];
    for (idx, corner) in corners.iter().enumerate() {
      let (tx, ty, _tz, tw) = transform.transform_point(corner.x, corner.y, 0.0);
      if tw.abs() < 1e-6 || !tx.is_finite() || !ty.is_finite() || !tw.is_finite() {
        return None;
      }
      projected[idx] = Point::new(tx / tw, ty / tw);
    }
    Some(quad_bounds(&projected))
  };

  let mut base = rect;
  let outline_bounds = compute_outline_bounds(commands);
  if let Some(desc) = compute_descendant_bounds(commands, rect) {
    let clipped = if let Some(clip) = clip {
      clip_non_outline(commands, desc, clip.rect, clip.clip_x, clip.clip_y)
    } else {
      desc
    };
    base = base.union(clipped);
  }
  if let Some(path) = clip_path {
    let clip_bounds = path.bounds();
    base = base.intersection(clip_bounds).unwrap_or(clip_bounds);
  }
  if let Some(outline_bounds) = outline_bounds {
    base = base.union(outline_bounds);
  }
  let (l, t, r, b) = compute_filter_outset(filters, base, 1.0);
  let (bl, bt, br, bb) = compute_filter_outset(backdrop_filters, base, 1.0);
  let total_l = l.max(bl);
  let total_t = t.max(bt);
  let total_r = r.max(br);
  let total_b = b.max(bb);
  if total_l > 0.0 || total_t > 0.0 || total_r > 0.0 || total_b > 0.0 {
    base = Rect::from_xywh(
      base.min_x() - total_l,
      base.min_y() - total_t,
      base.width() + total_l + total_r,
      base.height() + total_t + total_b,
    );
  }
  if let Some(ts) = transform {
    if let Some(transformed) = project_rect_bounds(base, ts) {
      base = base.union(transformed);
    }
  }
  Some(base)
}

fn command_bounds(cmd: &DisplayCommand) -> Option<Rect> {
  match cmd {
    DisplayCommand::Background { rect, .. }
    | DisplayCommand::Border { rect, .. }
    | DisplayCommand::Outline { rect, .. }
    | DisplayCommand::Text { rect, .. }
    | DisplayCommand::Replaced { rect, .. } => Some(*rect),
    DisplayCommand::StackingContext {
      commands,
      filters,
      backdrop_filters,
      clip,
      clip_path,
      rect,
      transform_3d,
      ..
    } => stacking_context_bounds(
      commands,
      filters,
      backdrop_filters,
      *rect,
      transform_3d.as_ref(),
      clip.as_ref(),
      clip_path.as_ref(),
    ),
  }
}

#[allow(dead_code)]
fn compute_commands_bounds(commands: &[DisplayCommand]) -> Option<Rect> {
  let mut current: Option<Rect> = None;
  for cmd in commands {
    if let Some(r) = command_bounds(cmd) {
      current = Some(match current {
        Some(acc) => acc.union(r),
        None => r,
      });
    }
  }
  current
}

fn clip_non_outline(
  commands: &[DisplayCommand],
  mut bounds: Rect,
  clip_rect: Rect,
  clip_x: bool,
  clip_y: bool,
) -> Rect {
  let mut current: Option<Rect> = None;
  for cmd in commands {
    if matches!(cmd, DisplayCommand::Outline { .. }) {
      continue;
    }
    if let Some(r) = command_bounds(cmd) {
      let clipped = clip_rect_axes(r, clip_rect, clip_x, clip_y);
      current = Some(current.map(|c| c.union(clipped)).unwrap_or(clipped));
    }
  }
  if let Some(c) = current {
    bounds = bounds.union(c);
  }
  bounds
}

fn translate_commands(commands: Vec<DisplayCommand>, dx: f32, dy: f32) -> Vec<DisplayCommand> {
  let offset = Point::new(dx, dy);
  commands
    .into_iter()
    .map(|cmd| match cmd {
      DisplayCommand::Background { rect, style } => DisplayCommand::Background {
        rect: rect.translate(offset),
        style,
      },
      DisplayCommand::Border { rect, style } => DisplayCommand::Border {
        rect: rect.translate(offset),
        style,
      },
      DisplayCommand::Outline { rect, style } => DisplayCommand::Outline {
        rect: rect.translate(offset),
        style,
      },
      DisplayCommand::Text {
        rect,
        baseline_offset,
        text,
        runs,
        style,
      } => DisplayCommand::Text {
        rect: rect.translate(offset),
        baseline_offset,
        text,
        runs,
        style,
      },
      DisplayCommand::Replaced {
        rect,
        replaced_type,
        style,
      } => DisplayCommand::Replaced {
        rect: rect.translate(offset),
        replaced_type,
        style,
      },
      DisplayCommand::StackingContext {
        rect,
        opacity,
        transform,
        transform_3d,
        blend_mode,
        isolated,
        mask,
        filters,
        backdrop_filters,
        radii,
        clip,
        clip_path,
        commands,
      } => DisplayCommand::StackingContext {
        rect: rect.translate(offset),
        opacity,
        transform,
        transform_3d,
        blend_mode,
        isolated,
        mask,
        filters,
        backdrop_filters,
        radii,
        clip: clip.map(|clip| StackingClip {
          rect: clip.rect.translate(offset),
          ..clip
        }),
        clip_path: clip_path.map(|cp| cp.translate(dx, dy)),
        commands: translate_commands(commands, dx, dy),
      },
    })
    .collect()
}

fn describe_content(content: &FragmentContent) -> &'static str {
  match content {
    FragmentContent::Block { .. } => "block",
    FragmentContent::Inline { .. } => "inline",
    FragmentContent::Text { is_marker, .. } => {
      if *is_marker {
        "marker-text"
      } else {
        "text"
      }
    }
    FragmentContent::Line { .. } => "line",
    FragmentContent::Replaced { .. } => "replaced",
  }
}

fn fragment_counts(node: &FragmentNode) -> (usize, usize, usize, usize, usize) {
  let mut total = 1;
  let mut text = 0;
  let mut replaced = 0;
  let mut lines = 0;
  let mut inline = 0;
  match node.content {
    FragmentContent::Text { .. } => text += 1,
    FragmentContent::Replaced { .. } => replaced += 1,
    FragmentContent::Line { .. } => lines += 1,
    FragmentContent::Inline { .. } => inline += 1,
    FragmentContent::Block { .. } => {}
  }
  for child in &node.children {
    let (t, tx, r, l, i) = fragment_counts(child);
    total += t;
    text += tx;
    replaced += r;
    lines += l;
    inline += i;
  }
  (total, text, replaced, lines, inline)
}

fn fragment_tree_counts(tree: &FragmentTree) -> (usize, usize, usize, usize, usize) {
  std::iter::once(&tree.root)
    .chain(tree.additional_fragments.iter())
    .map(fragment_counts)
    .fold(
      (0, 0, 0, 0, 0),
      |(t0, tx0, r0, l0, i0), (t, tx, r, l, i)| (t0 + t, tx0 + tx, r0 + r, l0 + l, i0 + i),
    )
}

fn resolve_filters(
  filters: &[FilterFunction],
  style: &ComputedStyle,
  viewport: (f32, f32),
  font_ctx: &FontContext,
  svg_filters: &mut SvgFilterResolver,
) -> Vec<ResolvedFilter> {
  filters
    .iter()
    .filter_map(|f| match f {
      FilterFunction::Blur(len) => {
        let radius = resolve_filter_length(len, style, viewport, font_ctx)?;
        (radius >= 0.0).then_some(ResolvedFilter::Blur(radius))
      }
      FilterFunction::Brightness(v) => Some(ResolvedFilter::Brightness((*v).max(0.0))),
      FilterFunction::Contrast(v) => Some(ResolvedFilter::Contrast((*v).max(0.0))),
      FilterFunction::Grayscale(v) => Some(ResolvedFilter::Grayscale(v.clamp(0.0, 1.0))),
      FilterFunction::Sepia(v) => Some(ResolvedFilter::Sepia(v.clamp(0.0, 1.0))),
      FilterFunction::Saturate(v) => Some(ResolvedFilter::Saturate((*v).max(0.0))),
      FilterFunction::HueRotate(deg) => Some(ResolvedFilter::HueRotate(*deg)),
      FilterFunction::Invert(v) => Some(ResolvedFilter::Invert(v.clamp(0.0, 1.0))),
      FilterFunction::Opacity(v) => Some(ResolvedFilter::Opacity(v.clamp(0.0, 1.0))),
      FilterFunction::DropShadow(shadow) => {
        let color = match shadow.color {
          FilterColor::CurrentColor => style.color,
          FilterColor::Color(c) => c,
        };
        let offset_x = resolve_filter_length(&shadow.offset_x, style, viewport, font_ctx)?;
        let offset_y = resolve_filter_length(&shadow.offset_y, style, viewport, font_ctx)?;
        let blur_radius = resolve_filter_length(&shadow.blur_radius, style, viewport, font_ctx)?;
        if blur_radius < 0.0 {
          return None;
        }
        let spread = resolve_filter_length(&shadow.spread, style, viewport, font_ctx)?;
        Some(ResolvedFilter::DropShadow {
          offset_x,
          offset_y,
          blur_radius,
          spread,
          color,
        })
      }
      FilterFunction::Url(url) => svg_filters.resolve(url).map(ResolvedFilter::SvgFilter),
    })
    .collect()
}

fn resolve_filter_length(
  len: &Length,
  style: &ComputedStyle,
  viewport: (f32, f32),
  font_ctx: &FontContext,
) -> Option<f32> {
  match len.unit {
    LengthUnit::Percent => None,
    unit if unit.is_font_relative() => Some(resolve_font_relative_length(*len, style, font_ctx)),
    unit if unit.is_viewport_relative() => len.resolve_with_viewport(viewport.0, viewport.1),
    unit if unit.is_absolute() => Some(len.to_px()),
    _ => None,
  }
}

fn apply_filters(pixmap: &mut Pixmap, filters: &[ResolvedFilter], scale: f32, bbox: Rect) {
  for filter in filters {
    match filter {
      ResolvedFilter::Blur(radius) => apply_gaussian_blur(pixmap, *radius * scale),
      ResolvedFilter::Brightness(amount) => {
        apply_color_filter(pixmap, |c, a| (scale_color(c, *amount), a))
      }
      ResolvedFilter::Contrast(amount) => {
        apply_color_filter(pixmap, |c, a| (apply_contrast(c, *amount), a))
      }
      ResolvedFilter::Grayscale(amount) => {
        apply_color_filter(pixmap, |c, a| (grayscale(c, *amount), a))
      }
      ResolvedFilter::Sepia(amount) => apply_color_filter(pixmap, |c, a| (sepia(c, *amount), a)),
      ResolvedFilter::Saturate(amount) => {
        apply_color_filter(pixmap, |c, a| (saturate(c, *amount), a))
      }
      ResolvedFilter::HueRotate(deg) => apply_color_filter(pixmap, |c, a| (hue_rotate(c, *deg), a)),
      ResolvedFilter::Invert(amount) => apply_color_filter(pixmap, |c, a| (invert(c, *amount), a)),
      ResolvedFilter::Opacity(amount) => apply_color_filter(pixmap, |c, a| (c, a * *amount)),
      ResolvedFilter::DropShadow {
        offset_x,
        offset_y,
        blur_radius,
        spread,
        color,
      } => apply_drop_shadow(
        pixmap,
        *offset_x * scale,
        *offset_y * scale,
        *blur_radius * scale,
        *spread * scale,
        *color,
      ),
      ResolvedFilter::SvgFilter(filter) => {
        crate::paint::svg_filter::apply_svg_filter(filter.as_ref(), pixmap, scale, bbox);
      }
    }
  }
}

fn apply_backdrop_filters(
  pixmap: &mut Pixmap,
  bounds: &Rect,
  filters: &[ResolvedFilter],
  radii: BorderRadii,
  scale: f32,
  filter_bounds: Rect,
) {
  if filters.is_empty() {
    return;
  }
  let (out_l, out_t, out_r, out_b) = compute_filter_outset(filters, filter_bounds, scale);

  let x = (bounds.min_x() - out_l).floor() as i32;
  let y = (bounds.min_y() - out_t).floor() as i32;
  let width = (bounds.width() + out_l + out_r).ceil() as u32;
  let height = (bounds.height() + out_t + out_b).ceil() as u32;
  if width == 0 || height == 0 {
    return;
  }

  let pix_w = pixmap.width() as i32;
  let pix_h = pixmap.height() as i32;
  if x >= pix_w || y >= pix_h {
    return;
  }

  let clamped_x = x.max(0) as u32;
  let clamped_y = y.max(0) as u32;
  let max_w = pix_w.saturating_sub(clamped_x as i32).max(0) as u32;
  let max_h = pix_h.saturating_sub(clamped_y as i32).max(0) as u32;
  let region_w = width.min(max_w);
  let region_h = height.min(max_h);
  if region_w == 0 || region_h == 0 {
    return;
  }

  let mut region = match Pixmap::new(region_w, region_h) {
    Some(p) => p,
    None => return,
  };

  // Copy region
  let bytes_per_row = pixmap.width() as usize * 4;
  let region_row_bytes = region_w as usize * 4;
  let start = (clamped_y as usize * bytes_per_row) + clamped_x as usize * 4;
  let data = pixmap.data();
  let dest = region.data_mut();
  for row in 0..region_h as usize {
    let src_offset = start + row * bytes_per_row;
    let dst_offset = row * region_row_bytes;
    let src_slice = &data[src_offset..src_offset + region_row_bytes];
    let dst_slice = &mut dest[dst_offset..dst_offset + region_row_bytes];
    dst_slice.copy_from_slice(src_slice);
  }

  let local_bbox = Rect::from_xywh(
    bounds.x() - clamped_x as f32,
    bounds.y() - clamped_y as f32,
    bounds.width(),
    bounds.height(),
  );
  apply_filters(&mut region, filters, scale, local_bbox);
  if !radii.is_zero() {
    apply_clip_mask_rect(&mut region, local_bbox, radii);
  }

  let mut paint = PixmapPaint::default();
  paint.blend_mode = SkiaBlendMode::SourceOver;
  pixmap.draw_pixmap(
    clamped_x as i32,
    clamped_y as i32,
    region.as_ref(),
    &paint,
    Transform::identity(),
    None,
  );
}

fn apply_color_filter<F>(pixmap: &mut Pixmap, mut f: F)
where
  F: FnMut([f32; 3], f32) -> ([f32; 3], f32),
{
  for px in pixmap.pixels_mut() {
    let alpha = px.alpha() as f32 / 255.0;
    let base = if alpha > 0.0 {
      [
        (px.red() as f32 / 255.0) / alpha,
        (px.green() as f32 / 255.0) / alpha,
        (px.blue() as f32 / 255.0) / alpha,
      ]
    } else {
      [0.0, 0.0, 0.0]
    };
    let (mut color, mut new_alpha) = f(base, alpha);
    new_alpha = new_alpha.clamp(0.0, 1.0);
    color[0] = color[0].clamp(0.0, 1.0);
    color[1] = color[1].clamp(0.0, 1.0);
    color[2] = color[2].clamp(0.0, 1.0);

    let r = (color[0] * new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
    let g = (color[1] * new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
    let b = (color[2] * new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
    let a = (new_alpha * 255.0).round().clamp(0.0, 255.0) as u8;

    *px = PremultipliedColorU8::from_rgba(r, g, b, a).unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }
}

fn scale_color(color: [f32; 3], factor: f32) -> [f32; 3] {
  [color[0] * factor, color[1] * factor, color[2] * factor]
}

fn apply_contrast(color: [f32; 3], factor: f32) -> [f32; 3] {
  [
    ((color[0] - 0.5) * factor + 0.5),
    ((color[1] - 0.5) * factor + 0.5),
    ((color[2] - 0.5) * factor + 0.5),
  ]
}

fn grayscale(color: [f32; 3], amount: f32) -> [f32; 3] {
  let gray = color[0] * 0.2126 + color[1] * 0.7152 + color[2] * 0.0722;
  [
    color[0] + (gray - color[0]) * amount,
    color[1] + (gray - color[1]) * amount,
    color[2] + (gray - color[2]) * amount,
  ]
}

fn sepia(color: [f32; 3], amount: f32) -> [f32; 3] {
  let sepia_r = color[0] * 0.393 + color[1] * 0.769 + color[2] * 0.189;
  let sepia_g = color[0] * 0.349 + color[1] * 0.686 + color[2] * 0.168;
  let sepia_b = color[0] * 0.272 + color[1] * 0.534 + color[2] * 0.131;
  [
    color[0] + (sepia_r - color[0]) * amount,
    color[1] + (sepia_g - color[1]) * amount,
    color[2] + (sepia_b - color[2]) * amount,
  ]
}

fn saturate(color: [f32; 3], factor: f32) -> [f32; 3] {
  let rw = 0.213;
  let gw = 0.715;
  let bw = 0.072;
  [
    (rw + (1.0 - rw) * factor) * color[0]
      + (gw - gw * factor) * color[1]
      + (bw - bw * factor) * color[2],
    (rw - rw * factor) * color[0]
      + (gw + (1.0 - gw) * factor) * color[1]
      + (bw - bw * factor) * color[2],
    (rw - rw * factor) * color[0]
      + (gw - gw * factor) * color[1]
      + (bw + (1.0 - bw) * factor) * color[2],
  ]
}

fn hue_rotate(color: [f32; 3], degrees: f32) -> [f32; 3] {
  let angle = degrees.to_radians();
  let cos = angle.cos();
  let sin = angle.sin();

  let r = color[0];
  let g = color[1];
  let b = color[2];

  [
    r * (0.213 + cos * 0.787 - sin * 0.213)
      + g * (0.715 - 0.715 * cos - 0.715 * sin)
      + b * (0.072 - 0.072 * cos + 0.928 * sin),
    r * (0.213 - 0.213 * cos + 0.143 * sin)
      + g * (0.715 + 0.285 * cos + 0.140 * sin)
      + b * (0.072 - 0.072 * cos - 0.283 * sin),
    r * (0.213 - 0.213 * cos - 0.787 * sin)
      + g * (0.715 - 0.715 * cos + 0.715 * sin)
      + b * (0.072 + 0.928 * cos + 0.072 * sin),
  ]
}

fn invert(color: [f32; 3], amount: f32) -> [f32; 3] {
  [
    color[0] + (1.0 - color[0] - color[0]) * amount,
    color[1] + (1.0 - color[1] - color[1]) * amount,
    color[2] + (1.0 - color[2] - color[2]) * amount,
  ]
}

fn apply_drop_shadow(
  pixmap: &mut Pixmap,
  offset_x: f32,
  offset_y: f32,
  blur_radius: f32,
  spread: f32,
  color: Rgba,
) {
  if pixmap.width() == 0 || pixmap.height() == 0 {
    return;
  }

  let source = pixmap.clone();
  let mut shadow = match Pixmap::new(source.width(), source.height()) {
    Some(p) => p,
    None => return,
  };

  {
    let src = source.pixels();
    let dst = shadow.pixels_mut();
    for (src_px, dst_px) in src.iter().zip(dst.iter_mut()) {
      let alpha = src_px.alpha() as f32 / 255.0;
      if alpha == 0.0 {
        *dst_px = PremultipliedColorU8::TRANSPARENT;
        continue;
      }
      let total_alpha = (color.a * alpha).clamp(0.0, 1.0);
      let r = (color.r as f32 / 255.0) * total_alpha;
      let g = (color.g as f32 / 255.0) * total_alpha;
      let b = (color.b as f32 / 255.0) * total_alpha;
      let a = total_alpha * 255.0;
      *dst_px = PremultipliedColorU8::from_rgba(
        (r * 255.0).round() as u8,
        (g * 255.0).round() as u8,
        (b * 255.0).round() as u8,
        a.round().clamp(0.0, 255.0) as u8,
      )
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
    }
  }

  if spread != 0.0 {
    apply_spread(&mut shadow, spread);
  }

  if blur_radius > 0.0 {
    apply_gaussian_blur(&mut shadow, blur_radius);
  }

  let mut result = match Pixmap::new(source.width(), source.height()) {
    Some(p) => p,
    None => return,
  };

  let mut paint = PixmapPaint::default();
  paint.blend_mode = SkiaBlendMode::SourceOver;
  result.draw_pixmap(
    0,
    0,
    shadow.as_ref(),
    &paint,
    Transform::from_translate(offset_x, offset_y),
    None,
  );
  result.draw_pixmap(0, 0, source.as_ref(), &paint, Transform::identity(), None);

  *pixmap = result;
}

fn apply_spread(pixmap: &mut Pixmap, spread: f32) {
  let radius = spread.abs().ceil() as i32;
  if radius <= 0 || spread == 0.0 {
    return;
  }
  let expand = spread > 0.0;
  let width = pixmap.width() as i32;
  let height = pixmap.height() as i32;
  let original = pixmap.clone();
  let src = original.pixels();
  let dst = pixmap.pixels_mut();

  let mut base_ratio = (0.0, 0.0, 0.0);
  for px in src.iter() {
    let alpha = px.alpha();
    if alpha > 0 {
      let a = alpha as f32;
      base_ratio = (
        px.red() as f32 / a,
        px.green() as f32 / a,
        px.blue() as f32 / a,
      );
      break;
    }
  }

  for y in 0..height {
    for x in 0..width {
      let mut agg_alpha = if expand { 0u8 } else { 255u8 };
      for dy in -radius..=radius {
        for dx in -radius..=radius {
          let ny = (y + dy).clamp(0, height - 1);
          let nx = (x + dx).clamp(0, width - 1);
          let idx = (ny as usize) * (width as usize) + nx as usize;
          let px = src[idx];
          if expand {
            agg_alpha = agg_alpha.max(px.alpha());
          } else {
            agg_alpha = agg_alpha.min(px.alpha());
          }
        }
      }
      let idx = (y as usize) * (width as usize) + x as usize;
      if agg_alpha == 0 {
        dst[idx] = PremultipliedColorU8::TRANSPARENT;
        continue;
      }

      let orig = src[idx];
      let orig_alpha = orig.alpha();
      if orig_alpha > 0 {
        let factor = (agg_alpha as f32) / (orig_alpha as f32);
        let r = (orig.red() as f32 * factor).round().clamp(0.0, 255.0) as u8;
        let g = (orig.green() as f32 * factor).round().clamp(0.0, 255.0) as u8;
        let b = (orig.blue() as f32 * factor).round().clamp(0.0, 255.0) as u8;
        dst[idx] = PremultipliedColorU8::from_rgba(r, g, b, agg_alpha)
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      } else {
        let r = (base_ratio.0 * agg_alpha as f32).round().clamp(0.0, 255.0) as u8;
        let g = (base_ratio.1 * agg_alpha as f32).round().clamp(0.0, 255.0) as u8;
        let b = (base_ratio.2 * agg_alpha as f32).round().clamp(0.0, 255.0) as u8;
        dst[idx] = PremultipliedColorU8::from_rgba(r, g, b, agg_alpha)
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      }
    }
  }
}

fn is_hsl_blend(mode: MixBlendMode) -> bool {
  matches!(
    mode,
    MixBlendMode::Hue | MixBlendMode::Saturation | MixBlendMode::Color | MixBlendMode::Luminosity
  )
}

fn rgb_to_hsl(r: f32, g: f32, b: f32) -> (f32, f32, f32) {
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
    p + (q - p) * 6.0 * t
  } else if t < 0.5 {
    q
  } else if t < 2.0 / 3.0 {
    p + (q - p) * (2.0 / 3.0 - t) * 6.0
  } else {
    p
  }
}

fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (f32, f32, f32) {
  if s <= 0.0 {
    return (l, l, l);
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
  (r, g, b)
}

fn apply_hsl_blend(
  mode: MixBlendMode,
  src: (f32, f32, f32),
  dst: (f32, f32, f32),
) -> (f32, f32, f32) {
  let (sh, ss, sl) = rgb_to_hsl(src.0, src.1, src.2);
  let (dh, ds, dl) = rgb_to_hsl(dst.0, dst.1, dst.2);
  match mode {
    MixBlendMode::Hue => hsl_to_rgb(sh, ds, dl),
    MixBlendMode::Saturation => hsl_to_rgb(dh, ss, dl),
    MixBlendMode::Color => hsl_to_rgb(sh, ss, dl),
    MixBlendMode::Luminosity => hsl_to_rgb(dh, ds, sl),
    _ => dst,
  }
}

fn composite_hsl_layer(
  dest: &mut Pixmap,
  layer: &Pixmap,
  opacity: f32,
  mode: MixBlendMode,
  area: Option<Rect>,
) {
  if !is_hsl_blend(mode) {
    return;
  }
  let dest_width = dest.width() as usize;
  let dest_height = dest.height() as usize;
  let layer_width = layer.width() as usize;
  let layer_height = layer.height() as usize;
  if dest_width == 0 || dest_height == 0 || layer_width == 0 || layer_height == 0 {
    return;
  }

  let max_width = dest_width.min(layer_width);
  let max_height = dest_height.min(layer_height);
  let (mut x0, mut y0, mut x1, mut y1) = if let Some(rect) = area {
    let x0 = rect.min_x().floor() as i32;
    let y0 = rect.min_y().floor() as i32;
    let x1 = rect.max_x().ceil() as i32;
    let y1 = rect.max_y().ceil() as i32;
    (x0, y0, x1, y1)
  } else {
    (0, 0, max_width as i32, max_height as i32)
  };
  x0 = x0.clamp(0, max_width as i32);
  y0 = y0.clamp(0, max_height as i32);
  x1 = x1.clamp(0, max_width as i32);
  y1 = y1.clamp(0, max_height as i32);
  if x0 >= x1 || y0 >= y1 {
    return;
  }

  let src_pixels = layer.pixels();
  let dst_pixels = dest.pixels_mut();
  let src_stride = layer_width;
  let dst_stride = dest_width;
  let opacity = opacity.clamp(0.0, 1.0);

  for y in y0..y1 {
    let yi = y as usize;
    for x in x0..x1 {
      let xi = x as usize;
      let src_px = src_pixels[yi * src_stride + xi];
      let raw_sa = src_px.alpha() as f32 / 255.0;
      if raw_sa == 0.0 || opacity == 0.0 {
        continue;
      }
      let dst_px = &mut dst_pixels[yi * dst_stride + xi];
      let sa = (raw_sa * opacity).clamp(0.0, 1.0);
      let da = dst_px.alpha() as f32 / 255.0;

      let src_rgb = if raw_sa > 0.0 {
        (
          (src_px.red() as f32 / 255.0) / raw_sa,
          (src_px.green() as f32 / 255.0) / raw_sa,
          (src_px.blue() as f32 / 255.0) / raw_sa,
        )
      } else {
        (0.0, 0.0, 0.0)
      };
      let dst_rgb = if da > 0.0 {
        (
          (dst_px.red() as f32 / 255.0) / da,
          (dst_px.green() as f32 / 255.0) / da,
          (dst_px.blue() as f32 / 255.0) / da,
        )
      } else {
        (0.0, 0.0, 0.0)
      };

      let blended_rgb = apply_hsl_blend(mode, src_rgb, dst_rgb);

      let out_a = sa + da * (1.0 - sa);
      let out_rgb = if out_a > 0.0 {
        (
          (blended_rgb.0 * sa + dst_rgb.0 * da * (1.0 - sa)) / out_a,
          (blended_rgb.1 * sa + dst_rgb.1 * da * (1.0 - sa)) / out_a,
          (blended_rgb.2 * sa + dst_rgb.2 * da * (1.0 - sa)) / out_a,
        )
      } else {
        (0.0, 0.0, 0.0)
      };

      let out_a_u8 = (out_a * 255.0 + 0.5).clamp(0.0, 255.0) as u8;
      let scale = out_a;
      let r = ((out_rgb.0 * scale) * 255.0 + 0.5).clamp(0.0, out_a_u8 as f32) as u8;
      let g = ((out_rgb.1 * scale) * 255.0 + 0.5).clamp(0.0, out_a_u8 as f32) as u8;
      let b = ((out_rgb.2 * scale) * 255.0 + 0.5).clamp(0.0, out_a_u8 as f32) as u8;
      *dst_px = PremultipliedColorU8::from_rgba(r, g, b, out_a_u8)
        .unwrap_or(PremultipliedColorU8::TRANSPARENT);
    }
  }
}

fn map_blend_mode(mode: MixBlendMode) -> SkiaBlendMode {
  match mode {
    MixBlendMode::Normal => SkiaBlendMode::SourceOver,
    MixBlendMode::Multiply => SkiaBlendMode::Multiply,
    MixBlendMode::Screen => SkiaBlendMode::Screen,
    MixBlendMode::Overlay => SkiaBlendMode::Overlay,
    MixBlendMode::Darken => SkiaBlendMode::Darken,
    MixBlendMode::Lighten => SkiaBlendMode::Lighten,
    MixBlendMode::ColorDodge => SkiaBlendMode::ColorDodge,
    MixBlendMode::ColorBurn => SkiaBlendMode::ColorBurn,
    MixBlendMode::HardLight => SkiaBlendMode::HardLight,
    MixBlendMode::SoftLight => SkiaBlendMode::SoftLight,
    MixBlendMode::Difference => SkiaBlendMode::Difference,
    MixBlendMode::Exclusion => SkiaBlendMode::Exclusion,
    MixBlendMode::Hue => SkiaBlendMode::Hue,
    MixBlendMode::Saturation => SkiaBlendMode::Saturation,
    MixBlendMode::Color => SkiaBlendMode::Color,
    MixBlendMode::Luminosity => SkiaBlendMode::Luminosity,
    MixBlendMode::PlusLighter => SkiaBlendMode::Plus,
    MixBlendMode::PlusDarker
    | MixBlendMode::HueHsv
    | MixBlendMode::SaturationHsv
    | MixBlendMode::ColorHsv
    | MixBlendMode::LuminosityHsv
    | MixBlendMode::HueOklch
    | MixBlendMode::ChromaOklch
    | MixBlendMode::ColorOklch
    | MixBlendMode::LuminosityOklch => SkiaBlendMode::SourceOver,
  }
}

fn resolve_border_radii(style: Option<&ComputedStyle>, bounds: Rect) -> BorderRadii {
  let Some(style) = style else {
    return BorderRadii::ZERO;
  };
  let w = bounds.width().max(0.0);
  let h = bounds.height().max(0.0);
  if w <= 0.0 || h <= 0.0 {
    return BorderRadii::ZERO;
  }

  let resolve_radius = |len: &Length, reference: f32| -> f32 {
    resolve_length_for_paint(
      len,
      style.font_size,
      style.root_font_size,
      reference,
      (w, h),
    )
  };

  let radii = BorderRadii {
    top_left: crate::paint::display_list::BorderRadius {
      x: resolve_radius(&style.border_top_left_radius.x, w).max(0.0),
      y: resolve_radius(&style.border_top_left_radius.y, h).max(0.0),
    },
    top_right: crate::paint::display_list::BorderRadius {
      x: resolve_radius(&style.border_top_right_radius.x, w).max(0.0),
      y: resolve_radius(&style.border_top_right_radius.y, h).max(0.0),
    },
    bottom_right: crate::paint::display_list::BorderRadius {
      x: resolve_radius(&style.border_bottom_right_radius.x, w).max(0.0),
      y: resolve_radius(&style.border_bottom_right_radius.y, h).max(0.0),
    },
    bottom_left: crate::paint::display_list::BorderRadius {
      x: resolve_radius(&style.border_bottom_left_radius.x, w).max(0.0),
      y: resolve_radius(&style.border_bottom_left_radius.y, h).max(0.0),
    },
  };
  radii.clamped(w, h)
}

fn resolve_clip_radii(
  style: &ComputedStyle,
  rects: &BackgroundRects,
  clip: crate::style::types::BackgroundBox,
  viewport: Option<(f32, f32)>,
) -> BorderRadii {
  let base = resolve_border_radii(Some(style), rects.border);
  if base.is_zero() {
    return base;
  }

  let percentage_base = rects.border.width().max(0.0);
  let font_size = style.font_size;
  let vp = viewport.unwrap_or((percentage_base, percentage_base));
  let border_left = resolve_length_for_paint(
    &style.border_left_width,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );
  let border_right = resolve_length_for_paint(
    &style.border_right_width,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );
  let border_top = resolve_length_for_paint(
    &style.border_top_width,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );
  let border_bottom = resolve_length_for_paint(
    &style.border_bottom_width,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );

  let padding_left = resolve_length_for_paint(
    &style.padding_left,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );
  let padding_right = resolve_length_for_paint(
    &style.padding_right,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );
  let padding_top = resolve_length_for_paint(
    &style.padding_top,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );
  let padding_bottom = resolve_length_for_paint(
    &style.padding_bottom,
    font_size,
    style.root_font_size,
    percentage_base,
    vp,
  );

  match clip {
    crate::style::types::BackgroundBox::BorderBox => base,
    crate::style::types::BackgroundBox::PaddingBox => {
      let shrunk = BorderRadii {
        top_left: crate::paint::display_list::BorderRadius {
          x: (base.top_left.x - border_left).max(0.0),
          y: (base.top_left.y - border_top).max(0.0),
        },
        top_right: crate::paint::display_list::BorderRadius {
          x: (base.top_right.x - border_right).max(0.0),
          y: (base.top_right.y - border_top).max(0.0),
        },
        bottom_right: crate::paint::display_list::BorderRadius {
          x: (base.bottom_right.x - border_right).max(0.0),
          y: (base.bottom_right.y - border_bottom).max(0.0),
        },
        bottom_left: crate::paint::display_list::BorderRadius {
          x: (base.bottom_left.x - border_left).max(0.0),
          y: (base.bottom_left.y - border_bottom).max(0.0),
        },
      };
      shrunk.clamped(rects.padding.width(), rects.padding.height())
    }
    crate::style::types::BackgroundBox::ContentBox => {
      let shrink_left = border_left + padding_left;
      let shrink_right = border_right + padding_right;
      let shrink_top = border_top + padding_top;
      let shrink_bottom = border_bottom + padding_bottom;
      let shrunk = BorderRadii {
        top_left: crate::paint::display_list::BorderRadius {
          x: (base.top_left.x - shrink_left).max(0.0),
          y: (base.top_left.y - shrink_top).max(0.0),
        },
        top_right: crate::paint::display_list::BorderRadius {
          x: (base.top_right.x - shrink_right).max(0.0),
          y: (base.top_right.y - shrink_top).max(0.0),
        },
        bottom_right: crate::paint::display_list::BorderRadius {
          x: (base.bottom_right.x - shrink_right).max(0.0),
          y: (base.bottom_right.y - shrink_bottom).max(0.0),
        },
        bottom_left: crate::paint::display_list::BorderRadius {
          x: (base.bottom_left.x - shrink_left).max(0.0),
          y: (base.bottom_left.y - shrink_bottom).max(0.0),
        },
      };
      shrunk.clamped(rects.content.width(), rects.content.height())
    }
  }
}

fn build_rounded_rect_mask(
  rect: Rect,
  radii: BorderRadii,
  canvas_w: u32,
  canvas_h: u32,
) -> Option<Mask> {
  if canvas_w == 0 || canvas_h == 0 || rect.width() <= 0.0 || rect.height() <= 0.0 {
    return None;
  }

  let mut mask_pixmap = Pixmap::new(canvas_w, canvas_h)?;
  let _ = fill_rounded_rect(
    &mut mask_pixmap,
    rect.x(),
    rect.y(),
    rect.width(),
    rect.height(),
    &radii,
    Rgba::new(255, 255, 255, 1.0),
  );
  Some(Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha))
}

fn apply_clip_mask_rect(pixmap: &mut Pixmap, rect: Rect, radii: BorderRadii) {
  if rect.width() <= 0.0 || rect.height() <= 0.0 {
    return;
  }
  let width = pixmap.width();
  let height = pixmap.height();
  if width == 0 || height == 0 {
    return;
  }

  let mut mask_pixmap = match Pixmap::new(width, height) {
    Some(p) => p,
    None => return,
  };
  let clamped = radii.clamped(rect.width(), rect.height());
  let _ = fill_rounded_rect(
    &mut mask_pixmap,
    rect.x(),
    rect.y(),
    rect.width(),
    rect.height(),
    &clamped,
    Rgba::new(255, 255, 255, 1.0),
  );

  let mask = Mask::from_pixmap(mask_pixmap.as_ref(), MaskType::Alpha);
  pixmap.apply_mask(&mask);

  // Hard-clip pixels outside the rectangle to avoid filter bleed.
  let x0 = rect.x().floor().max(0.0) as u32;
  let y0 = rect.y().floor().max(0.0) as u32;
  let x1 = (rect.x() + rect.width()).ceil().min(width as f32) as u32;
  let y1 = (rect.y() + rect.height()).ceil().min(height as f32) as u32;
  let data = pixmap.data_mut();
  let stride = (width as usize) * 4;
  for y in 0..height {
    for x in 0..width {
      if x < x0 || x >= x1 || y < y0 || y >= y1 {
        let idx = (y as usize * stride) + (x as usize * 4);
        data[idx] = 0;
        data[idx + 1] = 0;
        data[idx + 2] = 0;
        data[idx + 3] = 0;
      }
    }
  }
}

fn mask_value_from_pixel(pixel: &[u8], mode: MaskMode) -> u8 {
  let a = pixel.get(3).copied().unwrap_or(0) as f32 / 255.0;
  let value = match mode {
    MaskMode::Alpha => a,
    MaskMode::Luminance => {
      if a <= 0.0 {
        0.0
      } else {
        let r = pixel.get(0).copied().unwrap_or(0) as f32 / 255.0 / a;
        let g = pixel.get(1).copied().unwrap_or(0) as f32 / 255.0 / a;
        let b = pixel.get(2).copied().unwrap_or(0) as f32 / 255.0 / a;
        (0.2126 * r + 0.7152 * g + 0.0722 * b) * a
      }
    }
  };
  (value * 255.0).round().clamp(0.0, 255.0) as u8
}

fn mask_tile_from_image(tile: &Pixmap, mode: MaskMode) -> Option<Pixmap> {
  let size = IntSize::from_wh(tile.width(), tile.height())?;
  let mut data = Vec::with_capacity(tile.data().len());
  for chunk in tile.data().chunks(4) {
    let v = mask_value_from_pixel(chunk, mode);
    data.extend_from_slice(&[v, v, v, v]);
  }
  Pixmap::from_vec(data, size)
}

fn paint_mask_tile(
  dest: &mut Pixmap,
  tile: &Pixmap,
  tx: f32,
  ty: f32,
  tile_w: f32,
  tile_h: f32,
  clip_rect: Rect,
  scale: f32,
) {
  if tile_w <= 0.0 || tile_h <= 0.0 {
    return;
  }
  let tile_rect = Rect::from_xywh(tx, ty, tile_w, tile_h);
  let Some(intersection) = tile_rect.intersection(clip_rect) else {
    return;
  };
  if intersection.width() <= 0.0 || intersection.height() <= 0.0 {
    return;
  }

  let device_clip = Rect::from_xywh(
    intersection.x() * scale,
    intersection.y() * scale,
    intersection.width() * scale,
    intersection.height() * scale,
  );
  if device_clip.width() <= 0.0 || device_clip.height() <= 0.0 {
    return;
  }
  let device_bounds = Rect::from_xywh(0.0, 0.0, dest.width() as f32, dest.height() as f32);
  let Some(device_clip) = device_clip.intersection(device_bounds) else {
    return;
  };
  let Some(src_rect) = SkiaRect::from_xywh(
    device_clip.x(),
    device_clip.y(),
    device_clip.width(),
    device_clip.height(),
  ) else {
    return;
  };

  let scale_x = tile_w / tile.width() as f32;
  let scale_y = tile_h / tile.height() as f32;

  let mut paint = Paint::default();
  paint.shader = Pattern::new(
    tile.as_ref(),
    SpreadMode::Pad,
    FilterQuality::Bilinear,
    1.0,
    Transform::from_row(
      scale_x * scale,
      0.0,
      0.0,
      scale_y * scale,
      tx * scale,
      ty * scale,
    ),
  );
  paint.anti_alias = false;
  dest.fill_rect(src_rect, &paint, Transform::identity(), None);
}

fn apply_mask_composite(dest: &mut Mask, src: &Mask, op: MaskComposite) {
  if dest.width() != src.width() || dest.height() != src.height() {
    return;
  }

  let dest_data = dest.data_mut();
  let src_data = src.data();
  for (d, s) in dest_data.iter_mut().zip(src_data.iter()) {
    let src = *s as u16;
    let dst = *d as u16;
    let out = match op {
      MaskComposite::Add => src + dst.saturating_mul(255 - src) / 255,
      MaskComposite::Subtract => src.saturating_mul(255 - dst) / 255,
      MaskComposite::Intersect => src.saturating_mul(dst) / 255,
      MaskComposite::Exclude => {
        let src_out = src.saturating_mul(255 - dst) / 255;
        let dst_out = dst.saturating_mul(255 - src) / 255;
        src_out + dst_out
      }
    };
    *d = out.min(255) as u8;
  }
}

#[derive(Clone, Copy)]
struct BackgroundRects {
  border: Rect,
  padding: Rect,
  content: Rect,
}

fn background_rects(
  x: f32,
  y: f32,
  width: f32,
  height: f32,
  style: &ComputedStyle,
  viewport: Option<(f32, f32)>,
) -> BackgroundRects {
  let base = width.max(0.0);
  let font_size = style.font_size;
  let vp = viewport.unwrap_or((base, base));

  let border_left = resolve_length_for_paint(
    &style.border_left_width,
    font_size,
    style.root_font_size,
    base,
    vp,
  );
  let border_right = resolve_length_for_paint(
    &style.border_right_width,
    font_size,
    style.root_font_size,
    base,
    vp,
  );
  let border_top = resolve_length_for_paint(
    &style.border_top_width,
    font_size,
    style.root_font_size,
    base,
    vp,
  );
  let border_bottom = resolve_length_for_paint(
    &style.border_bottom_width,
    font_size,
    style.root_font_size,
    base,
    vp,
  );

  let padding_left = resolve_length_for_paint(
    &style.padding_left,
    font_size,
    style.root_font_size,
    base,
    vp,
  );
  let padding_right = resolve_length_for_paint(
    &style.padding_right,
    font_size,
    style.root_font_size,
    base,
    vp,
  );
  let padding_top = resolve_length_for_paint(
    &style.padding_top,
    font_size,
    style.root_font_size,
    base,
    vp,
  );
  let padding_bottom = resolve_length_for_paint(
    &style.padding_bottom,
    font_size,
    style.root_font_size,
    base,
    vp,
  );

  let border_rect = Rect::from_xywh(x, y, width, height);
  let padding_rect = inset_rect(
    border_rect,
    border_left,
    border_top,
    border_right,
    border_bottom,
  );
  let content_rect = inset_rect(
    padding_rect,
    padding_left,
    padding_top,
    padding_right,
    padding_bottom,
  );

  BackgroundRects {
    border: border_rect,
    padding: padding_rect,
    content: content_rect,
  }
}

fn inset_rect(rect: Rect, left: f32, top: f32, right: f32, bottom: f32) -> Rect {
  let new_x = rect.x() + left;
  let new_y = rect.y() + top;
  let new_w = (rect.width() - left - right).max(0.0);
  let new_h = (rect.height() - top - bottom).max(0.0);
  Rect::from_xywh(new_x, new_y, new_w, new_h)
}

fn resolve_length_for_paint(
  len: &Length,
  font_size: f32,
  root_font_size: f32,
  percentage_base: f32,
  viewport: (f32, f32),
) -> f32 {
  len
    .resolve_with_context(
      Some(percentage_base),
      viewport.0,
      viewport.1,
      font_size,
      root_font_size,
    )
    .unwrap_or_else(|| {
      if len.unit.is_absolute() {
        len.to_px()
      } else {
        len.value * font_size
      }
    })
}

fn compute_background_size(
  layer: &BackgroundLayer,
  font_size: f32,
  root_font_size: f32,
  viewport: (f32, f32),
  area_w: f32,
  area_h: f32,
  img_w: f32,
  img_h: f32,
) -> (f32, f32) {
  let natural_w = if img_w > 0.0 { Some(img_w) } else { None };
  let natural_h = if img_h > 0.0 { Some(img_h) } else { None };
  let ratio = if img_w > 0.0 && img_h > 0.0 {
    Some(img_w / img_h)
  } else {
    None
  };

  match layer.size {
    BackgroundSize::Keyword(BackgroundSizeKeyword::Cover) => {
      if let (Some(w), Some(h)) = (natural_w, natural_h) {
        let scale = (area_w / w).max(area_h / h);
        (w * scale, h * scale)
      } else {
        (area_w.max(0.0), area_h.max(0.0))
      }
    }
    BackgroundSize::Keyword(BackgroundSizeKeyword::Contain) => {
      if let (Some(w), Some(h)) = (natural_w, natural_h) {
        let scale = (area_w / w).min(area_h / h);
        (w * scale, h * scale)
      } else {
        (area_w.max(0.0), area_h.max(0.0))
      }
    }
    BackgroundSize::Explicit(x, y) => {
      let resolve = |component: BackgroundSizeComponent, area: f32| -> Option<f32> {
        match component {
          BackgroundSizeComponent::Auto => None,
          BackgroundSizeComponent::Length(len) => {
            Some(resolve_length_for_paint(&len, font_size, root_font_size, area, viewport).max(0.0))
          }
        }
      };

      let resolved_x = resolve(x, area_w);
      let resolved_y = resolve(y, area_h);

      match (resolved_x, resolved_y) {
        (Some(w), Some(h)) => (w, h),
        (Some(w), None) => {
          if let Some(r) = ratio {
            (w, (w / r).max(0.0))
          } else if let Some(h) = natural_h {
            (w, h)
          } else {
            (w, area_h.max(0.0))
          }
        }
        (None, Some(h)) => {
          if let Some(r) = ratio {
            ((h * r).max(0.0), h)
          } else if let Some(w) = natural_w {
            (w, h)
          } else {
            (area_w.max(0.0), h)
          }
        }
        (None, None) => {
          if let (Some(w), Some(h)) = (natural_w, natural_h) {
            (w, h)
          } else {
            (area_w.max(0.0), area_h.max(0.0))
          }
        }
      }
    }
  }
}

fn resolve_background_offset(
  pos: BackgroundPosition,
  area_w: f32,
  area_h: f32,
  tile_w: f32,
  tile_h: f32,
  font_size: f32,
  root_font_size: f32,
  viewport: (f32, f32),
) -> (f32, f32) {
  let resolve_axis =
    |comp: crate::style::types::BackgroundPositionComponent, area: f32, tile: f32| -> f32 {
      let available = area - tile;
      let offset =
        resolve_length_for_paint(&comp.offset, font_size, root_font_size, available, viewport);
      comp.alignment * available + offset
    };

  match pos {
    BackgroundPosition::Position { x, y } => {
      let x = resolve_axis(x, area_w, tile_w);
      let y = resolve_axis(y, area_h, tile_h);
      (x, y)
    }
  }
}

fn normalize_color_stops(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
  if stops.is_empty() {
    return Vec::new();
  }

  let mut positions: Vec<Option<f32>> = stops.iter().map(|s| s.position).collect();
  if positions.iter().all(|p| p.is_none()) {
    if stops.len() == 1 {
      return vec![(0.0, stops[0].color.to_rgba(current_color))];
    }
    let denom = (stops.len() - 1) as f32;
    return stops
      .iter()
      .enumerate()
      .map(|(i, s)| (i as f32 / denom, s.color.to_rgba(current_color)))
      .collect();
  }

  if positions.first().and_then(|p| *p).is_none() {
    positions[0] = Some(0.0);
  }
  if positions.last().and_then(|p| *p).is_none() {
    if let Some(last) = positions.last_mut() {
      *last = Some(1.0);
    }
  }

  let mut last_known: Option<(usize, f32)> = None;
  for i in 0..positions.len() {
    if let Some(pos) = positions[i] {
      if let Some((start_idx, start_pos)) = last_known {
        if let Some(gap) = i.checked_sub(start_idx + 1) {
          if gap > 0 {
            let step = (pos - start_pos) / (gap as f32 + 1.0);
            for (j, slot) in positions
              .iter_mut()
              .enumerate()
              .take(start_idx + gap + 1)
              .skip(start_idx + 1)
            {
              *slot = Some((start_pos + step * j as f32).max(start_pos));
            }
          }
        }
      } else if i > 0 {
        let gap = i;
        let step = pos / gap as f32;
        for (j, slot) in positions.iter_mut().take(i).enumerate() {
          *slot = Some(step * j as f32);
        }
      }
      last_known = Some((i, pos));
    }
  }

  let mut output = Vec::with_capacity(stops.len());
  let mut prev = 0.0;
  for (idx, pos_opt) in positions.iter().enumerate() {
    let pos = pos_opt.map_or(prev, |p| p);
    let clamped = pos.max(prev).clamp(0.0, 1.0);
    prev = clamped;
    output.push((clamped, stops[idx].color.to_rgba(current_color)));
  }

  output
}

fn normalize_color_stops_unclamped(stops: &[ColorStop], current_color: Rgba) -> Vec<(f32, Rgba)> {
  if stops.is_empty() {
    return Vec::new();
  }
  let mut positions: Vec<Option<f32>> = stops.iter().map(|s| s.position).collect();
  if positions.iter().all(|p| p.is_none()) {
    if stops.len() == 1 {
      return vec![(0.0, stops[0].color.to_rgba(current_color))];
    }
    let denom = (stops.len() - 1) as f32;
    return stops
      .iter()
      .enumerate()
      .map(|(i, s)| (i as f32 / denom, s.color.to_rgba(current_color)))
      .collect();
  }
  if positions.first().and_then(|p| *p).is_none() {
    positions[0] = Some(0.0);
  }
  if positions.last().and_then(|p| *p).is_none() {
    if let Some(last) = positions.last_mut() {
      *last = Some(1.0);
    }
  }
  let mut last_known: Option<(usize, f32)> = None;
  for i in 0..positions.len() {
    if let Some(pos) = positions[i] {
      if let Some((start_idx, start_pos)) = last_known {
        if let Some(gap) = i.checked_sub(start_idx + 1) {
          if gap > 0 {
            let step = (pos - start_pos) / (gap as f32 + 1.0);
            for (j, slot) in positions
              .iter_mut()
              .enumerate()
              .take(start_idx + gap + 1)
              .skip(start_idx + 1)
            {
              *slot = Some(start_pos + step * j as f32);
            }
          }
        }
      } else if i > 0 {
        let gap = i;
        let step = pos / gap as f32;
        for (j, slot) in positions.iter_mut().take(i).enumerate() {
          *slot = Some(step * j as f32);
        }
      }
      last_known = Some((i, pos));
    }
  }
  let mut output = Vec::with_capacity(stops.len());
  let mut prev = 0.0;
  for (idx, pos_opt) in positions.iter().enumerate() {
    let pos = pos_opt.map_or(prev, |p| p);
    let monotonic = pos.max(prev);
    prev = monotonic;
    output.push((monotonic, stops[idx].color.to_rgba(current_color)));
  }
  output
}

fn radial_geometry(
  rect: Rect,
  position: &BackgroundPosition,
  size: &RadialGradientSize,
  shape: RadialGradientShape,
  font_size: f32,
  root_font_size: f32,
  viewport: (f32, f32),
) -> (f32, f32, f32, f32) {
  let (align_x, off_x, align_y, off_y) = match position {
    BackgroundPosition::Position { x, y } => {
      let ox =
        resolve_length_for_paint(&x.offset, font_size, root_font_size, rect.width(), viewport);
      let oy = resolve_length_for_paint(
        &y.offset,
        font_size,
        root_font_size,
        rect.height(),
        viewport,
      );
      (x.alignment, ox, y.alignment, oy)
    }
  };
  let cx = rect.x() + align_x * rect.width() + off_x;
  let cy = rect.y() + align_y * rect.height() + off_y;

  let dx_left = (cx - rect.x()).max(0.0);
  let dx_right = (rect.x() + rect.width() - cx).max(0.0);
  let dy_top = (cy - rect.y()).max(0.0);
  let dy_bottom = (rect.y() + rect.height() - cy).max(0.0);

  let (mut radius_x, mut radius_y) = match size {
    RadialGradientSize::ClosestSide => (dx_left.min(dx_right), dy_top.min(dy_bottom)),
    RadialGradientSize::FarthestSide => (dx_left.max(dx_right), dy_top.max(dy_bottom)),
    RadialGradientSize::ClosestCorner => {
      let corners = [
        (dx_left, dy_top),
        (dx_left, dy_bottom),
        (dx_right, dy_top),
        (dx_right, dy_bottom),
      ];
      let mut best = f32::INFINITY;
      let mut best_pair = (0.0, 0.0);
      for (dx, dy) in corners {
        let dist = (dx * dx + dy * dy).sqrt();
        if dist < best {
          best = dist;
          best_pair = (dx, dy);
        }
      }
      (
        best_pair.0 * std::f32::consts::SQRT_2,
        best_pair.1 * std::f32::consts::SQRT_2,
      )
    }
    RadialGradientSize::FarthestCorner => {
      let corners = [
        (dx_left, dy_top),
        (dx_left, dy_bottom),
        (dx_right, dy_top),
        (dx_right, dy_bottom),
      ];
      let mut best = -f32::INFINITY;
      let mut best_pair = (0.0, 0.0);
      for (dx, dy) in corners {
        let dist = (dx * dx + dy * dy).sqrt();
        if dist > best {
          best = dist;
          best_pair = (dx, dy);
        }
      }
      (
        best_pair.0 * std::f32::consts::SQRT_2,
        best_pair.1 * std::f32::consts::SQRT_2,
      )
    }
    RadialGradientSize::Explicit { x, y } => {
      let rx =
        resolve_length_for_paint(x, font_size, root_font_size, rect.width(), viewport).max(0.0);
      let ry = y
        .as_ref()
        .map(|yy| {
          resolve_length_for_paint(yy, font_size, root_font_size, rect.height(), viewport).max(0.0)
        })
        .unwrap_or(rx);
      (rx, ry)
    }
  };

  if matches!(shape, RadialGradientShape::Circle) {
    let r = if matches!(
      size,
      RadialGradientSize::ClosestCorner | RadialGradientSize::FarthestCorner
    ) {
      // Corner-based sizes already yield isotropic radii; use hypotenuse distance instead.
      let r_corner = ((radius_x * radius_x + radius_y * radius_y) / 2.0).sqrt();
      r_corner
    } else {
      radius_x.min(radius_y)
    };
    radius_x = r;
    radius_y = r;
  }

  (cx, cy, radius_x.max(0.0), radius_y.max(0.0))
}

fn resolve_gradient_center(
  rect: Rect,
  position: &BackgroundPosition,
  paint_rect: Rect,
  font_size: f32,
  root_font_size: f32,
  viewport: (f32, f32),
) -> Point {
  let (align_x, off_x, align_y, off_y) = match position {
    BackgroundPosition::Position { x, y } => {
      let ox =
        resolve_length_for_paint(&x.offset, font_size, root_font_size, rect.width(), viewport);
      let oy = resolve_length_for_paint(
        &y.offset,
        font_size,
        root_font_size,
        rect.height(),
        viewport,
      );
      (x.alignment, ox, y.alignment, oy)
    }
  };
  let cx = rect.x() + align_x * rect.width() + off_x - paint_rect.x();
  let cy = rect.y() + align_y * rect.height() + off_y - paint_rect.y();
  Point::new(cx, cy)
}

fn sample_stops(stops: &[(f32, Rgba)], t: f32, repeating: bool, period: f32) -> Rgba {
  if stops.is_empty() {
    return Rgba::TRANSPARENT;
  }
  if stops.len() == 1 {
    return stops[0].1;
  }
  let total = if repeating {
    period
  } else {
    stops.last().map(|(p, _)| *p).unwrap_or(1.0)
  };
  let mut pos = t;
  if repeating && total > 0.0 {
    pos = pos.rem_euclid(total);
  }
  if pos <= stops[0].0 {
    return stops[0].1;
  }
  if pos >= stops.last().unwrap().0 && !repeating {
    return stops.last().unwrap().1;
  }
  for window in stops.windows(2) {
    let (p0, c0) = window[0];
    let (p1, c1) = window[1];
    if pos < p0 {
      return c0;
    }
    if pos <= p1 || (repeating && (p1 - p0).abs() < f32::EPSILON) {
      let span = (p1 - p0).max(1e-6);
      let frac = ((pos - p0) / span).clamp(0.0, 1.0);
      return Rgba {
        r: ((1.0 - frac) * c0.r as f32 + frac * c1.r as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        g: ((1.0 - frac) * c0.g as f32 + frac * c1.g as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        b: ((1.0 - frac) * c0.b as f32 + frac * c1.b as f32)
          .round()
          .clamp(0.0, 255.0) as u8,
        a: ((1.0 - frac) * c0.a + frac * c1.a),
      };
    }
  }
  stops.last().unwrap().1
}

fn gradient_stops(stops: &[(f32, Rgba)]) -> Vec<tiny_skia::GradientStop> {
  stops
    .iter()
    .map(|(pos, color)| {
      let alpha = (color.a * 255.0).round().clamp(0.0, 255.0) as u8;
      tiny_skia::GradientStop::new(
        pos.clamp(0.0, 1.0),
        tiny_skia::Color::from_rgba8(color.r, color.g, color.b, alpha),
      )
    })
    .collect()
}

fn aligned_start(origin: f32, tile: f32, clip_min: f32) -> f32 {
  if tile == 0.0 {
    return origin;
  }
  let steps = ((clip_min - origin) / tile).floor();
  origin + steps * tile
}

fn round_tile_length(area_len: f32, tile_len: f32) -> f32 {
  if tile_len == 0.0 {
    return 0.0;
  }
  let count = (area_len / tile_len).round().max(1.0);
  area_len / count
}

fn tile_positions(
  repeat: BackgroundRepeatKeyword,
  area_start: f32,
  area_len: f32,
  tile_len: f32,
  offset: f32,
  clip_min: f32,
  clip_max: f32,
) -> Vec<f32> {
  if tile_len <= 0.0 {
    return Vec::new();
  }

  match repeat {
    BackgroundRepeatKeyword::NoRepeat => vec![area_start + offset],
    BackgroundRepeatKeyword::Repeat | BackgroundRepeatKeyword::Round => {
      let start = aligned_start(area_start + offset, tile_len, clip_min);
      let mut positions = Vec::new();
      let mut pos = start;
      while pos < clip_max {
        positions.push(pos);
        pos += tile_len;
      }
      positions
    }
    BackgroundRepeatKeyword::Space => {
      let count = (area_len / tile_len).floor() as i32;
      if count >= 2 {
        let spacing = (area_len - tile_len * count as f32) / (count as f32 - 1.0);
        let step = tile_len + spacing;
        let anchor = area_start;
        let mut positions = Vec::new();
        let k = ((clip_min - anchor) / step).floor();
        let mut pos = anchor + k * step;
        while pos < clip_max {
          positions.push(pos);
          pos += step;
        }
        positions
      } else {
        // When fewer than two tiles fit, center the single tile per CSS Backgrounds 3.
        let centered = area_start + offset + (area_len - tile_len) * 0.5;
        vec![centered]
      }
    }
  }
}

/// Painting backends supported by the renderer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PaintBackend {
  Legacy,
  DisplayList,
}

fn paint_backend_from_env() -> PaintBackend {
  static BACKEND: OnceLock<PaintBackend> = OnceLock::new();
  *BACKEND.get_or_init(|| {
    let Ok(raw) = std::env::var("FASTR_PAINT_BACKEND") else {
      return PaintBackend::Legacy;
    };
    match raw.trim().to_ascii_lowercase().as_str() {
      "display_list" | "display-list" | "displaylist" => PaintBackend::DisplayList,
      "legacy" | "immediate" => PaintBackend::Legacy,
      _ => PaintBackend::Legacy,
    }
  })
}

fn legacy_paint_tree_with_resources_scaled_offset(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
  scale: f32,
  offset: Point,
) -> Result<Pixmap> {
  let painter =
    Painter::with_resources_scaled(width, height, background, font_ctx, image_cache, scale)?;
  painter.paint_with_offset(tree, offset)
}

/// Paints a fragment tree via the display-list pipeline (builder → optimize → renderer).
pub fn paint_tree_display_list_with_resources_scaled_offset(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
  scale: f32,
  offset: Point,
) -> Result<Pixmap> {
  let viewport = tree.viewport_size();
  let display_list = DisplayListBuilder::with_image_cache(image_cache)
    .with_font_context(font_ctx.clone())
    .with_device_pixel_ratio(scale)
    .with_viewport_size(viewport.width, viewport.height)
    .build_with_stacking_tree_offset(&tree.root, offset);

  let optimizer = DisplayListOptimizer::new();
  let viewport_rect = Rect::from_xywh(0.0, 0.0, viewport.width, viewport.height);
  let (optimized, _) = optimizer.optimize(display_list, viewport_rect);

  let renderer = DisplayListRenderer::new_scaled(width, height, background, font_ctx, scale)?;
  renderer.render(&optimized)
}

/// Paints a fragment tree via the display-list pipeline using explicit resources.
pub fn paint_tree_display_list_with_resources(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
) -> Result<Pixmap> {
  paint_tree_display_list_with_resources_scaled_offset(
    tree,
    width,
    height,
    background,
    font_ctx,
    image_cache,
    1.0,
    Point::ZERO,
  )
}

/// Paints a fragment tree via the display-list pipeline with default resources.
pub fn paint_tree_display_list(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
) -> Result<Pixmap> {
  paint_tree_display_list_with_resources_scaled_offset(
    tree,
    width,
    height,
    background,
    FontContext::new(),
    ImageCache::new(),
    1.0,
    Point::ZERO,
  )
}

/// Paints a fragment tree at a device scale with an additional translation applied using the
/// selected backend.
pub fn paint_tree_with_resources_scaled_offset_backend(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
  scale: f32,
  offset: Point,
  backend: PaintBackend,
) -> Result<Pixmap> {
  match backend {
    PaintBackend::Legacy => legacy_paint_tree_with_resources_scaled_offset(
      tree,
      width,
      height,
      background,
      font_ctx,
      image_cache,
      scale,
      offset,
    ),
    PaintBackend::DisplayList => paint_tree_display_list_with_resources_scaled_offset(
      tree,
      width,
      height,
      background,
      font_ctx,
      image_cache,
      scale,
      offset,
    ),
  }
}

/// Paints a fragment tree at a device scale with an additional translation applied.
pub fn paint_tree_with_resources_scaled_offset(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
  scale: f32,
  offset: Point,
) -> Result<Pixmap> {
  paint_tree_with_resources_scaled_offset_backend(
    tree,
    width,
    height,
    background,
    font_ctx,
    image_cache,
    scale,
    offset,
    paint_backend_from_env(),
  )
}

/// Paints a fragment tree using explicit resources at the provided device scale.
pub fn paint_tree_with_resources_scaled(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
  scale: f32,
) -> Result<Pixmap> {
  paint_tree_with_resources_scaled_offset(
    tree,
    width,
    height,
    background,
    font_ctx,
    image_cache,
    scale,
    Point::ZERO,
  )
}

/// Paints a fragment tree using provided font and image resources.
pub fn paint_tree_with_resources(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
) -> Result<Pixmap> {
  paint_tree_with_resources_scaled(tree, width, height, background, font_ctx, image_cache, 1.0)
}

/// Paints a fragment tree at the given device scale.
pub fn paint_tree_scaled(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  scale: f32,
) -> Result<Pixmap> {
  paint_tree_with_resources_scaled(
    tree,
    width,
    height,
    background,
    FontContext::new(),
    ImageCache::new(),
    scale,
  )
}

/// Paints a fragment tree to a pixmap
///
/// This is the main entry point for painting.
pub fn paint_tree(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
) -> Result<Pixmap> {
  paint_tree_scaled(tree, width, height, background, 1.0)
}

/// Paints a fragment tree with tracing enabled.
pub(crate) fn paint_tree_with_resources_scaled_offset_with_trace(
  tree: &FragmentTree,
  width: u32,
  height: u32,
  background: Rgba,
  font_ctx: FontContext,
  image_cache: ImageCache,
  scale: f32,
  offset: Point,
  trace: TraceHandle,
) -> Result<Pixmap> {
  let painter =
    Painter::with_resources_scaled(width, height, background, font_ctx, image_cache, scale)?
      .with_trace(trace);
  painter.paint_with_offset(tree, offset)
}

/// Scales a pixmap by the given device pixel ratio, returning a new pixmap.
/// This is a coarse fallback for high-DPI outputs; painting should ideally
/// happen directly at device resolution.
pub fn scale_pixmap_for_dpr(pixmap: Pixmap, dpr: f32) -> Result<Pixmap> {
  let dpr = if dpr.is_finite() && dpr > 0.0 {
    dpr
  } else {
    1.0
  };
  if (dpr - 1.0).abs() < f32::EPSILON {
    return Ok(pixmap);
  }

  let new_w = (((pixmap.width() as f32) * dpr).round()).max(1.0) as u32;
  let new_h = (((pixmap.height() as f32) * dpr).round()).max(1.0) as u32;
  let mut target = Pixmap::new(new_w, new_h).ok_or_else(|| RenderError::InvalidParameters {
    message: "Failed to allocate scaled pixmap".to_string(),
  })?;

  let mut paint = PixmapPaint::default();
  paint.quality = FilterQuality::Bilinear;
  let transform = Transform::from_scale(dpr, dpr);
  target.draw_pixmap(0, 0, pixmap.as_ref(), &paint, transform, None);
  Ok(target)
}

#[derive(Copy, Clone)]
struct BorderWidths {
  top: f32,
  right: f32,
  bottom: f32,
  left: f32,
}

fn resolve_slice_value(value: crate::style::types::BorderImageSliceValue, axis_len: u32) -> f32 {
  match value {
    crate::style::types::BorderImageSliceValue::Number(n) => n.max(0.0),
    crate::style::types::BorderImageSliceValue::Percentage(p) => (p / 100.0) * axis_len as f32,
  }
}

fn resolve_border_image_widths(
  widths: &crate::style::types::BorderImageWidth,
  border: BorderWidths,
  box_width: f32,
  box_height: f32,
  font_size: f32,
  root_font_size: f32,
  viewport: (f32, f32),
) -> BorderWidths {
  let resolve_single = |value: BorderImageWidthValue, border: f32, axis: f32| -> f32 {
    match value {
      BorderImageWidthValue::Auto => border,
      BorderImageWidthValue::Number(n) => (n * border).max(0.0),
      BorderImageWidthValue::Length(len) => {
        resolve_length_for_paint(&len, font_size, root_font_size, axis, viewport).max(0.0)
      }
      BorderImageWidthValue::Percentage(p) => ((p / 100.0) * axis).max(0.0),
    }
  };

  BorderWidths {
    top: resolve_single(widths.top, border.top, box_height),
    right: resolve_single(widths.right, border.right, box_width),
    bottom: resolve_single(widths.bottom, border.bottom, box_height),
    left: resolve_single(widths.left, border.left, box_width),
  }
}

fn resolve_border_image_outset(
  outset: &crate::style::types::BorderImageOutset,
  border: BorderWidths,
  font_size: f32,
  root_font_size: f32,
  viewport: (f32, f32),
) -> BorderWidths {
  fn resolve_single(
    value: BorderImageOutsetValue,
    border: f32,
    font_size: f32,
    root_font_size: f32,
    viewport: (f32, f32),
  ) -> f32 {
    match value {
      BorderImageOutsetValue::Number(n) => (n * border).max(0.0),
      BorderImageOutsetValue::Length(len) => {
        resolve_length_for_paint(&len, font_size, root_font_size, border.max(1.0), viewport)
          .max(0.0)
      }
    }
  }

  BorderWidths {
    top: resolve_single(outset.top, border.top, font_size, root_font_size, viewport),
    right: resolve_single(
      outset.right,
      border.right,
      font_size,
      root_font_size,
      viewport,
    ),
    bottom: resolve_single(
      outset.bottom,
      border.bottom,
      font_size,
      root_font_size,
      viewport,
    ),
    left: resolve_single(
      outset.left,
      border.left,
      font_size,
      root_font_size,
      viewport,
    ),
  }
}

fn escape_style_end_tags(css: &str) -> String {
  const STYLE: [u8; 5] = *b"style";
  let bytes = css.as_bytes();

  let has_sequence = bytes.windows(7).any(|window| {
    window[0] == b'<'
      && window[1] == b'/'
      && window[2..]
        .iter()
        .zip(STYLE.iter())
        .all(|(b, expected)| b.to_ascii_lowercase() == *expected)
  });

  if !has_sequence {
    return css.to_string();
  }

  let mut out = Vec::with_capacity(bytes.len());
  let mut idx = 0;
  while idx < bytes.len() {
    if idx + 7 <= bytes.len() && bytes[idx] == b'<' && bytes[idx + 1] == b'/' {
      if STYLE
        .iter()
        .enumerate()
        .all(|(offset, expected)| bytes[idx + 2 + offset].to_ascii_lowercase() == *expected)
      {
        out.extend_from_slice(b"<\\/style");
        idx += 7;
        continue;
      }
    }

    out.push(bytes[idx]);
    idx += 1;
  }

  String::from_utf8(out).unwrap_or_default()
}

fn build_foreign_object_document(info: &ForeignObjectInfo, shared_css: &str) -> String {
  let mut html = String::from("<!DOCTYPE html><html><head><meta charset=\"utf-8\">");
  if !shared_css.trim().is_empty() {
    let sanitized_css = escape_style_end_tags(shared_css);
    html.push_str("<style>");
    html.push_str(&sanitized_css);
    html.push_str("</style>");
  }
  html.push_str("</head><body style=\"");
  html.push_str(&foreign_object_body_style(info));
  html.push_str("\">");
  html.push_str(&info.html);
  html.push_str("</body></html>");
  html
}

fn foreign_object_body_style(info: &ForeignObjectInfo) -> String {
  let mut style =
    String::from("margin:0;padding:0;width:100%;height:100%;display:block;box-sizing:border-box;");
  let overflow = match (info.overflow_x, info.overflow_y) {
    (Overflow::Visible, Overflow::Visible) => "visible",
    _ => "hidden",
  };
  let _ = write!(&mut style, "overflow:{};", overflow);

  if let Some(bg) = info.background {
    style.push_str("background:");
    style.push_str(&format_css_color(bg));
    style.push(';');
  }

  style.push_str("color:");
  style.push_str(&format_css_color(info.style.color));
  style.push(';');

  if !info.style.font_family.is_empty() {
    let families: Vec<String> = info
      .style
      .font_family
      .iter()
      .map(|f| {
        if f.contains(' ') && !(f.starts_with('"') && f.ends_with('"')) {
          format!("\"{}\"", f)
        } else {
          f.clone()
        }
      })
      .collect();
    style.push_str("font-family:");
    style.push_str(&families.join(", "));
    style.push(';');
  }

  let _ = write!(
    &mut style,
    "font-size:{:.2}px;font-weight:{};",
    info.style.font_size,
    info.style.font_weight.to_u16()
  );

  match info.style.font_style {
    CssFontStyle::Italic => style.push_str("font-style: italic;"),
    CssFontStyle::Oblique(Some(angle)) => {
      let _ = write!(&mut style, "font-style: oblique {}deg;", angle);
    }
    CssFontStyle::Oblique(None) => style.push_str("font-style: oblique;"),
    CssFontStyle::Normal => {}
  }

  if info.style.direction == Direction::Rtl {
    style.push_str("direction: rtl;");
  }

  if info.style.writing_mode != WritingMode::HorizontalTb {
    style.push_str("writing-mode:");
    style.push_str(writing_mode_keyword(info.style.writing_mode));
    style.push(';');
  }

  style
}

fn writing_mode_keyword(mode: WritingMode) -> &'static str {
  match mode {
    WritingMode::HorizontalTb => "horizontal-tb",
    WritingMode::VerticalRl => "vertical-rl",
    WritingMode::VerticalLr => "vertical-lr",
    WritingMode::SidewaysRl => "sideways-rl",
    WritingMode::SidewaysLr => "sideways-lr",
  }
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

fn escape_attr_value(value: &str) -> String {
  value
    .replace('&', "&amp;")
    .replace('<', "&lt;")
    .replace('"', "&quot;")
    .replace('\'', "&apos;")
}

fn pixmap_to_data_url(pixmap: Pixmap) -> Option<String> {
  let width = pixmap.width();
  let height = pixmap.height();
  let data = pixmap.take();
  let image = image::RgbaImage::from_raw(width, height, data)?;
  let mut buf = Vec::new();
  PngEncoder::new(&mut buf)
    .write_image(image.as_raw(), width, height, ColorType::Rgba8.into())
    .ok()?;

  Some(format!(
    "data:image/png;base64,{}",
    base64::engine::general_purpose::STANDARD.encode(buf)
  ))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::types::ColorStop;
  use crate::css::types::TextShadow;
  use crate::geometry::Rect;
  use crate::image_loader::ImageCache;
  use crate::paint::display_list::BorderRadii;
  use crate::style::types::BackgroundAttachment;
  use crate::style::types::BackgroundBox;
  use crate::style::types::BackgroundImage;
  use crate::style::types::BackgroundPosition;
  use crate::style::types::BackgroundPositionComponent;
  use crate::style::types::BackgroundRepeat;
  use crate::style::types::BackgroundSize;
  use crate::style::types::BackgroundSizeComponent;
  use crate::style::types::BorderImage;
  use crate::style::types::BorderImageRepeat;
  use crate::style::types::BorderImageSlice;
  use crate::style::types::BorderImageSliceValue;
  use crate::style::types::BorderImageSource;
  use crate::style::types::ClipPath;
  use crate::style::types::FilterShadow;
  use crate::style::types::ImageRendering;
  use crate::style::types::Isolation;
  use crate::style::types::MixBlendMode;
  use crate::style::types::OutlineColor;
  use crate::style::types::OutlineStyle;
  use crate::style::types::Overflow;
  use crate::style::types::ShapeRadius;
  use crate::style::types::TransformBox;
  use crate::style::values::Length;
  use crate::style::ComputedStyle;
  use crate::text::font_loader::FontContext;
  use crate::tree::box_tree::SrcsetCandidate;
  use crate::tree::box_tree::SrcsetDescriptor;
  use crate::Position;
  use base64::Engine;
  use image::codecs::png::PngEncoder;
  use image::ExtendedColorType;
  use image::ImageEncoder;
  use image::RgbaImage;
  use std::sync::Arc;

  fn make_empty_tree() -> FragmentTree {
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![]);
    FragmentTree::new(root)
  }

  fn red_svg() -> String {
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><rect width=\"1\" height=\"1\" fill=\"red\"/></svg>"
            .to_string()
  }

  fn color_at(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let px = pixmap.pixel(x, y).expect("pixel in bounds");
    let a = px.alpha();
    if a == 0 {
      return (0, 0, 0, 0);
    }
    let r = ((px.red() as u16 * 255) / a as u16) as u8;
    let g = ((px.green() as u16 * 255) / a as u16) as u8;
    let b = ((px.blue() as u16 * 255) / a as u16) as u8;
    (r, g, b, a)
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
    let (h, s, l) = rgb_to_hsl(
      actual.0 as f32 / 255.0,
      actual.1 as f32 / 255.0,
      actual.2 as f32 / 255.0,
    );
    assert!(
      hue_delta(h, expected_hsl.0) <= tol_h
        && (s - expected_hsl.1).abs() <= tol_s
        && (l - expected_hsl.2).abs() <= tol_l,
      "{context}: expected hsl {:?}, got hsl ({h:.3},{s:.3},{l:.3})",
      expected_hsl
    );
  }

  fn bounding_box_for_color(
    pixmap: &Pixmap,
    predicate: impl Fn((u8, u8, u8, u8)) -> bool,
  ) -> Option<(u32, u32, u32, u32)> {
    let mut min_x = u32::MAX;
    let mut min_y = u32::MAX;
    let mut max_x = 0u32;
    let mut max_y = 0u32;

    for y in 0..pixmap.height() {
      for x in 0..pixmap.width() {
        let color = color_at(pixmap, x, y);
        if predicate(color) {
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

  fn downsample_half(pixmap: Pixmap) -> Pixmap {
    scale_pixmap_for_dpr(pixmap, 0.5).expect("downsample pixmap")
  }

  fn mean_abs_diff(a: &Pixmap, b: &Pixmap) -> f32 {
    assert_eq!(a.width(), b.width());
    assert_eq!(a.height(), b.height());

    let mut total: u64 = 0;
    let mut count: u64 = 0;
    for (pa, pb) in a.data().chunks_exact(4).zip(b.data().chunks_exact(4)) {
      for i in 0..4 {
        total += pa[i].abs_diff(pb[i]) as u64;
        count += 1;
      }
    }
    total as f32 / count as f32
  }

  fn two_color_data_url() -> String {
    let pixels = vec![
      255, 0, 0, 255, // red
      0, 0, 255, 255, // blue
    ];
    let mut buf = Vec::new();
    PngEncoder::new(&mut buf)
      .write_image(&pixels, 2, 1, ExtendedColorType::Rgba8)
      .expect("encode png");
    format!(
      "data:image/png;base64,{}",
      base64::engine::general_purpose::STANDARD.encode(buf)
    )
  }

  #[test]
  fn image_rendering_crisp_edges_uses_nearest_filter_quality() {
    let mut style = ComputedStyle::default();
    style.image_rendering = ImageRendering::CrispEdges;
    assert_eq!(
      Painter::filter_quality_for_image(Some(&style)),
      FilterQuality::Nearest
    );
  }

  #[test]
  fn background_image_rendering_pixelated_uses_nearest_sampling() {
    let url = two_color_data_url();

    let mut style = ComputedStyle::default();
    style.image_rendering = ImageRendering::Pixelated;
    style.background_color = Rgba::WHITE;
    style.background_layers = vec![BackgroundLayer {
      image: Some(BackgroundImage::Url(url)),
      size: BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(5.0)),
        BackgroundSizeComponent::Length(Length::px(1.0)),
      ),
      repeat: BackgroundRepeat::no_repeat(),
      ..BackgroundLayer::default()
    }];

    let fragment =
      FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 5.0, 1.0), vec![], Arc::new(style));
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 5, 1, Rgba::WHITE).expect("paint");

    // Pixelated sampling should keep the left half fully red and the right half blue without purple blending.
    assert_eq!(color_at(&pixmap, 1, 0), (255, 0, 0, 255));
    assert_eq!(color_at(&pixmap, 3, 0), (0, 0, 255, 255));
  }

  #[test]
  fn background_image_rendering_crisp_edges_uses_nearest_sampling() {
    let url = two_color_data_url();

    let mut style = ComputedStyle::default();
    style.image_rendering = ImageRendering::CrispEdges;
    style.background_color = Rgba::WHITE;
    style.background_layers = vec![BackgroundLayer {
      image: Some(BackgroundImage::Url(url)),
      size: BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(5.0)),
        BackgroundSizeComponent::Length(Length::px(1.0)),
      ),
      repeat: BackgroundRepeat::no_repeat(),
      ..BackgroundLayer::default()
    }];

    let fragment =
      FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 5.0, 1.0), vec![], Arc::new(style));
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 5, 1, Rgba::WHITE).expect("paint");

    // Crisp edges should also use nearest-neighbor sampling when upscaling backgrounds.
    assert_eq!(color_at(&pixmap, 1, 0), (255, 0, 0, 255));
    assert_eq!(color_at(&pixmap, 3, 0), (0, 0, 255, 255));
  }

  #[test]
  fn stacking_order_places_floats_between_blocks_and_inlines() {
    // Build four children that should paint in block → float → inline → positioned order.
    let mut block_style = ComputedStyle::default();
    block_style.display = Display::Block;
    block_style.background_color = Rgba::RED;
    let mut block = FragmentNode::new_block(Rect::from_xywh(10.0, 0.0, 5.0, 5.0), vec![]);
    block.style = Some(Arc::new(block_style));

    let mut float_style = ComputedStyle::default();
    float_style.display = Display::Block;
    float_style.float = crate::style::float::Float::Left;
    float_style.background_color = Rgba::GREEN;
    let mut float_frag = FragmentNode::new_block(Rect::from_xywh(20.0, 0.0, 5.0, 5.0), vec![]);
    float_frag.style = Some(Arc::new(float_style));

    let mut inline_style = ComputedStyle::default();
    inline_style.display = Display::Inline;
    inline_style.background_color = Rgba::BLUE;
    let mut inline =
      FragmentNode::new_text(Rect::from_xywh(30.0, 0.0, 5.0, 5.0), "x".to_string(), 12.0);
    inline.style = Some(Arc::new(inline_style));

    let mut positioned_style = ComputedStyle::default();
    positioned_style.display = Display::Block;
    positioned_style.position = Position::Relative;
    positioned_style.background_color = Rgba::WHITE;
    let mut positioned = FragmentNode::new_block(Rect::from_xywh(40.0, 0.0, 5.0, 5.0), vec![]);
    positioned.style = Some(Arc::new(positioned_style));

    let root = FragmentNode::new_block(
      Rect::from_xywh(0.0, 0.0, 100.0, 10.0),
      vec![block, float_frag, inline, positioned],
    );

    let painter = Painter::new(100, 10, Rgba::WHITE).expect("painter");
    let mut commands = Vec::new();
    let mut svg_filters = SvgFilterResolver::new(None, vec![&root], None);
    painter.collect_stacking_context(
      &root,
      Point::ZERO,
      None,
      true,
      RootPaintOptions {
        use_root_background: false,
        extend_background_to_viewport: false,
      },
      &mut commands,
      &mut svg_filters,
    );

    // Background commands occur in paint order; filter child backgrounds to check ordering.
    let xs: Vec<f32> = commands
      .iter()
      .filter_map(|cmd| match cmd {
        DisplayCommand::Background { rect, .. } if rect.width() == 5.0 => Some(rect.x()),
        _ => None,
      })
      .collect();

    assert_eq!(xs, vec![10.0, 20.0, 30.0, 40.0]);
  }

  #[test]
  fn test_painter_creation() {
    let painter = Painter::new(100, 100, Rgba::WHITE);
    assert!(painter.is_ok());
  }

  #[test]
  fn test_paint_empty_tree() {
    let tree = make_empty_tree();
    let result = paint_tree(&tree, 100, 100, Rgba::WHITE);
    assert!(result.is_ok());

    let pixmap = result.unwrap();
    assert_eq!(pixmap.width(), 100);
    assert_eq!(pixmap.height(), 100);
  }

  #[test]
  fn test_paint_with_text() {
    let text_fragment = FragmentNode::new_text(
      Rect::from_xywh(10.0, 10.0, 50.0, 16.0),
      "Hello".to_string(),
      12.0,
    );
    let root =
      FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 100.0), vec![text_fragment]);
    let tree = FragmentTree::new(root);

    let result = paint_tree(&tree, 100, 100, Rgba::WHITE);
    assert!(result.is_ok());
  }

  #[test]
  fn test_paint_nested_fragments() {
    let inner = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 30.0, 30.0), vec![]);
    let outer = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 50.0, 50.0), vec![inner]);
    let tree = FragmentTree::new(outer);

    let result = paint_tree(&tree, 100, 100, Rgba::WHITE);
    assert!(result.is_ok());
  }

  #[test]
  fn test_background_white() {
    let tree = make_empty_tree();
    let result = paint_tree(&tree, 10, 10, Rgba::WHITE);
    assert!(result.is_ok());

    let pixmap = result.unwrap();
    let data = pixmap.data();
    // tiny-skia stores premultiplied RGBA bytes
    // WHITE in RGBA is (255, 255, 255, 255)
    assert_eq!(data[0], 255); // R
    assert_eq!(data[1], 255); // G
    assert_eq!(data[2], 255); // B
    assert_eq!(data[3], 255); // A
  }

  #[test]
  fn text_decoration_metrics_available() {
    let painter = Painter::new(10, 10, Rgba::WHITE).expect("painter");
    let style = ComputedStyle::default();
    let metrics = painter.decoration_metrics(None, &style);
    assert!(metrics.is_some());
  }

  #[test]
  fn text_shadow_offsets_are_painted() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 16.0;
    style.text_shadow = vec![TextShadow {
      offset_x: Length::px(4.0),
      offset_y: Length::px(0.0),
      blur_radius: Length::px(0.0),
      color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    }];
    let style = Arc::new(style);

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(10.0, 10.0, 80.0, 30.0),
      "Hi".to_string(),
      16.0,
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 60.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 120, 60, Rgba::WHITE).expect("paint");

    let black_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
        .expect("black text");
    let red_bbox = bounding_box_for_color(&pixmap, |(r, g, b, _)| {
      let (r, g, b) = (r as u16, g as u16, b as u16);
      r > g + 20 && r > b + 20 && !(r < 40 && g < 40 && b < 40) && (g < 250 || b < 250)
    })
    .expect("shadow");

    assert!(
      red_bbox.0 > black_bbox.0 + 2,
      "shadow should be offset to the right of the glyphs"
    );
    assert!(
      red_bbox.1.abs_diff(black_bbox.1) <= 2,
      "shadow should align vertically when no y offset is set"
    );
  }

  #[test]
  fn text_shadow_offset_scales_with_device_pixel_ratio() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 16.0;
    style.text_shadow = vec![TextShadow {
      offset_x: Length::px(2.0),
      offset_y: Length::px(0.0),
      blur_radius: Length::px(0.0),
      color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    }];
    let style = Arc::new(style);

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(10.0, 10.0, 80.0, 30.0),
      "Hi".to_string(),
      16.0,
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 60.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree_scaled(&tree, 120, 60, Rgba::WHITE, 2.0).expect("paint");

    let black_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
        .expect("black text");
    let shadow_pixels = pixmap
      .data()
      .chunks_exact(4)
      .filter(|px| px[3] > 0 && px[0] > px[1] && px[0] > px[2])
      .count();
    assert!(shadow_pixels > 0, "expected shadow pixels to be present");

    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > g && r > b).expect("shadow");

    let dx = red_bbox.0.saturating_sub(black_bbox.0);
    assert!(
      (3..=5).contains(&dx),
      "shadow should shift ~4 device px (2 CSS px at 2x), got {dx}"
    );
    assert!(
      red_bbox.1.abs_diff(black_bbox.1) <= 2,
      "shadow should align vertically when no y offset is set"
    );
  }

  #[test]
  fn text_shadow_blur_scales_with_device_pixel_ratio() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 16.0;
    style.text_shadow = vec![TextShadow {
      offset_x: Length::px(0.0),
      offset_y: Length::px(0.0),
      blur_radius: Length::px(4.0),
      color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    }];
    let style = Arc::new(style);

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(10.0, 10.0, 80.0, 30.0),
      "Hi".to_string(),
      16.0,
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 60.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree_scaled(&tree, 120, 60, Rgba::TRANSPARENT, 2.0).expect("paint");

    let black_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
        .expect("black text");
    let shadow_pixels = pixmap
      .data()
      .chunks_exact(4)
      .filter(|px| px[3] > 0 && px[0] > px[1] && px[0] > px[2])
      .count();
    assert!(shadow_pixels > 0, "expected shadow pixels to be present");

    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > g && r > b).expect("shadow");

    let mut outside = 0;
    let width = pixmap.width();
    for y in 0..pixmap.height() {
      for x in 0..width {
        if x >= black_bbox.0 && x <= black_bbox.2 && y >= black_bbox.1 && y <= black_bbox.3 {
          continue;
        }
        let (r, g, b, a) = color_at(&pixmap, x, y);
        if a > 0 && r > g && r > b {
          outside += 1;
        }
      }
    }
    assert!(
            outside > 0,
            "blur should paint shadow pixels outside the glyph bounds (black bbox {:?}, red bbox {:?}, shadow pixels {})",
            black_bbox,
            red_bbox,
            shadow_pixels
        );
  }

  #[test]
  fn perspective_transform_is_consistent_across_device_scale() {
    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::RED;
    child_style.perspective = Some(Length::px(320.0));
    child_style.transform = vec![
      crate::css::types::Transform::RotateY(30.0),
      crate::css::types::Transform::Translate(Length::px(8.0), Length::px(0.0)),
    ];
    let child_style = Arc::new(child_style);

    let mut root_style = ComputedStyle::default();
    root_style.background_color = Rgba::WHITE;
    let root_style = Arc::new(root_style);

    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(20.0, 12.0, 36.0, 28.0),
      vec![],
      child_style,
    );
    let root = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 80.0, 60.0),
      vec![child],
      root_style,
    );
    let tree = FragmentTree::new(root);

    let baseline = paint_tree_scaled(&tree, 80, 60, Rgba::WHITE, 1.0).expect("paint 1x");
    let hidpi = paint_tree_scaled(&tree, 80, 60, Rgba::WHITE, 2.0).expect("paint 2x");
    let hidpi_down = downsample_half(hidpi);

    let red_predicate = |(r, g, b, a): (u8, u8, u8, u8)| {
      a > 0 && (r as u16) > (g as u16 + 10) && (r as u16) > (b as u16 + 10)
    };
    let bbox_base = bounding_box_for_color(&baseline, red_predicate).expect("baseline bbox");
    let bbox_down = bounding_box_for_color(&hidpi_down, red_predicate).expect("hidpi bbox");

    assert!(
      bbox_base.0.abs_diff(bbox_down.0) <= 1
        && bbox_base.1.abs_diff(bbox_down.1) <= 1
        && bbox_base.2.abs_diff(bbox_down.2) <= 1
        && bbox_base.3.abs_diff(bbox_down.3) <= 1,
      "expected similar projected bounds: {:?} vs {:?}",
      bbox_base,
      bbox_down
    );

    let center_x = (bbox_base.0 + bbox_base.2) / 2;
    let center_y = (bbox_base.1 + bbox_base.3) / 2;
    let center_base = color_at(&baseline, center_x, center_y);
    let center_down = color_at(&hidpi_down, center_x, center_y);
    let max_center_delta = center_base
      .0
      .abs_diff(center_down.0)
      .max(center_base.1.abs_diff(center_down.1))
      .max(center_base.2.abs_diff(center_down.2));
    assert!(
      max_center_delta <= 4,
      "center color should remain consistent after downsampling (base {:?}, hidpi {:?})",
      center_base,
      center_down
    );

    let avg_diff = mean_abs_diff(&baseline, &hidpi_down);
    assert!(
      avg_diff <= 3.0,
      "overall difference should stay small after downsampling, got {avg_diff}"
    );
  }

  #[test]
  fn text_shadow_resolves_percent_and_em_units() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 20.0;
    style.text_shadow = vec![TextShadow {
      offset_x: Length::percent(50.0), // 10px
      offset_y: Length::em(1.0),       // 20px
      blur_radius: Length::px(0.0),
      color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    }];
    let style = Arc::new(style);

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(0.0, 0.0, 80.0, 60.0),
      "Hi".to_string(),
      20.0,
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 80.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 120, 100, Rgba::WHITE).expect("paint");

    let black_bbox = bounding_box_for_color(&pixmap, |(r, g, b, a)| {
      a > 0 && r.abs_diff(g) <= 10 && r.abs_diff(b) <= 10 && r < 96 && g < 96 && b < 96
    })
    .expect("black text");
    let red_bbox = bounding_box_for_color(&pixmap, |(r, g, b, a)| {
      a > 0 && r > 32 && r > g.saturating_add(20) && r > b.saturating_add(20)
    })
    .expect("shadow");

    let dx = red_bbox.0 as i32 - black_bbox.0 as i32;
    let dy = red_bbox.1 as i32 - black_bbox.1 as i32;
    assert!(
      (9..=11).contains(&dx),
      "percent offset_x should resolve to ~10px (got {dx})"
    );
    assert!(
      (19..=21).contains(&dy),
      "1em offset_y should resolve to ~20px (got {dy})"
    );
  }

  #[test]
  fn marker_text_shadow_is_painted() {
    let mut style = ComputedStyle::default();
    style.display = Display::Inline;
    style.color = Rgba::BLACK;
    style.font_size = 16.0;
    style.text_shadow = vec![TextShadow {
      offset_x: Length::px(3.0),
      offset_y: Length::px(0.0),
      blur_radius: Length::px(0.0),
      color: Some(Rgba::from_rgba8(255, 0, 0, 255)),
    }];
    let style = Arc::new(style);

    let marker = FragmentNode::new_with_style(
      Rect::from_xywh(10.0, 10.0, 20.0, 20.0),
      FragmentContent::Text {
        text: "•".to_string(),
        box_id: None,
        baseline_offset: 16.0,
        shaped: None,
        is_marker: true,
      },
      vec![],
      style,
    );

    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 40.0, 30.0), vec![marker]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 60, 40, Rgba::WHITE).expect("paint");

    let glyph_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
        .expect("marker glyph");
    let shadow_bbox = bounding_box_for_color(&pixmap, |(r, g, b, _)| {
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
  fn filter_lengths_resolve_viewport_units() {
    let mut style = ComputedStyle::default();
    style.filter = vec![FilterFunction::Blur(Length::new(10.0, LengthUnit::Vw))];
    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = resolve_filters(
      &style.filter,
      &style,
      (200.0, 100.0),
      &FontContext::new(),
      &mut resolver,
    );
    match filters.first() {
      Some(ResolvedFilter::Blur(radius)) => assert!((radius - 20.0).abs() < 0.001),
      other => panic!("expected blur filter, got {:?}", other),
    }
  }

  #[test]
  fn filter_lengths_ignore_percentages() {
    let mut style = ComputedStyle::default();
    style.filter = vec![FilterFunction::Blur(Length::percent(50.0))];
    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = resolve_filters(
      &style.filter,
      &style,
      (200.0, 100.0),
      &FontContext::new(),
      &mut resolver,
    );
    assert!(filters.is_empty(), "percentage blur should be discarded");
  }

  #[test]
  fn filter_lengths_resolve_ex_units() {
    let mut style = ComputedStyle::default();
    style.font_size = 20.0;
    style.filter = vec![FilterFunction::Blur(Length::new(1.0, LengthUnit::Ex))];
    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = resolve_filters(
      &style.filter,
      &style,
      (200.0, 100.0),
      &FontContext::new(),
      &mut resolver,
    );
    match filters.first() {
      Some(ResolvedFilter::Blur(radius)) => assert!(
        (radius - 10.0).abs() < 2.0,
        "expected ex to resolve near half the font size (got {radius})"
      ),
      other => panic!("expected blur filter, got {:?}", other),
    }
  }

  #[test]
  fn negative_blur_lengths_resolve_to_no_filter() {
    let mut style = ComputedStyle::default();
    style.filter = vec![FilterFunction::Blur(Length::px(-2.0))];
    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = resolve_filters(
      &style.filter,
      &style,
      (200.0, 100.0),
      &FontContext::new(),
      &mut resolver,
    );
    assert!(filters.is_empty(), "negative blur should drop the filter");
  }

  #[test]
  fn drop_shadow_negative_spread_reduces_outset() {
    let filters = vec![ResolvedFilter::DropShadow {
      offset_x: 0.0,
      offset_y: 0.0,
      blur_radius: 4.0,
      spread: -2.0,
      color: Rgba::BLACK,
    }];
    let bbox = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let (l, t, r, b) = compute_filter_outset(&filters, bbox, 1.0);
    let with_zero_spread = vec![ResolvedFilter::DropShadow {
      offset_x: 0.0,
      offset_y: 0.0,
      blur_radius: 4.0,
      spread: 0.0,
      color: Rgba::BLACK,
    }];
    let (l0, t0, r0, b0) = compute_filter_outset(&with_zero_spread, bbox, 1.0);
    assert!(
      (l - 10.0).abs() < 0.01
        && (t - 10.0).abs() < 0.01
        && (r - 10.0).abs() < 0.01
        && (b - 10.0).abs() < 0.01,
      "negative spread should reduce blur outset (got {l},{t},{r},{b})"
    );
    assert!(
      l < l0 && t < t0 && r < r0 && b < b0,
      "reduced spread should shrink outsets"
    );
  }

  #[test]
  fn filter_outset_accumulates_blurs() {
    let filters = vec![ResolvedFilter::Blur(2.0), ResolvedFilter::Blur(3.0)];
    let bbox = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let (l, t, r, b) = compute_filter_outset(&filters, bbox, 1.0);
    assert!(
      (l - 15.0).abs() < 0.01
        && (t - 15.0).abs() < 0.01
        && (r - 15.0).abs() < 0.01
        && (b - 15.0).abs() < 0.01,
      "blur outsets should add up across filter chain"
    );
  }

  #[test]
  fn filter_outset_accumulates_drop_shadow_offsets() {
    let filters = vec![
      ResolvedFilter::Blur(2.0),
      ResolvedFilter::DropShadow {
        offset_x: -4.0,
        offset_y: 3.0,
        blur_radius: 1.0,
        spread: 0.0,
        color: Rgba::BLACK,
      },
    ];
    let bbox = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let (l, t, r, b) = compute_filter_outset(&filters, bbox, 1.0);
    // Blur contributes 6px first; drop shadow adds another 3px blur and shifts left/up by offsets.
    assert!(
      (l - 13.0).abs() < 0.01
        && (t - 6.0).abs() < 0.01
        && (r - 6.0).abs() < 0.01
        && (b - 12.0).abs() < 0.01,
      "expected accumulated outsets to be l=13,t=6,r=6,b=12 but got {l},{t},{r},{b}"
    );
  }

  #[test]
  fn blur_filter_outset_scales_with_device_pixel_ratio() {
    let filters = vec![ResolvedFilter::Blur(4.0)];
    let bbox = Rect::from_xywh(0.0, 0.0, 10.0, 10.0);
    let (l, t, r, b) = compute_filter_outset(&filters, bbox, 1.0);
    // Blur outset is radius * 3 per side.
    assert!(
      (l - 12.0).abs() < 0.01
        && (t - 12.0).abs() < 0.01
        && (r - 12.0).abs() < 0.01
        && (b - 12.0).abs() < 0.01
    );

    let filters = vec![ResolvedFilter::Blur(2.0)];
    let (l, t, r, b) = compute_filter_outset(&filters, bbox, 2.0);
    // Device pixel ratio doubles the blur radius before computing outsets.
    assert!(
      (l - 12.0).abs() < 0.01
        && (t - 12.0).abs() < 0.01
        && (r - 12.0).abs() < 0.01
        && (b - 12.0).abs() < 0.01
    );
  }

  #[test]
  fn drop_shadow_negative_spread_erodes_shadow() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::BLACK;
    style.filter = vec![FilterFunction::DropShadow(Box::new(FilterShadow {
      offset_x: Length::px(6.0),
      offset_y: Length::px(6.0),
      blur_radius: Length::px(0.0),
      spread: Length::px(-2.0),
      color: FilterColor::Color(Rgba::from_rgba8(255, 0, 0, 255)),
    }))];
    let mut root = FragmentNode::new_block(Rect::from_xywh(10.0, 10.0, 20.0, 10.0), Vec::new());
    root.style = Some(Arc::new(style));

    let pixmap = paint_tree(&FragmentTree::new(root), 60, 40, Rgba::WHITE).expect("paint");
    let shadow_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > g && r > b).expect("shadow");
    let width = shadow_bbox.2 - shadow_bbox.0 + 1;
    assert!(
      width < 20,
      "negative spread should shrink shadow width (got width {width})"
    );
  }

  #[test]
  fn unit_interval_filters_clamp_to_one() {
    let mut style = ComputedStyle::default();
    style.filter = vec![
      FilterFunction::Grayscale(2.0),
      FilterFunction::Sepia(1.5),
      FilterFunction::Invert(1.3),
      FilterFunction::Opacity(1.7),
    ];
    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = resolve_filters(
      &style.filter,
      &style,
      (200.0, 100.0),
      &FontContext::new(),
      &mut resolver,
    );
    assert_eq!(filters.len(), 4);
    assert!(filters.iter().all(|f| match f {
      ResolvedFilter::Grayscale(v) | ResolvedFilter::Sepia(v) | ResolvedFilter::Invert(v) =>
        (*v - 1.0).abs() < 0.001,
      ResolvedFilter::Opacity(v) => (*v - 1.0).abs() < 0.001,
      _ => false,
    }));
  }

  #[test]
  fn multiplicative_filters_keep_values_above_one() {
    let mut style = ComputedStyle::default();
    style.filter = vec![
      FilterFunction::Brightness(2.5),
      FilterFunction::Contrast(1.7),
      FilterFunction::Saturate(3.2),
    ];
    let mut resolver = SvgFilterResolver::new(None, Vec::new(), None);
    let filters = resolve_filters(
      &style.filter,
      &style,
      (200.0, 100.0),
      &FontContext::new(),
      &mut resolver,
    );
    assert_eq!(filters.len(), 3);
    assert!(filters
      .iter()
      .any(|f| matches!(f, ResolvedFilter::Brightness(v) if (*v - 2.5).abs() < 0.001)));
    assert!(filters
      .iter()
      .any(|f| matches!(f, ResolvedFilter::Contrast(v) if (*v - 1.7).abs() < 0.001)));
    assert!(filters
      .iter()
      .any(|f| matches!(f, ResolvedFilter::Saturate(v) if (*v - 3.2).abs() < 0.001)));
  }

  #[test]
  fn grayscale_filter_converts_pixel_values() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::BLUE;
    style.filter = vec![FilterFunction::Grayscale(1.0)];

    let mut root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), Vec::new());
    root.style = Some(Arc::new(style));

    let pixmap = paint_tree(&FragmentTree::new(root), 30, 30, Rgba::WHITE).expect("paint");
    let pixel = pixmap.pixel(10, 10).expect("sample");
    // Blue (0,0,1) converted to grayscale yields ~0.0722 in each channel.
    assert!(
      pixel.red().abs_diff(18) <= 1,
      "expected ~18, got {}",
      pixel.red()
    );
    assert!(
      pixel.green().abs_diff(18) <= 1,
      "expected ~18, got {}",
      pixel.green()
    );
    assert!(
      pixel.blue().abs_diff(18) <= 1,
      "expected ~18, got {}",
      pixel.blue()
    );
  }

  #[test]
  fn mix_blend_mode_multiply_combines_colors() {
    // Background: red, child: semi-opaque blue with multiply blend → purple-ish with reduced alpha.
    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::from_rgba8(0, 0, 255, 128);
    child_style.mix_blend_mode = MixBlendMode::Multiply;

    let child = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      FragmentContent::Block { box_id: None },
      vec![],
      child_style.into(),
    );

    let mut root_style = ComputedStyle::default();
    root_style.background_color = Rgba::RED;
    let mut root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![child]);
    root.style = Some(Arc::new(root_style));

    let pixmap = paint_tree(&FragmentTree::new(root), 30, 30, Rgba::WHITE).expect("paint");
    let pixel = pixmap.pixel(10, 10).expect("sample");

    // Multiply red (1,0,0) by blue (0,0,1) → (0,0,0); with 50% alpha over red bg we expect
    // the result to stay dark and keep a nonzero blue component but low green.
    assert!(
      pixel.red() < 200,
      "red should darken under multiply (got {})",
      pixel.red()
    );
    assert!(
      pixel.blue() < 130,
      "blue should darken under multiply (got {})",
      pixel.blue()
    );
    // Green should remain near zero for red*blue.
    assert!(
      pixel.green() < 30,
      "green should stay low (got {})",
      pixel.green()
    );
  }

  #[test]
  fn underline_offset_moves_line() {
    let mut style = ComputedStyle::default();
    style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::BLACK);
    style.font_size = 20.0;

    let painter = Painter::new(10, 10, Rgba::WHITE).expect("painter");
    let runs = painter
      .shaper
      .shape("Hi", &style, &painter.font_ctx)
      .expect("shape");
    let metrics = painter
      .decoration_metrics(Some(&runs), &style)
      .expect("metrics");
    let baseline = 20.0;
    let thickness = match style.text_decoration.thickness {
      crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
      _ => metrics.underline_thickness,
    };
    let base_center = baseline
      - painter.underline_position(&metrics, style.text_underline_position, 0.0, thickness);

    style.text_underline_offset = crate::style::types::TextUnderlineOffset::Length(Length::px(4.0));
    let shifted_center = baseline
      - painter.underline_position(
        &metrics,
        style.text_underline_position,
        painter.resolve_underline_offset(&style),
        thickness,
      );

    assert!(
      shifted_center > base_center,
      "positive offset should move underline further from the baseline"
    );
    assert!(
      (shifted_center - base_center - 4.0).abs() < 0.5,
      "underline offset should roughly follow the authored length"
    );
  }

  #[test]
  fn underline_position_under_moves_line_downward() {
    let mut style = ComputedStyle::default();
    style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::BLACK);
    style.font_size = 20.0;

    let painter = Painter::new(20, 20, Rgba::WHITE).expect("painter");
    let runs = painter
      .shaper
      .shape("Hg", &style, &painter.font_ctx)
      .expect("shape");
    let metrics = painter
      .decoration_metrics(Some(&runs), &style)
      .expect("metrics");
    let thickness = match style.text_decoration.thickness {
      crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
      _ => metrics.underline_thickness,
    };
    let baseline = 20.0;

    let auto_center = baseline
      - painter.underline_position(&metrics, style.text_underline_position, 0.0, thickness);
    style.text_underline_position = crate::style::types::TextUnderlinePosition::Under;
    let under_center = baseline
      - painter.underline_position(&metrics, style.text_underline_position, 0.0, thickness);

    assert!(
      under_center > auto_center,
      "requesting under position should place the underline below the auto position"
    );
    assert!(
      under_center - auto_center > 0.5,
      "under position should move the line a noticeable distance below the baseline"
    );
  }

  #[test]
  fn text_decoration_thickness_uses_font_relative_units() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 20.0;
    style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
    style.text_decoration.thickness =
      crate::style::types::TextDecorationThickness::Length(Length::percent(50.0));
    let style = Arc::new(style);

    let painter = Painter::new(100, 60, Rgba::WHITE).expect("painter");
    let runs = painter
      .shaper
      .shape("Hi", &style, &painter.font_ctx)
      .expect("shape");
    let width: f32 = runs.iter().map(|r| r.advance).sum();
    let baseline = 32.0;
    let rect = Rect::from_xywh(10.0, 10.0, width + 2.0, 40.0);
    let fragment =
      FragmentNode::new_text_shaped(rect, "Hi".to_string(), baseline - rect.y(), runs, style);
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 100.0, 60.0), vec![fragment]);
    let pixmap = paint_tree(&FragmentTree::new(root), 100, 60, Rgba::WHITE).expect("paint");

    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
        .expect("underline");
    let height = red_bbox.3 - red_bbox.1 + 1;
    assert!(
      height >= 9 && height <= 11,
      "expected underline thickness around 10px (50% of 20px font), got {height}"
    );
  }

  #[test]
  fn underline_offset_accepts_ex_units() {
    let mut style = ComputedStyle::default();
    style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::BLACK);
    style.font_size = 20.0;

    let painter = Painter::new(60, 40, Rgba::WHITE).expect("painter");
    let runs = painter
      .shaper
      .shape("Hi", &style, &painter.font_ctx)
      .expect("shape");
    let metrics = painter
      .decoration_metrics(Some(&runs), &style)
      .expect("metrics");
    let thickness = match style.text_decoration.thickness {
      crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
      _ => metrics.underline_thickness,
    };
    let baseline = 24.0;

    let base_center = baseline
      - painter.underline_position(&metrics, style.text_underline_position, 0.0, thickness);

    let mut ex_style = style;
    ex_style.text_underline_offset =
      crate::style::types::TextUnderlineOffset::Length(Length::ex(1.0));
    let offset = painter.resolve_underline_offset_value(ex_style.text_underline_offset, &ex_style);
    let ex_center = baseline
      - painter.underline_position(
        &metrics,
        ex_style.text_underline_position,
        offset,
        thickness,
      );

    assert!(
      ex_center > base_center,
      "ex-based underline offset should push the line farther from the baseline"
    );
  }

  #[test]
  fn underline_thickness_scales_with_device_pixel_ratio() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 20.0;
    style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
    style.text_decoration.thickness =
      crate::style::types::TextDecorationThickness::Length(Length::px(4.0));
    style.text_decoration_skip_ink = crate::style::types::TextDecorationSkipInk::None;
    let style = Arc::new(style);

    let fragment = FragmentNode::new_text_styled(
      Rect::from_xywh(10.0, 10.0, 80.0, 30.0),
      "Hi".to_string(),
      22.0,
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 60.0), vec![fragment]);
    let pixmap =
      paint_tree_scaled(&FragmentTree::new(root), 120, 60, Rgba::WHITE, 2.0).expect("paint");

    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
        .expect("underline");
    let height = red_bbox.3 - red_bbox.1 + 1;
    assert!(
      (7..=9).contains(&height),
      "expected underline thickness around 8 device px (4 CSS px at 2x), got {height}"
    );
  }

  #[test]
  fn skip_ink_all_forces_exclusions_even_without_overlap() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 24.0;
    style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
    style.text_decoration_skip_ink = crate::style::types::TextDecorationSkipInk::All;
    style.text_decoration.thickness =
      crate::style::types::TextDecorationThickness::Length(Length::px(6.0));
    style.text_underline_offset = crate::style::types::TextUnderlineOffset::Length(Length::px(0.0));

    let painter = Painter::new(240, 140, Rgba::WHITE).expect("painter");
    let runs = painter
      .shaper
      .shape("HI HI", &style, &painter.font_ctx)
      .expect("shape");
    let metrics = painter
      .decoration_metrics(Some(&runs), &style)
      .expect("metrics");
    let width: f32 = runs.iter().map(|r| r.advance).sum();
    let baseline = 50.0;
    // Place the underline well below the glyph ink so auto skip-ink would keep it continuous.
    let center = baseline + 30.0;
    let thickness = match style.text_decoration.thickness {
      crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
      _ => metrics.underline_thickness,
    };
    let line_start = 10.0;
    let segments_all = painter.build_underline_segments(
      &runs,
      line_start,
      width,
      center,
      thickness,
      baseline,
      false,
      crate::style::types::TextDecorationSkipInk::All,
    );

    let segments_auto = painter.build_underline_segments(
      &runs,
      line_start,
      width,
      center,
      thickness,
      baseline,
      false,
      crate::style::types::TextDecorationSkipInk::Auto,
    );

    assert!(
      !segments_all.is_empty(),
      "skip-ink all should still paint around glyphs rather than dropping the line entirely"
    );
    assert_eq!(
      segments_auto.len(),
      1,
      "auto skip-ink should keep a continuous line when nothing overlaps the band"
    );
    let full_span = (line_start, line_start + width);
    let auto_span = segments_auto[0];
    assert!(
      (auto_span.0 - full_span.0).abs() < 0.01 && (auto_span.1 - full_span.1).abs() < 0.01,
      "auto span should cover the entire underline"
    );
    let carved_length: f32 = segments_all.iter().map(|(s, e)| e - s).sum();
    assert!(
      carved_length < width - 0.5,
      "skip-ink: all should carve out glyph intervals even when the underline is far from ink"
    );
  }

  #[test]
  fn underline_skip_ink_carves_descenders() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 28.0;
    style.text_decoration.lines = crate::style::types::TextDecorationLine::UNDERLINE;
    style.text_decoration.color = Some(Rgba::from_rgba8(255, 0, 0, 255));
    style.text_decoration.thickness =
      crate::style::types::TextDecorationThickness::Length(Length::px(3.0));
    style.text_underline_offset =
      crate::style::types::TextUnderlineOffset::Length(Length::px(-1.0));

    let painter = Painter::new(160, 100, Rgba::WHITE).expect("painter");
    let runs = painter
      .shaper
      .shape("gy", &style, &painter.font_ctx)
      .expect("shape");
    assert!(
      !runs.is_empty(),
      "shaping should yield glyphs for skip-ink evaluation"
    );
    let metrics = painter
      .decoration_metrics(Some(&runs), &style)
      .expect("metrics");
    let line_start = 10.0;
    let line_width: f32 = runs.iter().map(|r| r.advance).sum();
    let baseline = 50.0;
    let thickness = match style.text_decoration.thickness {
      crate::style::types::TextDecorationThickness::Length(l) => l.to_px(),
      _ => metrics.underline_thickness,
    };
    let center = baseline
      - painter.underline_position(
        &metrics,
        style.text_underline_position,
        painter.resolve_underline_offset(&style),
        thickness,
      );
    let exclusions = collect_underline_exclusions(
      &runs,
      line_start,
      baseline,
      center - thickness * 0.5,
      center + thickness * 0.5,
      false,
      painter.scale,
    );
    let target = exclusions
      .iter()
      .max_by(|a, b| {
        (a.1 - a.0)
          .partial_cmp(&(b.1 - b.0))
          .unwrap_or(std::cmp::Ordering::Equal)
      })
      .copied()
      .expect("descenders should intersect the underline band when skip-ink is evaluated");
    let sample_x_f = (target.0 + target.1) * 0.5;
    let sample_x = sample_x_f.round().clamp(0.0, 159.0) as u32;
    let segments = painter.build_underline_segments(
      &runs,
      line_start,
      line_width,
      center,
      thickness,
      baseline,
      false,
      crate::style::types::TextDecorationSkipInk::Auto,
    );
    assert!(
      !segments
        .iter()
        .any(|(s, e)| sample_x_f >= *s && sample_x_f <= *e),
      "segments should omit the exclusion area chosen for sampling"
    );
    let sample_y = center.round().clamp(0.0, 99.0) as u32;

    let baseline_offset = baseline - 10.0;
    let rect = Rect::from_xywh(line_start, 10.0, line_width, 60.0);
    let fragment_auto = FragmentNode::new_text_shaped(
      rect,
      "gy".to_string(),
      baseline_offset,
      runs.clone(),
      Arc::new(style.clone()),
    );
    let root_auto =
      FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 160.0, 100.0), vec![fragment_auto]);
    let pix_auto =
      paint_tree(&FragmentTree::new(root_auto), 160, 100, Rgba::WHITE).expect("auto paint");

    let mut no_skip_style = style;
    no_skip_style.text_decoration_skip_ink = crate::style::types::TextDecorationSkipInk::None;
    let fragment_none = FragmentNode::new_text_shaped(
      rect,
      "gy".to_string(),
      baseline_offset,
      runs,
      Arc::new(no_skip_style),
    );
    let root_none =
      FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 160.0, 100.0), vec![fragment_none]);
    let pix_none =
      paint_tree(&FragmentTree::new(root_none), 160, 100, Rgba::WHITE).expect("no-skip paint");

    let auto_px = color_at(&pix_auto, sample_x, sample_y);
    let no_skip_px = color_at(&pix_none, sample_x, sample_y);

    assert!(
            no_skip_px.0 > 200 && no_skip_px.1 < 80 && no_skip_px.2 < 80,
            "a continuous underline should paint the decoration color through descenders when skip-ink is none"
        );
    assert!(
            !(auto_px.0 > 200 && auto_px.1 < 80 && auto_px.2 < 80),
            "skip-ink should prevent the underline color from painting through descenders (auto={:?}, no-skip={:?})",
            auto_px,
            no_skip_px
        );
  }

  #[test]
  fn paints_text_emphasis_marks_above_text() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;
    style.font_size = 24.0;
    style.text_emphasis_style = crate::style::types::TextEmphasisStyle::Mark {
      fill: crate::style::types::TextEmphasisFill::Filled,
      shape: crate::style::types::TextEmphasisShape::Circle,
    };
    style.text_emphasis_color = Some(Rgba::from_rgba8(255, 0, 0, 255));
    let style = Arc::new(style);

    let painter = Painter::new(80, 60, Rgba::WHITE).expect("painter");
    let runs = painter
      .shaper
      .shape("A", &style, &painter.font_ctx)
      .expect("shape");
    let width: f32 = runs.iter().map(|r| r.advance).sum();
    let baseline = 32.0;
    let rect = Rect::from_xywh(10.0, 8.0, width + 2.0, 40.0);
    let fragment =
      FragmentNode::new_text_shaped(rect, "A".to_string(), baseline - rect.y(), runs, style);
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 80.0, 60.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 80, 60, Rgba::WHITE).expect("paint");

    let black_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r < 32 && g < 32 && b < 32)
        .expect("text");
    let red_bbox =
      bounding_box_for_color(&pixmap, |(r, g, b, a)| a > 0 && r > 200 && g < 80 && b < 80)
        .expect("emphasis mark");

    assert!(
      red_bbox.1 < black_bbox.1,
      "emphasis mark should appear above the glyphs when positioned over the text"
    );
  }

  #[test]
  fn replaced_content_respects_padding_box() {
    let mut style = ComputedStyle::default();
    style.padding_left = Length::px(4.0);
    style.padding_right = Length::px(4.0);
    style.padding_top = Length::px(4.0);
    style.padding_bottom = Length::px(4.0);
    style.background_color = Rgba::BLUE;

    let mut painter =
      Painter::with_resources(40, 40, Rgba::WHITE, FontContext::new(), ImageCache::new())
        .expect("painter");
    painter.fill_background();

    let box_x = 10.0;
    let box_y = 10.0;
    let box_size = 20.0;
    let rects = background_rects(box_x, box_y, box_size, box_size, &style, None);
    assert!((rects.content.x() - 14.0).abs() < 0.01);
    assert!((rects.content.y() - 14.0).abs() < 0.01);
    assert!((rects.content.width() - 12.0).abs() < 0.01);
    assert!((rects.content.height() - 12.0).abs() < 0.01);
    painter.paint_background(box_x, box_y, box_size, box_size, &style);
    painter.paint_replaced(
      &ReplacedType::Svg {
        content: SvgContent::raw(red_svg()),
      },
      Some(&style),
      box_x,
      box_y,
      box_size,
      box_size,
    );

    let pixmap = painter.pixmap;
    assert_eq!(color_at(&pixmap, 11, 11), (0, 0, 255, 255));
    assert_eq!(color_at(&pixmap, 15, 15), (255, 0, 0, 255));
    assert_eq!(color_at(&pixmap, 20, 20), (255, 0, 0, 255));
    assert_eq!(color_at(&pixmap, 27, 27), (0, 0, 255, 255));
  }

  #[test]
  fn paints_alt_text_when_image_missing() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLACK;

    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 50.0, 20.0),
      FragmentContent::Replaced {
        replaced_type: ReplacedType::Image {
          src: String::new(),
          alt: Some("alt".to_string()),
          sizes: None,
          srcset: Vec::new(),
          picture_sources: Vec::new(),
        },
        box_id: None,
      },
      vec![],
      Arc::new(style),
    );

    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 60.0, 30.0), vec![fragment]);
    let tree = FragmentTree::new(root);
    let pixmap = paint_tree(&tree, 60, 30, Rgba::WHITE).expect("paint alt");

    let center = color_at(&pixmap, 25, 10);
    assert_ne!(
      center,
      (200, 200, 200, 255),
      "alt text should prevent placeholder rectangles"
    );

    let mut has_ink = false;
    for y in 0..pixmap.height() {
      for x in 0..pixmap.width() {
        if color_at(&pixmap, x, y) != (255, 255, 255, 255) {
          has_ink = true;
          break;
        }
      }
      if has_ink {
        break;
      }
    }
    assert!(has_ink, "alt text should paint glyphs");
  }

  #[test]
  fn paints_embed_svg_content() {
    let svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"2\" height=\"2\"><rect width=\"2\" height=\"2\" fill=\"red\"/></svg>";
    let style = Arc::new(ComputedStyle::default());
    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      FragmentContent::Replaced {
        replaced_type: ReplacedType::Embed {
          src: svg.to_string(),
        },
        box_id: None,
      },
      vec![],
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint embed");
    assert_eq!(
      color_at(&pixmap, 10, 10),
      (255, 0, 0, 255),
      "embed should render svg content"
    );
  }

  #[test]
  fn paints_video_poster_image_content() {
    let poster =
            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"8\" height=\"8\"><rect width=\"8\" height=\"8\" fill=\"lime\"/></svg>";
    let style = Arc::new(ComputedStyle::default());
    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      FragmentContent::Replaced {
        replaced_type: ReplacedType::Video {
          src: String::new(),
          poster: Some(poster.to_string()),
        },
        box_id: None,
      },
      vec![],
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 12.0, 12.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 12, 12, Rgba::WHITE).expect("paint video poster");
    assert_eq!(
      color_at(&pixmap, 5, 5),
      (0, 255, 0, 255),
      "poster content should paint instead of placeholder"
    );
  }

  #[test]
  fn paints_audio_placeholder() {
    let style = Arc::new(ComputedStyle::default());
    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 12.0, 8.0),
      FragmentContent::Replaced {
        replaced_type: ReplacedType::Audio { src: String::new() },
        box_id: None,
      },
      vec![],
      style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 14.0, 10.0), vec![fragment]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 14, 10, Rgba::WHITE).expect("paint audio placeholder");
    let center = color_at(&pixmap, 6, 4);
    assert_eq!(
      center,
      (200, 200, 200, 255),
      "audio should render placeholder fill when no media is available"
    );
  }

  fn svg_data_url(color: &str) -> String {
    format!(
            "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' width='1' height='1'><rect width='1' height='1' fill='{color}'/></svg>"
        )
  }

  #[test]
  fn srcset_chooses_best_density_for_device_scale() {
    let red = svg_data_url("red");
    let blue = svg_data_url("blue");

    let replaced = ReplacedType::Image {
      src: red.clone(),
      alt: None,
      sizes: None,
      srcset: vec![
        SrcsetCandidate {
          url: red.clone(),
          descriptor: SrcsetDescriptor::Density(1.0),
        },
        SrcsetCandidate {
          url: blue.clone(),
          descriptor: SrcsetDescriptor::Density(2.0),
        },
      ],
      picture_sources: Vec::new(),
    };

    let style = ComputedStyle::default();
    let mut painter = Painter::with_resources_scaled(
      2,
      2,
      Rgba::WHITE,
      FontContext::new(),
      ImageCache::new(),
      2.0,
    )
    .expect("painter");
    painter.paint_replaced(&replaced, Some(&style), 0.0, 0.0, 1.0, 1.0);

    let px = painter.pixmap.pixel(0, 0).unwrap();
    assert_eq!(
      (px.red(), px.green(), px.blue()),
      (0, 0, 255),
      "2x density should pick the blue candidate at scale 2.0"
    );
  }

  #[test]
  fn srcset_width_descriptor_uses_slot_width() {
    let red = svg_data_url("red");
    let blue = svg_data_url("blue");

    let replaced = ReplacedType::Image {
      src: red.clone(),
      alt: None,
      sizes: None,
      srcset: vec![
        SrcsetCandidate {
          url: red.clone(),
          descriptor: SrcsetDescriptor::Width(100),
        },
        SrcsetCandidate {
          url: blue.clone(),
          descriptor: SrcsetDescriptor::Width(300),
        },
      ],
      picture_sources: Vec::new(),
    };

    let style = ComputedStyle::default();
    let mut painter = Painter::with_resources_scaled(
      120,
      20,
      Rgba::WHITE,
      FontContext::new(),
      ImageCache::new(),
      2.0,
    )
    .expect("painter");

    painter.paint_replaced(&replaced, Some(&style), 0.0, 0.0, 100.0, 10.0);

    let px = painter.pixmap.pixel(100, 10).unwrap();
    assert_eq!(
      (px.red(), px.green(), px.blue()),
      (0, 0, 255),
      "with 100px slot at DPR=2, the 300w candidate (density 3) should be chosen"
    );
  }

  #[test]
  fn paints_linear_gradient_background() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 90.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(1.0),
          },
        ],
      }),
      ..BackgroundLayer::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
    let left = color_at(&pixmap, 2, 10);
    let right = color_at(&pixmap, 18, 10);
    assert!(left.0 > right.0, "left should be redder than right");
    assert!(right.2 > left.2, "right should be bluer than left");
  }

  #[test]
  fn normalizes_missing_gradient_stops() {
    let stops = vec![
      ColorStop {
        color: Color::Rgba(Rgba::RED),
        position: None,
      },
      ColorStop {
        color: Color::Rgba(Rgba::GREEN),
        position: Some(0.5),
      },
      ColorStop {
        color: Color::Rgba(Rgba::BLUE),
        position: None,
      },
    ];

    let resolved = super::normalize_color_stops(&stops, Rgba::WHITE);

    assert_eq!(resolved.len(), 3);
    assert!((resolved[0].0 - 0.0).abs() < 1e-6);
    assert!((resolved[1].0 - 0.5).abs() < 1e-6);
    assert!((resolved[2].0 - 1.0).abs() < 1e-6);
  }

  #[test]
  fn normalize_color_stops_resolves_current_color() {
    let stops = vec![
      crate::css::types::ColorStop {
        color: Color::CurrentColor,
        position: Some(0.0),
      },
      crate::css::types::ColorStop {
        color: Color::Rgba(Rgba::BLUE),
        position: Some(1.0),
      },
    ];
    let resolved = normalize_color_stops(&stops, Rgba::new(10, 20, 30, 1.0));
    assert_eq!(resolved.len(), 2);
    assert_eq!(resolved[0].1, Rgba::new(10, 20, 30, 1.0));
    assert_eq!(resolved[1].1, Rgba::BLUE);
  }

  #[test]
  fn paints_repeating_linear_gradient_background() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::RepeatingLinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(0.5),
          },
        ],
      }),
      ..BackgroundLayer::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");

    let top = color_at(&pixmap, 10, 2);
    let middle = color_at(&pixmap, 10, 10);
    let bottom = color_at(&pixmap, 10, 18);

    // Repeating stripes: samples at different rows should not all match; require at least two distinct colors.
    assert!(top != middle || middle != bottom);
    let mut distinct = std::collections::HashSet::new();
    distinct.insert(top);
    distinct.insert(middle);
    distinct.insert(bottom);
    assert!(
      distinct.len() >= 2,
      "expected at least two colors in repeating gradient"
    );
  }

  #[test]
  fn radial_gradient_uses_farthest_corner_ellipse() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::RadialGradient {
        shape: RadialGradientShape::Ellipse,
        size: RadialGradientSize::FarthestCorner,
        position: BackgroundPosition::Position {
          x: crate::style::types::BackgroundPositionComponent {
            alignment: 0.5,
            offset: Length::px(0.0),
          },
          y: crate::style::types::BackgroundPositionComponent {
            alignment: 0.5,
            offset: Length::px(0.0),
          },
        },
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(1.0),
          },
        ],
      }),
      ..BackgroundLayer::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 20, 10, Rgba::WHITE).expect("paint");

    let top_center = color_at(&pixmap, 10, 0);
    let right_center = color_at(&pixmap, 19, 5);
    let diff_r = (top_center.0 as i32 - right_center.0 as i32).abs();
    let diff_b = (top_center.2 as i32 - right_center.2 as i32).abs();
    assert!(
      diff_r < 32 && diff_b < 32,
      "elliptical gradient should make horizontal/vertical edges equally distant (diffs r={} b={})",
      diff_r,
      diff_b
    );

    let corner = color_at(&pixmap, 19, 9);
    assert!(
      corner.2 > corner.0,
      "farthest-corner sizing should leave the corner closest to the final stop"
    );
  }

  #[test]
  fn radial_gradient_honors_position() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::RadialGradient {
        shape: RadialGradientShape::Ellipse,
        size: RadialGradientSize::FarthestCorner,
        position: BackgroundPosition::Position {
          x: crate::style::types::BackgroundPositionComponent {
            alignment: 0.0,
            offset: Length::px(0.0),
          },
          y: crate::style::types::BackgroundPositionComponent {
            alignment: 0.0,
            offset: Length::px(0.0),
          },
        },
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(1.0),
          },
        ],
      }),
      ..BackgroundLayer::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 20, 10, Rgba::WHITE).expect("paint");

    let top_left = color_at(&pixmap, 0, 0);
    assert!(
      top_left.0 > top_left.2,
      "top-left should start at the first stop (more red than blue)"
    );
    let corner = color_at(&pixmap, 19, 9);
    assert!(
      corner.2 > corner.0,
      "gradient centered at top-left should reach final stop toward far corner"
    );
  }

  #[test]
  fn conic_gradient_respects_angles() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::ConicGradient {
        from_angle: 0.0,
        position: BackgroundPosition::Position {
          x: crate::style::types::BackgroundPositionComponent {
            alignment: 0.5,
            offset: Length::px(0.0),
          },
          y: crate::style::types::BackgroundPositionComponent {
            alignment: 0.5,
            offset: Length::px(0.0),
          },
        },
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(0.5),
          },
        ],
      }),
      ..BackgroundLayer::default()
    }]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");

    let top = color_at(&pixmap, 10, 0);
    let bottom = color_at(&pixmap, 10, 19);
    assert!(top.0 > top.2, "top should be dominated by first stop (red)");
    assert!(
      bottom.2 > bottom.0,
      "bottom should reflect halfway stop (blue)"
    );
  }

  #[test]
  fn background_attachment_fixed_anchors_to_viewport() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 90.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(1.0),
          },
        ],
      }),
      attachment: BackgroundAttachment::Fixed,
      ..BackgroundLayer::default()
    }]);

    // Gradient anchors to viewport: samples at successive x positions diverge even though elements have their own origins.
    let style_arc = Arc::new(style);
    let first = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
      vec![],
      style_arc.clone(),
    );
    let second = FragmentNode::new_block_styled(
      Rect::from_xywh(1.0, 0.0, 1.0, 1.0),
      vec![],
      style_arc.clone(),
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 4.0, 2.0), vec![first, second]);
    let tree = FragmentTree::new(root);
    let pixmap = paint_tree(&tree, 4, 2, Rgba::WHITE).expect("paint");

    let left = color_at(&pixmap, 0, 0);
    let right = color_at(&pixmap, 1, 0);
    assert!(
      left.0 > right.0,
      "fixed attachment should keep gradient anchored to viewport"
    );
    assert!(right.2 > left.2);
  }

  #[test]
  fn background_attachment_local_uses_scrollable_overflow_area() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::RED),
            position: Some(1.0),
          },
        ],
      }),
      attachment: BackgroundAttachment::Local,
      ..BackgroundLayer::default()
    }]);
    style.overflow_x = Overflow::Scroll;
    style.overflow_y = Overflow::Scroll;
    style.border_top_width = Length::px(2.0);
    style.border_right_width = Length::px(2.0);
    style.border_bottom_width = Length::px(2.0);
    style.border_left_width = Length::px(2.0);
    style.border_top_style = CssBorderStyle::Solid;
    style.border_right_style = CssBorderStyle::Solid;
    style.border_bottom_style = CssBorderStyle::Solid;
    style.border_left_style = CssBorderStyle::Solid;
    style.border_top_color = Rgba::TRANSPARENT;
    style.border_right_color = Rgba::TRANSPARENT;
    style.border_bottom_color = Rgba::TRANSPARENT;
    style.border_left_color = Rgba::TRANSPARENT;

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 12.0, 12.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 12, 12, Rgba::WHITE).expect("paint");

    // Border-box samples should stay transparent because local attachment anchors to the scrollable overflow area
    // (padding box) and border-box clipping collapses to padding-box per CSS Backgrounds 3.
    assert_eq!(color_at(&pixmap, 1, 1), (255, 255, 255, 255));
    // Padding box should still paint the background image.
    assert_eq!(color_at(&pixmap, 6, 6), (255, 0, 0, 255));
  }

  #[test]
  fn background_color_uses_first_layer_clip_value() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.background_images = vec![None, None];
    style.background_clips = vec![BackgroundBox::PaddingBox, BackgroundBox::BorderBox];
    style.rebuild_background_layers();
    style.border_top_width = Length::px(2.0);
    style.border_right_width = Length::px(2.0);
    style.border_bottom_width = Length::px(2.0);
    style.border_left_width = Length::px(2.0);
    style.border_top_style = CssBorderStyle::Solid;
    style.border_right_style = CssBorderStyle::Solid;
    style.border_bottom_style = CssBorderStyle::Solid;
    style.border_left_style = CssBorderStyle::Solid;
    style.border_top_color = Rgba::WHITE;
    style.border_right_color = Rgba::WHITE;
    style.border_bottom_color = Rgba::WHITE;
    style.border_left_color = Rgba::WHITE;

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 12.0, 12.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 12, 12, Rgba::WHITE).expect("paint");

    // Color should be clipped to the first layer's clip (padding-box), leaving border white.
    assert_eq!(color_at(&pixmap, 1, 1), (255, 255, 255, 255));
    assert_eq!(color_at(&pixmap, 3, 3), (255, 0, 0, 255));
  }

  #[test]
  fn overflow_hidden_clips_children() {
    let mut parent_style = ComputedStyle::default();
    parent_style.overflow_x = Overflow::Hidden;
    parent_style.overflow_y = Overflow::Visible;
    parent_style.position = Position::Relative;
    parent_style.background_color = Rgba::BLUE;
    let parent_style = Arc::new(parent_style);

    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::RED;
    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(-5.0, -5.0, 30.0, 40.0),
      vec![],
      Arc::new(child_style),
    );
    let parent = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![child],
      parent_style,
    );
    let tree = FragmentTree::new(parent);

    let pixmap = paint_tree(&tree, 40, 40, Rgba::WHITE).expect("paint");
    assert_eq!(color_at(&pixmap, 10, 10), (255, 0, 0, 255));
    // Horizontal overflow should clip; vertical overflow should remain visible.
    assert_eq!(color_at(&pixmap, 22, 2), (255, 255, 255, 255));
    assert_eq!(color_at(&pixmap, 10, 25), (255, 0, 0, 255));
  }

  #[test]
  fn overflow_y_hidden_clips_vertical_only() {
    let mut parent_style = ComputedStyle::default();
    parent_style.overflow_x = Overflow::Visible;
    parent_style.overflow_y = Overflow::Hidden;
    parent_style.position = Position::Relative;
    parent_style.background_color = Rgba::BLUE;
    let parent_style = Arc::new(parent_style);

    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::RED;
    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(-5.0, -5.0, 30.0, 40.0),
      vec![],
      Arc::new(child_style),
    );
    let parent = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![child],
      parent_style,
    );
    let tree = FragmentTree::new(parent);

    let pixmap = paint_tree(&tree, 40, 40, Rgba::WHITE).expect("paint");
    // Vertical overflow clipped; horizontal overflow visible.
    assert_eq!(color_at(&pixmap, 10, 10), (255, 0, 0, 255));
    assert_eq!(color_at(&pixmap, 10, 25), (255, 255, 255, 255));
    assert_eq!(color_at(&pixmap, 22, 10), (255, 0, 0, 255));
  }

  #[test]
  fn clip_rect_clips_contents() {
    let mut style = ComputedStyle::default();
    style.position = Position::Absolute;
    style.background_color = Rgba::RED;
    style.clip = Some(crate::style::types::ClipRect {
      top: ClipComponent::Length(Length::px(5.0)),
      right: ClipComponent::Length(Length::px(15.0)),
      bottom: ClipComponent::Length(Length::px(15.0)),
      left: ClipComponent::Length(Length::px(5.0)),
    });
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");

    // Outside the clip rect should stay white; inside should paint red.
    assert_eq!(color_at(&pixmap, 2, 2), (255, 255, 255, 255));
    assert_eq!(color_at(&pixmap, 10, 10), (255, 0, 0, 255));
  }

  #[test]
  fn overflow_clip_limits_layer_bounds() {
    let style = Arc::new(ComputedStyle::default());
    let root_rect = Rect::from_xywh(0.0, 0.0, 20.0, 20.0);
    let commands = vec![
      DisplayCommand::Background {
        rect: root_rect,
        style: style.clone(),
      },
      DisplayCommand::Background {
        rect: Rect::from_xywh(1000.0, 0.0, 10.0, 10.0),
        style,
      },
    ];
    let clip = Some(StackingClip {
      rect: root_rect,
      radii: BorderRadii::ZERO,
      clip_x: true,
      clip_y: true,
      clip_root: false,
    });
    let bounds = stacking_context_bounds(&commands, &[], &[], root_rect, None, clip.as_ref(), None)
      .expect("bounds");
    assert!((bounds.width() - 1010.0).abs() < 0.01);
    assert!(bounds.min_x().abs() < 0.01);
    assert!((bounds.max_x() - 1010.0).abs() < 0.01);
  }

  #[test]
  fn transformed_child_extends_parent_bounds() {
    let mut parent_style = ComputedStyle::default();
    parent_style.isolation = Isolation::Isolate;
    let parent_style = Arc::new(parent_style);

    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::RED;
    child_style.transform = vec![crate::css::types::Transform::Translate(
      Length::px(60.0),
      Length::px(0.0),
    )];
    let child_style = Arc::new(child_style);

    let child =
      FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 20.0, 20.0), vec![], child_style);
    let parent = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 1.0, 1.0),
      vec![child],
      parent_style,
    );
    let root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 120.0, 40.0), vec![parent]);
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 120, 40, Rgba::WHITE).expect("paint");
    // The translated child should remain visible even though the parent stacking context bounds
    // are derived from its untransformed content.
    assert_eq!(color_at(&pixmap, 70, 10), (255, 0, 0, 255));
    assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
  }

  #[test]
  fn object_fit_contain_centers_image() {
    let fit = ObjectFit::Contain;
    let position = crate::style::types::ObjectPosition {
      x: crate::style::types::PositionComponent::Keyword(
        crate::style::types::PositionKeyword::Center,
      ),
      y: crate::style::types::PositionComponent::Keyword(
        crate::style::types::PositionKeyword::Center,
      ),
    };

    let (offset_x, offset_y, dest_w, dest_h) = compute_object_fit(
      fit,
      position,
      200.0,
      100.0,
      100.0,
      100.0,
      16.0,
      Some((200.0, 100.0)),
    )
    .expect("fit computed");
    assert_eq!(dest_h, 100.0);
    assert_eq!(dest_w, 100.0);
    assert!((offset_x - 50.0).abs() < 0.01);
    assert!((offset_y - 0.0).abs() < 0.01);
  }

  #[test]
  fn background_cover_scales_to_fill() {
    let mut layer = BackgroundLayer::default();
    layer.size = BackgroundSize::Keyword(BackgroundSizeKeyword::Cover);
    let (tw, th) =
      compute_background_size(&layer, 16.0, 16.0, (200.0, 100.0), 200.0, 100.0, 50.0, 50.0);
    let (ox, oy) = resolve_background_offset(
      layer.position,
      200.0,
      100.0,
      tw,
      th,
      16.0,
      16.0,
      (200.0, 100.0),
    );
    assert!((tw - 200.0).abs() < 0.01);
    assert!((th - 200.0).abs() < 0.01);
    assert!((ox - 0.0).abs() < 0.01);
    assert!((oy - 0.0).abs() < 0.01);
  }

  #[test]
  fn background_position_alignment_and_offsets_resolve_against_available_space() {
    let layer = BackgroundLayer {
      position: BackgroundPosition::Position {
        x: crate::style::types::BackgroundPositionComponent {
          alignment: 1.0,
          offset: Length::px(-10.0),
        },
        y: crate::style::types::BackgroundPositionComponent {
          alignment: 1.0,
          offset: Length::percent(-20.0),
        },
      },
      ..BackgroundLayer::default()
    };

    let (ox, oy) = resolve_background_offset(
      layer.position,
      100.0,
      60.0,
      20.0,
      10.0,
      16.0,
      16.0,
      (100.0, 60.0),
    );
    // available_x = 80; 1 * 80 - 10 = 70
    // available_y = 50; 1 * 50 - 20%*50 = 40
    assert!((ox - 70.0).abs() < 0.01);
    assert!((oy - 40.0).abs() < 0.01);
  }

  #[test]
  fn background_size_single_dimension_auto_uses_intrinsic_ratio() {
    let mut layer = BackgroundLayer::default();
    layer.size = BackgroundSize::Explicit(
      BackgroundSizeComponent::Auto,
      BackgroundSizeComponent::Length(Length::px(25.0)),
    );
    let (tw, th) = compute_background_size(
      &layer,
      16.0,
      16.0,
      (200.0, 100.0),
      200.0,
      100.0,
      100.0,
      50.0,
    );
    assert!((tw - 50.0).abs() < 0.01);
    assert!((th - 25.0).abs() < 0.01);
  }

  #[test]
  fn background_size_auto_auto_uses_intrinsic_size_or_falls_back() {
    let layer = BackgroundLayer::default();
    let (tw, th) =
      compute_background_size(&layer, 16.0, 16.0, (120.0, 80.0), 120.0, 80.0, 30.0, 10.0);
    assert!((tw - 30.0).abs() < 0.01);
    assert!((th - 10.0).abs() < 0.01);

    let (tw, th) = compute_background_size(&layer, 16.0, 16.0, (50.0, 60.0), 50.0, 60.0, 0.0, 0.0);
    assert!((tw - 50.0).abs() < 0.01);
    assert!((th - 60.0).abs() < 0.01);
  }

  #[test]
  fn background_repeat_space_distributes_evenly() {
    let positions = tile_positions(
      BackgroundRepeatKeyword::Space,
      0.0,
      100.0,
      30.0,
      0.0,
      0.0,
      100.0,
    );
    assert_eq!(positions.len(), 3);
    assert!((positions[0] - 0.0).abs() < 1e-4);
    assert!((positions[1] - 35.0).abs() < 1e-3);
    assert!((positions[2] - 70.0).abs() < 1e-3);
  }

  #[test]
  fn background_repeat_space_centers_single_tile() {
    let positions = tile_positions(
      BackgroundRepeatKeyword::Space,
      10.0,
      40.0,
      30.0,
      0.0,
      0.0,
      100.0,
    );
    assert_eq!(positions, vec![15.0]); // 10 + (40-30)/2

    let with_offset = tile_positions(
      BackgroundRepeatKeyword::Space,
      0.0,
      40.0,
      30.0,
      5.0,
      0.0,
      100.0,
    );
    assert_eq!(with_offset, vec![10.0]); // centered (5) plus offset (5)
  }

  #[test]
  fn background_repeat_space_centers_oversized_tile() {
    // Tile larger than area => still center it.
    let positions = tile_positions(
      BackgroundRepeatKeyword::Space,
      0.0,
      20.0,
      30.0,
      0.0,
      0.0,
      100.0,
    );
    assert_eq!(positions, vec![-5.0]); // (20-30)/2
  }

  #[test]
  fn background_repeat_round_resizes_to_integer_tiles() {
    let rounded = round_tile_length(1099.0, 100.0);
    assert!((rounded - (1099.0 / 11.0)).abs() < 1e-3);
  }

  #[test]
  fn background_blend_mode_multiplies_layers() {
    let make_style = |blend_mode| {
      let mut style = ComputedStyle::default();
      style.background_color = Rgba::BLUE;
      style.set_background_layers(vec![BackgroundLayer {
        image: Some(BackgroundImage::LinearGradient {
          angle: 0.0,
          stops: vec![
            crate::css::types::ColorStop {
              color: Color::Rgba(Rgba::new(255, 255, 0, 1.0)),
              position: Some(0.0),
            },
            crate::css::types::ColorStop {
              color: Color::Rgba(Rgba::new(255, 255, 0, 1.0)),
              position: Some(1.0),
            },
          ],
        }),
        repeat: BackgroundRepeat::no_repeat(),
        blend_mode,
        ..BackgroundLayer::default()
      }]);
      style
    };

    let normal_fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(make_style(MixBlendMode::Normal)),
    );
    let multiply_fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(make_style(MixBlendMode::Multiply)),
    );

    let normal =
      paint_tree(&FragmentTree::new(normal_fragment), 10, 10, Rgba::WHITE).expect("paint");
    let multiplied = paint_tree(&FragmentTree::new(multiply_fragment), 10, 10, Rgba::WHITE)
      .expect("paint multiply");

    assert_eq!(color_at(&normal, 5, 5), (255, 255, 0, 255));
    let blended = color_at(&multiplied, 5, 5);
    assert!(
      blended.0 < 5 && blended.1 < 5 && blended.2 < 5,
      "multiply blend should blacken blue + yellow, got {:?}",
      blended
    );
  }

  #[test]
  fn background_blend_mode_plus_lighter_adds_layers() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::from_rgba8(100, 100, 100, 255);
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::from_rgba8(200, 0, 0, 255)),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::from_rgba8(200, 0, 0, 255)),
            position: Some(1.0),
          },
        ],
      }),
      repeat: BackgroundRepeat::no_repeat(),
      blend_mode: MixBlendMode::PlusLighter,
      ..BackgroundLayer::default()
    }]);

    let fragment =
      FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![], Arc::new(style));
    let pixmap = paint_tree(&FragmentTree::new(fragment), 2, 2, Rgba::WHITE).expect("paint");
    let (r, g, b, _) = color_at(&pixmap, 0, 0);
    assert_eq!(
      (r, g, b),
      (255, 100, 100),
      "plus-lighter background blend should add colors"
    );
  }

  #[test]
  fn mix_blend_mode_hue_preserves_hsl_components() {
    let dst = (30u8, 120u8, 220u8);
    let src = (200u8, 30u8, 30u8);

    let mut root_style = ComputedStyle::default();
    root_style.background_color = Rgba::from_rgba8(dst.0, dst.1, dst.2, 255);

    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::from_rgba8(src.0, src.1, src.2, 255);
    child_style.mix_blend_mode = MixBlendMode::Hue;

    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
      vec![],
      Arc::new(child_style),
    );
    let mut root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![child]);
    root.style = Some(Arc::new(root_style));

    let pixmap = paint_tree(&FragmentTree::new(root), 2, 2, Rgba::WHITE).expect("paint");
    let (r, g, b, _) = color_at(&pixmap, 0, 0);

    let expected = apply_hsl_blend(
      MixBlendMode::Hue,
      (
        src.0 as f32 / 255.0,
        src.1 as f32 / 255.0,
        src.2 as f32 / 255.0,
      ),
      (
        dst.0 as f32 / 255.0,
        dst.1 as f32 / 255.0,
        dst.2 as f32 / 255.0,
      ),
    );
    let expected_hsl = rgb_to_hsl(expected.0, expected.1, expected.2);
    assert_hsl_components(
      (r, g, b),
      expected_hsl,
      0.02,
      0.05,
      0.05,
      "hue mix-blend-mode",
    );
  }

  #[test]
  fn mix_blend_mode_plus_lighter_adds_colors() {
    let mut root_style = ComputedStyle::default();
    root_style.background_color = Rgba::from_rgba8(100, 100, 100, 255);

    let mut child_style = ComputedStyle::default();
    child_style.background_color = Rgba::from_rgba8(200, 0, 0, 255);
    child_style.mix_blend_mode = MixBlendMode::PlusLighter;

    let child = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 2.0, 2.0),
      vec![],
      Arc::new(child_style),
    );
    let mut root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![child]);
    root.style = Some(Arc::new(root_style));

    let pixmap = paint_tree(&FragmentTree::new(root), 2, 2, Rgba::WHITE).expect("paint");
    let (r, g, b, _) = color_at(&pixmap, 0, 0);
    assert_eq!(
      (r, g, b),
      (255, 100, 100),
      "plus-lighter should add source and destination colors"
    );
  }

  #[test]
  fn clip_path_polygon_masks_painter_output() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.clip_path = crate::style::types::ClipPath::BasicShape(
      Box::new(crate::style::types::BasicShape::Polygon {
        fill: crate::style::types::FillRule::NonZero,
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
    let tree = FragmentTree::new(fragment);

    let pixmap =
      paint_tree(&FragmentTree::new(tree.root.clone()), 10, 10, Rgba::WHITE).expect("paint");

    assert_eq!(color_at(&pixmap, 2, 2), (255, 0, 0, 255));
    assert_eq!(color_at(&pixmap, 9, 9), (255, 255, 255, 255));
  }

  #[test]
  fn background_blend_mode_color_preserves_luminance() {
    let dst = (30u8, 120u8, 220u8);
    let src = (200u8, 30u8, 30u8);

    let mut style = ComputedStyle::default();
    style.background_color = Rgba::from_rgba8(dst.0, dst.1, dst.2, 255);
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::from_rgba8(src.0, src.1, src.2, 255)),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::from_rgba8(src.0, src.1, src.2, 255)),
            position: Some(1.0),
          },
        ],
      }),
      repeat: BackgroundRepeat::no_repeat(),
      blend_mode: MixBlendMode::Color,
      ..BackgroundLayer::default()
    }]);

    let fragment =
      FragmentNode::new_block_styled(Rect::from_xywh(0.0, 0.0, 2.0, 2.0), vec![], Arc::new(style));
    let pixmap = paint_tree(&FragmentTree::new(fragment), 2, 2, Rgba::WHITE).expect("paint");
    let (r, g, b, _) = color_at(&pixmap, 0, 0);

    let expected = apply_hsl_blend(
      MixBlendMode::Color,
      (
        src.0 as f32 / 255.0,
        src.1 as f32 / 255.0,
        src.2 as f32 / 255.0,
      ),
      (
        dst.0 as f32 / 255.0,
        dst.1 as f32 / 255.0,
        dst.2 as f32 / 255.0,
      ),
    );
    let expected_hsl = rgb_to_hsl(expected.0, expected.1, expected.2);
    assert_hsl_components(
      (r, g, b),
      expected_hsl,
      0.02,
      0.05,
      0.05,
      "background color blend",
    );
  }

  #[test]
  fn background_layers_paint_top_to_bottom() {
    let mut style = ComputedStyle::default();

    let top = BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::from_rgba8(0, 255, 0, 128)),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::from_rgba8(0, 255, 0, 128)),
            position: Some(1.0),
          },
        ],
      }),
      ..BackgroundLayer::default()
    };
    let bottom = BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(1.0),
          },
        ],
      }),
      ..BackgroundLayer::default()
    };
    style.set_background_layers(vec![top, bottom]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 10, 10, Rgba::WHITE).expect("paint");

    let center = color_at(&pixmap, 5, 5);
    // Top (semi-transparent green) over opaque blue yields roughly 50/50 mix.
    assert!(
      center.1 > 110 && center.1 < 150 && center.2 > 110 && center.2 < 150 && center.0 == 0,
      "expected blended green-over-blue, got {:?}",
      center
    );
  }

  #[test]
  fn background_layers_use_per_layer_clips() {
    let mut style = ComputedStyle::default();
    style.padding_left = Length::px(4.0);
    style.padding_right = Length::px(4.0);
    style.padding_top = Length::px(4.0);
    style.padding_bottom = Length::px(4.0);

    let top = BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::GREEN),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::GREEN),
            position: Some(1.0),
          },
        ],
      }),
      clip: crate::style::types::BackgroundBox::ContentBox,
      ..BackgroundLayer::default()
    };
    let bottom = BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 0.0,
        stops: vec![
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(0.0),
          },
          crate::css::types::ColorStop {
            color: Color::Rgba(Rgba::BLUE),
            position: Some(1.0),
          },
        ],
      }),
      clip: crate::style::types::BackgroundBox::BorderBox,
      ..BackgroundLayer::default()
    };
    style.set_background_layers(vec![top, bottom]);

    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");

    // Padding area should see only the border-box layer (blue).
    let padding_px = color_at(&pixmap, 1, 1);
    assert!(
      padding_px.2 > 200 && padding_px.1 < 50,
      "expected blue in padding area, got {:?}",
      padding_px
    );

    // Content area should be covered by the content-clipped top layer (green).
    let content_px = color_at(&pixmap, 10, 10);
    assert!(
      content_px.1 > 200 && content_px.2 < 50,
      "expected green in content area, got {:?}",
      content_px
    );
  }

  #[test]
  fn outline_draws_outside_box() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::BLUE;
    style.outline_style = OutlineStyle::Solid;
    style.outline_width = Length::px(4.0);
    style.outline_color = OutlineColor::Color(Rgba::RED);
    style.outline_offset = Length::px(2.0);
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(4.0, 4.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 30, 30, Rgba::WHITE).expect("paint");
    assert_eq!(color_at(&pixmap, 5, 5), (0, 0, 255, 255));
    let mut found_outline = false;
    for y in 0..5 {
      for x in 0..10 {
        let px = color_at(&pixmap, x, y);
        if px != (255, 255, 255, 255) {
          found_outline = true;
        }
      }
    }
    assert!(found_outline, "outline stroke should paint outside the box");
    assert_eq!(color_at(&pixmap, 0, 0), (255, 255, 255, 255));
  }

  #[test]
  fn filters_expand_clip_under_radii() {
    let mut painter = Painter::with_resources_scaled(
      4,
      4,
      Rgba::TRANSPARENT,
      FontContext::new(),
      ImageCache::new(),
      1.0,
    )
    .unwrap();
    painter.fill_background();

    let mut style = ComputedStyle::default();
    style.background_color = Rgba::RED;
    let style = Arc::new(style);

    let cmd = DisplayCommand::StackingContext {
      rect: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
      opacity: 1.0,
      transform: None,
      transform_3d: None,
      blend_mode: MixBlendMode::Normal,
      isolated: true,
      mask: None,
      filters: vec![ResolvedFilter::Blur(1.0)],
      backdrop_filters: Vec::new(),
      radii: BorderRadii::uniform(0.5),
      clip: None,
      clip_path: None,
      commands: vec![DisplayCommand::Background {
        rect: Rect::from_xywh(1.0, 1.0, 2.0, 2.0),
        style,
      }],
    };

    painter.execute_command(cmd).unwrap();
    let alpha_near = painter.pixmap.pixel(0, 1).unwrap().alpha();
    assert!(
      alpha_near > 0,
      "expected blur spill outside rounded rect; alpha at (0,1) was {alpha_near}"
    );
    assert!(
      alpha_near < 255,
      "blurred spill should be softer than solid fill; alpha at (0,1) was {alpha_near}"
    );
  }

  #[test]
  fn backdrop_filters_follow_transforms() {
    let mut painter = Painter::new(24, 16, Rgba::BLUE).expect("painter");
    painter.fill_background();

    let cmd = DisplayCommand::StackingContext {
      rect: Rect::from_xywh(0.0, 0.0, 6.0, 6.0),
      opacity: 1.0,
      transform: Some(Transform::from_translate(12.0, 8.0)),
      transform_3d: Some(Transform3D::from_2d(&Transform2D::translate(12.0, 8.0))),
      blend_mode: MixBlendMode::Normal,
      isolated: false,
      mask: None,
      filters: Vec::new(),
      backdrop_filters: vec![ResolvedFilter::Invert(1.0)],
      radii: BorderRadii::ZERO,
      clip: None,
      clip_path: None,
      commands: Vec::new(),
    };

    painter.execute_command(cmd).expect("execute");
    let pixmap = painter.pixmap;

    let origin_px = color_at(&pixmap, 1, 1);
    assert_eq!(
      origin_px,
      (0, 0, 255, 255),
      "backdrop filter should not affect the origin"
    );

    let mut inverted = Vec::new();
    let (w, h) = (pixmap.width(), pixmap.height());
    for y in 0..h {
      for x in 0..w {
        let (r, g, b, _) = color_at(&pixmap, x, y);
        if r > 200 && g > 200 && b < 80 {
          inverted.push((x, y));
        }
      }
    }
    let expected: Vec<(u32, u32)> = (8..14)
      .flat_map(|y| (12..18).map(move |x| (x, y)))
      .collect();
    assert_eq!(
      inverted, expected,
      "backdrop filter should track the translated box"
    );
  }

  #[test]
  fn backdrop_filters_cover_bounds() {
    let mut pixmap = Pixmap::new(10, 10).expect("pixmap");
    pixmap.fill(tiny_skia::Color::from_rgba8(0, 0, 255, 255));

    let bounds = Rect::from_xywh(2.0, 3.0, 4.0, 2.0);
    let filters = vec![ResolvedFilter::Invert(1.0)];
    apply_backdrop_filters(
      &mut pixmap,
      &bounds,
      &filters,
      BorderRadii::ZERO,
      1.0,
      bounds,
    );

    let mut inverted = Vec::new();
    for y in 0..pixmap.height() {
      for x in 0..pixmap.width() {
        let (r, g, b, _) = color_at(&pixmap, x, y);
        if r > 200 && g > 200 && b < 80 {
          inverted.push((x, y));
        }
      }
    }

    let expected: Vec<(u32, u32)> = (3..5).flat_map(|y| (2..6).map(move |x| (x, y))).collect();
    assert_eq!(
      inverted, expected,
      "backdrop filter should invert the full bounds"
    );
  }

  #[test]
  fn clip_path_masks_stacking_contents() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.clip_path = ClipPath::BasicShape(
      Box::new(crate::style::types::BasicShape::Circle {
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

    let mut root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![]);
    root.style = Some(Arc::new(style));
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 10, 10, Rgba::WHITE).expect("painted");
    let center = pixmap.pixel(5, 5).expect("center pixel");
    let corner = pixmap.pixel(0, 0).expect("corner pixel");

    assert!(center.red() > 200 && center.green() < 60 && center.blue() < 60);
    assert_eq!(
      (corner.red(), corner.green(), corner.blue(), corner.alpha()),
      (255, 255, 255, 255)
    );
  }

  #[test]
  fn clip_path_polygon_masks_paint_output() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.clip_path = ClipPath::BasicShape(
      Box::new(crate::style::types::BasicShape::Polygon {
        fill: crate::style::types::FillRule::NonZero,
        points: vec![
          (Length::px(0.0), Length::px(0.0)),
          (Length::px(0.0), Length::px(10.0)),
          (Length::px(10.0), Length::px(0.0)),
        ],
      }),
      None,
    );

    let mut root = FragmentNode::new_block(Rect::from_xywh(0.0, 0.0, 10.0, 10.0), vec![]);
    root.style = Some(Arc::new(style));
    let tree = FragmentTree::new(root);

    let pixmap = paint_tree(&tree, 10, 10, Rgba::WHITE).expect("painted");
    let inside = pixmap.pixel(2, 2).expect("inside pixel");
    let outside = pixmap.pixel(9, 9).expect("outside pixel");

    assert!(inside.red() > 200 && inside.green() < 60 && inside.blue() < 60);
    assert_eq!(
      (
        outside.red(),
        outside.green(),
        outside.blue(),
        outside.alpha()
      ),
      (255, 255, 255, 255)
    );
  }

  #[test]
  fn outline_not_clipped_by_overflow_hidden() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::WHITE;
    style.outline_style = OutlineStyle::Solid;
    style.outline_width = Length::px(4.0);
    style.outline_color = OutlineColor::Color(Rgba::RED);
    style.overflow_x = Overflow::Hidden;
    style.overflow_y = Overflow::Hidden;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(10.0, 10.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 40, 40, Rgba::WHITE).expect("paint");
    // Outline should extend beyond the 10..20 box even though overflow is hidden.
    assert_eq!(color_at(&pixmap, 8, 15), (255, 0, 0, 255));
  }

  #[test]
  fn outline_not_clipped_by_clip_path() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::WHITE;
    style.outline_style = OutlineStyle::Solid;
    style.outline_width = Length::px(4.0);
    style.outline_color = OutlineColor::Color(Rgba::RED);
    style.clip_path = ClipPath::BasicShape(
      Box::new(crate::style::types::BasicShape::Circle {
        radius: ShapeRadius::Length(Length::px(2.0)),
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
      Rect::from_xywh(5.0, 5.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
    // Outline should remain visible outside the clip-path bounds.
    assert_eq!(color_at(&pixmap, 4, 10), (255, 0, 0, 255));
  }

  #[test]
  fn visibility_hidden_prevents_painting() {
    let mut style = ComputedStyle::default();
    style.visibility = crate::style::computed::Visibility::Hidden;
    style.background_color = Rgba::RED;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
    assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
  }

  #[test]
  fn visibility_collapse_prevents_painting() {
    let mut style = ComputedStyle::default();
    style.visibility = crate::style::computed::Visibility::Collapse;
    style.background_color = Rgba::BLUE;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
    assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
  }

  #[test]
  fn backface_hidden_prevents_painting() {
    let mut style = ComputedStyle::default();
    style.backface_visibility = BackfaceVisibility::Hidden;
    style.transform.push(css::types::Transform::RotateX(180.0));
    style.background_color = Rgba::RED;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
    assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
  }

  #[test]
  fn perspective_transform_warps_layer() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.perspective = Some(Length::px(400.0));
    style
      .transform
      .push(crate::css::types::Transform::RotateY(45.0));
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(10.0, 10.0, 20.0, 20.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree_scaled(&tree, 64, 40, Rgba::WHITE, 1.0).expect("paint");

    let mut min_x = pixmap.width();
    let mut min_y = pixmap.height();
    let mut max_x = 0;
    let mut max_y = 0;
    let mut count = 0;
    for y in 0..pixmap.height() {
      for x in 0..pixmap.width() {
        let (r, g, b, a) = color_at(&pixmap, x, y);
        if a > 0 && (r != 255 || g != 255 || b != 255) {
          count += 1;
          min_x = min_x.min(x);
          min_y = min_y.min(y);
          max_x = max_x.max(x);
          max_y = max_y.max(y);
        }
      }
    }

    assert!(count > 0, "expected warped content to paint");

    let bounds = Rect::from_xywh(
      min_x as f32,
      min_y as f32,
      (max_x - min_x + 1) as f32,
      (max_y - min_y + 1) as f32,
    );
    let expected = Rect::from_xywh(10.0, 10.0, 20.0, 20.0);
    assert!(
      bounds.width() < expected.width() - 0.5,
      "perspective should shrink projected width; expected < {}, got {}",
      expected.width(),
      bounds.width()
    );
  }

  #[test]
  fn backface_hidden_with_perspective_culls() {
    let mut style = ComputedStyle::default();
    style.backface_visibility = BackfaceVisibility::Hidden;
    style.perspective = Some(Length::px(400.0));
    style.transform.push(css::types::Transform::RotateX(190.0));
    style.background_color = Rgba::RED;
    let fragment = FragmentNode::new_block_styled(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      vec![],
      Arc::new(style),
    );
    let tree = FragmentTree::new(fragment);

    let pixmap = paint_tree(&tree, 20, 20, Rgba::WHITE).expect("paint");
    assert_eq!(color_at(&pixmap, 5, 5), (255, 255, 255, 255));
  }

  #[test]
  fn exif_orientation_rotates_images_by_default() {
    let mut painter =
      Painter::with_resources(1, 2, Rgba::WHITE, FontContext::new(), ImageCache::new())
        .expect("painter");
    let style = ComputedStyle::default();
    let ok = painter.paint_image_from_src(
      "tests/fixtures/image_orientation/orientation-6.jpg",
      Some(&style),
      0.0,
      0.0,
      1.0,
      2.0,
    );
    assert!(ok, "image should paint");
    let top = color_at(&painter.pixmap, 0, 0);
    let bottom = color_at(&painter.pixmap, 0, 1);
    assert!(
      top.0 > top.1 && top.0 > top.2,
      "expected red-dominant pixel at top after orientation, got {:?}",
      top
    );
    assert!(
      bottom.1 > bottom.0 && bottom.1 > bottom.2,
      "expected green-dominant pixel at bottom after orientation, got {:?}",
      bottom
    );
  }

  #[test]
  fn image_orientation_none_ignores_metadata() {
    let mut painter =
      Painter::with_resources(2, 1, Rgba::WHITE, FontContext::new(), ImageCache::new())
        .expect("painter");
    let mut style = ComputedStyle::default();
    style.image_orientation = ImageOrientation::None;
    let ok = painter.paint_image_from_src(
      "tests/fixtures/image_orientation/orientation-6.jpg",
      Some(&style),
      0.0,
      0.0,
      2.0,
      1.0,
    );
    assert!(ok, "image should paint");
    let left = color_at(&painter.pixmap, 0, 0);
    let right = color_at(&painter.pixmap, 1, 0);
    assert!(
      left.0 > left.1 && left.0 > left.2,
      "expected red-dominant pixel on the left without orientation, got {:?}",
      left
    );
    assert!(
      right.1 > right.0 && right.1 > right.2,
      "expected green-dominant pixel on the right without orientation, got {:?}",
      right
    );
  }

  #[test]
  fn paints_border_image_nine_slice() {
    let mut img = RgbaImage::new(3, 3);
    img.put_pixel(0, 0, image::Rgba([255, 0, 0, 255])); // TL
    img.put_pixel(2, 0, image::Rgba([0, 0, 255, 255])); // TR
    img.put_pixel(0, 2, image::Rgba([0, 255, 0, 255])); // BL
    img.put_pixel(2, 2, image::Rgba([255, 255, 0, 255])); // BR
    let edge = image::Rgba([0, 255, 255, 255]);
    img.put_pixel(1, 0, edge); // top edge
    img.put_pixel(1, 2, edge); // bottom edge
    img.put_pixel(0, 1, edge); // left edge
    img.put_pixel(2, 1, edge); // right edge
    img.put_pixel(1, 1, image::Rgba([255, 255, 255, 255])); // center

    let mut buf = Vec::new();
    image::codecs::png::PngEncoder::new(&mut buf)
      .write_image(img.as_raw(), 3, 3, image::ExtendedColorType::Rgba8)
      .unwrap();
    let data_url = format!(
      "data:image/png;base64,{}",
      base64::engine::general_purpose::STANDARD.encode(&buf)
    );

    let mut style = ComputedStyle::default();
    style.border_top_width = Length::px(4.0);
    style.border_right_width = Length::px(4.0);
    style.border_bottom_width = Length::px(4.0);
    style.border_left_width = Length::px(4.0);
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

    let mut painter = Painter::new(16, 16, Rgba::WHITE).expect("painter");
    painter.paint_borders(0.0, 0.0, 16.0, 16.0, &style);

    let tl = painter.pixmap.pixel(0, 0).unwrap();
    let tr = painter.pixmap.pixel(15, 0).unwrap();
    let bl = painter.pixmap.pixel(0, 15).unwrap();
    let br = painter.pixmap.pixel(15, 15).unwrap();
    assert_eq!((tl.red(), tl.green(), tl.blue()), (255, 0, 0));
    assert_eq!((tr.red(), tr.green(), tr.blue()), (0, 0, 255));
    assert_eq!((bl.red(), bl.green(), bl.blue()), (0, 255, 0));
    assert_eq!((br.red(), br.green(), br.blue()), (255, 255, 0));
    // Edge samples
    let edge_sample = painter.pixmap.pixel(8, 1).unwrap();
    assert_eq!(
      (edge_sample.red(), edge_sample.green(), edge_sample.blue()),
      (0, 255, 255)
    );
    let edge_sample_left = painter.pixmap.pixel(1, 8).unwrap();
    assert_eq!(
      (
        edge_sample_left.red(),
        edge_sample_left.green(),
        edge_sample_left.blue()
      ),
      (0, 255, 255)
    );
  }

  #[test]
  fn thin_double_border_falls_back_to_solid() {
    let mut style = ComputedStyle::default();
    style.border_top_width = Length::px(1.0);
    style.border_right_width = Length::px(1.0);
    style.border_bottom_width = Length::px(1.0);
    style.border_left_width = Length::px(1.0);
    style.border_top_style = CssBorderStyle::Double;
    style.border_right_style = CssBorderStyle::Double;
    style.border_bottom_style = CssBorderStyle::Double;
    style.border_left_style = CssBorderStyle::Double;
    style.border_top_color = Rgba::from_rgba8(0, 0, 0, 255);
    style.border_right_color = Rgba::from_rgba8(0, 0, 0, 255);
    style.border_bottom_color = Rgba::from_rgba8(0, 0, 0, 255);
    style.border_left_color = Rgba::from_rgba8(0, 0, 0, 255);

    let mut painter = Painter::new(6, 6, Rgba::WHITE).expect("painter");
    painter.paint_borders(0.0, 0.0, 6.0, 6.0, &style);

    // The top edge should paint a solid 1px line when double is too thin.
    for x in 0..6 {
      let pixel = painter.pixmap.pixel(x, 0).unwrap();
      assert_eq!((pixel.red(), pixel.green(), pixel.blue()), (0, 0, 0));
      assert!(
        pixel.alpha() >= 180,
        "expected visible solid stroke at ({},0) with alpha >= 180, got {}",
        x,
        pixel.alpha()
      );
    }
  }

  #[test]
  fn border_image_space_distributes_gaps() {
    let mut img = RgbaImage::new(3, 3);
    let magenta = image::Rgba([255, 0, 255, 255]);
    // Fill edges
    for x in 0..3 {
      img.put_pixel(x, 0, magenta);
      img.put_pixel(x, 2, magenta);
    }
    for y in 0..3 {
      img.put_pixel(0, y, magenta);
      img.put_pixel(2, y, magenta);
    }
    img.put_pixel(1, 1, image::Rgba([255, 255, 255, 255])); // center

    let mut buf = Vec::new();
    image::codecs::png::PngEncoder::new(&mut buf)
      .write_image(img.as_raw(), 3, 3, image::ExtendedColorType::Rgba8)
      .unwrap();
    let data_url = format!(
      "data:image/png;base64,{}",
      base64::engine::general_purpose::STANDARD.encode(&buf)
    );

    let mut style = ComputedStyle::default();
    style.border_top_width = Length::px(3.0);
    style.border_right_width = Length::px(3.0);
    style.border_bottom_width = Length::px(3.0);
    style.border_left_width = Length::px(3.0);
    style.border_image = BorderImage {
      source: BorderImageSource::Image(Box::new(BackgroundImage::Url(data_url))),
      slice: BorderImageSlice {
        top: BorderImageSliceValue::Number(1.0),
        right: BorderImageSliceValue::Number(1.0),
        bottom: BorderImageSliceValue::Number(1.0),
        left: BorderImageSliceValue::Number(1.0),
        fill: false,
      },
      repeat: (BorderImageRepeat::Space, BorderImageRepeat::Space),
      ..BorderImage::default()
    };

    let mut painter = Painter::new(14, 14, Rgba::WHITE).expect("painter");
    painter.fill_background();
    painter.paint_borders(0.0, 0.0, 14.0, 14.0, &style);

    // Top edge has a gap between tiles when spaced.
    let gap_top = painter.pixmap.pixel(7, 1).unwrap();
    assert_eq!(
      (gap_top.red(), gap_top.green(), gap_top.blue()),
      (255, 255, 255)
    );
    let painted_top = painter.pixmap.pixel(4, 1).unwrap();
    assert_eq!(
      (painted_top.red(), painted_top.green(), painted_top.blue()),
      (255, 0, 255)
    );

    // Left edge similarly spaces tiles vertically.
    let gap_left = painter.pixmap.pixel(1, 7).unwrap();
    assert_eq!(
      (gap_left.red(), gap_left.green(), gap_left.blue()),
      (255, 255, 255)
    );
    let painted_left = painter.pixmap.pixel(1, 4).unwrap();
    assert_eq!(
      (
        painted_left.red(),
        painted_left.green(),
        painted_left.blue()
      ),
      (255, 0, 255)
    );
  }

  #[test]
  fn border_image_accepts_gradients() {
    let mut style = ComputedStyle::default();
    style.border_top_width = Length::px(4.0);
    style.border_right_width = Length::px(4.0);
    style.border_bottom_width = Length::px(4.0);
    style.border_left_width = Length::px(4.0);
    style.border_image = BorderImage {
      source: BorderImageSource::Image(Box::new(BackgroundImage::LinearGradient {
        angle: 180.0,
        stops: vec![
          ColorStop {
            position: Some(0.0),
            color: crate::style::color::Color::Rgba(Rgba::new(255, 0, 0, 1.0)),
          },
          ColorStop {
            position: Some(1.0),
            color: crate::style::color::Color::Rgba(Rgba::new(0, 0, 255, 1.0)),
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

    let mut painter = Painter::new(16, 16, Rgba::WHITE).expect("painter");
    painter.fill_background();
    painter.paint_borders(0.0, 0.0, 16.0, 16.0, &style);

    let top = painter.pixmap.pixel(8, 0).unwrap();
    assert!(
      top.red() > top.blue(),
      "top should be red-ish, got {:?}",
      top
    );
    let bottom = painter.pixmap.pixel(8, 15).unwrap();
    assert!(
      bottom.blue() > bottom.red(),
      "bottom should be blue-ish, got {:?}",
      bottom
    );
    let center = painter.pixmap.pixel(8, 8).unwrap();
    assert_eq!(
      (center.red(), center.green(), center.blue()),
      (255, 255, 255),
      "center should remain unfilled without border-image fill"
    );
  }

  #[test]
  fn transform_box_uses_content_box_for_translate_percentage() {
    let mut style = ComputedStyle::default();
    style.transform_box = TransformBox::ContentBox;
    style.padding_left = Length::px(10.0);
    style.padding_right = Length::px(10.0);
    style.border_left_width = Length::px(5.0);
    style.border_right_width = Length::px(5.0);
    style
      .transform
      .push(crate::css::types::Transform::Translate(
        Length::percent(50.0),
        Length::percent(0.0),
      ));

    let bounds = Rect::from_xywh(0.0, 0.0, 200.0, 100.0);
    let transform = build_transform_3d(Some(&style), bounds, None).expect("transform should build");
    let transform = transform.to_2d().expect("should be affine");

    assert!((transform.e - 85.0).abs() < 1e-3);
    assert!(transform.f.abs() < 1e-3);
  }

  #[test]
  fn transform_box_moves_origin_into_content_box() {
    let mut style = ComputedStyle::default();
    style.transform_box = TransformBox::ContentBox;
    style.padding_left = Length::px(10.0);
    style.border_left_width = Length::px(5.0);
    style.transform_origin = crate::style::types::TransformOrigin {
      x: Length::percent(0.0),
      y: Length::percent(0.0),
    };
    style
      .transform
      .push(crate::css::types::Transform::Scale(2.0, 1.0));

    let bounds = Rect::from_xywh(0.0, 0.0, 200.0, 100.0);
    let transform = build_transform_3d(Some(&style), bounds, None).expect("transform should build");
    let transform = transform.to_2d().expect("should be affine");

    assert!((transform.e + 15.0).abs() < 1e-3);
    assert!(transform.f.abs() < 1e-3);
  }

  #[test]
  fn gradient_background_respects_size_and_repeat() {
    let mut style = ComputedStyle::default();
    style.background_layers = vec![BackgroundLayer {
      image: Some(BackgroundImage::LinearGradient {
        angle: 90.0,
        stops: vec![
          ColorStop {
            position: Some(0.0),
            color: crate::style::color::Color::Rgba(Rgba::new(255, 0, 0, 1.0)),
          },
          ColorStop {
            position: Some(1.0),
            color: crate::style::color::Color::Rgba(Rgba::new(0, 0, 255, 1.0)),
          },
        ],
      }),
      size: BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(4.0)),
        BackgroundSizeComponent::Length(Length::px(2.0)),
      ),
      ..BackgroundLayer::default()
    }];
    style.background_color = Rgba::WHITE;

    let mut painter = Painter::new(12, 6, Rgba::WHITE).expect("painter");
    painter.fill_background();
    painter.paint_background(0.0, 0.0, 12.0, 6.0, &style);

    let top_left = painter.pixmap.pixel(0, 0).unwrap();
    let top_right_tile = painter.pixmap.pixel(3, 0).unwrap();
    let next_tile = painter.pixmap.pixel(5, 0).unwrap();

    assert!(
      top_left.red() > top_left.blue(),
      "first tile should start red, got {:?}",
      top_left
    );
    assert!(
      top_right_tile.blue() > top_right_tile.red(),
      "end of first tile should be blue-ish, got {:?}",
      top_right_tile
    );
    assert!(
      next_tile.red() > next_tile.blue(),
      "second tile should repeat starting red, got {:?}",
      next_tile
    );
  }

  #[test]
  fn snap_upscale_prefers_integer_factor() {
    assert_eq!(snap_upscale(5.0, 2.0), Some((4.0, 0.5)));
    assert_eq!(snap_upscale(2.0, 2.0), None);
    assert_eq!(snap_upscale(1.0, 3.0), None);
  }

  #[test]
  fn iframe_srcdoc_renders_inline_content() {
    let html = r"
        <style>html, body { margin: 0; padding: 0; background: red; }</style>
        ";
    let painter = Painter::new(20, 20, Rgba::WHITE).expect("painter");
    let fragment = FragmentNode::new_with_style(
      Rect::from_xywh(0.0, 0.0, 10.0, 10.0),
      FragmentContent::Replaced {
        box_id: None,
        replaced_type: ReplacedType::Iframe {
          src: String::new(),
          srcdoc: Some(html.to_string()),
        },
      },
      vec![],
      Arc::new(ComputedStyle::default()),
    );
    let tree = FragmentTree::new(fragment);
    let pixmap = painter.paint(&tree).expect("paint");

    let center = pixmap.pixel(5, 5).unwrap();
    assert!(
      center.red() > 200 && center.green() < 50 && center.blue() < 50,
      "iframe srcdoc should paint red"
    );
  }
}
