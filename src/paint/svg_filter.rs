use crate::geometry::{Point, Rect};
use crate::image_loader::ImageCache;
use crate::paint::blur::apply_gaussian_blur_anisotropic;
use crate::style::color;
use crate::tree::box_tree::ReplacedType;
use crate::tree::fragment_tree::{FragmentContent, FragmentNode};
use crate::Rgba;
use rayon::prelude::*;
use roxmltree::Document;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use tiny_skia::{BlendMode, FilterQuality, Pixmap, PixmapPaint, PremultipliedColorU8, Transform};

mod turbulence;

static FILTER_CACHE: OnceLock<Mutex<HashMap<String, Arc<SvgFilter>>>> = OnceLock::new();

const MAX_TURBULENCE_OCTAVES: u32 = 8;

fn filter_cache() -> &'static Mutex<HashMap<String, Arc<SvgFilter>>> {
  FILTER_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[derive(Clone, Debug)]
pub struct SvgFilter {
  pub color_interpolation_filters: ColorInterpolationFilters,
  pub steps: Vec<FilterStep>,
  pub region: SvgFilterRegion,
  pub primitive_units: SvgFilterUnits,
}

#[derive(Clone, Debug)]
pub struct FilterStep {
  pub result: Option<String>,
  pub color_interpolation_filters: Option<ColorInterpolationFilters>,
  pub primitive: FilterPrimitive,
  pub region: Option<SvgFilterRegion>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ColorInterpolationFilters {
  LinearRGB,
  SRGB,
}

#[derive(Clone, Copy, Debug)]
pub enum SvgFilterUnits {
  ObjectBoundingBox,
  UserSpaceOnUse,
}

#[derive(Clone, Copy, Debug)]
pub struct SvgFilterRegion {
  pub x: SvgLength,
  pub y: SvgLength,
  pub width: SvgLength,
  pub height: SvgLength,
  pub units: SvgFilterUnits,
}

impl SvgFilterRegion {
  fn default_for_units(units: SvgFilterUnits) -> Self {
    Self {
      x: SvgLength::Percent(-0.1),
      y: SvgLength::Percent(-0.1),
      width: SvgLength::Percent(1.2),
      height: SvgLength::Percent(1.2),
      units,
    }
  }

  pub fn resolve(&self, bbox: Rect) -> Rect {
    let width_basis = bbox.width();
    let height_basis = bbox.height();
    let units = match self.units {
      SvgFilterUnits::ObjectBoundingBox => SvgCoordinateUnits::ObjectBoundingBox,
      SvgFilterUnits::UserSpaceOnUse => SvgCoordinateUnits::UserSpaceOnUse,
    };
    let resolve_x = |len: SvgLength| {
      let offset = match self.units {
        SvgFilterUnits::ObjectBoundingBox => bbox.min_x(),
        SvgFilterUnits::UserSpaceOnUse => match len {
          SvgLength::Percent(_) => bbox.min_x(),
          SvgLength::Number(_) => 0.0,
        },
      };
      offset + len.resolve(units, width_basis)
    };
    let resolve_y = |len: SvgLength| {
      let offset = match self.units {
        SvgFilterUnits::ObjectBoundingBox => bbox.min_y(),
        SvgFilterUnits::UserSpaceOnUse => match len {
          SvgLength::Percent(_) => bbox.min_y(),
          SvgLength::Number(_) => 0.0,
        },
      };
      offset + len.resolve(units, height_basis)
    };
    let resolve_width = |len: SvgLength| len.resolve(units, width_basis);
    let resolve_height = |len: SvgLength| len.resolve(units, height_basis);

    let width = resolve_width(self.width).max(0.0);
    let height = resolve_height(self.height).max(0.0);
    let x = resolve_x(self.x);
    let y = resolve_y(self.y);
    Rect::from_xywh(x, y, width, height)
  }
}

#[derive(Clone, Debug)]
pub enum FilterPrimitive {
  Flood {
    color: Rgba,
    opacity: f32,
  },
  GaussianBlur {
    input: FilterInput,
    std_dev: (f32, f32),
  },
  Offset {
    input: FilterInput,
    dx: f32,
    dy: f32,
  },
  ColorMatrix {
    input: FilterInput,
    kind: ColorMatrixKind,
  },
  Composite {
    input1: FilterInput,
    input2: FilterInput,
    operator: CompositeOperator,
  },
  Merge {
    inputs: Vec<FilterInput>,
  },
  DropShadow {
    input: FilterInput,
    dx: f32,
    dy: f32,
    std_dev: (f32, f32),
    color: Rgba,
    opacity: f32,
  },
  Blend {
    input1: FilterInput,
    input2: FilterInput,
    mode: BlendMode,
  },
  Morphology {
    input: FilterInput,
    radius: (f32, f32),
    op: MorphologyOp,
  },
  ComponentTransfer {
    input: FilterInput,
    r: TransferFn,
    g: TransferFn,
    b: TransferFn,
    a: TransferFn,
  },
  Image(ImagePrimitive),
  Tile {
    input: FilterInput,
  },
  Turbulence {
    base_frequency: (f32, f32),
    seed: u32,
    octaves: u32,
    stitch_tiles: bool,
    kind: TurbulenceType,
  },
  DisplacementMap {
    in1: FilterInput,
    in2: FilterInput,
    scale: f32,
    x_channel: ChannelSelector,
    y_channel: ChannelSelector,
  },
  ConvolveMatrix {
    input: FilterInput,
    order_x: usize,
    order_y: usize,
    kernel: Vec<f32>,
    divisor: Option<f32>,
    bias: f32,
    target_x: i32,
    target_y: i32,
    edge_mode: EdgeMode,
    preserve_alpha: bool,
    subregion: Option<(f32, f32, f32, f32)>,
  },
}

#[derive(Clone, Debug)]
pub enum FilterInput {
  SourceGraphic,
  SourceAlpha,
  BackgroundImage,
  BackgroundAlpha,
  FillPaint,
  StrokePaint,
  Reference(String),
  Previous,
}

#[derive(Clone, Copy, Debug)]
enum SvgCoordinateUnits {
  ObjectBoundingBox,
  UserSpaceOnUse,
}

impl SvgCoordinateUnits {
  fn parse(attr: Option<&str>, default: SvgCoordinateUnits) -> SvgCoordinateUnits {
    match attr.map(|v| v.trim().to_ascii_lowercase()) {
      Some(v) if v == "objectboundingbox" => SvgCoordinateUnits::ObjectBoundingBox,
      Some(v) if v == "userspaceonuse" => SvgCoordinateUnits::UserSpaceOnUse,
      _ => default,
    }
  }
}

#[derive(Clone, Copy, Debug)]
pub enum SvgLength {
  Number(f32),
  Percent(f32),
}

impl SvgLength {
  fn parse(attr: Option<&str>, default: SvgLength) -> SvgLength {
    let raw = match attr {
      Some(v) => v.trim(),
      None => return default,
    };
    if raw.is_empty() {
      return default;
    }
    if let Some(stripped) = raw.strip_suffix('%') {
      if let Ok(v) = stripped.trim().parse::<f32>() {
        return SvgLength::Percent(v / 100.0);
      }
    }
    raw.parse::<f32>().map(SvgLength::Number).unwrap_or(default)
  }

  fn resolve(self, units: SvgCoordinateUnits, reference: f32) -> f32 {
    match self {
      SvgLength::Percent(frac) => frac * reference,
      SvgLength::Number(v) => match units {
        SvgCoordinateUnits::UserSpaceOnUse => v,
        SvgCoordinateUnits::ObjectBoundingBox => v * reference,
      },
    }
  }
}

#[derive(Clone, Copy, Debug)]
enum PreserveAspectRatio {
  None,
  XMidYMidMeet,
}

impl PreserveAspectRatio {
  fn parse(attr: Option<&str>) -> PreserveAspectRatio {
    let raw = attr.unwrap_or("").trim();
    if raw.eq_ignore_ascii_case("none") {
      PreserveAspectRatio::None
    } else {
      PreserveAspectRatio::XMidYMidMeet
    }
  }
}

#[derive(Clone, Debug)]
pub struct ImagePrimitive {
  pixmap: Pixmap,
  x: SvgLength,
  y: SvgLength,
  width: SvgLength,
  height: SvgLength,
  preserve_aspect_ratio: PreserveAspectRatio,
  units: SvgCoordinateUnits,
}

impl ImagePrimitive {
  pub fn from_pixmap(pixmap: Pixmap) -> Self {
    Self {
      pixmap,
      x: SvgLength::Percent(0.0),
      y: SvgLength::Percent(0.0),
      width: SvgLength::Percent(1.0),
      height: SvgLength::Percent(1.0),
      preserve_aspect_ratio: PreserveAspectRatio::XMidYMidMeet,
      units: SvgCoordinateUnits::ObjectBoundingBox,
    }
  }
}

#[derive(Clone, Debug)]
struct FilterResult {
  pixmap: Pixmap,
  region: Rect,
}

impl FilterResult {
  fn new(pixmap: Pixmap, region: Rect, filter_region: Rect) -> Self {
    Self {
      pixmap,
      region: clip_region(region, filter_region),
    }
  }

  fn full_region(pixmap: Pixmap, filter_region: Rect) -> Self {
    Self::new(pixmap, filter_region, filter_region)
  }
}

fn clip_region(region: Rect, filter_region: Rect) -> Rect {
  region
    .intersection(filter_region)
    .unwrap_or(Rect::from_xywh(
      filter_region.x(),
      filter_region.y(),
      0.0,
      0.0,
    ))
}

fn inflate_rect_xy(region: Rect, dx: f32, dy: f32) -> Rect {
  Rect::from_xywh(
    region.x() - dx,
    region.y() - dy,
    (region.width() + dx * 2.0).max(0.0),
    (region.height() + dy * 2.0).max(0.0),
  )
}

fn filter_region_for_pixmap(pixmap: &Pixmap) -> Rect {
  Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32)
}

fn transparent_result(filter_region: Rect) -> Option<FilterResult> {
  let width = filter_region.width().max(0.0).round() as u32;
  let height = filter_region.height().max(0.0).round() as u32;
  Some(FilterResult::full_region(
    Pixmap::new(width, height)?,
    filter_region,
  ))
}

#[derive(Clone, Copy, Debug)]
pub enum ChannelSelector {
  R,
  G,
  B,
  A,
}

#[derive(Clone, Debug)]
pub enum ColorMatrixKind {
  Matrix([f32; 20]),
  Saturate(f32),
  HueRotate(f32),
  LuminanceToAlpha,
}

#[derive(Clone, Copy, Debug)]
pub enum CompositeOperator {
  Over,
  In,
  Out,
  Atop,
  Xor,
  Arithmetic { k1: f32, k2: f32, k3: f32, k4: f32 },
}

#[derive(Clone, Copy, Debug)]
pub enum MorphologyOp {
  Dilate,
  Erode,
}

#[derive(Clone, Copy, Debug)]
pub enum EdgeMode {
  Duplicate,
  Wrap,
  None,
}

#[derive(Clone, Copy, Debug)]
pub enum TurbulenceType {
  FractalNoise,
  Turbulence,
}

#[derive(Clone, Debug)]
pub enum TransferFn {
  Identity,
  Linear {
    slope: f32,
    intercept: f32,
  },
  Gamma {
    amplitude: f32,
    exponent: f32,
    offset: f32,
  },
  Table {
    values: Vec<f32>,
  },
  Discrete {
    values: Vec<f32>,
  },
}

#[derive(Clone, Copy, Debug)]
struct UnpremultipliedColor {
  r: f32,
  g: f32,
  b: f32,
  a: f32,
}

fn clamp01(v: f32) -> f32 {
  v.clamp(0.0, 1.0)
}

/// Convert a premultiplied pixel into unpremultiplied RGBA in [0, 1].
fn to_unpremultiplied(px: PremultipliedColorU8) -> UnpremultipliedColor {
  let a = px.alpha() as f32 / 255.0;
  if a <= 0.0 {
    return UnpremultipliedColor {
      r: 0.0,
      g: 0.0,
      b: 0.0,
      a: 0.0,
    };
  }
  let inv_a = 1.0 / a;
  UnpremultipliedColor {
    r: clamp01(px.red() as f32 / 255.0 * inv_a),
    g: clamp01(px.green() as f32 / 255.0 * inv_a),
    b: clamp01(px.blue() as f32 / 255.0 * inv_a),
    a,
  }
}

/// Convert unpremultiplied RGBA in [0, 1] back to premultiplied u8.
fn to_premultiplied(color: UnpremultipliedColor) -> PremultipliedColorU8 {
  let a = clamp01(color.a);
  let a_byte = (a * 255.0).round().clamp(0.0, 255.0) as u8;
  let premul_channel = |v: f32| -> u8 {
    let scaled = (clamp01(v) * a * 255.0).round().clamp(0.0, 255.0) as u8;
    scaled.min(a_byte)
  };
  PremultipliedColorU8::from_rgba(
    premul_channel(color.r),
    premul_channel(color.g),
    premul_channel(color.b),
    a_byte,
  )
  .unwrap_or(PremultipliedColorU8::TRANSPARENT)
}

const COLOR_LUT_SIZE: usize = 256;

static SRGB_TO_LINEAR_LUT: OnceLock<[f32; COLOR_LUT_SIZE + 1]> = OnceLock::new();
static LINEAR_TO_SRGB_LUT: OnceLock<[f32; COLOR_LUT_SIZE + 1]> = OnceLock::new();

fn build_lut<F>(f: F) -> [f32; COLOR_LUT_SIZE + 1]
where
  F: Fn(f32) -> f32,
{
  let mut lut = [0.0; COLOR_LUT_SIZE + 1];
  for (idx, slot) in lut.iter_mut().enumerate() {
    let x = idx as f32 / COLOR_LUT_SIZE as f32;
    *slot = f(x);
  }
  lut
}

fn sample_lut(value: f32, lut: &[f32; COLOR_LUT_SIZE + 1]) -> f32 {
  let v = value.clamp(0.0, 1.0);
  let scaled = v * COLOR_LUT_SIZE as f32;
  let idx = scaled.floor() as usize;
  let frac = scaled - idx as f32;
  let next = (idx + 1).min(COLOR_LUT_SIZE);
  let low = lut[idx];
  let high = lut[next];
  low + (high - low) * frac
}

fn srgb_to_linear(value: f32) -> f32 {
  sample_lut(
    value,
    SRGB_TO_LINEAR_LUT.get_or_init(|| {
      build_lut(|x| {
        if x <= 0.04045 {
          x / 12.92
        } else {
          ((x + 0.055) / 1.055).powf(2.4)
        }
      })
    }),
  )
}

fn linear_to_srgb(value: f32) -> f32 {
  sample_lut(
    value,
    LINEAR_TO_SRGB_LUT.get_or_init(|| {
      build_lut(|x| {
        if x <= 0.0031308 {
          12.92 * x
        } else {
          1.055 * x.powf(1.0 / 2.4) - 0.055
        }
      })
    }),
  )
}

fn unpack_color(
  px: PremultipliedColorU8,
  color_space: ColorInterpolationFilters,
) -> UnpremultipliedColor {
  let mut color = to_unpremultiplied(px);
  if matches!(color_space, ColorInterpolationFilters::LinearRGB) {
    color.r = srgb_to_linear(color.r);
    color.g = srgb_to_linear(color.g);
    color.b = srgb_to_linear(color.b);
  }
  color.a = color.a.clamp(0.0, 1.0);
  color
}

fn pack_color(
  color: UnpremultipliedColor,
  color_space: ColorInterpolationFilters,
) -> PremultipliedColorU8 {
  let mut color = color;
  color.a = clamp01(color.a);
  match color_space {
    ColorInterpolationFilters::LinearRGB => {
      color.r = linear_to_srgb(color.r);
      color.g = linear_to_srgb(color.g);
      color.b = linear_to_srgb(color.b);
    }
    ColorInterpolationFilters::SRGB => {}
  }
  to_premultiplied(color)
}

/// Load and parse an SVG filter from a URL (including data URLs), caching the result.
pub fn load_svg_filter(url: &str, image_cache: &ImageCache) -> Option<Arc<SvgFilter>> {
  let resolved = image_cache.resolve_url(url);
  if resolved.is_empty() {
    return None;
  }

  let (resource_url, fragment) = resolved
    .rsplit_once('#')
    .map(|(base, frag)| (base.to_string(), Some(frag.to_string())))
    .unwrap_or((resolved, None));

  if resource_url.is_empty() {
    return None;
  }

  let cache_key = format!("{}#{}", resource_url, fragment.as_deref().unwrap_or(""));
  if let Ok(guard) = filter_cache().lock() {
    if let Some(existing) = guard.get(&cache_key) {
      return Some(existing.clone());
    }
  }

  let resource = image_cache.fetcher().fetch(&resource_url).ok()?;
  let text = String::from_utf8(resource.bytes.clone()).ok()?;
  let mut scoped_cache = image_cache.clone();
  scoped_cache.set_base_url(resource_url.clone());
  let filter = parse_filter_definition(&text, fragment.as_deref(), &scoped_cache)?;

  if let Ok(mut guard) = filter_cache().lock() {
    guard.insert(cache_key, filter.clone());
  }

  Some(filter)
}

/// Parse an SVG filter definition from a raw SVG document string.
///
/// This shares the same parsing logic as [`load_svg_filter`] but assumes the caller already has
/// the SVG markup available (e.g. inline `<svg>` content) and does not perform any fetching or
/// cross-document caching.
pub fn parse_svg_filter_from_svg_document(
  svg: &str,
  fragment: Option<&str>,
  image_cache: &ImageCache,
) -> Option<Arc<SvgFilter>> {
  parse_filter_definition(svg, fragment, image_cache)
}

fn parse_filter_definition(
  svg: &str,
  fragment: Option<&str>,
  image_cache: &ImageCache,
) -> Option<Arc<SvgFilter>> {
  let doc = Document::parse(svg).ok()?;
  let filter_node = doc.descendants().find(|n| {
    n.has_tag_name("filter")
      && fragment
        .map(|id| n.attribute("id").map(|v| v == id).unwrap_or(false))
        .unwrap_or(true)
  })?;

  parse_filter_node(&filter_node, image_cache)
}

fn parse_filter_node(node: &roxmltree::Node, image_cache: &ImageCache) -> Option<Arc<SvgFilter>> {
  let units = match node.attribute("filterUnits") {
    Some(v) if v.eq_ignore_ascii_case("userspaceonuse") => SvgFilterUnits::UserSpaceOnUse,
    _ => SvgFilterUnits::ObjectBoundingBox,
  };
  let primitive_units = match node.attribute("primitiveUnits") {
    Some(v) if v.eq_ignore_ascii_case("objectboundingbox") => SvgFilterUnits::ObjectBoundingBox,
    Some(v) if v.eq_ignore_ascii_case("userspaceonuse") => SvgFilterUnits::UserSpaceOnUse,
    _ => SvgFilterUnits::UserSpaceOnUse,
  };
  let filter_color_interpolation_filters =
    parse_color_interpolation_filters(node.attribute("color-interpolation-filters"))
      .unwrap_or(ColorInterpolationFilters::LinearRGB);
  let default_region = SvgFilterRegion::default_for_units(units);
  let region = SvgFilterRegion {
    x: SvgLength::parse(node.attribute("x"), default_region.x),
    y: SvgLength::parse(node.attribute("y"), default_region.y),
    width: SvgLength::parse(node.attribute("width"), default_region.width),
    height: SvgLength::parse(node.attribute("height"), default_region.height),
    units,
  };
  let primitive_units_coord = match primitive_units {
    SvgFilterUnits::ObjectBoundingBox => SvgCoordinateUnits::ObjectBoundingBox,
    SvgFilterUnits::UserSpaceOnUse => SvgCoordinateUnits::UserSpaceOnUse,
  };

  let mut steps = Vec::new();
  for child in node.children().filter(|c| c.is_element()) {
    let result_name = child.attribute("result").map(|s| s.to_string());
    let color_interpolation_filters =
      parse_color_interpolation_filters(child.attribute("color-interpolation-filters"));
    let tag = child.tag_name().name().to_ascii_lowercase();
    let region_override = {
      let x_attr = child.attribute("x");
      let y_attr = child.attribute("y");
      let width_attr = child.attribute("width");
      let height_attr = child.attribute("height");
      if x_attr.is_some() || y_attr.is_some() || width_attr.is_some() || height_attr.is_some() {
        Some(SvgFilterRegion {
          x: SvgLength::parse(x_attr, region.x),
          y: SvgLength::parse(y_attr, region.y),
          width: SvgLength::parse(width_attr, region.width),
          height: SvgLength::parse(height_attr, region.height),
          units: primitive_units,
        })
      } else {
        None
      }
    };
    let primitive = match tag.as_str() {
      "feflood" => parse_fe_flood(&child),
      "fegaussianblur" => parse_fe_gaussian_blur(&child),
      "feoffset" => parse_fe_offset(&child),
      "fecolormatrix" => parse_fe_color_matrix(&child),
      "fecomposite" => parse_fe_composite(&child),
      "femerge" => parse_fe_merge(&child),
      "fedropshadow" => parse_fe_drop_shadow(&child),
      "feblend" => parse_fe_blend(&child),
      "femorphology" => parse_fe_morphology(&child),
      "fecomponenttransfer" => parse_fe_component_transfer(&child),
      "feimage" => parse_fe_image(&child, image_cache, primitive_units_coord),
      "fetile" => parse_fe_tile(&child),
      "feturbulence" => parse_fe_turbulence(&child),
      "fedisplacementmap" => parse_fe_displacement_map(&child),
      "feconvolvematrix" => parse_fe_convolve_matrix(&child),
      _ => None,
    };
    if let Some(prim) = primitive {
      steps.push(FilterStep {
        result: result_name,
        color_interpolation_filters,
        primitive: prim,
        region: region_override.clone(),
      });
    }
  }

  if steps.is_empty() {
    return None;
  }

  Some(Arc::new(SvgFilter {
    color_interpolation_filters: filter_color_interpolation_filters,
    steps,
    region,
    primitive_units,
  }))
}

impl SvgFilter {
  pub fn resolve_region(&self, bbox: Rect) -> Rect {
    self.region.resolve(bbox)
  }

  fn resolve_primitive_x(&self, value: f32, bbox: &Rect) -> f32 {
    match self.primitive_units {
      SvgFilterUnits::UserSpaceOnUse => value,
      SvgFilterUnits::ObjectBoundingBox => value * bbox.width().abs(),
    }
  }

  fn resolve_primitive_y(&self, value: f32, bbox: &Rect) -> f32 {
    match self.primitive_units {
      SvgFilterUnits::UserSpaceOnUse => value,
      SvgFilterUnits::ObjectBoundingBox => value * bbox.height().abs(),
    }
  }

  fn resolve_primitive_scalar(&self, value: f32, bbox: &Rect) -> f32 {
    match self.primitive_units {
      SvgFilterUnits::UserSpaceOnUse => value,
      SvgFilterUnits::ObjectBoundingBox => value * (bbox.width().abs() + bbox.height().abs()) * 0.5,
    }
  }

  fn resolve_primitive_pair(&self, values: (f32, f32), bbox: &Rect) -> (f32, f32) {
    if (values.0 - values.1).abs() < f32::EPSILON {
      let scalar = self.resolve_primitive_scalar(values.0, bbox);
      return (scalar, scalar);
    }
    match self.primitive_units {
      SvgFilterUnits::UserSpaceOnUse => values,
      SvgFilterUnits::ObjectBoundingBox => (
        self.resolve_primitive_x(values.0, bbox),
        self.resolve_primitive_y(values.1, bbox),
      ),
    }
  }
}

pub(crate) fn collect_svg_filters(
  svg: &str,
  image_cache: &ImageCache,
) -> HashMap<String, Arc<SvgFilter>> {
  let mut registry = HashMap::new();
  let Ok(doc) = Document::parse(svg) else {
    return registry;
  };

  for filter in doc.descendants().filter(|n| n.has_tag_name("filter")) {
    let Some(id) = filter.attribute("id") else {
      continue;
    };
    if id.trim().is_empty() {
      continue;
    }
    if let Some(parsed) = parse_filter_node(&filter, image_cache) {
      registry.entry(id.to_string()).or_insert(parsed);
    }
  }

  registry
}

/// Resolves SVG filter references against a document-level registry and fragment tree.
///
/// This resolver prioritizes serialized definitions collected from the DOM (which include display-
/// none SVGs) before falling back to scanning laid-out fragments for inline SVG content. Results
/// are cached per-instance to avoid reparsing filters referenced multiple times.
pub struct SvgFilterResolver<'a> {
  svg_defs: Option<Arc<HashMap<String, String>>>,
  fragment_roots: Vec<&'a FragmentNode>,
  image_cache: Option<&'a ImageCache>,
  cache: HashMap<String, Arc<SvgFilter>>,
}

impl<'a> SvgFilterResolver<'a> {
  /// Creates a new resolver.
  pub fn new(
    svg_defs: Option<Arc<HashMap<String, String>>>,
    fragment_roots: Vec<&'a FragmentNode>,
    image_cache: Option<&'a ImageCache>,
  ) -> Self {
    Self {
      svg_defs,
      fragment_roots,
      image_cache,
      cache: HashMap::new(),
    }
  }

  /// Resolves the given URL into a parsed SVG filter, caching the result.
  pub fn resolve(&mut self, url: &str) -> Option<Arc<SvgFilter>> {
    if let Some(existing) = self.cache.get(url) {
      return Some(existing.clone());
    }

    let resolved = self.resolve_uncached(url);
    if let Some(filter) = resolved.as_ref() {
      self.cache.insert(url.to_string(), filter.clone());
    }
    resolved
  }

  fn resolve_uncached(&self, url: &str) -> Option<Arc<SvgFilter>> {
    let cache = self.image_cache?;
    let trimmed = url.trim();
    if trimmed.is_empty() {
      return None;
    }

    if let Some(id) = trimmed.strip_prefix('#') {
      if let Some(defs) = &self.svg_defs {
        if let Some(serialized) = defs.get(id) {
          if let Some(filter) = parse_filter_definition(serialized, Some(id), cache) {
            return Some(filter);
          }
        }
      }

      for root in &self.fragment_roots {
        for fragment in root.iter_fragments() {
          if let FragmentContent::Replaced { replaced_type, .. } = &fragment.content {
            if let ReplacedType::Svg { content } = replaced_type {
              if let Some(filter) = parse_filter_definition(&content.svg, Some(id), cache) {
                return Some(filter);
              }
            }
          }
        }
      }

      None
    } else {
      load_svg_filter(trimmed, cache)
    }
  }
}

fn parse_input(attr: Option<&str>) -> FilterInput {
  match attr.map(|s| s.trim()) {
    Some(v) if v.eq_ignore_ascii_case("backgroundimage") => FilterInput::BackgroundImage,
    Some(v) if v.eq_ignore_ascii_case("backgroundalpha") => FilterInput::BackgroundAlpha,
    Some(v) if v.eq_ignore_ascii_case("fillpaint") => FilterInput::FillPaint,
    Some(v) if v.eq_ignore_ascii_case("strokepaint") => FilterInput::StrokePaint,
    Some(v) if v.eq_ignore_ascii_case("sourcealpha") => FilterInput::SourceAlpha,
    Some(v) if v.eq_ignore_ascii_case("sourcegraphic") => FilterInput::SourceGraphic,
    Some(v) if !v.is_empty() => FilterInput::Reference(v.to_string()),
    _ => FilterInput::Previous,
  }
}

fn parse_number(value: Option<&str>) -> f32 {
  value
    .and_then(|v| v.split_whitespace().next())
    .and_then(|v| v.parse::<f32>().ok())
    .unwrap_or(0.0)
}

fn parse_number_list(value: Option<&str>) -> Vec<f32> {
  value
    .unwrap_or("")
    .split(|c: char| c.is_whitespace() || c == ',')
    .filter_map(|v| {
      let trimmed = v.trim();
      if trimmed.is_empty() {
        None
      } else {
        trimmed.parse::<f32>().ok()
      }
    })
    .collect()
}

fn parse_number_pair(value: Option<&str>) -> (f32, f32) {
  let mut iter = parse_number_list(value).into_iter();
  let first = iter.next().unwrap_or(0.0);
  let second = iter.next().unwrap_or(first);
  (first, second)
}

fn parse_color(value: Option<&str>) -> Option<Rgba> {
  let raw = value?.trim();
  if let Ok(parsed) = color::Color::parse(raw) {
    return Some(parsed.to_rgba(Rgba::BLACK));
  }
  None
}

fn parse_color_interpolation_filters(value: Option<&str>) -> Option<ColorInterpolationFilters> {
  match value.map(|s| s.trim().to_ascii_lowercase()) {
    Some(v) if v == "srgb" => Some(ColorInterpolationFilters::SRGB),
    Some(v) if v == "linearrgb" => Some(ColorInterpolationFilters::LinearRGB),
    _ => None,
  }
}

fn parse_channel_selector(value: Option<&str>) -> ChannelSelector {
  match value.map(|v| v.trim().to_ascii_lowercase()) {
    Some(v) if v == "r" => ChannelSelector::R,
    Some(v) if v == "g" => ChannelSelector::G,
    Some(v) if v == "b" => ChannelSelector::B,
    Some(v) if v == "a" => ChannelSelector::A,
    _ => ChannelSelector::A,
  }
}

fn parse_fe_flood(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let color = parse_color(node.attribute("flood-color")).unwrap_or(Rgba::BLACK);
  let opacity = node
    .attribute("flood-opacity")
    .and_then(|v| v.parse::<f32>().ok())
    .unwrap_or(1.0)
    .clamp(0.0, 1.0);
  Some(FilterPrimitive::Flood { color, opacity })
}

fn parse_fe_gaussian_blur(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  let (sx, sy) = parse_number_pair(node.attribute("stdDeviation"));
  let std_dev = (sx.abs(), sy.abs());
  Some(FilterPrimitive::GaussianBlur { input, std_dev })
}

fn parse_fe_offset(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  let dx = parse_number(node.attribute("dx"));
  let dy = parse_number(node.attribute("dy"));
  Some(FilterPrimitive::Offset { input, dx, dy })
}

fn parse_fe_color_matrix(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  let kind_attr = node.attribute("type").unwrap_or("matrix");
  let kind = match kind_attr.to_ascii_lowercase().as_str() {
    "saturate" => {
      let amount = parse_number(node.attribute("values")).clamp(0.0, 1.0);
      ColorMatrixKind::Saturate(amount)
    }
    "huerotate" => {
      let angle = parse_number(node.attribute("values"));
      ColorMatrixKind::HueRotate(angle)
    }
    "luminancetoalpha" => ColorMatrixKind::LuminanceToAlpha,
    _ => {
      let values: Vec<f32> = node
        .attribute("values")
        .unwrap_or("")
        .split_whitespace()
        .filter_map(|v| v.parse::<f32>().ok())
        .collect();
      if values.len() >= 20 {
        let mut arr = [0.0; 20];
        arr.copy_from_slice(&values[..20]);
        ColorMatrixKind::Matrix(arr)
      } else {
        ColorMatrixKind::Matrix([
          1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0,
          1.0, 0.0,
        ])
      }
    }
  };
  Some(FilterPrimitive::ColorMatrix { input, kind })
}

fn parse_fe_blend(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input1 = parse_input(node.attribute("in"));
  let input2 = parse_input(node.attribute("in2"));
  let mode_attr = node
    .attribute("mode")
    .unwrap_or("normal")
    .to_ascii_lowercase();
  let mode = match mode_attr.as_str() {
    "multiply" => BlendMode::Multiply,
    "screen" => BlendMode::Screen,
    "darken" => BlendMode::Darken,
    "lighten" => BlendMode::Lighten,
    _ => BlendMode::SourceOver,
  };
  Some(FilterPrimitive::Blend {
    input1,
    input2,
    mode,
  })
}

fn parse_fe_morphology(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  let op = match node
    .attribute("operator")
    .map(|s| s.eq_ignore_ascii_case("dilate"))
    .unwrap_or(false)
  {
    true => MorphologyOp::Dilate,
    false => MorphologyOp::Erode,
  };
  let (rx, ry) = parse_number_pair(node.attribute("radius"));
  let radius = (rx.abs(), ry.abs());
  Some(FilterPrimitive::Morphology { input, radius, op })
}

fn parse_fe_component_transfer(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  let mut r = TransferFn::Identity;
  let mut g = TransferFn::Identity;
  let mut b = TransferFn::Identity;
  let mut a = TransferFn::Identity;

  for child in node.children().filter(|c| c.is_element()) {
    let func = parse_transfer_fn(&child).unwrap_or(TransferFn::Identity);
    let name = child.tag_name().name().to_ascii_lowercase();
    match name.as_str() {
      "fefuncr" => r = func,
      "fefuncg" => g = func,
      "fefuncb" => b = func,
      "fefunca" => a = func,
      _ => {}
    }
  }

  Some(FilterPrimitive::ComponentTransfer { input, r, g, b, a })
}

fn parse_transfer_fn(node: &roxmltree::Node) -> Option<TransferFn> {
  let ty = node
    .attribute("type")
    .unwrap_or("identity")
    .to_ascii_lowercase();
  let parse_or_default = |name: &str, default: f32| -> Option<f32> {
    match node.attribute(name) {
      Some(raw) => raw.parse::<f32>().ok(),
      None => Some(default),
    }
  };
  let parse_table_values = || {
    node
      .attribute("tableValues")
      .unwrap_or("")
      .split(|c: char| c.is_ascii_whitespace() || c == ',')
      .filter(|s| !s.is_empty())
      .filter_map(|v| v.parse::<f32>().ok())
      .collect::<Vec<f32>>()
  };
  match ty.as_str() {
    "linear" => {
      let slope = parse_or_default("slope", 1.0)?;
      let intercept = parse_or_default("intercept", 0.0)?;
      Some(TransferFn::Linear { slope, intercept })
    }
    "gamma" => {
      let exponent = node
        .attribute("exponent")
        .and_then(|v| v.parse::<f32>().ok())?;
      let amplitude = parse_or_default("amplitude", 1.0)?;
      let offset = parse_or_default("offset", 0.0)?;
      Some(TransferFn::Gamma {
        amplitude,
        exponent,
        offset,
      })
    }
    "table" => {
      let values = parse_table_values();
      if values.is_empty() {
        Some(TransferFn::Identity)
      } else {
        Some(TransferFn::Table { values })
      }
    }
    "discrete" => {
      let values = parse_table_values();
      if values.is_empty() {
        Some(TransferFn::Identity)
      } else {
        Some(TransferFn::Discrete { values })
      }
    }
    _ => Some(TransferFn::Identity),
  }
}

fn parse_fe_composite(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input1 = parse_input(node.attribute("in"));
  let input2 = parse_input(node.attribute("in2"));
  let operator_attr = node
    .attribute("operator")
    .unwrap_or("over")
    .to_ascii_lowercase();
  let operator = match operator_attr.as_str() {
    "in" => CompositeOperator::In,
    "out" => CompositeOperator::Out,
    "atop" => CompositeOperator::Atop,
    "xor" => CompositeOperator::Xor,
    "arithmetic" => CompositeOperator::Arithmetic {
      k1: parse_number(node.attribute("k1")),
      k2: parse_number(node.attribute("k2")),
      k3: parse_number(node.attribute("k3")),
      k4: parse_number(node.attribute("k4")),
    },
    _ => CompositeOperator::Over,
  };
  Some(FilterPrimitive::Composite {
    input1,
    input2,
    operator,
  })
}

fn parse_fe_merge(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let mut inputs = Vec::new();
  for child in node.children().filter(|c| c.is_element()) {
    if child.tag_name().name().eq_ignore_ascii_case("femergenode") {
      inputs.push(parse_input(child.attribute("in")));
    }
  }
  if inputs.is_empty() {
    inputs.push(FilterInput::Previous);
  }
  Some(FilterPrimitive::Merge { inputs })
}

fn parse_fe_drop_shadow(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  let dx = parse_number(node.attribute("dx"));
  let dy = parse_number(node.attribute("dy"));
  let (sx, sy) = parse_number_pair(node.attribute("stdDeviation"));
  let std_dev = (sx.abs(), sy.abs());
  let color = parse_color(node.attribute("flood-color")).unwrap_or(Rgba::BLACK);
  let opacity = node
    .attribute("flood-opacity")
    .and_then(|v| v.parse::<f32>().ok())
    .unwrap_or(1.0)
    .clamp(0.0, 1.0);
  Some(FilterPrimitive::DropShadow {
    input,
    dx,
    dy,
    std_dev,
    color,
    opacity,
  })
}

fn parse_fe_image(
  node: &roxmltree::Node,
  cache: &ImageCache,
  units: SvgCoordinateUnits,
) -> Option<FilterPrimitive> {
  let href = node
    .attribute("href")
    .or_else(|| node.attribute("xlink:href"))?;
  let loaded = cache.load(href).ok()?;
  let dyn_img = loaded.image.as_ref();
  let rgba = dyn_img.to_rgba8();
  let (w, h) = rgba.dimensions();
  let size = tiny_skia::IntSize::from_wh(w, h)?;
  let pixmap = Pixmap::from_vec(rgba.into_raw(), size)?;
  let x = SvgLength::parse(node.attribute("x"), SvgLength::Percent(0.0));
  let y = SvgLength::parse(node.attribute("y"), SvgLength::Percent(0.0));
  let width = SvgLength::parse(node.attribute("width"), SvgLength::Percent(1.0));
  let height = SvgLength::parse(node.attribute("height"), SvgLength::Percent(1.0));
  let preserve_aspect_ratio = PreserveAspectRatio::parse(node.attribute("preserveAspectRatio"));
  Some(FilterPrimitive::Image(ImagePrimitive {
    pixmap,
    x,
    y,
    width,
    height,
    preserve_aspect_ratio,
    units,
  }))
}

fn parse_fe_tile(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  Some(FilterPrimitive::Tile { input })
}

fn parse_fe_turbulence(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let base_freq_values = parse_number_list(node.attribute("baseFrequency"));
  let fx = base_freq_values.get(0).copied().unwrap_or(0.05).abs();
  let fy = base_freq_values.get(1).copied().unwrap_or(fx).abs();
  let seed_raw = node
    .attribute("seed")
    .and_then(|v| v.parse::<f32>().ok())
    .unwrap_or(0.0)
    .round();
  let seed = if seed_raw < 0.0 { 0 } else { seed_raw as u32 };
  let octaves = node
    .attribute("numOctaves")
    .and_then(|v| v.parse::<u32>().ok())
    .unwrap_or(1)
    .clamp(1, MAX_TURBULENCE_OCTAVES);
  let stitch_tiles = node
    .attribute("stitchTiles")
    .map(|v| {
      let v = v.trim().to_ascii_lowercase();
      v == "stitch" || v == "true" || v == "1"
    })
    .unwrap_or(false);
  let kind = match node
    .attribute("type")
    .unwrap_or("turbulence")
    .to_ascii_lowercase()
    .as_str()
  {
    "fractalnoise" => TurbulenceType::FractalNoise,
    _ => TurbulenceType::Turbulence,
  };

  Some(FilterPrimitive::Turbulence {
    base_frequency: (fx, fy),
    seed,
    octaves,
    stitch_tiles,
    kind,
  })
}

fn parse_fe_displacement_map(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let in1 = parse_input(node.attribute("in"));
  let in2 = parse_input(node.attribute("in2"));
  let scale = parse_number(node.attribute("scale"));
  let x_channel = parse_channel_selector(node.attribute("xChannelSelector"));
  let y_channel = parse_channel_selector(node.attribute("yChannelSelector"));
  Some(FilterPrimitive::DisplacementMap {
    in1,
    in2,
    scale,
    x_channel,
    y_channel,
  })
}

fn parse_fe_convolve_matrix(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  let order = parse_number_list(node.attribute("order"));
  let order_x = order.get(0).copied().unwrap_or(3.0).floor().max(1.0) as usize;
  let order_y = order
    .get(1)
    .copied()
    .unwrap_or(order_x as f32)
    .floor()
    .max(1.0) as usize;
  let kernel = parse_number_list(node.attribute("kernelMatrix"));
  if kernel.len() != order_x * order_y {
    return None;
  }
  let divisor = node
    .attribute("divisor")
    .and_then(|v| v.parse::<f32>().ok());
  let bias = node
    .attribute("bias")
    .and_then(|v| v.parse::<f32>().ok())
    .unwrap_or(0.0);
  let target_x = node
    .attribute("targetX")
    .and_then(|v| v.parse::<i32>().ok())
    .unwrap_or((order_x / 2) as i32)
    .clamp(0, order_x.saturating_sub(1) as i32);
  let target_y = node
    .attribute("targetY")
    .and_then(|v| v.parse::<i32>().ok())
    .unwrap_or((order_y / 2) as i32)
    .clamp(0, order_y.saturating_sub(1) as i32);
  let edge_mode = match node
    .attribute("edgeMode")
    .unwrap_or("duplicate")
    .to_ascii_lowercase()
    .as_str()
  {
    "none" => EdgeMode::None,
    "wrap" => EdgeMode::Wrap,
    _ => EdgeMode::Duplicate,
  };
  let preserve_alpha = node
    .attribute("preserveAlpha")
    .map(|v| {
      let lower = v.trim().to_ascii_lowercase();
      lower == "true" || lower == "1"
    })
    .unwrap_or(false);
  let subregion = match (
    node.attribute("x").and_then(|v| v.parse::<f32>().ok()),
    node.attribute("y").and_then(|v| v.parse::<f32>().ok()),
    node.attribute("width").and_then(|v| v.parse::<f32>().ok()),
    node.attribute("height").and_then(|v| v.parse::<f32>().ok()),
  ) {
    (Some(x), Some(y), Some(w), Some(h)) => Some((x, y, w, h)),
    _ => None,
  };

  Some(FilterPrimitive::ConvolveMatrix {
    input,
    order_x,
    order_y,
    kernel,
    divisor,
    bias,
    target_x,
    target_y,
    edge_mode,
    preserve_alpha,
    subregion,
  })
}

/// Applies an SVG filter definition to the given pixmap.
///
/// `scale` should be the device pixel ratio so that numeric parameters expressed in CSS pixels
/// (e.g. stdDeviation, dx/dy, radius) are interpreted in device pixels.
pub fn apply_svg_filter(def: &SvgFilter, pixmap: &mut Pixmap, scale: f32, bbox: Rect) {
  let scale = if scale.is_finite() && scale > 0.0 {
    scale
  } else {
    1.0
  };

  let css_bbox = Rect::from_xywh(
    bbox.x() / scale,
    bbox.y() / scale,
    bbox.width() / scale,
    bbox.height() / scale,
  );
  let css_region = def.resolve_region(css_bbox);
  let filter_region = Rect::from_xywh(
    css_region.x() * scale,
    css_region.y() * scale,
    css_region.width() * scale,
    css_region.height() * scale,
  );
  if !filter_region.x().is_finite()
    || !filter_region.y().is_finite()
    || !filter_region.width().is_finite()
    || !filter_region.height().is_finite()
    || filter_region.width() <= 0.0
    || filter_region.height() <= 0.0
  {
    for px in pixmap.pixels_mut() {
      *px = PremultipliedColorU8::TRANSPARENT;
    }
    return;
  }
  let default_primitive_region = SvgFilterRegion {
    x: def.region.x,
    y: def.region.y,
    width: def.region.width,
    height: def.region.height,
    units: def.primitive_units,
  };

  let source_region = clip_region(filter_region_for_pixmap(pixmap), filter_region);
  let source = FilterResult::new(pixmap.clone(), source_region, filter_region);
  let mut results: HashMap<String, FilterResult> = HashMap::new();
  let mut current = source.clone();

  for step in &def.steps {
    let color_interpolation_filters = step
      .color_interpolation_filters
      .unwrap_or(def.color_interpolation_filters);
    let primitive_region_spec = step.region.as_ref().unwrap_or(&default_primitive_region);
    let css_prim_region = primitive_region_spec.resolve(css_bbox);
    let mut primitive_region = Rect::from_xywh(
      css_prim_region.x() * scale,
      css_prim_region.y() * scale,
      css_prim_region.width() * scale,
      css_prim_region.height() * scale,
    );
    primitive_region = clip_region(primitive_region, filter_region);
    let primitive_region_valid = primitive_region.x().is_finite()
      && primitive_region.y().is_finite()
      && primitive_region.width().is_finite()
      && primitive_region.height().is_finite()
      && primitive_region.width() > 0.0
      && primitive_region.height() > 0.0;
    if !primitive_region_valid {
      let mut out = match Pixmap::new(source.pixmap.width(), source.pixmap.height()) {
        Some(p) => p,
        None => continue,
      };
      clip_to_region(&mut out, Rect::ZERO);
      let next = FilterResult {
        pixmap: out,
        region: Rect::ZERO,
      };
      if let Some(name) = &step.result {
        results.insert(name.clone(), next.clone());
      }
      current = next;
      continue;
    }

    if let Some(mut next) = apply_primitive(
      def,
      &css_bbox,
      &step.primitive,
      &source,
      &results,
      &current,
      scale,
      primitive_region,
      color_interpolation_filters,
    ) {
      next.region = clip_region(next.region, primitive_region);
      clip_to_region(&mut next.pixmap, primitive_region);
      if let Some(name) = &step.result {
        results.insert(name.clone(), next.clone());
      }
      current = next;
    }
  }

  *pixmap = current.pixmap;
  clip_to_region(pixmap, filter_region);
}

fn clip_to_region(pixmap: &mut Pixmap, region: Rect) {
  let width = pixmap.width() as i32;
  let height = pixmap.height() as i32;
  if width == 0 || height == 0 {
    return;
  }

  let min_x = region.min_x().floor() as i32;
  let min_y = region.min_y().floor() as i32;
  let max_x = region.max_x().ceil() as i32;
  let max_y = region.max_y().ceil() as i32;

  let clamped_min_x = min_x.clamp(0, width);
  let clamped_min_y = min_y.clamp(0, height);
  let clamped_max_x = max_x.clamp(0, width);
  let clamped_max_y = max_y.clamp(0, height);

  if clamped_min_x == 0 && clamped_min_y == 0 && clamped_max_x == width && clamped_max_y == height {
    return;
  }

  let row_stride = pixmap.width() as usize;
  for (y, row) in pixmap.pixels_mut().chunks_mut(row_stride).enumerate() {
    let y = y as i32;
    if y < clamped_min_y || y >= clamped_max_y {
      for px in row {
        *px = PremultipliedColorU8::TRANSPARENT;
      }
      continue;
    }

    for (x, px) in row.iter_mut().enumerate() {
      let x = x as i32;
      if x < clamped_min_x || x >= clamped_max_x {
        *px = PremultipliedColorU8::TRANSPARENT;
      }
    }
  }
}

fn apply_primitive(
  filter: &SvgFilter,
  css_bbox: &Rect,
  primitive: &FilterPrimitive,
  source: &FilterResult,
  results: &HashMap<String, FilterResult>,
  current: &FilterResult,
  scale: f32,
  filter_region: Rect,
  color_interpolation_filters: ColorInterpolationFilters,
) -> Option<FilterResult> {
  match primitive {
    FilterPrimitive::Flood { color, opacity } => flood(
      source.pixmap.width(),
      source.pixmap.height(),
      color,
      *opacity,
    )
    .map(|pixmap| FilterResult::full_region(pixmap, filter_region)),
    FilterPrimitive::GaussianBlur { input, std_dev } => {
      let mut img = resolve_input(input, source, results, current, filter_region)?;
      let (sx, sy) = filter.resolve_primitive_pair(*std_dev, css_bbox);
      let sigma_x = sx * scale;
      let sigma_y = sy * scale;
      if sigma_x != 0.0 || sigma_y != 0.0 {
        apply_gaussian_blur_anisotropic(&mut img.pixmap, sigma_x, sigma_y);
        let expanded = inflate_rect_xy(img.region, sigma_x.abs() * 3.0, sigma_y.abs() * 3.0);
        img.region = clip_region(expanded, filter_region);
      }
      Some(img)
    }
    FilterPrimitive::Offset { input, dx, dy } => {
      let dx = filter.resolve_primitive_x(*dx, css_bbox) * scale;
      let dy = filter.resolve_primitive_y(*dy, css_bbox) * scale;
      resolve_input(input, source, results, current, filter_region)
        .map(|img| offset_result(img, dx, dy, filter_region))
    }
    FilterPrimitive::ColorMatrix { input, kind } => {
      let mut img = resolve_input(input, source, results, current, filter_region)?;
      apply_color_matrix(&mut img.pixmap, kind, color_interpolation_filters);
      Some(img)
    }
    FilterPrimitive::Composite {
      input1,
      input2,
      operator,
    } => composite_pixmaps(
      resolve_input(input1, source, results, current, filter_region),
      resolve_input(input2, source, results, current, filter_region),
      *operator,
      filter_region,
    ),
    FilterPrimitive::Merge { inputs } => Some(merge_inputs(
      inputs,
      source,
      results,
      current,
      filter_region,
    )),
    FilterPrimitive::DropShadow {
      input,
      dx,
      dy,
      std_dev,
      color,
      opacity,
    } => resolve_input(input, source, results, current, filter_region).map(|img| {
      let dx = filter.resolve_primitive_x(*dx, css_bbox) * scale;
      let dy = filter.resolve_primitive_y(*dy, css_bbox) * scale;
      let std_dev = filter.resolve_primitive_pair(*std_dev, css_bbox);
      let std_dev = (std_dev.0 * scale, std_dev.1 * scale);
      drop_shadow_pixmap(img, dx, dy, std_dev, color, *opacity, filter_region)
    }),
    FilterPrimitive::Blend {
      input1,
      input2,
      mode,
    } => blend_pixmaps(
      resolve_input(input1, source, results, current, filter_region),
      resolve_input(input2, source, results, current, filter_region),
      *mode,
      filter_region,
    ),
    FilterPrimitive::Morphology { input, radius, op } => {
      resolve_input(input, source, results, current, filter_region).map(|mut img| {
        let radius = filter.resolve_primitive_pair(*radius, css_bbox);
        let radius = (radius.0 * scale, radius.1 * scale);
        apply_morphology(&mut img.pixmap, radius, *op);
        img.region = clip_region(
          match op {
            MorphologyOp::Dilate => inflate_rect_xy(img.region, radius.0, radius.1),
            MorphologyOp::Erode => inflate_rect_xy(img.region, -radius.0, -radius.1),
          },
          filter_region,
        );
        img
      })
    }
    FilterPrimitive::ComponentTransfer { input, r, g, b, a } => {
      resolve_input(input, source, results, current, filter_region).map(|mut img| {
        apply_component_transfer(&mut img.pixmap, r, g, b, a, color_interpolation_filters);
        img
      })
    }
    FilterPrimitive::Image(prim) => render_fe_image(prim, filter_region),
    FilterPrimitive::Tile { input } => {
      resolve_input(input, source, results, current, filter_region)
        .and_then(|img| tile_pixmap(img, filter_region))
    }
    FilterPrimitive::Turbulence {
      base_frequency,
      seed,
      octaves,
      stitch_tiles,
      kind,
    } => {
      let pixmap = turbulence::render_turbulence(
        source.pixmap.width(),
        source.pixmap.height(),
        filter_region,
        *base_frequency,
        *seed,
        *octaves,
        *stitch_tiles,
        *kind,
      )?;
      Some(FilterResult::full_region(pixmap, filter_region))
    }
    FilterPrimitive::DisplacementMap {
      in1,
      in2,
      scale: disp_scale,
      x_channel,
      y_channel,
    } => {
      let primary = resolve_input(in1, source, results, current, filter_region)?;
      let map = resolve_input(in2, source, results, current, filter_region)?;
      let output = apply_displacement_map(
        &primary.pixmap,
        &map.pixmap,
        filter.resolve_primitive_scalar(*disp_scale, css_bbox) * scale,
        *x_channel,
        *y_channel,
      )?;
      let region = clip_region(primary.region.union(map.region), filter_region);
      Some(FilterResult::new(output, region, filter_region))
    }
    FilterPrimitive::ConvolveMatrix {
      input,
      order_x,
      order_y,
      kernel,
      divisor,
      bias,
      target_x,
      target_y,
      edge_mode,
      preserve_alpha,
      subregion,
    } => resolve_input(input, source, results, current, filter_region).map(|img| {
      let output = apply_convolve_matrix(
        img.pixmap,
        *order_x,
        *order_y,
        kernel,
        *divisor,
        *bias,
        *target_x,
        *target_y,
        *edge_mode,
        *preserve_alpha,
        *subregion,
      );
      FilterResult::new(output, img.region, filter_region)
    }),
  }
}

fn resolve_input(
  input: &FilterInput,
  source: &FilterResult,
  results: &HashMap<String, FilterResult>,
  current: &FilterResult,
  filter_region: Rect,
) -> Option<FilterResult> {
  match input {
    FilterInput::SourceGraphic => Some(source.clone()),
    FilterInput::SourceAlpha => {
      let mut mask = Pixmap::new(source.pixmap.width(), source.pixmap.height())?;
      for (dst, src) in mask
        .pixels_mut()
        .iter_mut()
        .zip(source.pixmap.pixels().iter())
      {
        let alpha = src.alpha();
        *dst = PremultipliedColorU8::from_rgba(alpha, alpha, alpha, alpha)
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      }
      Some(FilterResult::new(mask, source.region, filter_region))
    }
    FilterInput::BackgroundImage | FilterInput::BackgroundAlpha => {
      transparent_result(filter_region)
    }
    FilterInput::FillPaint | FilterInput::StrokePaint => transparent_result(filter_region),
    FilterInput::Reference(name) => results
      .get(name)
      .cloned()
      .or_else(|| transparent_result(filter_region)),
    FilterInput::Previous => Some(current.clone()),
  }
}

fn render_fe_image(prim: &ImagePrimitive, filter_region: Rect) -> Option<FilterResult> {
  let canvas_width = filter_region.width().max(0.0).round() as u32;
  let canvas_height = filter_region.height().max(0.0).round() as u32;
  let mut out = Pixmap::new(canvas_width.max(1), canvas_height.max(1))?;
  let src_w = prim.pixmap.width() as f32;
  let src_h = prim.pixmap.height() as f32;
  if src_w == 0.0 || src_h == 0.0 {
    return Some(FilterResult::full_region(out, filter_region));
  }

  let dest_w = prim.width.resolve(prim.units, canvas_width as f32);
  let dest_h = prim.height.resolve(prim.units, canvas_height as f32);
  if dest_w <= 0.0 || dest_h <= 0.0 || !dest_w.is_finite() || !dest_h.is_finite() {
    return Some(FilterResult::full_region(out, filter_region));
  }

  let dest_x = prim.x.resolve(prim.units, canvas_width as f32);
  let dest_y = prim.y.resolve(prim.units, canvas_height as f32);

  let mut paint = PixmapPaint::default();
  paint.blend_mode = tiny_skia::BlendMode::SourceOver;
  paint.quality = FilterQuality::Nearest;

  let dest_region = Rect::from_xywh(dest_x, dest_y, dest_w, dest_h);

  match prim.preserve_aspect_ratio {
    PreserveAspectRatio::None => {
      let scale_x = dest_w / src_w;
      let scale_y = dest_h / src_h;
      if !scale_x.is_finite() || !scale_y.is_finite() {
        return Some(FilterResult::full_region(out, filter_region));
      }
      let transform = Transform::from_row(scale_x, 0.0, 0.0, scale_y, dest_x, dest_y);
      out.draw_pixmap(0, 0, prim.pixmap.as_ref(), &paint, transform, None);
    }
    PreserveAspectRatio::XMidYMidMeet => {
      let scale = (dest_w / src_w).min(dest_h / src_h);
      if !scale.is_finite() || scale <= 0.0 {
        return Some(FilterResult::full_region(out, filter_region));
      }
      let scaled_w = src_w * scale;
      let scaled_h = src_h * scale;
      let offset_x = dest_x + (dest_w - scaled_w) * 0.5;
      let offset_y = dest_y + (dest_h - scaled_h) * 0.5;
      let transform = Transform::from_row(scale, 0.0, 0.0, scale, offset_x, offset_y);
      out.draw_pixmap(0, 0, prim.pixmap.as_ref(), &paint, transform, None);
    }
  }

  Some(FilterResult::new(out, dest_region, filter_region))
}

fn flood(width: u32, height: u32, color: &Rgba, opacity: f32) -> Option<Pixmap> {
  let mut pixmap = Pixmap::new(width, height)?;
  let alpha = (color.a * opacity).clamp(0.0, 1.0);
  let paint = tiny_skia::Color::from_rgba8(color.r, color.g, color.b, (alpha * 255.0) as u8);
  pixmap.fill(paint);
  Some(pixmap)
}

fn offset_pixmap(input: Pixmap, dx: f32, dy: f32) -> Pixmap {
  let mut out = Pixmap::new(input.width(), input.height()).unwrap();
  let x = dx.round() as i32;
  let y = dy.round() as i32;
  let mut paint = PixmapPaint::default();
  paint.blend_mode = tiny_skia::BlendMode::SourceOver;
  out.draw_pixmap(x, y, input.as_ref(), &paint, Transform::identity(), None);
  out
}

fn offset_result(input: FilterResult, dx: f32, dy: f32, filter_region: Rect) -> FilterResult {
  let pixmap = offset_pixmap(input.pixmap, dx, dy);
  let offset = Point::new(dx.round(), dy.round());
  let region = clip_region(input.region.translate(offset), filter_region);
  FilterResult { pixmap, region }
}

fn composite_pixmaps(
  input1: Option<FilterResult>,
  input2: Option<FilterResult>,
  op: CompositeOperator,
  filter_region: Rect,
) -> Option<FilterResult> {
  let a = input1?;
  let b = input2.unwrap_or_else(|| a.clone());
  if a.pixmap.width() != b.pixmap.width() || a.pixmap.height() != b.pixmap.height() {
    return None;
  }

  let pixmap = match op {
    CompositeOperator::Arithmetic { k1, k2, k3, k4 } => {
      arithmetic_composite(&a.pixmap, &b.pixmap, k1, k2, k3, k4)?
    }
    CompositeOperator::Over => {
      composite_porter_duff(&a.pixmap, &b.pixmap, |a_a, _| (1.0, 1.0 - a_a))?
    }
    CompositeOperator::In => composite_porter_duff(&a.pixmap, &b.pixmap, |_, b_a| (b_a, 0.0))?,
    CompositeOperator::Out => {
      composite_porter_duff(&a.pixmap, &b.pixmap, |_, b_a| (1.0 - b_a, 0.0))?
    }
    CompositeOperator::Atop => {
      composite_porter_duff(&a.pixmap, &b.pixmap, |a_a, b_a| (b_a, 1.0 - a_a))?
    }
    CompositeOperator::Xor => composite_porter_duff(&a.pixmap, &b.pixmap, |a_a, b_a| {
      (1.0 - b_a, 1.0 - a_a)
    })?,
  };

  let region = match op {
    CompositeOperator::In => clip_region(
      a.region.intersection(b.region).unwrap_or(Rect::ZERO),
      filter_region,
    ),
    CompositeOperator::Out => clip_region(a.region, filter_region),
    _ => clip_region(a.region.union(b.region), filter_region),
  };
  Some(FilterResult { pixmap, region })
}

fn blend_pixmaps(
  a: Option<FilterResult>,
  b: Option<FilterResult>,
  mode: BlendMode,
  filter_region: Rect,
) -> Option<FilterResult> {
  let mut base = a?;
  let top = b.unwrap_or_else(|| base.clone());
  let mut paint = PixmapPaint::default();
  paint.blend_mode = mode;
  base.pixmap.draw_pixmap(
    0,
    0,
    top.pixmap.as_ref(),
    &paint,
    Transform::identity(),
    None,
  );
  let region = clip_region(base.region.union(top.region), filter_region);
  Some(FilterResult {
    pixmap: base.pixmap,
    region,
  })
}

fn merge_inputs(
  inputs: &[FilterInput],
  source: &FilterResult,
  results: &HashMap<String, FilterResult>,
  current: &FilterResult,
  filter_region: Rect,
) -> FilterResult {
  let mut out = Pixmap::new(source.pixmap.width(), source.pixmap.height()).unwrap();
  let mut region = Rect::ZERO;
  let mut seen_any = false;
  let mut paint = PixmapPaint::default();
  paint.blend_mode = tiny_skia::BlendMode::SourceOver;

  for input in inputs {
    if let Some(img) = resolve_input(input, source, results, current, filter_region) {
      out.draw_pixmap(
        0,
        0,
        img.pixmap.as_ref(),
        &paint,
        Transform::identity(),
        None,
      );
      region = if seen_any {
        region.union(img.region)
      } else {
        img.region
      };
      seen_any = true;
    }
  }

  FilterResult {
    pixmap: out,
    region: if seen_any {
      clip_region(region, filter_region)
    } else {
      Rect::ZERO
    },
  }
}

fn drop_shadow_pixmap(
  input: FilterResult,
  dx: f32,
  dy: f32,
  stddev: (f32, f32),
  color: &Rgba,
  opacity: f32,
  filter_region: Rect,
) -> FilterResult {
  let mut tinted = input.pixmap.clone();
  for px in tinted.pixels_mut() {
    let alpha = px.alpha() as f32 / 255.0 * opacity * color.a;
    let premul = |v: u8| {
      ((v as f32 / 255.0) * alpha * 255.0)
        .round()
        .clamp(0.0, 255.0) as u8
    };
    *px = PremultipliedColorU8::from_rgba(
      premul(color.r),
      premul(color.g),
      premul(color.b),
      (alpha * 255.0) as u8,
    )
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }

  let mut shadow = tinted.clone();
  if stddev.0 != 0.0 || stddev.1 != 0.0 {
    apply_gaussian_blur_anisotropic(&mut shadow, stddev.0, stddev.1);
  }

  let mut out = Pixmap::new(input.pixmap.width(), input.pixmap.height()).unwrap();
  let mut paint = PixmapPaint::default();
  paint.blend_mode = tiny_skia::BlendMode::SourceOver;
  out.draw_pixmap(
    dx.round() as i32,
    dy.round() as i32,
    shadow.as_ref(),
    &paint,
    Transform::identity(),
    None,
  );
  out.draw_pixmap(
    0,
    0,
    input.pixmap.as_ref(),
    &paint,
    Transform::identity(),
    None,
  );

  let base_region = input.region;
  let blur_spread_x = (stddev.0.abs() * 3.0).max(0.0);
  let blur_spread_y = (stddev.1.abs() * 3.0).max(0.0);
  let shadow_region = clip_region(
    inflate_rect_xy(base_region, blur_spread_x, blur_spread_y)
      .translate(Point::new(dx.round(), dy.round())),
    filter_region,
  );
  let region = clip_region(base_region.union(shadow_region), filter_region);

  FilterResult {
    pixmap: out,
    region,
  }
}

fn draw_over(bottom: &Pixmap, top: &Pixmap) -> Pixmap {
  let mut out = bottom.clone();
  let mut paint = PixmapPaint::default();
  paint.blend_mode = tiny_skia::BlendMode::SourceOver;
  out.draw_pixmap(0, 0, top.as_ref(), &paint, Transform::identity(), None);
  out
}

fn composite_porter_duff<F>(a: &Pixmap, b: &Pixmap, factors: F) -> Option<Pixmap>
where
  F: Fn(f32, f32) -> (f32, f32),
{
  let mut out = Pixmap::new(a.width(), a.height())?;
  let a_pixels = a.pixels();
  let b_pixels = b.pixels();
  for ((dst, pa), pb) in out
    .pixels_mut()
    .iter_mut()
    .zip(a_pixels.iter())
    .zip(b_pixels.iter())
  {
    let (ar, ag, ab, aa) = premultiplied_components(pa);
    let (br, bg, bb, ba) = premultiplied_components(pb);
    let (mut fa, mut fb) = factors(aa, ba);
    fa = fa.clamp(0.0, 1.0);
    fb = fb.clamp(0.0, 1.0);
    let out_a = aa * fa + ba * fb;
    let out_r = ar * fa + br * fb;
    let out_g = ag * fa + bg * fb;
    let out_b = ab * fa + bb * fb;
    *dst = premul_from_components(out_r, out_g, out_b, out_a);
  }
  Some(out)
}

fn arithmetic_composite(
  a: &Pixmap,
  b: &Pixmap,
  k1: f32,
  k2: f32,
  k3: f32,
  k4: f32,
) -> Option<Pixmap> {
  let mut out = Pixmap::new(a.width(), a.height())?;
  for ((dst, pa), pb) in out
    .pixels_mut()
    .iter_mut()
    .zip(a.pixels().iter())
    .zip(b.pixels().iter())
  {
    let (a_r, a_g, a_b, a_a) = unpremultiply_components(pa);
    let (b_r, b_g, b_b, b_a) = unpremultiply_components(pb);
    let apply =
      |i1: f32, i2: f32| -> f32 { (k1 * i1 * i2 + k2 * i1 + k3 * i2 + k4).clamp(0.0, 1.0) };
    let out_a = apply(a_a, b_a);
    let out_r = apply(a_r, b_r);
    let out_g = apply(a_g, b_g);
    let out_b = apply(a_b, b_b);
    *dst = premul_from_components(out_r * out_a, out_g * out_a, out_b * out_a, out_a);
  }
  Some(out)
}

fn premultiplied_components(px: &PremultipliedColorU8) -> (f32, f32, f32, f32) {
  let norm = |v: u8| v as f32 / 255.0;
  (
    norm(px.red()),
    norm(px.green()),
    norm(px.blue()),
    norm(px.alpha()),
  )
}

fn unpremultiply_components(px: &PremultipliedColorU8) -> (f32, f32, f32, f32) {
  let a = px.alpha() as f32 / 255.0;
  if a <= 0.0 {
    (0.0, 0.0, 0.0, 0.0)
  } else {
    let inv_a = 1.0 / a;
    (
      (px.red() as f32 / 255.0 * inv_a).clamp(0.0, 1.0),
      (px.green() as f32 / 255.0 * inv_a).clamp(0.0, 1.0),
      (px.blue() as f32 / 255.0 * inv_a).clamp(0.0, 1.0),
      a,
    )
  }
}

fn premul_from_components(r: f32, g: f32, b: f32, a: f32) -> PremultipliedColorU8 {
  let a_clamped = a.clamp(0.0, 1.0);
  let clamp_to_alpha = |v: f32| (v.clamp(0.0, a_clamped) * 255.0).round().clamp(0.0, 255.0) as u8;
  let a_byte = (a_clamped * 255.0).round().clamp(0.0, 255.0) as u8;
  PremultipliedColorU8::from_rgba(
    clamp_to_alpha(r),
    clamp_to_alpha(g),
    clamp_to_alpha(b),
    a_byte,
  )
  .unwrap_or(PremultipliedColorU8::TRANSPARENT)
}

fn region_to_int_bounds(
  region: Rect,
  max_width: u32,
  max_height: u32,
) -> Option<(u32, u32, u32, u32)> {
  let x0 = region.min_x().floor().max(0.0);
  let y0 = region.min_y().floor().max(0.0);
  let x1 = region.max_x().ceil().min(max_width as f32);
  let y1 = region.max_y().ceil().min(max_height as f32);
  if x1 <= x0 || y1 <= y0 {
    return None;
  }
  Some((x0 as u32, y0 as u32, (x1 - x0) as u32, (y1 - y0) as u32))
}

fn tile_pixmap(input: FilterResult, filter_region: Rect) -> Option<FilterResult> {
  let width = input.pixmap.width();
  let height = input.pixmap.height();
  let mut out = Pixmap::new(width, height)?;
  let Some((start_x, start_y, tile_width, tile_height)) =
    region_to_int_bounds(input.region, width, height)
  else {
    return Some(FilterResult {
      pixmap: out,
      region: Rect::ZERO,
    });
  };
  let Some((target_x, target_y, target_w, target_h)) =
    region_to_int_bounds(filter_region, width, height)
  else {
    return Some(FilterResult {
      pixmap: out,
      region: Rect::ZERO,
    });
  };

  let wrap = |value: i32, origin: i32, size: i32| -> i32 {
    if size == 0 {
      return origin;
    }
    let offset = value - origin;
    origin + ((offset % size + size) % size)
  };

  let tile_w = tile_width as i32;
  let tile_h = tile_height as i32;
  let src_stride = width as usize;
  let dst_stride = width as usize;
  let src_pixels = input.pixmap.pixels();
  let dst_pixels = out.pixels_mut();

  for y in (target_y as i32)..(target_y as i32 + target_h as i32) {
    let src_y = wrap(y, start_y as i32, tile_h);
    let src_row = src_y as usize * src_stride;
    let dst_row = y as usize * dst_stride;
    for x in (target_x as i32)..(target_x as i32 + target_w as i32) {
      let src_x = wrap(x, start_x as i32, tile_w);
      let dst_idx = dst_row + x as usize;
      let src_idx = src_row + src_x as usize;
      dst_pixels[dst_idx] = src_pixels[src_idx];
    }
  }

  let output_region = clip_region(filter_region, filter_region_for_pixmap(&out));
  Some(FilterResult {
    pixmap: out,
    region: output_region,
  })
}

fn apply_displacement_map(
  primary: &Pixmap,
  map: &Pixmap,
  scale: f32,
  x_channel: ChannelSelector,
  y_channel: ChannelSelector,
) -> Option<Pixmap> {
  let mut out = Pixmap::new(primary.width(), primary.height())?;
  let width = primary.width() as usize;

  for (idx, dst) in out.pixels_mut().iter_mut().enumerate() {
    let y = (idx / width) as u32;
    let x = (idx % width) as u32;

    let map_sample = sample_premultiplied(map, x as f32, y as f32);
    let channel_value_x = channel_value(map_sample, x_channel);
    let channel_value_y = channel_value(map_sample, y_channel);
    let dx = (channel_value_x - 0.5) * scale;
    let dy = (channel_value_y - 0.5) * scale;

    let sample = sample_premultiplied(primary, x as f32 + dx, y as f32 + dy);
    let a = to_byte(sample[3]);
    *dst = PremultipliedColorU8::from_rgba(
      to_byte(sample[0].min(sample[3])),
      to_byte(sample[1].min(sample[3])),
      to_byte(sample[2].min(sample[3])),
      a,
    )
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }

  Some(out)
}

fn channel_value(sample: [f32; 4], selector: ChannelSelector) -> f32 {
  let alpha = sample[3];
  let unpremul = |v: f32| {
    if alpha <= 0.0 {
      0.0
    } else {
      (v / alpha).clamp(0.0, 1.0)
    }
  };
  match selector {
    ChannelSelector::R => unpremul(sample[0]),
    ChannelSelector::G => unpremul(sample[1]),
    ChannelSelector::B => unpremul(sample[2]),
    ChannelSelector::A => alpha.clamp(0.0, 1.0),
  }
}

fn sample_premultiplied(pixmap: &Pixmap, x: f32, y: f32) -> [f32; 4] {
  let width = pixmap.width() as i32;
  let height = pixmap.height() as i32;
  let x0 = x.floor() as i32;
  let y0 = y.floor() as i32;
  let tx = (x - x0 as f32).clamp(0.0, 1.0);
  let ty = (y - y0 as f32).clamp(0.0, 1.0);

  let mut accum = [0.0; 4];
  let mut weight_sum = 0.0;
  for dy in 0..=1 {
    for dx in 0..=1 {
      let sx = x0 + dx;
      let sy = y0 + dy;
      let weight = if dx == 0 { 1.0 - tx } else { tx } * if dy == 0 { 1.0 - ty } else { ty };
      if weight <= 0.0 {
        continue;
      }
      weight_sum += weight;
      if sx < 0 || sy < 0 || sx >= width || sy >= height {
        continue;
      }
      let px = pixmap.pixel(sx as u32, sy as u32).unwrap();
      accum[0] += px.red() as f32 / 255.0 * weight;
      accum[1] += px.green() as f32 / 255.0 * weight;
      accum[2] += px.blue() as f32 / 255.0 * weight;
      accum[3] += px.alpha() as f32 / 255.0 * weight;
    }
  }
  if weight_sum > 0.0 {
    accum.iter_mut().for_each(|v| *v /= weight_sum);
  } else {
    return [0.0; 4];
  }
  accum
}

fn to_byte(v: f32) -> u8 {
  (v.clamp(0.0, 1.0) * 255.0).round().clamp(0.0, 255.0) as u8
}

fn apply_convolve_matrix(
  input: Pixmap,
  order_x: usize,
  order_y: usize,
  kernel: &[f32],
  divisor: Option<f32>,
  bias: f32,
  target_x: i32,
  target_y: i32,
  edge_mode: EdgeMode,
  preserve_alpha: bool,
  subregion: Option<(f32, f32, f32, f32)>,
) -> Pixmap {
  if order_x == 0 || order_y == 0 || kernel.is_empty() || input.width() == 0 || input.height() == 0
  {
    return input;
  }

  let divisor = match divisor {
    Some(d) if d.abs() > f32::EPSILON => d,
    Some(_) => 1.0,
    None => {
      let sum: f32 = kernel.iter().sum();
      if sum.abs() < f32::EPSILON {
        1.0
      } else {
        sum
      }
    }
  };

  let kernel_rows: Vec<&[f32]> = kernel.chunks(order_x).collect();
  let width = input.width() as usize;
  let width_i32 = input.width() as i32;
  let height_i32 = input.height() as i32;
  let mut out = Pixmap::new(input.width(), input.height()).unwrap();
  for px in out.pixels_mut() {
    *px = PremultipliedColorU8::TRANSPARENT;
  }
  let src_pixels = input.pixels();
  let dst_pixels = out.pixels_mut();
  let (sub_min_x, sub_min_y, sub_max_x, sub_max_y) = subregion
    .map(|(x, y, w, h)| {
      let min_x = x.floor() as i32;
      let min_y = y.floor() as i32;
      let max_x = (x + w).ceil() as i32;
      let max_y = (y + h).ceil() as i32;
      (
        min_x.max(0),
        min_y.max(0),
        max_x.min(width_i32),
        max_y.min(height_i32),
      )
    })
    .unwrap_or((0, 0, width_i32, height_i32));
  if sub_min_x >= sub_max_x || sub_min_y >= sub_max_y {
    return out;
  }

  dst_pixels
    .par_iter_mut()
    .enumerate()
    .for_each(|(idx, dst_px)| {
      let y = (idx / width) as i32;
      let x = (idx % width) as i32;
      if x < sub_min_x || x >= sub_max_x || y < sub_min_y || y >= sub_max_y {
        return;
      }
      let preserved_alpha = if preserve_alpha {
        src_pixels[idx].alpha() as f32 / 255.0
      } else {
        0.0
      };

      let mut sum_r = 0.0;
      let mut sum_g = 0.0;
      let mut sum_b = 0.0;
      let mut sum_a = 0.0;

      for (ky, row) in kernel_rows.iter().enumerate() {
        let sy = y + ky as i32 - target_y;
        for (kx, weight) in row.iter().enumerate() {
          if *weight == 0.0 {
            continue;
          }
          let sx = x + kx as i32 - target_x;
          if let Some(px) = sample_pixel(src_pixels, sx, sy, width_i32, height_i32, edge_mode) {
            let (r, g, b, a) = unpremultiply(px);
            sum_r += r * weight;
            sum_g += g * weight;
            sum_b += b * weight;
            sum_a += a * weight;
          }
        }
      }

      let r = sum_r / divisor + bias;
      let g = sum_g / divisor + bias;
      let b = sum_b / divisor + bias;
      let a = sum_a / divisor + bias;
      let clamp = |v: f32| v.clamp(0.0, 1.0);
      let out_alpha = if preserve_alpha {
        preserved_alpha
      } else {
        clamp(a)
      };
      let to_channel =
        |v: f32, alpha: f32| (clamp(v) * alpha * 255.0).round().clamp(0.0, 255.0) as u8;
      let a_byte = (out_alpha * 255.0).round().clamp(0.0, 255.0) as u8;
      *dst_px = PremultipliedColorU8::from_rgba(
        to_channel(r, out_alpha),
        to_channel(g, out_alpha),
        to_channel(b, out_alpha),
        a_byte,
      )
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
    });

  out
}

fn sample_pixel(
  pixels: &[PremultipliedColorU8],
  x: i32,
  y: i32,
  width: i32,
  height: i32,
  edge_mode: EdgeMode,
) -> Option<PremultipliedColorU8> {
  if width == 0 || height == 0 {
    return None;
  }
  let (sx, sy) = match edge_mode {
    EdgeMode::Duplicate => (x.clamp(0, width - 1), y.clamp(0, height - 1)),
    EdgeMode::Wrap => {
      let wrap = |v: i32, max: i32| {
        let mut v = v % max;
        if v < 0 {
          v += max;
        }
        v
      };
      (wrap(x, width), wrap(y, height))
    }
    EdgeMode::None => {
      if x < 0 || y < 0 || x >= width || y >= height {
        return None;
      }
      (x, y)
    }
  };
  let idx = sy as usize * width as usize + sx as usize;
  pixels.get(idx).copied()
}

fn unpremultiply(px: PremultipliedColorU8) -> (f32, f32, f32, f32) {
  let a = px.alpha() as f32 / 255.0;
  if a <= 0.0 {
    (0.0, 0.0, 0.0, 0.0)
  } else {
    (
      (px.red() as f32 / 255.0 / a).clamp(0.0, 1.0),
      (px.green() as f32 / 255.0 / a).clamp(0.0, 1.0),
      (px.blue() as f32 / 255.0 / a).clamp(0.0, 1.0),
      a,
    )
  }
}

fn apply_morphology(pixmap: &mut Pixmap, radius: (f32, f32), op: MorphologyOp) {
  let rx = radius.0.abs().ceil() as i32;
  let ry = radius.1.abs().ceil() as i32;
  if rx <= 0 && ry <= 0 {
    return;
  }
  let width = pixmap.width() as i32;
  let height = pixmap.height() as i32;
  let original = pixmap.clone();
  let src = original.pixels();
  let dst = pixmap.pixels_mut();
  let row_len = width as usize;

  dst.par_iter_mut().enumerate().for_each(|(idx, dst_px)| {
    let y = (idx / row_len) as i32;
    let x = (idx % row_len) as i32;
    let mut agg = match op {
      MorphologyOp::Dilate => [0u8; 4],
      MorphologyOp::Erode => [255u8; 4],
    };
    for dy in -ry..=ry {
      for dx in -rx..=rx {
        let ny = (y + dy).clamp(0, height - 1);
        let nx = (x + dx).clamp(0, width - 1);
        let sample_idx = (ny as usize) * row_len + nx as usize;
        let px = src[sample_idx];
        match op {
          MorphologyOp::Dilate => {
            agg[0] = agg[0].max(px.red());
            agg[1] = agg[1].max(px.green());
            agg[2] = agg[2].max(px.blue());
            agg[3] = agg[3].max(px.alpha());
          }
          MorphologyOp::Erode => {
            agg[0] = agg[0].min(px.red());
            agg[1] = agg[1].min(px.green());
            agg[2] = agg[2].min(px.blue());
            agg[3] = agg[3].min(px.alpha());
          }
        }
      }
    }
    *dst_px = PremultipliedColorU8::from_rgba(agg[0], agg[1], agg[2], agg[3])
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  });
}

fn apply_component_transfer(
  pixmap: &mut Pixmap,
  r: &TransferFn,
  g: &TransferFn,
  b: &TransferFn,
  a: &TransferFn,
  color_interpolation_filters: ColorInterpolationFilters,
) {
  let r_lut = build_transfer_lut(r);
  let g_lut = build_transfer_lut(g);
  let b_lut = build_transfer_lut(b);
  let a_lut = build_transfer_lut(a);
  let sample = |lut: &[f32; 256], v: f32| -> f32 {
    let idx = (clamp_unit(v) * 255.0).round() as usize;
    lut[idx.min(255)]
  };

  for px in pixmap.pixels_mut() {
    let input = unpack_color(*px, color_interpolation_filters);
    let out = UnpremultipliedColor {
      r: sample(&r_lut, input.r),
      g: sample(&g_lut, input.g),
      b: sample(&b_lut, input.b),
      a: sample(&a_lut, input.a),
    };
    *px = pack_color(out, color_interpolation_filters);
  }
}

fn clamp_unit(v: f32) -> f32 {
  if v.is_finite() {
    v.clamp(0.0, 1.0)
  } else {
    0.0
  }
}

fn build_transfer_lut(func: &TransferFn) -> [f32; 256] {
  let mut lut = [0.0f32; 256];
  for (idx, out) in lut.iter_mut().enumerate() {
    let input = idx as f32 / 255.0;
    *out = evaluate_transfer_fn(func, input);
  }
  lut
}

fn evaluate_transfer_fn(func: &TransferFn, v: f32) -> f32 {
  let result = match func {
    TransferFn::Identity => v,
    TransferFn::Linear { slope, intercept } => slope * v + intercept,
    TransferFn::Gamma {
      amplitude,
      exponent,
      offset,
    } => amplitude * v.powf(*exponent) + offset,
    TransferFn::Table { values } => {
      if values.is_empty() {
        v
      } else if values.len() == 1 {
        values[0]
      } else {
        let last = values.len() - 1;
        let scaled = v * last as f32;
        let idx = scaled.floor() as usize;
        if idx >= last {
          *values.last().unwrap_or(&v)
        } else {
          let start = values[idx];
          let end = values[idx + 1];
          let t = scaled - idx as f32;
          start + t * (end - start)
        }
      }
    }
    TransferFn::Discrete { values } => {
      if values.is_empty() {
        v
      } else {
        let n = values.len();
        let idx = (v * n as f32).floor() as usize;
        values[idx.min(n - 1)]
      }
    }
  };
  clamp_unit(result)
}

fn apply_color_matrix(
  pixmap: &mut Pixmap,
  kind: &ColorMatrixKind,
  color_interpolation_filters: ColorInterpolationFilters,
) {
  match kind {
    ColorMatrixKind::Matrix(values) => {
      apply_color_matrix_values(pixmap, values, color_interpolation_filters)
    }
    ColorMatrixKind::LuminanceToAlpha => {
      let pixels = pixmap.pixels_mut();
      for px in pixels.iter_mut() {
        let color = unpack_color(*px, color_interpolation_filters);
        let lum = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;
        *px = pack_color(
          UnpremultipliedColor {
            r: 0.0,
            g: 0.0,
            b: 0.0,
            a: clamp01(lum),
          },
          color_interpolation_filters,
        );
      }
    }
    ColorMatrixKind::Saturate(amount) => {
      let s = *amount;
      let matrix = [
        0.213 + 0.787 * s,
        0.715 - 0.715 * s,
        0.072 - 0.072 * s,
        0.0,
        0.0,
        0.213 - 0.213 * s,
        0.715 + 0.285 * s,
        0.072 - 0.072 * s,
        0.0,
        0.0,
        0.213 - 0.213 * s,
        0.715 - 0.715 * s,
        0.072 + 0.928 * s,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
      ];
      apply_color_matrix_values(pixmap, &matrix, color_interpolation_filters);
    }
    ColorMatrixKind::HueRotate(angle) => {
      let theta = angle.to_radians();
      let cos_t = theta.cos();
      let sin_t = theta.sin();
      let matrix = [
        0.213 + 0.787 * cos_t - 0.213 * sin_t,
        0.715 - 0.715 * cos_t - 0.715 * sin_t,
        0.072 - 0.072 * cos_t + 0.928 * sin_t,
        0.0,
        0.0,
        0.213 - 0.213 * cos_t + 0.143 * sin_t,
        0.715 + 0.285 * cos_t + 0.140 * sin_t,
        0.072 - 0.072 * cos_t - 0.283 * sin_t,
        0.0,
        0.0,
        0.213 - 0.213 * cos_t - 0.787 * sin_t,
        0.715 - 0.715 * cos_t + 0.715 * sin_t,
        0.072 + 0.928 * cos_t + 0.072 * sin_t,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
      ];
      apply_color_matrix_values(pixmap, &matrix, color_interpolation_filters);
    }
  }
}

fn apply_color_matrix_values(
  pixmap: &mut Pixmap,
  matrix: &[f32; 20],
  color_interpolation_filters: ColorInterpolationFilters,
) {
  let pixels = pixmap.pixels_mut();
  for px in pixels.iter_mut() {
    let input = unpack_color(*px, color_interpolation_filters);
    let r2 = matrix[0] * input.r
      + matrix[1] * input.g
      + matrix[2] * input.b
      + matrix[3] * input.a
      + matrix[4];
    let g2 = matrix[5] * input.r
      + matrix[6] * input.g
      + matrix[7] * input.b
      + matrix[8] * input.a
      + matrix[9];
    let b2 = matrix[10] * input.r
      + matrix[11] * input.g
      + matrix[12] * input.b
      + matrix[13] * input.a
      + matrix[14];
    let a2 = matrix[15] * input.r
      + matrix[16] * input.g
      + matrix[17] * input.b
      + matrix[18] * input.a
      + matrix[19];
    *px = pack_color(
      UnpremultipliedColor {
        r: clamp01(r2),
        g: clamp01(g2),
        b: clamp01(b2),
        a: clamp01(a2),
      },
      color_interpolation_filters,
    );
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::HashMap;
  use tiny_skia::ColorU8;

  fn pixmap_from_rgba(colors: &[(u8, u8, u8, u8)]) -> Pixmap {
    let mut pixmap = Pixmap::new(colors.len() as u32, 1).unwrap();
    for (idx, (r, g, b, a)) in colors.iter().copied().enumerate() {
      pixmap.pixels_mut()[idx] = ColorU8::from_rgba(r, g, b, a).premultiply();
    }
    pixmap
  }

  fn pixels_to_vec(pixmap: &Pixmap) -> Vec<(u8, u8, u8, u8)> {
    pixmap
      .pixels()
      .iter()
      .map(|px| (px.red(), px.green(), px.blue(), px.alpha()))
      .collect()
  }

  fn premul(r: u8, g: u8, b: u8, a: u8) -> PremultipliedColorU8 {
    let alpha = a as f32 / 255.0;
    let pm = |v: u8| ((v as f32) * alpha).round().clamp(0.0, 255.0) as u8;
    PremultipliedColorU8::from_rgba(pm(r), pm(g), pm(b), a).unwrap()
  }

  fn apply_for_test(prim: &FilterPrimitive, pixmap: &Pixmap) -> FilterResult {
    let region = filter_region_for_pixmap(pixmap);
    let src = FilterResult::full_region(pixmap.clone(), region);
    let filter = SvgFilter {
      color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
      steps: Vec::new(),
      region: SvgFilterRegion::default_for_units(SvgFilterUnits::UserSpaceOnUse),
      primitive_units: SvgFilterUnits::UserSpaceOnUse,
    };
    apply_primitive(
      &filter,
      &region,
      prim,
      &src,
      &HashMap::new(),
      &src,
      1.0,
      region,
      ColorInterpolationFilters::LinearRGB,
    )
    .unwrap()
  }

  #[test]
  fn color_matrix_uses_unpremultiplied_channels() {
    let mut pixmap = Pixmap::new(1, 1).unwrap();
    pixmap.pixels_mut()[0] =
      PremultipliedColorU8::from_rgba(128, 0, 0, 128).unwrap_or(PremultipliedColorU8::TRANSPARENT);

    let matrix = [
      0.0, 0.0, 0.0, 0.0, 0.0, //
      1.0, 0.0, 0.0, 0.0, 0.0, //
      0.0, 0.0, 0.0, 0.0, 0.0, //
      0.0, 0.0, 0.0, 1.0, 0.0, //
    ];

    apply_color_matrix_values(&mut pixmap, &matrix, ColorInterpolationFilters::LinearRGB);

    let px = pixmap.pixels()[0];
    assert_eq!(px.red(), 0);
    assert_eq!(px.green(), 128);
    assert_eq!(px.blue(), 0);
    assert_eq!(px.alpha(), 128);
  }

  #[test]
  fn component_transfer_operates_on_unpremultiplied_rgb() {
    let mut pixmap = Pixmap::new(1, 1).unwrap();
    pixmap.pixels_mut()[0] =
      PremultipliedColorU8::from_rgba(128, 0, 0, 128).unwrap_or(PremultipliedColorU8::TRANSPARENT);

    apply_component_transfer(
      &mut pixmap,
      &TransferFn::Linear {
        slope: 1.0,
        intercept: 0.25,
      },
      &TransferFn::Identity,
      &TransferFn::Identity,
      &TransferFn::Identity,
      ColorInterpolationFilters::LinearRGB,
    );

    let px = pixmap.pixels()[0];
    assert_eq!(px.red(), 128);
    assert_eq!(px.green(), 0);
    assert_eq!(px.blue(), 0);
    assert_eq!(px.alpha(), 128);
  }

  #[test]
  fn luminance_to_alpha_uses_unpremultiplied_rgb() {
    let mut pixmap = Pixmap::new(1, 1).unwrap();
    pixmap.pixels_mut()[0] =
      PremultipliedColorU8::from_rgba(128, 64, 0, 128).unwrap_or(PremultipliedColorU8::TRANSPARENT);

    apply_color_matrix(
      &mut pixmap,
      &ColorMatrixKind::LuminanceToAlpha,
      ColorInterpolationFilters::LinearRGB,
    );

    let px = pixmap.pixels()[0];
    assert_eq!(px.red(), 0);
    assert_eq!(px.green(), 0);
    assert_eq!(px.blue(), 0);

    let expected_alpha = (255.0f32 * (0.2126 * 1.0 + 0.7152 * 0.5 + 0.0722 * 0.0))
      .round()
      .clamp(0.0, 255.0) as u8;
    assert_eq!(px.alpha(), expected_alpha);
  }

  #[test]
  fn component_transfer_linear_and_alpha() {
    let mut pixmap = pixmap_from_rgba(&[(128, 64, 0, 128), (255, 128, 64, 255)]);
    apply_component_transfer(
      &mut pixmap,
      &TransferFn::Linear {
        slope: 0.5,
        intercept: 0.25,
      },
      &TransferFn::Linear {
        slope: 1.0,
        intercept: 0.1,
      },
      &TransferFn::Identity,
      &TransferFn::Linear {
        slope: 0.5,
        intercept: 0.25,
      },
      ColorInterpolationFilters::LinearRGB,
    );

    assert_eq!(
      pixels_to_vec(&pixmap),
      vec![(64, 45, 0, 128), (143, 115, 48, 191)]
    );
  }

  #[test]
  fn component_transfer_gamma() {
    let mut pixmap = pixmap_from_rgba(&[(64, 0, 0, 255), (200, 0, 0, 255)]);
    apply_component_transfer(
      &mut pixmap,
      &TransferFn::Gamma {
        amplitude: 0.7,
        exponent: 2.0,
        offset: 0.1,
      },
      &TransferFn::Identity,
      &TransferFn::Identity,
      &TransferFn::Identity,
      ColorInterpolationFilters::LinearRGB,
    );

    assert_eq!(
      pixels_to_vec(&pixmap),
      vec![(37, 0, 0, 255), (135, 0, 0, 255)]
    );
  }

  #[test]
  fn component_transfer_table_and_discrete() {
    let mut pixmap = pixmap_from_rgba(&[(51, 102, 0, 128), (204, 153, 0, 255)]);
    apply_component_transfer(
      &mut pixmap,
      &TransferFn::Table {
        values: vec![0.0, 0.2, 1.0],
      },
      &TransferFn::Discrete {
        values: vec![0.2, 0.6, 1.0],
      },
      &TransferFn::Identity,
      &TransferFn::Discrete {
        values: vec![0.25, 0.75],
      },
      ColorInterpolationFilters::LinearRGB,
    );

    assert_eq!(
      pixels_to_vec(&pixmap),
      vec![(16, 115, 0, 191), (130, 115, 0, 191)]
    );
  }

  #[test]
  fn convolve_identity_kernel_is_noop() {
    let mut pixmap = Pixmap::new(2, 2).unwrap();
    let pixels = [
      premul(255, 0, 0, 255),
      premul(0, 255, 0, 255),
      premul(0, 0, 255, 255),
      premul(255, 255, 0, 255),
    ];
    for (dst, src) in pixmap.pixels_mut().iter_mut().zip(pixels.iter()) {
      *dst = *src;
    }

    let prim = FilterPrimitive::ConvolveMatrix {
      input: FilterInput::SourceGraphic,
      order_x: 3,
      order_y: 3,
      kernel: vec![0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0],
      divisor: None,
      bias: 0.0,
      target_x: 1,
      target_y: 1,
      edge_mode: EdgeMode::Duplicate,
      preserve_alpha: false,
      subregion: None,
    };
    let out = apply_for_test(&prim, &pixmap);
    assert_eq!(pixmap.pixels(), out.pixmap.pixels());
  }

  #[test]
  fn convolve_blur_averages_neighbors() {
    let mut pixmap = Pixmap::new(3, 3).unwrap();
    for (idx, px) in pixmap.pixels_mut().iter_mut().enumerate() {
      let v = (idx as u8) * 10;
      *px = premul(v, v, v, 255);
    }

    let prim = FilterPrimitive::ConvolveMatrix {
      input: FilterInput::SourceGraphic,
      order_x: 3,
      order_y: 3,
      kernel: vec![1.0; 9],
      divisor: Some(9.0),
      bias: 0.0,
      target_x: 1,
      target_y: 1,
      edge_mode: EdgeMode::Duplicate,
      preserve_alpha: false,
      subregion: None,
    };

    let out = apply_for_test(&prim, &pixmap);
    let center = out.pixmap.pixels()[4];
    assert_eq!(center.red(), 40);
    assert_eq!(center.green(), 40);
    assert_eq!(center.blue(), 40);
    assert_eq!(center.alpha(), 255);
  }

  #[test]
  fn convolve_edge_mode_none_darkens_edges() {
    let mut pixmap = Pixmap::new(2, 2).unwrap();
    for px in pixmap.pixels_mut() {
      *px = premul(255, 255, 255, 255);
    }

    let base = FilterPrimitive::ConvolveMatrix {
      input: FilterInput::SourceGraphic,
      order_x: 3,
      order_y: 3,
      kernel: vec![1.0; 9],
      divisor: Some(9.0),
      bias: 0.0,
      target_x: 1,
      target_y: 1,
      edge_mode: EdgeMode::Duplicate,
      preserve_alpha: false,
      subregion: None,
    };
    let none_mode = FilterPrimitive::ConvolveMatrix {
      input: FilterInput::SourceGraphic,
      order_x: 3,
      order_y: 3,
      kernel: vec![1.0; 9],
      divisor: Some(9.0),
      bias: 0.0,
      target_x: 1,
      target_y: 1,
      edge_mode: EdgeMode::None,
      preserve_alpha: false,
      subregion: None,
    };

    let dup = apply_for_test(&base, &pixmap);
    let none = apply_for_test(&none_mode, &pixmap);
    let dup_corner = dup.pixmap.pixels()[0];
    let none_corner = none.pixmap.pixels()[0];

    assert!(none_corner.red() < dup_corner.red());
    assert!(none_corner.alpha() < dup_corner.alpha());
  }

  #[test]
  fn convolve_respects_subregion() {
    let mut pixmap = Pixmap::new(2, 2).unwrap();
    let pixels = [
      premul(255, 0, 0, 255),
      premul(0, 255, 0, 255),
      premul(0, 0, 255, 255),
      premul(255, 255, 0, 255),
    ];
    for (dst, src) in pixmap.pixels_mut().iter_mut().zip(pixels.iter()) {
      *dst = *src;
    }

    let prim = FilterPrimitive::ConvolveMatrix {
      input: FilterInput::SourceGraphic,
      order_x: 3,
      order_y: 3,
      kernel: vec![0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0],
      divisor: None,
      bias: 0.0,
      target_x: 1,
      target_y: 1,
      edge_mode: EdgeMode::Duplicate,
      preserve_alpha: false,
      subregion: Some((0.0, 0.0, 1.0, 1.0)),
    };
    let out = apply_for_test(&prim, &pixmap);
    let out_pixels = out.pixmap.pixels();
    assert_eq!(out_pixels[0], pixels[0]); // inside subregion
    assert_eq!(out_pixels[1], PremultipliedColorU8::TRANSPARENT);
    assert_eq!(out_pixels[2], PremultipliedColorU8::TRANSPARENT);
    assert_eq!(out_pixels[3], PremultipliedColorU8::TRANSPARENT);
  }

  #[test]
  fn tile_aligns_to_input_region_origin() {
    let mut pixmap = Pixmap::new(4, 4).unwrap();
    let width = pixmap.width() as usize;
    let set = |pixmap: &mut Pixmap, x: usize, y: usize, rgba: (u8, u8, u8, u8)| {
      pixmap.pixels_mut()[y * width + x] =
        ColorU8::from_rgba(rgba.0, rgba.1, rgba.2, rgba.3).premultiply();
    };

    set(&mut pixmap, 1, 0, (255, 0, 0, 255)); // red
    set(&mut pixmap, 2, 0, (0, 255, 0, 255)); // green
    set(&mut pixmap, 1, 1, (0, 0, 255, 255)); // blue
    set(&mut pixmap, 2, 1, (255, 255, 255, 255)); // white

    let input = FilterResult {
      pixmap,
      region: Rect::from_xywh(1.0, 0.0, 2.0, 2.0),
    };
    let out = tile_pixmap(input, Rect::from_xywh(0.0, 0.0, 4.0, 4.0)).unwrap();

    let px = |pixmap: &Pixmap, x: usize, y: usize| {
      let p = pixmap.pixels()[y * 4 + x];
      (p.red(), p.green(), p.blue(), p.alpha())
    };

    assert_eq!(px(&out.pixmap, 0, 0), (0, 255, 0, 255)); // green
    assert_eq!(px(&out.pixmap, 1, 0), (255, 0, 0, 255)); // red
    assert_eq!(px(&out.pixmap, 0, 1), (255, 255, 255, 255)); // white
    assert_eq!(px(&out.pixmap, 1, 1), (0, 0, 255, 255)); // blue
  }

  #[test]
  fn empty_primitive_region_produces_transparent_output() {
    let mut pixmap = Pixmap::new(4, 4).unwrap();
    for px in pixmap.pixels_mut() {
      *px = premul(10, 20, 30, 255);
    }

    let bbox = Rect::from_xywh(0.0, 0.0, 4.0, 4.0);
    let filter = SvgFilter {
      color_interpolation_filters: ColorInterpolationFilters::LinearRGB,
      steps: vec![
        FilterStep {
          result: None,
          color_interpolation_filters: None,
          primitive: FilterPrimitive::Flood {
            color: Rgba::new(255, 0, 0, 1.0),
            opacity: 1.0,
          },
          region: None,
        },
        FilterStep {
          result: None,
          color_interpolation_filters: None,
          primitive: FilterPrimitive::Offset {
            input: FilterInput::Previous,
            dx: 0.0,
            dy: 0.0,
          },
          region: Some(SvgFilterRegion {
            x: SvgLength::Number(0.0),
            y: SvgLength::Number(0.0),
            width: SvgLength::Number(0.0),
            height: SvgLength::Number(0.0),
            units: SvgFilterUnits::UserSpaceOnUse,
          }),
        },
      ],
      region: SvgFilterRegion {
        x: SvgLength::Number(0.0),
        y: SvgLength::Number(0.0),
        width: SvgLength::Number(4.0),
        height: SvgLength::Number(4.0),
        units: SvgFilterUnits::UserSpaceOnUse,
      },
      primitive_units: SvgFilterUnits::UserSpaceOnUse,
    };

    apply_svg_filter(&filter, &mut pixmap, 1.0, bbox);
    assert!(pixmap.pixels().iter().all(|px| px.alpha() == 0));
  }
}

#[cfg(test)]
mod fe_image_tests {
  use super::*;
  use base64::engine::general_purpose::STANDARD;
  use base64::Engine;
  use image::codecs::png::PngEncoder;
  use image::ColorType;
  use image::ImageEncoder;

  fn test_image_data_url() -> String {
    let mut buffer = Vec::new();
    let pixels = [255, 0, 0, 255, 0, 0, 255, 255];
    let encoder = PngEncoder::new(&mut buffer);
    encoder
      .write_image(&pixels, 2, 1, ColorType::Rgba8.into())
      .expect("encode png");
    format!("data:image/png;base64,{}", STANDARD.encode(buffer))
  }

  fn pixel_rgba(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let width = pixmap.width() as usize;
    let idx = y as usize * width + x as usize;
    let px = pixmap.pixels()[idx];
    (px.red(), px.green(), px.blue(), px.alpha())
  }

  #[test]
  fn fe_image_defaults_keep_aspect_ratio_and_center() {
    let data_url = test_image_data_url();
    let svg = format!(
      r#"<svg xmlns="http://www.w3.org/2000/svg"><filter id="f" primitiveUnits="objectBoundingBox"><feImage href="{url}" x="0.2" y="0.2" width="0.4" height="0.6"/></filter></svg>"#,
      url = data_url
    );
    let cache = ImageCache::new();
    let filter = parse_filter_definition(&svg, Some("f"), &cache).expect("filter");
    let mut canvas = Pixmap::new(5, 5).unwrap();
    let bbox = Rect::from_xywh(0.0, 0.0, canvas.width() as f32, canvas.height() as f32);
    apply_svg_filter(filter.as_ref(), &mut canvas, 1.0, bbox);

    assert_eq!(pixel_rgba(&canvas, 1, 2), (255, 0, 0, 255));
    assert_eq!(pixel_rgba(&canvas, 2, 2), (0, 0, 255, 255));
    assert_eq!(pixel_rgba(&canvas, 1, 1), (0, 0, 0, 0));
    assert_eq!(pixel_rgba(&canvas, 1, 3), (0, 0, 0, 0));
  }

  #[test]
  fn fe_image_preserve_aspect_ratio_none_stretches() {
    let data_url = test_image_data_url();
    let svg = format!(
      r#"<svg xmlns="http://www.w3.org/2000/svg"><filter id="f"><feImage href="{url}" x="20%" y="20%" width="40%" height="60%" preserveAspectRatio="none"/></filter></svg>"#,
      url = data_url
    );
    let cache = ImageCache::new();
    let filter = parse_filter_definition(&svg, Some("f"), &cache).expect("filter");
    let mut canvas = Pixmap::new(5, 5).unwrap();
    let bbox = Rect::from_xywh(0.0, 0.0, canvas.width() as f32, canvas.height() as f32);
    apply_svg_filter(filter.as_ref(), &mut canvas, 1.0, bbox);

    for y in 1..=3 {
      assert_eq!(pixel_rgba(&canvas, 1, y), (255, 0, 0, 255));
      assert_eq!(pixel_rgba(&canvas, 2, y), (0, 0, 255, 255));
    }
    assert_eq!(pixel_rgba(&canvas, 0, 0), (0, 0, 0, 0));
    assert_eq!(pixel_rgba(&canvas, 4, 4), (0, 0, 0, 0));
  }
}

#[cfg(test)]
mod tests_composite {
  use super::*;

  fn premul_rgba(r: u8, g: u8, b: u8, a: u8) -> PremultipliedColorU8 {
    let alpha = a as f32 / 255.0;
    let premul = |v: u8| {
      ((v as f32 / 255.0) * alpha * 255.0)
        .round()
        .clamp(0.0, 255.0) as u8
    };
    PremultipliedColorU8::from_rgba(premul(r), premul(g), premul(b), a).unwrap()
  }

  fn pixmap_from_colors(width: u32, height: u32, colors: &[(u8, u8, u8, u8)]) -> Pixmap {
    assert_eq!((width * height) as usize, colors.len());
    let mut pixmap = Pixmap::new(width, height).unwrap();
    for (dst, &(r, g, b, a)) in pixmap.pixels_mut().iter_mut().zip(colors.iter()) {
      *dst = premul_rgba(r, g, b, a);
    }
    pixmap
  }

  fn pixel(pixmap: &Pixmap, x: u32, y: u32) -> (u8, u8, u8, u8) {
    let px = pixmap.pixel(x, y).unwrap();
    (px.red(), px.green(), px.blue(), px.alpha())
  }

  fn composite_pixmaps(
    input1: Option<Pixmap>,
    input2: Option<Pixmap>,
    op: CompositeOperator,
  ) -> Option<Pixmap> {
    let a = input1?;
    let b = input2.unwrap_or_else(|| a.clone());
    if a.width() != b.width() || a.height() != b.height() {
      return None;
    }
    match op {
      CompositeOperator::Arithmetic { k1, k2, k3, k4 } => {
        arithmetic_composite(&a, &b, k1, k2, k3, k4)
      }
      CompositeOperator::Over => composite_porter_duff(&a, &b, |a_a, _| (1.0, 1.0 - a_a)),
      CompositeOperator::In => composite_porter_duff(&a, &b, |_, b_a| (b_a, 0.0)),
      CompositeOperator::Out => composite_porter_duff(&a, &b, |_, b_a| (1.0 - b_a, 0.0)),
      CompositeOperator::Atop => composite_porter_duff(&a, &b, |a_a, b_a| (b_a, 1.0 - a_a)),
      CompositeOperator::Xor => {
        composite_porter_duff(&a, &b, |a_a, b_a| (1.0 - b_a, 1.0 - a_a))
      }
    }
  }

  #[test]
  fn composite_over_blends_in_premultiplied_space() {
    let top = pixmap_from_colors(1, 1, &[(255, 0, 0, 128)]);
    let bottom = pixmap_from_colors(1, 1, &[(0, 0, 255, 255)]);
    let result = composite_pixmaps(Some(top), Some(bottom), CompositeOperator::Over).unwrap();
    assert_eq!(pixel(&result, 0, 0), (128, 0, 127, 255));
  }

  #[test]
  fn composite_in_uses_second_alpha() {
    let top = pixmap_from_colors(1, 1, &[(255, 0, 0, 128)]);
    let mask = pixmap_from_colors(1, 1, &[(0, 0, 0, 64)]);
    let result = composite_pixmaps(Some(top), Some(mask), CompositeOperator::In).unwrap();
    assert_eq!(pixel(&result, 0, 0), (32, 0, 0, 32));
  }

  #[test]
  fn composite_out_excludes_overlap() {
    let top = pixmap_from_colors(1, 1, &[(0, 255, 0, 128)]);
    let mask = pixmap_from_colors(1, 1, &[(0, 0, 0, 128)]);
    let result = composite_pixmaps(Some(top), Some(mask), CompositeOperator::Out).unwrap();
    assert_eq!(pixel(&result, 0, 0), (0, 64, 0, 64));
  }

  #[test]
  fn composite_atop_preserves_destination_shape() {
    let top = pixmap_from_colors(2, 1, &[(255, 0, 0, 255), (255, 0, 0, 128)]);
    let bottom = pixmap_from_colors(2, 1, &[(0, 0, 255, 255), (0, 0, 255, 255)]);
    let result = composite_pixmaps(Some(top), Some(bottom), CompositeOperator::Atop).unwrap();
    assert_eq!(pixel(&result, 0, 0), (255, 0, 0, 255));
    assert_eq!(pixel(&result, 1, 0), (128, 0, 127, 255));
  }

  #[test]
  fn composite_xor_combines_non_overlapping_regions() {
    let top = pixmap_from_colors(2, 1, &[(255, 0, 0, 255), (0, 0, 0, 0)]);
    let bottom = pixmap_from_colors(2, 1, &[(0, 0, 255, 255), (0, 0, 255, 255)]);
    let result = composite_pixmaps(Some(top), Some(bottom), CompositeOperator::Xor).unwrap();
    assert_eq!(pixel(&result, 0, 0), (0, 0, 0, 0));
    assert_eq!(pixel(&result, 1, 0), (0, 0, 255, 255));
  }

  #[test]
  fn composite_arithmetic_matches_svg_formula() {
    let input1 = pixmap_from_colors(1, 1, &[(255, 0, 0, 128)]);
    let input2 = pixmap_from_colors(1, 1, &[(0, 255, 0, 64)]);
    let result = composite_pixmaps(
      Some(input1),
      Some(input2),
      CompositeOperator::Arithmetic {
        k1: 0.5,
        k2: 0.1,
        k3: 0.2,
        k4: 0.05,
      },
    )
    .unwrap();
    assert_eq!(pixel(&result, 0, 0), (8, 14, 3, 54));
  }
}
