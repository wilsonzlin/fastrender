use crate::geometry::Rect;
use crate::image_loader::ImageCache;
use crate::paint::blur::apply_gaussian_blur;
use crate::style::color;
use crate::Rgba;
use rayon::prelude::*;
use roxmltree::Document;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::OnceLock;
use tiny_skia::{BlendMode, Pixmap, PixmapPaint, PremultipliedColorU8, Transform};

static FILTER_CACHE: OnceLock<Mutex<HashMap<String, Arc<SvgFilter>>>> = OnceLock::new();

fn filter_cache() -> &'static Mutex<HashMap<String, Arc<SvgFilter>>> {
  FILTER_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[derive(Clone, Debug)]
pub struct SvgFilter {
  pub steps: Vec<FilterStep>,
  pub region: SvgFilterRegion,
}

#[derive(Clone, Debug)]
pub struct FilterStep {
  pub result: Option<String>,
  pub primitive: FilterPrimitive,
}

#[derive(Clone, Copy, Debug)]
pub enum SvgFilterUnits {
  ObjectBoundingBox,
  UserSpaceOnUse,
}

#[derive(Clone, Copy, Debug)]
pub enum SvgLength {
  Px(f32),
  Percent(f32),
}

impl SvgLength {
  fn parse(raw: Option<&str>) -> Option<Self> {
    let raw = raw?.trim();
    if raw.is_empty() {
      return None;
    }

    if let Some(value) = raw.strip_suffix('%') {
      let number = value.trim().parse::<f32>().ok()? / 100.0;
      Some(SvgLength::Percent(number))
    } else {
      raw.parse::<f32>().ok().map(SvgLength::Px)
    }
  }

  fn resolve(&self, basis: f32) -> f32 {
    match self {
      SvgLength::Px(v) => *v,
      SvgLength::Percent(p) => *p * basis,
    }
  }
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
    let resolve_x = |len: SvgLength| match self.units {
      SvgFilterUnits::ObjectBoundingBox => bbox.min_x() + len.resolve(width_basis),
      SvgFilterUnits::UserSpaceOnUse => bbox.min_x() + len.resolve(width_basis),
    };
    let resolve_y = |len: SvgLength| match self.units {
      SvgFilterUnits::ObjectBoundingBox => bbox.min_y() + len.resolve(height_basis),
      SvgFilterUnits::UserSpaceOnUse => bbox.min_y() + len.resolve(height_basis),
    };
    let resolve_width = |len: SvgLength| match self.units {
      SvgFilterUnits::ObjectBoundingBox => len.resolve(width_basis),
      SvgFilterUnits::UserSpaceOnUse => len.resolve(width_basis),
    };
    let resolve_height = |len: SvgLength| match self.units {
      SvgFilterUnits::ObjectBoundingBox => len.resolve(height_basis),
      SvgFilterUnits::UserSpaceOnUse => len.resolve(height_basis),
    };

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
    std_dev: f32,
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
    std_dev: f32,
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
    radius: f32,
    op: MorphologyOp,
  },
  ComponentTransfer {
    input: FilterInput,
    r: TransferFn,
    g: TransferFn,
    b: TransferFn,
    a: TransferFn,
  },
  Image(Pixmap),
  Tile {
    input: FilterInput,
  },
  Turbulence,
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
  Reference(String),
  Previous,
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
  let filter = parse_filter_definition(&text, fragment.as_deref(), image_cache)?;

  if let Ok(mut guard) = filter_cache().lock() {
    guard.insert(cache_key, filter.clone());
  }

  Some(filter)
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

  let units = match filter_node.attribute("filterUnits") {
    Some(v) if v.eq_ignore_ascii_case("userspaceonuse") => SvgFilterUnits::UserSpaceOnUse,
    _ => SvgFilterUnits::ObjectBoundingBox,
  };
  let default_region = SvgFilterRegion::default_for_units(units);
  let region = SvgFilterRegion {
    x: SvgLength::parse(filter_node.attribute("x")).unwrap_or(default_region.x),
    y: SvgLength::parse(filter_node.attribute("y")).unwrap_or(default_region.y),
    width: SvgLength::parse(filter_node.attribute("width")).unwrap_or(default_region.width),
    height: SvgLength::parse(filter_node.attribute("height")).unwrap_or(default_region.height),
    units,
  };

  let mut steps = Vec::new();
  for child in filter_node.children().filter(|c| c.is_element()) {
    let result_name = child.attribute("result").map(|s| s.to_string());
    let tag = child.tag_name().name().to_ascii_lowercase();
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
      "feimage" => parse_fe_image(&child, image_cache),
      "fetile" => parse_fe_tile(&child),
      "feturbulence" => Some(FilterPrimitive::Turbulence),
      "fedisplacementmap" => parse_fe_displacement_map(&child),
      "feconvolvematrix" => parse_fe_convolve_matrix(&child),
      _ => None,
    };
    if let Some(prim) = primitive {
      steps.push(FilterStep {
        result: result_name,
        primitive: prim,
      });
    }
  }

  if steps.is_empty() {
    return None;
  }

  Some(Arc::new(SvgFilter { steps, region }))
}

impl SvgFilter {
  pub fn resolve_region(&self, bbox: Rect) -> Rect {
    self.region.resolve(bbox)
  }
}

fn parse_input(attr: Option<&str>) -> FilterInput {
  match attr.map(|s| s.trim()) {
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

fn parse_color(value: Option<&str>) -> Option<Rgba> {
  let raw = value?.trim();
  if let Ok(parsed) = color::Color::parse(raw) {
    return Some(parsed.to_rgba(Rgba::BLACK));
  }
  None
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
  let std_dev = parse_number(node.attribute("stdDeviation")).abs();
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
  let radius = parse_number(node.attribute("radius")).abs();
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
  let operator = match node
    .attribute("operator")
    .unwrap_or("over")
    .to_ascii_lowercase()
    .as_str()
  {
    "in" => CompositeOperator::In,
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
  let std_dev = parse_number(node.attribute("stdDeviation")).abs();
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

fn parse_fe_image(node: &roxmltree::Node, cache: &ImageCache) -> Option<FilterPrimitive> {
  let href = node
    .attribute("href")
    .or_else(|| node.attribute("xlink:href"))?;
  let loaded = cache.load(href).ok()?;
  let dyn_img = loaded.image.as_ref();
  let rgba = dyn_img.to_rgba8();
  let (w, h) = rgba.dimensions();
  let size = tiny_skia::IntSize::from_wh(w, h)?;
  let pixmap = Pixmap::from_vec(rgba.into_raw(), size)?;
  Some(FilterPrimitive::Image(pixmap))
}

fn parse_fe_tile(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  Some(FilterPrimitive::Tile { input })
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

pub fn apply_svg_filter(def: &SvgFilter, pixmap: &mut Pixmap) {
  let source = pixmap.clone();
  let mut results: HashMap<String, Pixmap> = HashMap::new();
  let mut current = source.clone();

  for step in &def.steps {
    if let Some(next) = apply_primitive(&step.primitive, &source, &results, &current) {
      if let Some(name) = &step.result {
        results.insert(name.clone(), next.clone());
      }
      current = next;
    }
  }

  *pixmap = current;
}

fn apply_primitive(
  primitive: &FilterPrimitive,
  source: &Pixmap,
  results: &HashMap<String, Pixmap>,
  current: &Pixmap,
) -> Option<Pixmap> {
  match primitive {
    FilterPrimitive::Flood { color, opacity } => {
      flood(source.width(), source.height(), color, *opacity)
    }
    FilterPrimitive::GaussianBlur { input, std_dev } => {
      let mut img = resolve_input(input, source, results, current)?;
      if *std_dev > 0.0 {
        apply_gaussian_blur(&mut img, *std_dev);
      }
      Some(img)
    }
    FilterPrimitive::Offset { input, dx, dy } => {
      resolve_input(input, source, results, current).map(|img| offset_pixmap(img, *dx, *dy))
    }
    FilterPrimitive::ColorMatrix { input, kind } => {
      let mut img = resolve_input(input, source, results, current)?;
      apply_color_matrix(&mut img, kind);
      Some(img)
    }
    FilterPrimitive::Composite {
      input1,
      input2,
      operator,
    } => composite_pixmaps(
      resolve_input(input1, source, results, current),
      resolve_input(input2, source, results, current),
      *operator,
    ),
    FilterPrimitive::Merge { inputs } => Some(merge_inputs(inputs, source, results, current)),
    FilterPrimitive::DropShadow {
      input,
      dx,
      dy,
      std_dev,
      color,
      opacity,
    } => resolve_input(input, source, results, current)
      .map(|img| drop_shadow_pixmap(img, *dx, *dy, *std_dev, color, *opacity)),
    FilterPrimitive::Blend {
      input1,
      input2,
      mode,
    } => blend_pixmaps(
      resolve_input(input1, source, results, current),
      resolve_input(input2, source, results, current),
      *mode,
    ),
    FilterPrimitive::Morphology { input, radius, op } => {
      resolve_input(input, source, results, current).map(|mut img| {
        apply_morphology(&mut img, *radius, *op);
        img
      })
    }
    FilterPrimitive::ComponentTransfer { input, r, g, b, a } => {
      resolve_input(input, source, results, current).map(|mut img| {
        apply_component_transfer(&mut img, r, g, b, a);
        img
      })
    }
    FilterPrimitive::Image(pix) => Some(pix.clone()),
    FilterPrimitive::Tile { input } => resolve_input(input, source, results, current),
    FilterPrimitive::Turbulence => Some(source.clone()),
    FilterPrimitive::DisplacementMap {
      in1,
      in2,
      scale,
      x_channel,
      y_channel,
    } => {
      let primary = resolve_input(in1, source, results, current)?;
      let map = resolve_input(in2, source, results, current)?;
      apply_displacement_map(&primary, &map, *scale, *x_channel, *y_channel)
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
    } => resolve_input(input, source, results, current).map(|img| {
      apply_convolve_matrix(
        img,
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
      )
    }),
  }
}

fn resolve_input(
  input: &FilterInput,
  source: &Pixmap,
  results: &HashMap<String, Pixmap>,
  current: &Pixmap,
) -> Option<Pixmap> {
  match input {
    FilterInput::SourceGraphic => Some(source.clone()),
    FilterInput::SourceAlpha => {
      let mut mask = Pixmap::new(source.width(), source.height())?;
      for (dst, src) in mask.pixels_mut().iter_mut().zip(source.pixels().iter()) {
        *dst = PremultipliedColorU8::from_rgba(0, 0, 0, src.alpha())
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
      }
      Some(mask)
    }
    FilterInput::Reference(name) => results.get(name).cloned().or_else(|| Some(source.clone())),
    FilterInput::Previous => Some(current.clone()),
  }
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

fn composite_pixmaps(
  input1: Option<Pixmap>,
  input2: Option<Pixmap>,
  op: CompositeOperator,
) -> Option<Pixmap> {
  let a = input1?;
  let b = input2.unwrap_or_else(|| a.clone());
  match op {
    CompositeOperator::In => Some(mask_with(&a, &b)),
    CompositeOperator::Over => Some(draw_over(&b, &a)),
  }
}

fn blend_pixmaps(a: Option<Pixmap>, b: Option<Pixmap>, mode: BlendMode) -> Option<Pixmap> {
  let mut base = a?;
  let top = b.unwrap_or_else(|| base.clone());
  let mut paint = PixmapPaint::default();
  paint.blend_mode = mode;
  base.draw_pixmap(0, 0, top.as_ref(), &paint, Transform::identity(), None);
  Some(base)
}

fn merge_inputs(
  inputs: &[FilterInput],
  source: &Pixmap,
  results: &HashMap<String, Pixmap>,
  current: &Pixmap,
) -> Pixmap {
  let mut out = Pixmap::new(source.width(), source.height()).unwrap();
  for input in inputs {
    if let Some(img) = resolve_input(input, source, results, current) {
      out = draw_over(&out, &img);
    }
  }
  out
}

fn drop_shadow_pixmap(
  input: Pixmap,
  dx: f32,
  dy: f32,
  stddev: f32,
  color: &Rgba,
  opacity: f32,
) -> Pixmap {
  let mut tinted = input.clone();
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
  if stddev > 0.0 {
    apply_gaussian_blur(&mut shadow, stddev);
  }

  let mut out = Pixmap::new(input.width(), input.height()).unwrap();
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
  out.draw_pixmap(0, 0, input.as_ref(), &paint, Transform::identity(), None);
  out
}

fn draw_over(bottom: &Pixmap, top: &Pixmap) -> Pixmap {
  let mut out = bottom.clone();
  let mut paint = PixmapPaint::default();
  paint.blend_mode = tiny_skia::BlendMode::SourceOver;
  out.draw_pixmap(0, 0, top.as_ref(), &paint, Transform::identity(), None);
  out
}

fn mask_with(src: &Pixmap, mask: &Pixmap) -> Pixmap {
  let mut out = src.clone();
  let mask_pixels = mask.pixels();
  for (dst, m) in out.pixels_mut().iter_mut().zip(mask_pixels.iter()) {
    let factor = m.alpha() as f32 / 255.0;
    let r = (dst.red() as f32 * factor).round().clamp(0.0, 255.0) as u8;
    let g = (dst.green() as f32 * factor).round().clamp(0.0, 255.0) as u8;
    let b = (dst.blue() as f32 * factor).round().clamp(0.0, 255.0) as u8;
    let a = (dst.alpha() as f32 * factor).round().clamp(0.0, 255.0) as u8;
    *dst = PremultipliedColorU8::from_rgba(r, g, b, a).unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }
  out
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

fn apply_morphology(pixmap: &mut Pixmap, radius: f32, op: MorphologyOp) {
  let radius = radius.abs().ceil() as i32;
  if radius <= 0 {
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
    for dy in -radius..=radius {
      for dx in -radius..=radius {
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
    let input = to_unpremultiplied(*px);
    let out = UnpremultipliedColor {
      r: sample(&r_lut, input.r),
      g: sample(&g_lut, input.g),
      b: sample(&b_lut, input.b),
      a: sample(&a_lut, input.a),
    };
    *px = to_premultiplied(out);
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

fn apply_color_matrix(pixmap: &mut Pixmap, kind: &ColorMatrixKind) {
  match kind {
    ColorMatrixKind::Matrix(values) => apply_color_matrix_values(pixmap, values),
    ColorMatrixKind::LuminanceToAlpha => {
      let pixels = pixmap.pixels_mut();
      for px in pixels.iter_mut() {
        let color = to_unpremultiplied(*px);
        let lum = 0.2126 * color.r + 0.7152 * color.g + 0.0722 * color.b;
        *px = to_premultiplied(UnpremultipliedColor {
          r: 0.0,
          g: 0.0,
          b: 0.0,
          a: clamp01(lum),
        });
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
      apply_color_matrix_values(pixmap, &matrix);
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
      apply_color_matrix_values(pixmap, &matrix);
    }
  }
}

fn apply_color_matrix_values(pixmap: &mut Pixmap, matrix: &[f32; 20]) {
  let pixels = pixmap.pixels_mut();
  for px in pixels.iter_mut() {
    let input = to_unpremultiplied(*px);
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
    *px = to_premultiplied(UnpremultipliedColor {
      r: clamp01(r2),
      g: clamp01(g2),
      b: clamp01(b2),
      a: clamp01(a2),
    });
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

    apply_color_matrix_values(&mut pixmap, &matrix);

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

    apply_color_matrix(&mut pixmap, &ColorMatrixKind::LuminanceToAlpha);

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
    let out = apply_primitive(&prim, &pixmap, &HashMap::new(), &pixmap).unwrap();
    assert_eq!(pixmap.pixels(), out.pixels());
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

    let out = apply_primitive(&prim, &pixmap, &HashMap::new(), &pixmap).unwrap();
    let center = out.pixels()[4];
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

    let dup = apply_primitive(&base, &pixmap, &HashMap::new(), &pixmap).unwrap();
    let none = apply_primitive(&none_mode, &pixmap, &HashMap::new(), &pixmap).unwrap();
    let dup_corner = dup.pixels()[0];
    let none_corner = none.pixels()[0];

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
    let out = apply_primitive(&prim, &pixmap, &HashMap::new(), &pixmap).unwrap();
    let out_pixels = out.pixels();
    assert_eq!(out_pixels[0], pixels[0]); // inside subregion
    assert_eq!(out_pixels[1], PremultipliedColorU8::TRANSPARENT);
    assert_eq!(out_pixels[2], PremultipliedColorU8::TRANSPARENT);
    assert_eq!(out_pixels[3], PremultipliedColorU8::TRANSPARENT);
  }
}
