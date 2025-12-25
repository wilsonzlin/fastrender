use crate::image_loader::ImageCache;
use crate::paint::blur::apply_gaussian_blur;
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
use tiny_skia::{BlendMode, Pixmap, PixmapPaint, PremultipliedColorU8, Transform};

static FILTER_CACHE: OnceLock<Mutex<HashMap<String, Arc<SvgFilter>>>> = OnceLock::new();

fn filter_cache() -> &'static Mutex<HashMap<String, Arc<SvgFilter>>> {
  FILTER_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

#[derive(Clone, Debug)]
pub struct SvgFilter {
  pub steps: Vec<FilterStep>,
}

#[derive(Clone, Debug)]
pub struct FilterStep {
  pub result: Option<String>,
  pub primitive: FilterPrimitive,
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
    input: FilterInput,
  },
  ConvolveMatrix {
    input: FilterInput,
  },
}

#[derive(Clone, Debug)]
pub enum FilterInput {
  SourceGraphic,
  SourceAlpha,
  Reference(String),
  Previous,
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
pub enum TransferFn {
  Identity,
  Linear { slope: f32, intercept: f32 },
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

  Some(Arc::new(SvgFilter { steps }))
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

fn parse_color(value: Option<&str>) -> Option<Rgba> {
  let raw = value?.trim();
  if let Ok(parsed) = color::Color::parse(raw) {
    return Some(parsed.to_rgba(Rgba::BLACK));
  }
  None
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
  match ty.as_str() {
    "linear" => {
      let slope = node
        .attribute("slope")
        .and_then(|v| v.parse::<f32>().ok())
        .unwrap_or(1.0);
      let intercept = node
        .attribute("intercept")
        .and_then(|v| v.parse::<f32>().ok())
        .unwrap_or(0.0);
      Some(TransferFn::Linear { slope, intercept })
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
  let input = parse_input(node.attribute("in"));
  Some(FilterPrimitive::DisplacementMap { input })
}

fn parse_fe_convolve_matrix(node: &roxmltree::Node) -> Option<FilterPrimitive> {
  let input = parse_input(node.attribute("in"));
  Some(FilterPrimitive::ConvolveMatrix { input })
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
        apply_component_transfer(&mut img, *r, *g, *b, *a);
        img
      })
    }
    FilterPrimitive::Image(pix) => Some(pix.clone()),
    FilterPrimitive::Tile { input } => resolve_input(input, source, results, current),
    FilterPrimitive::Turbulence => Some(source.clone()),
    FilterPrimitive::DisplacementMap { input } => resolve_input(input, source, results, current),
    FilterPrimitive::ConvolveMatrix { input } => resolve_input(input, source, results, current),
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
  r: TransferFn,
  g: TransferFn,
  b: TransferFn,
  a: TransferFn,
) {
  let apply = |v: f32, f: TransferFn| -> f32 {
    match f {
      TransferFn::Identity => v,
      TransferFn::Linear { slope, intercept } => (slope * v + intercept).clamp(0.0, 1.0),
    }
  };

  for px in pixmap.pixels_mut() {
    let rf = px.red() as f32 / 255.0;
    let gf = px.green() as f32 / 255.0;
    let bf = px.blue() as f32 / 255.0;
    let af = px.alpha() as f32 / 255.0;
    let ra = apply(rf, r);
    let ga = apply(gf, g);
    let ba = apply(bf, b);
    let aa = apply(af, a);
    *px = PremultipliedColorU8::from_rgba(
      (ra * aa * 255.0).round().clamp(0.0, 255.0) as u8,
      (ga * aa * 255.0).round().clamp(0.0, 255.0) as u8,
      (ba * aa * 255.0).round().clamp(0.0, 255.0) as u8,
      (aa * 255.0).round().clamp(0.0, 255.0) as u8,
    )
    .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }
}

fn apply_color_matrix(pixmap: &mut Pixmap, kind: &ColorMatrixKind) {
  match kind {
    ColorMatrixKind::Matrix(values) => apply_color_matrix_values(pixmap, values),
    ColorMatrixKind::LuminanceToAlpha => {
      let pixels = pixmap.pixels_mut();
      for px in pixels.iter_mut() {
        let a = px.alpha() as f32 / 255.0;
        let r = px.red() as f32 / 255.0;
        let g = px.green() as f32 / 255.0;
        let b = px.blue() as f32 / 255.0;
        let lum = 0.2126 * r + 0.7152 * g + 0.0722 * b;
        let alpha = (lum * a * 255.0).round().clamp(0.0, 255.0) as u8;
        *px = PremultipliedColorU8::from_rgba(0, 0, 0, alpha)
          .unwrap_or(PremultipliedColorU8::TRANSPARENT);
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
    let r = px.red() as f32 / 255.0;
    let g = px.green() as f32 / 255.0;
    let b = px.blue() as f32 / 255.0;
    let a = px.alpha() as f32 / 255.0;
    let r2 = matrix[0] * r + matrix[1] * g + matrix[2] * b + matrix[3] * a + matrix[4];
    let g2 = matrix[5] * r + matrix[6] * g + matrix[7] * b + matrix[8] * a + matrix[9];
    let b2 = matrix[10] * r + matrix[11] * g + matrix[12] * b + matrix[13] * a + matrix[14];
    let a2 = matrix[15] * r + matrix[16] * g + matrix[17] * b + matrix[18] * a + matrix[19];
    let clamp = |v: f32| v.clamp(0.0, 1.0);
    let a_out = clamp(a2);
    let to_channel = |v: f32| (clamp(v) * a_out * 255.0).round().clamp(0.0, 255.0) as u8;
    let a_byte = (a_out * 255.0).round().clamp(0.0, 255.0) as u8;
    *px = PremultipliedColorU8::from_rgba(to_channel(r2), to_channel(g2), to_channel(b2), a_byte)
      .unwrap_or(PremultipliedColorU8::TRANSPARENT);
  }
}
