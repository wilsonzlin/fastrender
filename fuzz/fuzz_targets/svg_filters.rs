#![no_main]

use arbitrary::Arbitrary;
use fastrender::geometry::Rect;
use fastrender::image_loader::ImageCache;
use fastrender::paint::svg_filter::{apply_svg_filter, parse_svg_filter_from_svg_document};
use fastrender::resource::{FetchedResource, ResourceFetcher};
use libfuzzer_sys::fuzz_target;
use std::sync::Arc;
use tiny_skia::Pixmap;

const MAX_FILTER_STEPS: usize = 8;
const MAX_PIXMAP_DIM: u32 = 64;

#[derive(Debug, Arbitrary)]
struct SvgFilterInput {
  id_seed: String,
  steps: Vec<PrimitiveSpec>,
  width: u16,
  height: u16,
  filter_res: Option<(u16, u16)>,
  scale: f32,
}

#[derive(Debug, Arbitrary)]
struct PrimitiveSpec {
  kind: PrimitiveKind,
  value_a: f32,
  value_b: f32,
  value_c: f32,
  channels: (u8, u8),
  extra: String,
}

#[derive(Debug, Arbitrary, Copy, Clone)]
enum PrimitiveKind {
  Flood,
  GaussianBlur,
  Offset,
  ColorMatrix,
  Composite,
  Merge,
  DropShadow,
  Blend,
  Morphology,
  ComponentTransfer,
  Tile,
  Turbulence,
  DisplacementMap,
  ConvolveMatrix,
}

#[derive(Clone)]
struct FuzzFetcher;

impl ResourceFetcher for FuzzFetcher {
  fn fetch(&self, _url: &str) -> fastrender::Result<FetchedResource> {
    Err(fastrender::error::Error::Other(
      "network access disabled in fuzz target".to_string(),
    ))
  }
}

fn sanitize_ident(raw: &str, fallback: &str) -> String {
  let filtered: String = raw
    .chars()
    .filter(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_' | ':'))
    .take(32)
    .collect();
  if filtered.is_empty() {
    fallback.to_string()
  } else {
    filtered
  }
}

fn clamp_float(v: f32, limit: f32, default: f32) -> f32 {
  if v.is_finite() {
    v.clamp(-limit, limit)
  } else {
    default
  }
}

fn positive_float(v: f32, max: f32, default: f32) -> f32 {
  if v.is_finite() {
    v.abs().min(max)
  } else {
    default
  }
}

fn build_filter_svg(input: &SvgFilterInput) -> (String, String) {
  let id = sanitize_ident(&input.id_seed, "fuzz");
  let res_attr = input.filter_res.map(|(w, h)| {
    let w = w.clamp(1, 128);
    let h = h.clamp(1, 128);
    format!(" filterRes=\"{} {}\"", w, h)
  });

  let mut svg = format!(
    "<svg xmlns=\"http://www.w3.org/2000/svg\"><filter id=\"{id}\" x=\"-20%\" y=\"-20%\" width=\"140%\" height=\"140%\" primitiveUnits=\"userSpaceOnUse\"{}>",
    res_attr.unwrap_or_default()
  );

  if input.steps.is_empty() {
    svg.push_str("<feGaussianBlur stdDeviation=\"0.5\" result=\"res0\"/>");
  } else {
    let mut current_input = "SourceGraphic".to_string();
    for (idx, prim) in input.steps.iter().take(MAX_FILTER_STEPS).enumerate() {
      let (xml, result_name) = primitive_to_xml(idx, prim, &current_input);
      svg.push_str(&xml);
      current_input = result_name;
    }
  }

  svg.push_str("</filter></svg>");
  (svg, id)
}

fn primitive_to_xml(idx: usize, prim: &PrimitiveSpec, current_input: &str) -> (String, String) {
  let result = format!("res{idx}");
  let safe_a = clamp_float(prim.value_a, 64.0, 0.0);
  let safe_b = clamp_float(prim.value_b, 64.0, 0.0);
  let safe_c = clamp_float(prim.value_c, 64.0, 0.0);
  let pct = |v: f32| positive_float(v, 200.0, 0.0);
  let channel = |idx: u8| match idx % 4 {
    0 => "R",
    1 => "G",
    2 => "B",
    _ => "A",
  };

  let xml = match prim.kind {
    PrimitiveKind::Flood => {
      let r = prim.channels.0;
      let g = prim.channels.1;
      let b = ((safe_c.abs() as i32) % 255) as u8;
      let opacity = (safe_a / 10.0).clamp(0.0, 1.0);
      format!("<feFlood flood-color=\"rgb({r},{g},{b})\" flood-opacity=\"{opacity:.2}\" result=\"{result}\"/>")
    }
    PrimitiveKind::GaussianBlur => {
      let sx = positive_float(safe_a, 12.0, 1.0);
      let sy = positive_float(safe_b, 12.0, sx);
      format!("<feGaussianBlur in=\"{current_input}\" stdDeviation=\"{sx:.2} {sy:.2}\" result=\"{result}\"/>")
    }
    PrimitiveKind::Offset => {
      let dx = clamp_float(safe_a, 50.0, 0.0);
      let dy = clamp_float(safe_b, 50.0, 0.0);
      format!("<feOffset in=\"{current_input}\" dx=\"{dx:.1}\" dy=\"{dy:.1}\" result=\"{result}\"/>")
    }
    PrimitiveKind::ColorMatrix => {
      let values = [
        1.0 + safe_a * 0.01,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0 + safe_b * 0.01,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0 + safe_c * 0.01,
        0.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
      ];
      let mut buf = String::new();
      for v in values {
        buf.push_str(&format!("{v:.3} "));
      }
      format!("<feColorMatrix in=\"{current_input}\" type=\"matrix\" values=\"{buf}\" result=\"{result}\"/>")
    }
    PrimitiveKind::Composite => {
      let ops = ["over", "in", "out", "atop", "xor", "arithmetic"];
      let op = ops[(prim.channels.0 as usize) % ops.len()];
      let (k1, k2, k3, k4) = (
        (safe_a * 0.05).clamp(-1.0, 1.0),
        (safe_b * 0.05).clamp(-1.0, 1.0),
        (safe_c * 0.05).clamp(-1.0, 1.0),
        0.5,
      );
      format!(
        "<feComposite in=\"{current_input}\" in2=\"SourceAlpha\" operator=\"{op}\" k1=\"{k1:.2}\" k2=\"{k2:.2}\" k3=\"{k3:.2}\" k4=\"{k4:.2}\" result=\"{result}\"/>"
      )
    }
    PrimitiveKind::Merge => format!(
      "<feMerge result=\"{result}\"><feMergeNode in=\"{current_input}\"/><feMergeNode in=\"SourceGraphic\"/></feMerge>"
    ),
    PrimitiveKind::DropShadow => {
      let dx = clamp_float(safe_a, 20.0, 0.0);
      let dy = clamp_float(safe_b, 20.0, 0.0);
      let std_dev = positive_float(prim.value_c, 20.0, 1.0);
      let opacity = ((prim.channels.0 as f32) / 255.0).clamp(0.0, 1.0);
      format!("<feDropShadow in=\"{current_input}\" dx=\"{dx:.1}\" dy=\"{dy:.1}\" stdDeviation=\"{std_dev:.1}\" flood-opacity=\"{opacity:.2}\" result=\"{result}\"/>")
    }
    PrimitiveKind::Blend => {
      let modes = [
        "normal", "multiply", "screen", "darken", "lighten", "overlay", "color-dodge", "color-burn",
        "hard-light", "soft-light", "difference", "exclusion", "hue", "saturation", "color",
        "luminosity",
      ];
      let mode = modes[(prim.channels.1 as usize) % modes.len()];
      format!(
        "<feBlend in=\"{current_input}\" in2=\"SourceGraphic\" mode=\"{mode}\" result=\"{result}\"/>"
      )
    }
    PrimitiveKind::Morphology => {
      let rx = positive_float(safe_a, 20.0, 1.0);
      let ry = positive_float(safe_b, 20.0, rx);
      let op = if prim.channels.0 % 2 == 0 { "dilate" } else { "erode" };
      format!("<feMorphology in=\"{current_input}\" operator=\"{op}\" radius=\"{rx:.1} {ry:.1}\" result=\"{result}\"/>")
    }
    PrimitiveKind::ComponentTransfer => {
      let slope = (safe_a * 0.1 + 1.0).clamp(0.1, 4.0);
      let intercept = (safe_b * 0.02).clamp(-1.0, 1.0);
      format!(
        "<feComponentTransfer in=\"{current_input}\" result=\"{result}\"><feFuncR type=\"linear\" slope=\"{slope:.3}\" intercept=\"{intercept:.3}\"/><feFuncG type=\"linear\" slope=\"{slope:.3}\" intercept=\"{intercept:.3}\"/><feFuncB type=\"linear\" slope=\"{slope:.3}\" intercept=\"{intercept:.3}\"/><feFuncA type=\"identity\"/></feComponentTransfer>"
      )
    }
    PrimitiveKind::Tile => {
      format!("<feTile in=\"{current_input}\" result=\"{result}\"/>")
    }
    PrimitiveKind::Turbulence => {
      let fx = (pct(safe_a) + 1.0) * 0.005;
      let fy = (pct(safe_b) + 1.0) * 0.005;
      let octaves = (1 + (prim.channels.0 % 4)).max(1);
      let seed = (prim.channels.1 as u32) % 1024;
      let kind = if prim.channels.0 % 2 == 0 { "turbulence" } else { "fractalNoise" };
      format!("<feTurbulence type=\"{kind}\" baseFrequency=\"{fx:.4} {fy:.4}\" numOctaves=\"{octaves}\" seed=\"{seed}\" result=\"{result}\"/>")
    }
    PrimitiveKind::DisplacementMap => {
      let scale = pct(safe_a).min(80.0);
      let x_channel = channel(prim.channels.0);
      let y_channel = channel(prim.channels.1);
      format!(
        "<feDisplacementMap in=\"{current_input}\" in2=\"SourceGraphic\" scale=\"{scale:.1}\" xChannelSelector=\"{x_channel}\" yChannelSelector=\"{y_channel}\" result=\"{result}\"/>"
      )
    }
    PrimitiveKind::ConvolveMatrix => {
      let order_x = (prim.channels.0 % 3 + 1) as usize;
      let order_y = (prim.channels.1 % 3 + 1) as usize;
      let mut kernel = String::new();
      for idx in 0..(order_x * order_y) {
        let v = clamp_float(prim.value_a + prim.value_b * idx as f32 * 0.1, 4.0, 0.0);
        kernel.push_str(&format!("{v:.2} "));
      }
      format!(
        "<feConvolveMatrix in=\"{current_input}\" order=\"{order_x} {order_y}\" kernelMatrix=\"{kernel}\" preserveAlpha=\"true\" result=\"{result}\"/>"
      )
    }
  };

  (xml, result)
}

fn build_pixmap(width: u16, height: u16) -> Option<Pixmap> {
  let width = width.clamp(1, MAX_PIXMAP_DIM as u16) as u32;
  let height = height.clamp(1, MAX_PIXMAP_DIM as u16) as u32;
  Pixmap::new(width, height)
}

fuzz_target!(|input: SvgFilterInput| {
  let fetcher = Arc::new(FuzzFetcher) as Arc<dyn ResourceFetcher>;
  let cache = ImageCache::with_fetcher(fetcher);

  let (svg, id) = build_filter_svg(&input);
  let Some(filter) = parse_svg_filter_from_svg_document(&svg, Some(&id), &cache) else {
    return;
  };

  let Some(mut pixmap) = build_pixmap(input.width, input.height) else {
    return;
  };

  let scale = positive_float(input.scale, 8.0, 1.0).max(0.1);
  let bbox = Rect::from_xywh(0.0, 0.0, pixmap.width() as f32, pixmap.height() as f32);
  apply_svg_filter(&filter, &mut pixmap, scale, bbox);
});
