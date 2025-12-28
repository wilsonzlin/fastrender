use roxmltree::Document;
use tiny_skia::Transform;

/// Utility helpers for working with SVG metadata.
///
/// SVG length attributes accept a subset of CSS absolute units. Percentages are
/// valid in the SVG grammar but cannot be resolved to an intrinsic size without
/// a viewport, so they are surfaced as [`SvgLength::Percentage`] and treated as
/// non-definite by callers.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum SvgLength {
  Px(f32),
  Percentage(f32),
}

impl SvgLength {
  pub(crate) fn to_px(self) -> Option<f32> {
    match self {
      SvgLength::Px(px) => Some(px),
      SvgLength::Percentage(_) => None,
    }
  }
}

pub(crate) fn parse_svg_length(value: &str) -> Option<SvgLength> {
  let trimmed = value.trim();
  if trimmed.is_empty() {
    return None;
  }

  let mut end = 0;
  for (idx, ch) in trimmed.char_indices() {
    if matches!(ch, '0'..='9' | '+' | '-' | '.' | 'e' | 'E') {
      end = idx + ch.len_utf8();
    } else {
      break;
    }
  }

  if end == 0 {
    return None;
  }

  let number = trimmed[..end].parse::<f32>().ok()?;
  if !number.is_finite() {
    return None;
  }

  let unit = trimmed[end..].trim();
  if unit.eq_ignore_ascii_case("%") {
    return Some(SvgLength::Percentage(number));
  }

  let px = if unit.is_empty() || unit.eq_ignore_ascii_case("px") {
    number
  } else if unit.eq_ignore_ascii_case("in") {
    number * 96.0
  } else if unit.eq_ignore_ascii_case("cm") {
    number * (96.0 / 2.54)
  } else if unit.eq_ignore_ascii_case("mm") {
    number * (96.0 / 25.4)
  } else if unit.eq_ignore_ascii_case("pt") {
    number * (96.0 / 72.0)
  } else if unit.eq_ignore_ascii_case("pc") {
    number * (96.0 / 6.0)
  } else {
    return None;
  };

  px.is_finite().then_some(SvgLength::Px(px))
}

pub(crate) fn parse_svg_length_px(value: &str) -> Option<f32> {
  parse_svg_length(value)?.to_px()
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct SvgIntrinsicDimensions {
  pub width: Option<f32>,
  pub height: Option<f32>,
  pub aspect_ratio: Option<f32>,
  pub aspect_ratio_none: bool,
}

pub(crate) fn svg_intrinsic_dimensions_from_attributes(
  width: Option<&str>,
  height: Option<&str>,
  view_box: Option<&str>,
  preserve_aspect_ratio: Option<&str>,
) -> SvgIntrinsicDimensions {
  let width_px = width
    .and_then(parse_svg_length)
    .and_then(SvgLength::to_px)
    .filter(|v| v.is_finite());
  let height_px = height
    .and_then(parse_svg_length)
    .and_then(SvgLength::to_px)
    .filter(|v| v.is_finite());

  let aspect_ratio_none = preserve_aspect_ratio
    .and_then(|par| par.split_whitespace().next())
    .map(|v| v.eq_ignore_ascii_case("none"))
    .unwrap_or(false);

  let view_box_ratio = if aspect_ratio_none {
    None
  } else {
    parse_view_box_ratio(view_box)
  };

  let aspect_ratio = if aspect_ratio_none {
    None
  } else if let (Some(w), Some(h)) = (width_px, height_px) {
    (h > 0.0).then_some(w / h)
  } else {
    view_box_ratio
  };

  let mut resolved_width = width_px;
  let mut resolved_height = height_px;
  if let Some(r) = aspect_ratio {
    if resolved_width.is_some() && resolved_height.is_none() && r > 0.0 {
      resolved_height = resolved_width.map(|w| w / r);
    } else if resolved_height.is_some() && resolved_width.is_none() && r > 0.0 {
      resolved_width = resolved_height.map(|h| h * r);
    }
  }

  SvgIntrinsicDimensions {
    width: resolved_width,
    height: resolved_height,
    aspect_ratio,
    aspect_ratio_none,
  }
}

fn parse_view_box_ratio(view_box: Option<&str>) -> Option<f32> {
  let raw = view_box?;
  let mut parts = raw
    .split(|c: char| c == ',' || c.is_whitespace())
    .filter(|s| !s.is_empty())
    .filter_map(|s| s.parse::<f32>().ok());
  let _ = parts.next();
  let _ = parts.next();
  let width = parts.next()?;
  let height = parts.next()?;
  if width > 0.0 && height > 0.0 && width.is_finite() && height.is_finite() {
    Some(width / height)
  } else {
    None
  }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct SvgViewBox {
  pub(crate) min_x: f32,
  pub(crate) min_y: f32,
  pub(crate) width: f32,
  pub(crate) height: f32,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum SvgAlign {
  XMinYMin,
  XMidYMin,
  XMaxYMin,
  XMinYMid,
  XMidYMid,
  XMaxYMid,
  XMinYMax,
  XMidYMax,
  XMaxYMax,
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum SvgMeetOrSlice {
  Meet,
  Slice,
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct SvgPreserveAspectRatio {
  pub(crate) none: bool,
  pub(crate) align: SvgAlign,
  pub(crate) meet_or_slice: SvgMeetOrSlice,
}

impl SvgPreserveAspectRatio {
  pub fn parse(value: Option<&str>) -> Self {
    let mut parsed = Self {
      none: false,
      align: SvgAlign::XMidYMid,
      meet_or_slice: SvgMeetOrSlice::Meet,
    };

    let raw = value.unwrap_or("").trim();
    if raw.is_empty() {
      return parsed;
    }
    let mut parts = raw.split_whitespace();
    let first = parts.next().unwrap_or("");
    if first.eq_ignore_ascii_case("none") {
      parsed.none = true;
      return parsed;
    }

    parsed.align = match first {
      "xMinYMin" => SvgAlign::XMinYMin,
      "xMidYMin" => SvgAlign::XMidYMin,
      "xMaxYMin" => SvgAlign::XMaxYMin,
      "xMinYMid" => SvgAlign::XMinYMid,
      "xMidYMid" => SvgAlign::XMidYMid,
      "xMaxYMid" => SvgAlign::XMaxYMid,
      "xMinYMax" => SvgAlign::XMinYMax,
      "xMidYMax" => SvgAlign::XMidYMax,
      "xMaxYMax" => SvgAlign::XMaxYMax,
      _ => SvgAlign::XMidYMid,
    };

    if let Some(second) = parts.next() {
      if second.eq_ignore_ascii_case("slice") {
        parsed.meet_or_slice = SvgMeetOrSlice::Slice;
      } else if second.eq_ignore_ascii_case("meet") {
        parsed.meet_or_slice = SvgMeetOrSlice::Meet;
      }
    }

    parsed
  }
}

pub(crate) fn parse_svg_view_box(value: &str) -> Option<SvgViewBox> {
  let mut nums = value
    .split(|c: char| c == ',' || c.is_whitespace())
    .filter(|s| !s.is_empty())
    .filter_map(|s| s.parse::<f32>().ok());
  let min_x = nums.next()?;
  let min_y = nums.next()?;
  let width = nums.next()?;
  let height = nums.next()?;
  if !(min_x.is_finite()
    && min_y.is_finite()
    && width.is_finite()
    && height.is_finite()
    && width > 0.0
    && height > 0.0)
  {
    return None;
  }
  Some(SvgViewBox {
    min_x,
    min_y,
    width,
    height,
  })
}

pub(crate) fn map_svg_aspect_ratio(
  view_box: SvgViewBox,
  preserve: SvgPreserveAspectRatio,
  render_width: f32,
  render_height: f32,
) -> Transform {
  let sx = render_width / view_box.width;
  let sy = render_height / view_box.height;
  if preserve.none {
    return Transform::from_row(sx, 0.0, 0.0, sy, -view_box.min_x * sx, -view_box.min_y * sy);
  }

  let scale = match preserve.meet_or_slice {
    SvgMeetOrSlice::Meet => sx.min(sy),
    SvgMeetOrSlice::Slice => sx.max(sy),
  };
  let scaled_w = view_box.width * scale;
  let scaled_h = view_box.height * scale;

  let (align_x, align_y) = match preserve.align {
    SvgAlign::XMinYMin => (0.0, 0.0),
    SvgAlign::XMidYMin => ((render_width - scaled_w) * 0.5, 0.0),
    SvgAlign::XMaxYMin => (render_width - scaled_w, 0.0),
    SvgAlign::XMinYMid => (0.0, (render_height - scaled_h) * 0.5),
    SvgAlign::XMidYMid => (
      (render_width - scaled_w) * 0.5,
      (render_height - scaled_h) * 0.5,
    ),
    SvgAlign::XMaxYMid => (render_width - scaled_w, (render_height - scaled_h) * 0.5),
    SvgAlign::XMinYMax => (0.0, render_height - scaled_h),
    SvgAlign::XMidYMax => ((render_width - scaled_w) * 0.5, render_height - scaled_h),
    SvgAlign::XMaxYMax => (render_width - scaled_w, render_height - scaled_h),
  };

  Transform::from_row(
    scale,
    0.0,
    0.0,
    scale,
    align_x - view_box.min_x * scale,
    align_y - view_box.min_y * scale,
  )
}

/// Extracts the root viewBox if present.
pub(crate) fn svg_root_view_box(svg_content: &str) -> Option<SvgViewBox> {
  let doc = Document::parse(svg_content).ok()?;
  let root = doc.root_element();
  if !root.tag_name().name().eq_ignore_ascii_case("svg") {
    return None;
  }
  root.attribute("viewBox").and_then(parse_svg_view_box)
}

pub(crate) fn svg_view_box_root_transform(
  svg_content: &str,
  source_width: f32,
  source_height: f32,
  dest_width: f32,
  dest_height: f32,
) -> Option<Transform> {
  let doc = Document::parse(svg_content).ok()?;
  let root = doc.root_element();
  if !root.tag_name().name().eq_ignore_ascii_case("svg") {
    return None;
  }

  let view_box = root.attribute("viewBox").and_then(parse_svg_view_box)?;
  let preserve = SvgPreserveAspectRatio::parse(root.attribute("preserveAspectRatio"));
  let source = map_svg_aspect_ratio(view_box, preserve, source_width, source_height);
  let dest = map_svg_aspect_ratio(view_box, preserve, dest_width, dest_height);

  Some(dest.pre_concat(source.invert().unwrap_or_else(Transform::identity)))
}
