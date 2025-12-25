//! CSS Property Application
//!
//! This module handles applying individual CSS property declarations
//! to a computed style struct.
//!
//! Reference: CSS Cascading and Inheritance Level 4
//! <https://www.w3.org/TR/css-cascade-4/>

use crate::css::properties::parse_calc_function_length;
use crate::css::properties::parse_length;
use crate::css::types::Declaration;
use crate::css::types::PropertyValue;
use crate::geometry::Size;
use crate::style::block_axis_is_horizontal;
use crate::style::color::Color;
use crate::style::color::Rgba;
use crate::style::content::parse_content;
use crate::style::content::ContentValue;
use crate::style::counters::CounterSet;
use crate::style::display::Display;
use crate::style::float::Clear;
use crate::style::float::Float;
use crate::style::grid::parse_grid_shorthand;
use crate::style::grid::parse_grid_template_areas;
use crate::style::grid::parse_grid_template_shorthand;
use crate::style::grid::parse_grid_tracks_with_names;
use crate::style::grid::parse_subgrid_line_names;
use crate::style::grid::parse_track_list;
use crate::style::grid::ParsedTracks;
use crate::style::inline_axis_is_horizontal;
use crate::style::position::Position;
use crate::style::types::*;
use crate::style::values::Length;
use crate::style::values::LengthUnit;
use crate::style::var_resolution::resolve_var_for_property;
use crate::style::var_resolution::VarResolutionResult;
use crate::style::ComputedStyle;
use cssparser::BasicParseErrorKind;
use cssparser::Parser;
use cssparser::ParserInput;
use cssparser::Token;
use std::cell::Cell;
use std::collections::HashMap;
use svgtypes::PathParser;

thread_local! {
    static IMAGE_SET_DPR: Cell<f32> = const { Cell::new(1.0) };
}

/// Default viewport used by tests and legacy callers.
pub const DEFAULT_VIEWPORT: Size = Size::new(1200.0, 800.0);

/// Executes a closure with the image-set selection density overridden.
/// Restores the previous value afterward.
pub fn with_image_set_dpr<T>(dpr: f32, f: impl FnOnce() -> T) -> T {
  IMAGE_SET_DPR.with(|cell| {
    let prev = cell.get();
    let clamped = if dpr.is_finite() && dpr > 0.0 {
      dpr
    } else {
      1.0
    };
    cell.set(clamped);
    let result = f();
    cell.set(prev);
    result
  })
}

fn current_image_set_dpr() -> f32 {
  IMAGE_SET_DPR.with(|cell| cell.get())
}

/// Populate line names for named grid areas (`<name>-start`/`<name>-end`).
fn synthesize_area_line_names(styles: &mut ComputedStyle) {
  if styles.grid_template_areas.is_empty() {
    return;
  }

  let col_count = styles.grid_template_columns.len();
  let row_count = styles.grid_template_rows.len();
  if col_count == 0 || row_count == 0 {
    return;
  }

  if let Some(bounds) = crate::style::grid::validate_area_rectangles(&styles.grid_template_areas) {
    let ensure_line = |lines: &mut Vec<Vec<String>>,
                       names: &mut HashMap<String, Vec<usize>>,
                       idx: usize,
                       name: String| {
      if lines.len() <= idx {
        lines.resize(idx + 1, Vec::new());
      }
      if !lines[idx].contains(&name) {
        lines[idx].push(name.clone());
        names.entry(name).or_default().push(idx);
      }
    };

    // Ensure line vectors are large enough for the current track counts.
    if styles.grid_column_line_names.len() < col_count + 1 {
      styles
        .grid_column_line_names
        .resize(col_count + 1, Vec::new());
    }
    if styles.grid_row_line_names.len() < row_count + 1 {
      styles.grid_row_line_names.resize(row_count + 1, Vec::new());
    }

    for (name, (top, bottom, left, right)) in bounds {
      let col_start = left;
      let col_end = right + 1;
      let row_start = top;
      let row_end = bottom + 1;

      ensure_line(
        &mut styles.grid_column_line_names,
        &mut styles.grid_column_names,
        col_start,
        format!("{}-start", name),
      );
      ensure_line(
        &mut styles.grid_column_line_names,
        &mut styles.grid_column_names,
        col_end,
        format!("{}-end", name),
      );
      ensure_line(
        &mut styles.grid_row_line_names,
        &mut styles.grid_row_names,
        row_start,
        format!("{}-start", name),
      );
      ensure_line(
        &mut styles.grid_row_line_names,
        &mut styles.grid_row_names,
        row_end,
        format!("{}-end", name),
      );
    }
  }
}

fn split_layers(tokens: &[PropertyValue]) -> Vec<Vec<PropertyValue>> {
  let mut layers = Vec::new();
  let mut current = Vec::new();
  for token in tokens {
    if matches!(token, PropertyValue::Keyword(k) if k == ",") {
      if !current.is_empty() {
        layers.push(current.clone());
        current.clear();
      }
    } else {
      current.push(token.clone());
    }
  }
  if !current.is_empty() {
    layers.push(current);
  }
  if layers.is_empty() {
    layers.push(Vec::new());
  }
  layers
}

fn parse_background_image_value(value: &PropertyValue) -> Option<BackgroundImage> {
  match value {
    PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => {
      parse_image_set(kw)
    }
    PropertyValue::Url(url) => Some(BackgroundImage::Url(url.clone())),
    PropertyValue::LinearGradient { angle, stops } => Some(BackgroundImage::LinearGradient {
      angle: *angle,
      stops: stops.clone(),
    }),
    PropertyValue::RadialGradient {
      shape,
      size,
      position,
      stops,
    } => Some(BackgroundImage::RadialGradient {
      shape: *shape,
      size: size.clone(),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: position.x.alignment,
          offset: position.x.offset,
        },
        y: BackgroundPositionComponent {
          alignment: position.y.alignment,
          offset: position.y.offset,
        },
      },
      stops: stops.clone(),
    }),
    PropertyValue::RepeatingLinearGradient { angle, stops } => {
      Some(BackgroundImage::RepeatingLinearGradient {
        angle: *angle,
        stops: stops.clone(),
      })
    }
    PropertyValue::RepeatingRadialGradient {
      shape,
      size,
      position,
      stops,
    } => Some(BackgroundImage::RepeatingRadialGradient {
      shape: *shape,
      size: size.clone(),
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: position.x.alignment,
          offset: position.x.offset,
        },
        y: BackgroundPositionComponent {
          alignment: position.y.alignment,
          offset: position.y.offset,
        },
      },
      stops: stops.clone(),
    }),
    PropertyValue::ConicGradient {
      from_angle,
      position,
      stops,
    } => Some(BackgroundImage::ConicGradient {
      from_angle: *from_angle,
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: position.x.alignment,
          offset: position.x.offset,
        },
        y: BackgroundPositionComponent {
          alignment: position.y.alignment,
          offset: position.y.offset,
        },
      },
      stops: stops.clone(),
    }),
    PropertyValue::RepeatingConicGradient {
      from_angle,
      position,
      stops,
    } => Some(BackgroundImage::RepeatingConicGradient {
      from_angle: *from_angle,
      position: BackgroundPosition::Position {
        x: BackgroundPositionComponent {
          alignment: position.x.alignment,
          offset: position.x.offset,
        },
        y: BackgroundPositionComponent {
          alignment: position.y.alignment,
          offset: position.y.offset,
        },
      },
      stops: stops.clone(),
    }),
    PropertyValue::Keyword(kw) if kw == "none" => Some(BackgroundImage::None),
    _ => None,
  }
}

fn parse_image_set_resolution(token: &str) -> Option<f32> {
  let lower = token.trim().to_ascii_lowercase();
  lower
    .strip_suffix('x')
    .and_then(|rest| rest.parse::<f32>().ok())
    .filter(|v| *v > 0.0)
    .or_else(|| {
      lower
        .strip_suffix("dppx")
        .and_then(|rest| rest.parse::<f32>().ok())
        .filter(|v| *v > 0.0)
    })
    .or_else(|| {
      lower
        .strip_suffix("dpi")
        .and_then(|rest| rest.parse::<f32>().ok())
        .filter(|v| *v > 0.0)
        .map(|dpi| dpi / 96.0)
    })
    .or_else(|| {
      lower
        .strip_suffix("dpcm")
        .and_then(|rest| rest.parse::<f32>().ok())
        .filter(|v| *v > 0.0)
        .map(|dpcm| (dpcm * 2.54) / 96.0)
    })
}

fn split_image_set_candidates(inner: &str) -> Vec<String> {
  let mut paren = 0usize;
  let mut bracket = 0usize;
  let mut brace = 0usize;
  let mut current = String::new();
  let mut parts = Vec::new();
  let mut in_string: Option<char> = None;
  let mut chars = inner.chars();

  while let Some(ch) = chars.next() {
    if let Some(quote) = in_string {
      current.push(ch);
      if ch == '\\' {
        if let Some(next) = chars.next() {
          current.push(next);
        }
        continue;
      }
      if ch == quote {
        in_string = None;
      }
      continue;
    }

    match ch {
      '"' | '\'' => {
        in_string = Some(ch);
        current.push(ch);
      }
      '(' => {
        paren += 1;
        current.push(ch);
      }
      ')' => {
        paren = paren.saturating_sub(1);
        current.push(ch);
      }
      '[' => {
        bracket += 1;
        current.push(ch);
      }
      ']' => {
        bracket = bracket.saturating_sub(1);
        current.push(ch);
      }
      '{' => {
        brace += 1;
        current.push(ch);
      }
      '}' => {
        brace = brace.saturating_sub(1);
        current.push(ch);
      }
      ',' if paren == 0 && bracket == 0 && brace == 0 => {
        if !current.trim().is_empty() {
          parts.push(current.trim().to_string());
        }
        current.clear();
      }
      _ => current.push(ch),
    }
  }

  if !current.trim().is_empty() {
    parts.push(current.trim().to_string());
  }

  parts
}

pub(crate) fn parse_image_set(text: &str) -> Option<BackgroundImage> {
  let trimmed = text.trim();
  if !trimmed.to_ascii_lowercase().starts_with("image-set(") {
    return None;
  }

  let start = trimmed.find('(')?;
  let mut depth = 0usize;
  let mut end = None;
  for (idx, ch) in trimmed.char_indices().skip(start) {
    match ch {
      '(' => depth += 1,
      ')' => {
        if depth > 0 {
          depth -= 1;
          if depth == 0 {
            end = Some(idx);
            break;
          }
        }
      }
      _ => {}
    }
  }

  let end_idx = end?;
  if !trimmed[end_idx + 1..].trim().is_empty() {
    return None;
  }
  let inner = trimmed.get(start + 1..end_idx)?.trim();
  let mut candidates = Vec::new();

  for candidate in split_image_set_candidates(inner) {
    let tokens = tokenize_image_set_candidate(&candidate);
    if tokens.is_empty() {
      continue;
    }
    let image_token = &tokens[0];
    let image = if image_token.trim().to_ascii_lowercase().starts_with("url(")
      && image_token.trim().ends_with(')')
    {
      let trimmed = image_token.trim();
      let open = trimmed.find('(').unwrap_or(0);
      let inner = trimmed
        .get(open + 1..trimmed.len() - 1)
        .unwrap_or("")
        .trim()
        .trim_matches(|c| c == '"' || c == '\'')
        .to_string();
      Some(BackgroundImage::Url(inner))
    } else {
      crate::css::properties::parse_property_value("background-image", image_token)
        .and_then(|prop| parse_background_image_value(&prop))
    };
    let Some(image) = image else { continue };

    let mut density = 1.0;
    for token in tokens.iter().skip(1) {
      if let Some(res) = parse_image_set_resolution(token) {
        density = res;
      }
    }
    candidates.push((image, density));
  }

  if candidates.is_empty() {
    return None;
  }

  let desired = current_image_set_dpr();
  let mut best_ge: Option<(BackgroundImage, f32)> = None;
  let mut best_lt: Option<(BackgroundImage, f32)> = None;

  for (image, density) in candidates {
    if !density.is_finite() || density <= 0.0 {
      continue;
    }
    if density >= desired {
      let replace = best_ge.as_ref().map(|(_, d)| density < *d).unwrap_or(true);
      if replace {
        best_ge = Some((image, density));
      }
    } else {
      let replace = best_lt.as_ref().map(|(_, d)| density > *d).unwrap_or(true);
      if replace {
        best_lt = Some((image, density));
      }
    }
  }

  if let Some((image, _)) = best_ge {
    Some(image)
  } else if let Some((image, _)) = best_lt {
    Some(image)
  } else {
    None
  }
}

fn tokenize_image_set_candidate(value_str: &str) -> Vec<String> {
  let mut tokens = Vec::new();
  let mut current = String::new();
  let mut paren = 0;
  let mut bracket = 0;
  let mut brace: usize = 0;
  let mut in_string: Option<char> = None;
  let mut chars = value_str.chars().peekable();

  while let Some(ch) = chars.next() {
    if let Some(quote) = in_string {
      current.push(ch);
      if ch == '\\' {
        if let Some(next) = chars.next() {
          current.push(next);
        }
        continue;
      }
      if ch == quote {
        in_string = None;
      }
      continue;
    }

    match ch {
      '"' | '\'' => {
        in_string = Some(ch);
        current.push(ch);
      }
      '(' => {
        paren += 1;
        current.push(ch);
      }
      ')' => {
        if paren > 0 {
          paren -= 1;
        }
        current.push(ch);
      }
      '[' => {
        bracket += 1;
        current.push(ch);
      }
      ']' => {
        if bracket > 0 {
          bracket -= 1;
        }
        current.push(ch);
      }
      '{' => {
        brace += 1;
        current.push(ch);
      }
      '}' => {
        brace = brace.saturating_sub(1);
        current.push(ch);
      }
      ch if ch.is_whitespace() && paren == 0 && bracket == 0 && brace == 0 => {
        if !current.trim().is_empty() {
          tokens.push(current.trim().to_string());
        }
        current.clear();
      }
      _ => current.push(ch),
    }
  }

  if !current.trim().is_empty() {
    tokens.push(current.trim().to_string());
  }
  tokens
}

fn parse_cursor_keyword(kw: &str) -> Option<CursorKeyword> {
  match kw.to_ascii_lowercase().as_str() {
    "auto" => Some(CursorKeyword::Auto),
    "default" => Some(CursorKeyword::Default),
    "none" => Some(CursorKeyword::None),
    "context-menu" => Some(CursorKeyword::ContextMenu),
    "help" => Some(CursorKeyword::Help),
    "pointer" => Some(CursorKeyword::Pointer),
    "progress" => Some(CursorKeyword::Progress),
    "wait" => Some(CursorKeyword::Wait),
    "cell" => Some(CursorKeyword::Cell),
    "crosshair" => Some(CursorKeyword::Crosshair),
    "text" => Some(CursorKeyword::Text),
    "vertical-text" => Some(CursorKeyword::VerticalText),
    "alias" => Some(CursorKeyword::Alias),
    "copy" => Some(CursorKeyword::Copy),
    "move" => Some(CursorKeyword::Move),
    "no-drop" => Some(CursorKeyword::NoDrop),
    "not-allowed" => Some(CursorKeyword::NotAllowed),
    "grab" => Some(CursorKeyword::Grab),
    "grabbing" => Some(CursorKeyword::Grabbing),
    "all-scroll" => Some(CursorKeyword::AllScroll),
    "col-resize" => Some(CursorKeyword::ColResize),
    "row-resize" => Some(CursorKeyword::RowResize),
    "n-resize" => Some(CursorKeyword::NResize),
    "s-resize" => Some(CursorKeyword::SResize),
    "e-resize" => Some(CursorKeyword::EResize),
    "w-resize" => Some(CursorKeyword::WResize),
    "ne-resize" => Some(CursorKeyword::NeResize),
    "nw-resize" => Some(CursorKeyword::NwResize),
    "se-resize" => Some(CursorKeyword::SeResize),
    "sw-resize" => Some(CursorKeyword::SwResize),
    "ew-resize" => Some(CursorKeyword::EwResize),
    "ns-resize" => Some(CursorKeyword::NsResize),
    "nesw-resize" => Some(CursorKeyword::NeswResize),
    "nwse-resize" => Some(CursorKeyword::NwseResize),
    "zoom-in" => Some(CursorKeyword::ZoomIn),
    "zoom-out" => Some(CursorKeyword::ZoomOut),
    _ => None,
  }
}

fn cursor_hotspot_value(value: &PropertyValue) -> Option<f32> {
  match value {
    PropertyValue::Number(n) => Some(*n),
    PropertyValue::Length(l) => Some(l.to_px()),
    _ => None,
  }
}

fn parse_cursor(value: &PropertyValue) -> Option<(Vec<CursorImage>, CursorKeyword)> {
  let tokens: Vec<PropertyValue> = match value {
    PropertyValue::Multiple(list) => list.clone(),
    other => vec![other.clone()],
  };

  if tokens.is_empty() {
    return None;
  }

  let mut images = Vec::new();
  let mut fallback: Option<CursorKeyword> = None;
  let mut idx = 0;
  while idx < tokens.len() {
    match &tokens[idx] {
      PropertyValue::Keyword(k) if k == "," => {
        idx += 1;
        continue;
      }
      PropertyValue::Url(url) => {
        let mut hotspot = None;
        if idx + 2 < tokens.len() {
          if let (Some(x), Some(y)) = (
            cursor_hotspot_value(&tokens[idx + 1]),
            cursor_hotspot_value(&tokens[idx + 2]),
          ) {
            hotspot = Some((x, y));
            idx += 2;
          }
        }
        images.push(CursorImage {
          url: url.clone(),
          hotspot,
        });
      }
      PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => {
        if let Some(BackgroundImage::Url(url)) = parse_image_set(kw) {
          images.push(CursorImage { url, hotspot: None });
        }
      }
      PropertyValue::Keyword(kw) => {
        if let Some(parsed) = parse_cursor_keyword(kw) {
          fallback = Some(parsed);
          break;
        }
      }
      _ => {}
    }
    idx += 1;
  }

  if images.is_empty() && fallback.is_none() {
    return None;
  }

  Some((images, fallback.unwrap_or(CursorKeyword::Auto)))
}

fn parse_color_scheme(value: &PropertyValue) -> Option<ColorSchemePreference> {
  let tokens: Vec<String> = match value {
    PropertyValue::Keyword(kw) => kw
      .split_whitespace()
      .filter(|t| !t.is_empty())
      .map(|t| t.to_string())
      .collect(),
    PropertyValue::Multiple(values) => {
      let mut out = Vec::new();
      for v in values {
        if let PropertyValue::Keyword(k) = v {
          if k == "," {
            return None;
          }
          out.push(k.clone());
        } else {
          return None;
        }
      }
      out
    }
    _ => return None,
  };

  parse_color_scheme_tokens(&tokens)
}

fn parse_color_scheme_tokens(tokens: &[String]) -> Option<ColorSchemePreference> {
  if tokens.is_empty() {
    return None;
  }

  if tokens.len() == 1 && tokens[0].eq_ignore_ascii_case("normal") {
    return Some(ColorSchemePreference::Normal);
  }

  let mut only = false;
  let mut schemes: Vec<ColorSchemeEntry> = Vec::new();
  for raw in tokens {
    if raw.eq_ignore_ascii_case("only") {
      only = true;
      continue;
    }
    if raw.eq_ignore_ascii_case("normal") {
      return None;
    }
    let lower = raw.to_ascii_lowercase();
    let entry = match lower.as_str() {
      "light" => ColorSchemeEntry::Light,
      "dark" => ColorSchemeEntry::Dark,
      _ => ColorSchemeEntry::Custom(lower),
    };
    if !schemes.contains(&entry) {
      schemes.push(entry);
    }
  }

  if schemes.is_empty() {
    return None;
  }

  Some(ColorSchemePreference::Supported { schemes, only })
}

fn content_value_from_property(value: &PropertyValue) -> Option<ContentValue> {
  let css_text = match value {
    PropertyValue::String(s) => format!("\"{}\"", s),
    PropertyValue::Url(url) => format!("url({})", url),
    PropertyValue::Keyword(kw) => kw.clone(),
    PropertyValue::Multiple(list) => {
      let mut parts = Vec::new();
      for item in list {
        match item {
          PropertyValue::String(s) => parts.push(format!("\"{}\"", s)),
          PropertyValue::Url(url) => parts.push(format!("url({})", url)),
          PropertyValue::Keyword(kw) => parts.push(kw.clone()),
          PropertyValue::Number(n) => parts.push(n.to_string()),
          PropertyValue::Percentage(p) => parts.push(format!("{}%", p)),
          _ => return None,
        }
      }
      parts.join(" ")
    }
    _ => return None,
  };

  parse_content(&css_text)
}

fn parse_background_image_list(value: &PropertyValue) -> Option<Vec<Option<BackgroundImage>>> {
  match value {
    PropertyValue::Multiple(tokens)
      if tokens
        .iter()
        .any(|t| matches!(t, PropertyValue::Keyword(k) if k == ",")) =>
    {
      let mut images = Vec::new();
      for layer_tokens in split_layers(tokens) {
        let candidate = if layer_tokens.len() == 1 {
          parse_background_image_value(&layer_tokens[0])
        } else {
          parse_background_image_value(&PropertyValue::Multiple(layer_tokens.clone()))
        };
        match candidate {
          Some(BackgroundImage::None) => images.push(None),
          Some(img) => images.push(Some(img)),
          None => return None,
        }
      }
      if images.is_empty() {
        None
      } else {
        Some(images)
      }
    }
    _ => parse_background_image_value(value).map(|img| match img {
      BackgroundImage::None => vec![None],
      other => vec![Some(other)],
    }),
  }
}

fn parse_overscroll_keyword(kw: &str) -> Option<OverscrollBehavior> {
  match kw.to_ascii_lowercase().as_str() {
    "auto" => Some(OverscrollBehavior::Auto),
    "contain" => Some(OverscrollBehavior::Contain),
    "none" => Some(OverscrollBehavior::None),
    _ => None,
  }
}

fn parse_scroll_snap_type_keywords(parts: &[String]) -> Option<ScrollSnapType> {
  if parts.is_empty() || parts.len() > 2 {
    return None;
  }
  let axis = match parts[0].to_ascii_lowercase().as_str() {
    "none" => ScrollSnapAxis::None,
    "x" => ScrollSnapAxis::X,
    "y" => ScrollSnapAxis::Y,
    "block" => ScrollSnapAxis::Block,
    "inline" => ScrollSnapAxis::Inline,
    "both" => ScrollSnapAxis::Both,
    _ => return None,
  };
  let strictness = if parts.len() == 2 {
    match parts[1].to_ascii_lowercase().as_str() {
      "mandatory" => ScrollSnapStrictness::Mandatory,
      "proximity" => ScrollSnapStrictness::Proximity,
      _ => return None,
    }
  } else {
    ScrollSnapStrictness::Proximity
  };
  Some(ScrollSnapType { axis, strictness })
}

fn parse_scroll_snap_align_keywords(parts: &[String]) -> Option<ScrollSnapAlignments> {
  if parts.is_empty() || parts.len() > 2 {
    return None;
  }
  let parse_align = |kw: &str| match kw {
    "none" => Some(ScrollSnapAlign::None),
    "start" => Some(ScrollSnapAlign::Start),
    "end" => Some(ScrollSnapAlign::End),
    "center" => Some(ScrollSnapAlign::Center),
    _ => None,
  };

  let first = parse_align(&parts[0].to_ascii_lowercase())?;
  let second = if parts.len() == 2 {
    parse_align(&parts[1].to_ascii_lowercase())?
  } else {
    first
  };

  Some(ScrollSnapAlignments {
    inline: first,
    block: second,
  })
}

fn parse_timeline_axis(token: &str) -> Option<TimelineAxis> {
  match token {
    "block" => Some(TimelineAxis::Block),
    "inline" => Some(TimelineAxis::Inline),
    "x" => Some(TimelineAxis::X),
    "y" => Some(TimelineAxis::Y),
    _ => None,
  }
}

fn parse_percentage(token: &str) -> Option<f32> {
  let trimmed = token.trim();
  let pct = trimmed.strip_suffix('%')?;
  pct.trim().parse::<f32>().ok()
}

fn parse_progress_value(token: &str) -> Option<f32> {
  if let Some(pct) = parse_percentage(token) {
    return Some(pct / 100.0);
  }
  token.trim().parse::<f32>().ok()
}

fn parse_view_phase(token: &str) -> Option<ViewTimelinePhase> {
  match token {
    "entry" => Some(ViewTimelinePhase::Entry),
    "exit" => Some(ViewTimelinePhase::Exit),
    "cross" => Some(ViewTimelinePhase::Cross),
    _ => None,
  }
}

fn parse_timeline_offset_token(token: &str) -> Option<TimelineOffset> {
  if token.eq_ignore_ascii_case("auto") {
    return Some(TimelineOffset::Auto);
  }
  if let Some(len) = parse_length(token) {
    return Some(TimelineOffset::Length(len));
  }
  if let Some(pct) = parse_percentage(token) {
    return Some(TimelineOffset::Percentage(pct));
  }
  None
}

fn parse_scroll_timeline_list(raw: &str) -> Vec<ScrollTimeline> {
  let mut timelines = Vec::new();
  for part in raw.split(',') {
    let tokens: Vec<&str> = part.split_whitespace().collect();
    if tokens.is_empty() {
      continue;
    }
    if tokens.len() == 1 && tokens[0].eq_ignore_ascii_case("none") {
      return Vec::new();
    }
    let mut name: Option<String> = None;
    let mut axis = TimelineAxis::Block;
    let mut offsets: Vec<TimelineOffset> = Vec::new();
    for token in tokens {
      let lower = token.to_ascii_lowercase();
      if let Some(ax) = parse_timeline_axis(&lower) {
        axis = ax;
        continue;
      }
      if let Some(offset) = parse_timeline_offset_token(token) {
        offsets.push(offset);
        continue;
      }
      if name.is_none() {
        name = Some(token.to_string());
      }
    }
    let start = offsets.get(0).cloned().unwrap_or_default();
    let end = offsets.get(1).cloned().unwrap_or(TimelineOffset::Auto);
    timelines.push(ScrollTimeline {
      name,
      axis,
      start,
      end,
    });
  }
  timelines
}

fn parse_view_timeline_list(raw: &str) -> Vec<ViewTimeline> {
  let mut timelines = Vec::new();
  for part in raw.split(',') {
    let tokens: Vec<&str> = part.split_whitespace().collect();
    if tokens.is_empty() {
      continue;
    }
    if tokens.len() == 1 && tokens[0].eq_ignore_ascii_case("none") {
      return Vec::new();
    }
    let mut name: Option<String> = None;
    let mut axis = TimelineAxis::Block;
    for token in tokens {
      let lower = token.to_ascii_lowercase();
      if let Some(ax) = parse_timeline_axis(&lower) {
        axis = ax;
        continue;
      }
      if name.is_none() {
        name = Some(token.to_string());
      }
    }
    timelines.push(ViewTimeline { name, axis });
  }
  timelines
}

fn parse_animation_timeline_list(raw: &str) -> Vec<AnimationTimeline> {
  let mut timelines = Vec::new();
  for part in raw.split(',') {
    let trimmed = part.trim();
    if trimmed.is_empty() {
      continue;
    }
    let lower = trimmed.to_ascii_lowercase();
    let timeline = match lower.as_str() {
      "auto" => AnimationTimeline::Auto,
      "none" => AnimationTimeline::None,
      _ => AnimationTimeline::Named(trimmed.to_string()),
    };
    timelines.push(timeline);
  }
  if timelines
    .iter()
    .any(|t| matches!(t, AnimationTimeline::None))
  {
    return timelines
      .into_iter()
      .filter(|t| !matches!(t, AnimationTimeline::None))
      .collect();
  }
  timelines
}

fn parse_animation_names(raw: &str) -> Vec<String> {
  let mut names = Vec::new();
  for part in raw.split(',') {
    let trimmed = part.trim();
    if trimmed.is_empty() {
      continue;
    }
    if trimmed.eq_ignore_ascii_case("none") {
      return Vec::new();
    }
    names.push(trimmed.to_string());
  }
  names
}

fn parse_range_offset(tokens: &[&str]) -> Option<(RangeOffset, usize)> {
  if tokens.is_empty() {
    return None;
  }
  let lower = tokens[0].to_ascii_lowercase();
  if let Some(phase) = parse_view_phase(&lower) {
    if tokens.len() >= 2 {
      if let Some(progress) = parse_progress_value(tokens[1]) {
        return Some((RangeOffset::View(phase, progress), 2));
      }
    }
    return Some((RangeOffset::View(phase, 0.0), 1));
  }
  if let Some(progress) = parse_progress_value(tokens[0]) {
    return Some((RangeOffset::Progress(progress), 1));
  }
  None
}

fn parse_animation_range_list(raw: &str) -> Vec<AnimationRange> {
  let mut ranges = Vec::new();
  for part in raw.split(',') {
    let tokens: Vec<&str> = part.split_whitespace().collect();
    if tokens.is_empty() {
      continue;
    }
    if tokens.len() == 1 && tokens[0].eq_ignore_ascii_case("none") {
      return Vec::new();
    }
    let (start, consumed_start) =
      parse_range_offset(&tokens).unwrap_or((RangeOffset::Progress(0.0), 0));
    let (end, _) =
      parse_range_offset(&tokens[consumed_start..]).unwrap_or((RangeOffset::Progress(1.0), 0));
    ranges.push(AnimationRange { start, end });
  }
  ranges
}

fn parse_touch_action_keywords(tokens: &[String]) -> Option<TouchAction> {
  if tokens.is_empty() {
    return None;
  }

  if tokens.len() == 1 {
    let kw = tokens[0].as_str();
    return match kw {
      "auto" => Some(TouchAction::auto()),
      "none" => Some(TouchAction::none()),
      "pan-x" => Some(TouchAction {
        pan_x: true,
        ..TouchAction::empty()
      }),
      "pan-y" => Some(TouchAction {
        pan_y: true,
        ..TouchAction::empty()
      }),
      "pan-left" => Some(TouchAction {
        pan_left: true,
        ..TouchAction::empty()
      }),
      "pan-right" => Some(TouchAction {
        pan_right: true,
        ..TouchAction::empty()
      }),
      "pan-up" => Some(TouchAction {
        pan_up: true,
        ..TouchAction::empty()
      }),
      "pan-down" => Some(TouchAction {
        pan_down: true,
        ..TouchAction::empty()
      }),
      "pinch-zoom" => Some(TouchAction {
        pinch_zoom: true,
        ..TouchAction::empty()
      }),
      "manipulation" => Some(TouchAction {
        manipulation: true,
        ..TouchAction::empty()
      }),
      _ => None,
    };
  }

  let mut action = TouchAction::empty();
  for kw in tokens {
    match kw.as_str() {
      "auto" => return Some(TouchAction::auto()),
      "none" => return Some(TouchAction::none()),
      "pan-x" => action.pan_x = true,
      "pan-y" => action.pan_y = true,
      "pan-left" => action.pan_left = true,
      "pan-right" => action.pan_right = true,
      "pan-up" => action.pan_up = true,
      "pan-down" => action.pan_down = true,
      "pinch-zoom" => action.pinch_zoom = true,
      "manipulation" => action.manipulation = true,
      _ => {}
    }
  }

  Some(action)
}

fn parse_layer_list<T>(
  value: &PropertyValue,
  parse: impl Fn(&PropertyValue) -> Option<T>,
) -> Option<Vec<T>> {
  match value {
    PropertyValue::Multiple(tokens)
      if tokens
        .iter()
        .any(|t| matches!(t, PropertyValue::Keyword(k) if k == ",")) =>
    {
      let mut items = Vec::new();
      for layer_tokens in split_layers(tokens) {
        let parsed = if layer_tokens.len() == 1 {
          parse(&layer_tokens[0])
        } else {
          parse(&PropertyValue::Multiple(layer_tokens.clone()))
        };
        if let Some(item) = parsed {
          items.push(item);
        } else {
          return None;
        }
      }
      if items.is_empty() {
        None
      } else {
        Some(items)
      }
    }
    _ => parse(value).map(|v| vec![v]),
  }
}

fn extract_color_values_with(
  value: &PropertyValue,
  resolver: &dyn Fn(&PropertyValue) -> Option<Rgba>,
) -> Option<Vec<Rgba>> {
  match value {
    PropertyValue::Multiple(values) => {
      let colors: Vec<Rgba> = values.iter().filter_map(resolver).collect();
      if colors.is_empty() {
        None
      } else {
        Some(colors)
      }
    }
    _ => resolver(value).map(|c| vec![c]),
  }
}

fn extract_color_pair_with(
  value: &PropertyValue,
  resolver: &dyn Fn(&PropertyValue) -> Option<Rgba>,
) -> Option<(Rgba, Rgba)> {
  extract_color_values_with(value, resolver).and_then(|colors| {
    if colors.len() < 2 {
      None
    } else {
      Some((colors[0], colors[1]))
    }
  })
}

fn side_order(orders: &crate::style::SideOrders, side: crate::style::PhysicalSide) -> i32 {
  match side {
    crate::style::PhysicalSide::Top => orders.top,
    crate::style::PhysicalSide::Right => orders.right,
    crate::style::PhysicalSide::Bottom => orders.bottom,
    crate::style::PhysicalSide::Left => orders.left,
  }
}

fn side_order_mut<'a>(
  orders: &'a mut crate::style::SideOrders,
  side: crate::style::PhysicalSide,
) -> &'a mut i32 {
  match side {
    crate::style::PhysicalSide::Top => &mut orders.top,
    crate::style::PhysicalSide::Right => &mut orders.right,
    crate::style::PhysicalSide::Bottom => &mut orders.bottom,
    crate::style::PhysicalSide::Left => &mut orders.left,
  }
}

fn set_margin_side(
  styles: &mut ComputedStyle,
  side: crate::style::PhysicalSide,
  value: Option<Length>,
  order: i32,
) {
  if order < side_order(&styles.logical.margin_orders, side) {
    return;
  }
  match side {
    crate::style::PhysicalSide::Top => styles.margin_top = value,
    crate::style::PhysicalSide::Right => styles.margin_right = value,
    crate::style::PhysicalSide::Bottom => styles.margin_bottom = value,
    crate::style::PhysicalSide::Left => styles.margin_left = value,
  }
  *side_order_mut(&mut styles.logical.margin_orders, side) = order;
}

fn sanitize_non_negative_length(value: Length) -> Length {
  if value.calc.is_none() && value.value < 0.0 {
    Length::px(0.0)
  } else {
    value
  }
}

fn set_padding_side(
  styles: &mut ComputedStyle,
  side: crate::style::PhysicalSide,
  value: Length,
  order: i32,
) {
  if order < side_order(&styles.logical.padding_orders, side) {
    return;
  }
  let value = sanitize_non_negative_length(value);
  match side {
    crate::style::PhysicalSide::Top => styles.padding_top = value,
    crate::style::PhysicalSide::Right => styles.padding_right = value,
    crate::style::PhysicalSide::Bottom => styles.padding_bottom = value,
    crate::style::PhysicalSide::Left => styles.padding_left = value,
  }
  *side_order_mut(&mut styles.logical.padding_orders, side) = order;
}

fn set_border_width_side(
  styles: &mut ComputedStyle,
  side: crate::style::PhysicalSide,
  value: Length,
  order: i32,
) {
  if order < side_order(&styles.logical.border_width_orders, side) {
    return;
  }
  let value = sanitize_non_negative_length(value);
  match side {
    crate::style::PhysicalSide::Top => styles.border_top_width = value,
    crate::style::PhysicalSide::Right => styles.border_right_width = value,
    crate::style::PhysicalSide::Bottom => styles.border_bottom_width = value,
    crate::style::PhysicalSide::Left => styles.border_left_width = value,
  }
  *side_order_mut(&mut styles.logical.border_width_orders, side) = order;
}

fn set_border_style_side(
  styles: &mut ComputedStyle,
  side: crate::style::PhysicalSide,
  value: BorderStyle,
  order: i32,
) {
  if order < side_order(&styles.logical.border_style_orders, side) {
    return;
  }
  match side {
    crate::style::PhysicalSide::Top => styles.border_top_style = value,
    crate::style::PhysicalSide::Right => styles.border_right_style = value,
    crate::style::PhysicalSide::Bottom => styles.border_bottom_style = value,
    crate::style::PhysicalSide::Left => styles.border_left_style = value,
  }
  *side_order_mut(&mut styles.logical.border_style_orders, side) = order;
}

fn set_border_color_side(
  styles: &mut ComputedStyle,
  side: crate::style::PhysicalSide,
  value: Rgba,
  order: i32,
) {
  if order < side_order(&styles.logical.border_color_orders, side) {
    return;
  }
  match side {
    crate::style::PhysicalSide::Top => styles.border_top_color = value,
    crate::style::PhysicalSide::Right => styles.border_right_color = value,
    crate::style::PhysicalSide::Bottom => styles.border_bottom_color = value,
    crate::style::PhysicalSide::Left => styles.border_left_color = value,
  }
  *side_order_mut(&mut styles.logical.border_color_orders, side) = order;
}

fn push_logical(styles: &mut ComputedStyle, property: crate::style::LogicalProperty, order: i32) {
  styles
    .logical
    .pending
    .push(crate::style::PendingLogical { order, property });
}

fn set_inset_side(
  styles: &mut ComputedStyle,
  side: crate::style::PhysicalSide,
  value: Option<Length>,
  order: i32,
) {
  if order < side_order(&styles.logical.inset_orders, side) {
    return;
  }
  match side {
    crate::style::PhysicalSide::Top => styles.top = value,
    crate::style::PhysicalSide::Right => styles.right = value,
    crate::style::PhysicalSide::Bottom => styles.bottom = value,
    crate::style::PhysicalSide::Left => styles.left = value,
  }
  *side_order_mut(&mut styles.logical.inset_orders, side) = order;
}

fn set_length_with_order(
  target: &mut Option<Length>,
  order_slot: &mut i32,
  value: Option<Length>,
  order: i32,
) {
  if order < *order_slot {
    return;
  }
  *target = value;
  *order_slot = order;
}

fn sanitize_min_length(value: Option<Length>) -> Option<Length> {
  if let Some(len) = value {
    if len.calc.is_none() && len.value < 0.0 {
      Some(Length::px(0.0))
    } else {
      Some(len)
    }
  } else {
    None
  }
}

fn sanitize_max_length(value: Option<Length>) -> Option<Length> {
  value.and_then(|len| {
    if len.calc.is_none() && len.value < 0.0 {
      None
    } else {
      Some(len)
    }
  })
}

#[derive(Debug, Clone, Copy)]
enum PhysicalCorner {
  TopLeft,
  TopRight,
  BottomRight,
  BottomLeft,
}

fn set_corner_radius(
  styles: &mut ComputedStyle,
  corner: PhysicalCorner,
  value: Option<BorderCornerRadius>,
  order: i32,
) {
  let order_slot = match corner {
    PhysicalCorner::TopLeft => &mut styles.logical.corner_orders.top_left,
    PhysicalCorner::TopRight => &mut styles.logical.corner_orders.top_right,
    PhysicalCorner::BottomRight => &mut styles.logical.corner_orders.bottom_right,
    PhysicalCorner::BottomLeft => &mut styles.logical.corner_orders.bottom_left,
  };
  if order < *order_slot {
    return;
  }
  let value = value.map(|v| BorderCornerRadius {
    x: sanitize_non_negative_length(v.x),
    y: sanitize_non_negative_length(v.y),
  });
  match corner {
    PhysicalCorner::TopLeft => {
      if let Some(v) = value {
        styles.border_top_left_radius = v;
      }
    }
    PhysicalCorner::TopRight => {
      if let Some(v) = value {
        styles.border_top_right_radius = v;
      }
    }
    PhysicalCorner::BottomRight => {
      if let Some(v) = value {
        styles.border_bottom_right_radius = v;
      }
    }
    PhysicalCorner::BottomLeft => {
      if let Some(v) = value {
        styles.border_bottom_left_radius = v;
      }
    }
  }
  *order_slot = order;
}

fn extract_border_radius_length(value: &PropertyValue) -> Option<Length> {
  match value {
    PropertyValue::Length(len) => Some(*len),
    PropertyValue::Percentage(p) => Some(Length::new(*p, LengthUnit::Percent)),
    PropertyValue::Number(n) if *n == 0.0 => Some(Length::px(0.0)),
    PropertyValue::Keyword(kw) => parse_length(kw),
    _ => None,
  }
}

fn expand_corner_list(values: &[Length]) -> Option<[Length; 4]> {
  if values.is_empty() {
    return None;
  }
  let v0 = values[0];
  let v1 = *values.get(1).unwrap_or(&v0);
  let v2 = *values.get(2).unwrap_or(&v0);
  let v3 = *values.get(3).unwrap_or(&v1);
  Some([v0, v1, v2, v3])
}

fn parse_border_radius_shorthand(value: &PropertyValue) -> Option<[BorderCornerRadius; 4]> {
  let mut horizontal: Vec<Length> = Vec::new();
  let mut vertical: Vec<Length> = Vec::new();
  let mut seen_slash = false;

  match value {
    PropertyValue::Multiple(values) => {
      for token in values {
        if matches!(token, PropertyValue::Keyword(kw) if kw == "/") {
          if seen_slash {
            return None;
          }
          seen_slash = true;
          continue;
        }
        let len = extract_border_radius_length(token)?;
        if seen_slash {
          vertical.push(len);
        } else {
          horizontal.push(len);
        }
      }
    }
    _ => {
      let len = extract_border_radius_length(value)?;
      horizontal.push(len);
    }
  }

  if horizontal.is_empty() || horizontal.len() > 4 || vertical.len() > 4 {
    return None;
  }
  if seen_slash && vertical.is_empty() {
    return None;
  }
  if vertical.is_empty() {
    vertical = horizontal.clone();
  }

  let h = expand_corner_list(&horizontal)?;
  let v = expand_corner_list(&vertical)?;
  Some([
    BorderCornerRadius { x: h[0], y: v[0] },
    BorderCornerRadius { x: h[1], y: v[1] },
    BorderCornerRadius { x: h[2], y: v[2] },
    BorderCornerRadius { x: h[3], y: v[3] },
  ])
}

fn parse_single_corner_radius(value: &PropertyValue) -> Option<BorderCornerRadius> {
  match value {
    PropertyValue::Multiple(values) => {
      if values
        .iter()
        .any(|v| matches!(v, PropertyValue::Keyword(kw) if kw == "/"))
      {
        return None;
      }
      let lengths: Vec<Length> = values
        .iter()
        .filter_map(extract_border_radius_length)
        .collect();
      match lengths.len() {
        1 => Some(BorderCornerRadius::uniform(lengths[0])),
        2 => Some(BorderCornerRadius {
          x: lengths[0],
          y: lengths[1],
        }),
        _ => None,
      }
    }
    _ => extract_border_radius_length(value).map(BorderCornerRadius::uniform),
  }
}

fn set_axis_dimension(
  styles: &mut ComputedStyle,
  axis: crate::style::LogicalAxis,
  value: Option<Length>,
  order: i32,
) {
  let is_horizontal = match axis {
    crate::style::LogicalAxis::Inline => inline_axis_is_horizontal(styles.writing_mode),
    crate::style::LogicalAxis::Block => block_axis_is_horizontal(styles.writing_mode),
  };
  if is_horizontal {
    set_length_with_order(
      &mut styles.width,
      &mut styles.logical.width_order,
      value,
      order,
    );
  } else {
    set_length_with_order(
      &mut styles.height,
      &mut styles.logical.height_order,
      value,
      order,
    );
  }
}

fn set_axis_min_dimension(
  styles: &mut ComputedStyle,
  axis: crate::style::LogicalAxis,
  value: Option<Length>,
  order: i32,
) {
  let is_horizontal = match axis {
    crate::style::LogicalAxis::Inline => inline_axis_is_horizontal(styles.writing_mode),
    crate::style::LogicalAxis::Block => block_axis_is_horizontal(styles.writing_mode),
  };
  let value = sanitize_min_length(value);
  if is_horizontal {
    set_length_with_order(
      &mut styles.min_width,
      &mut styles.logical.min_width_order,
      value,
      order,
    );
  } else {
    set_length_with_order(
      &mut styles.min_height,
      &mut styles.logical.min_height_order,
      value,
      order,
    );
  }
}

fn set_axis_max_dimension(
  styles: &mut ComputedStyle,
  axis: crate::style::LogicalAxis,
  value: Option<Length>,
  order: i32,
) {
  let is_horizontal = match axis {
    crate::style::LogicalAxis::Inline => inline_axis_is_horizontal(styles.writing_mode),
    crate::style::LogicalAxis::Block => block_axis_is_horizontal(styles.writing_mode),
  };
  let value = sanitize_max_length(value);
  if is_horizontal {
    set_length_with_order(
      &mut styles.max_width,
      &mut styles.logical.max_width_order,
      value,
      order,
    );
  } else {
    set_length_with_order(
      &mut styles.max_height,
      &mut styles.logical.max_height_order,
      value,
      order,
    );
  }
}

#[derive(Copy, Clone)]
enum GlobalKeyword {
  Inherit,
  Initial,
  Unset,
  Revert,
  RevertLayer,
}

fn global_keyword(value: &PropertyValue) -> Option<GlobalKeyword> {
  if let PropertyValue::Keyword(kw) = value {
    if kw.eq_ignore_ascii_case("inherit") {
      return Some(GlobalKeyword::Inherit);
    }
    if kw.eq_ignore_ascii_case("initial") {
      return Some(GlobalKeyword::Initial);
    }
    if kw.eq_ignore_ascii_case("unset") {
      return Some(GlobalKeyword::Unset);
    }
    if kw.eq_ignore_ascii_case("revert-layer") {
      return Some(GlobalKeyword::RevertLayer);
    }
    if kw.eq_ignore_ascii_case("revert") {
      return Some(GlobalKeyword::Revert);
    }
  }
  None
}

fn set_all_logical_orders(logical: &mut crate::style::LogicalState, order: i32, next_order: i32) {
  logical.pending.clear();
  logical.margin_orders.top = order;
  logical.margin_orders.right = order;
  logical.margin_orders.bottom = order;
  logical.margin_orders.left = order;

  logical.padding_orders.top = order;
  logical.padding_orders.right = order;
  logical.padding_orders.bottom = order;
  logical.padding_orders.left = order;

  logical.border_width_orders.top = order;
  logical.border_width_orders.right = order;
  logical.border_width_orders.bottom = order;
  logical.border_width_orders.left = order;

  logical.border_style_orders.top = order;
  logical.border_style_orders.right = order;
  logical.border_style_orders.bottom = order;
  logical.border_style_orders.left = order;

  logical.border_color_orders.top = order;
  logical.border_color_orders.right = order;
  logical.border_color_orders.bottom = order;
  logical.border_color_orders.left = order;

  logical.inset_orders.top = order;
  logical.inset_orders.right = order;
  logical.inset_orders.bottom = order;
  logical.inset_orders.left = order;

  logical.corner_orders.top_left = order;
  logical.corner_orders.top_right = order;
  logical.corner_orders.bottom_right = order;
  logical.corner_orders.bottom_left = order;

  logical.width_order = order;
  logical.height_order = order;
  logical.min_width_order = order;
  logical.min_height_order = order;
  logical.max_width_order = order;
  logical.max_height_order = order;

  logical.set_next_order(next_order);
}

fn is_inherited_property(name: &str) -> bool {
  matches!(
    name,
    "color"
      | "color-scheme"
      | "caret-color"
      | "accent-color"
      | "cursor"
      | "visibility"
      | "pointer-events"
      | "user-select"
      | "direction"
      | "writing-mode"
      | "font"
      | "font-family"
      | "font-size"
      | "font-size-adjust"
      | "font-style"
      | "font-weight"
      | "font-stretch"
      | "font-variant"
      | "font-variant-caps"
      | "font-variant-alternates"
      | "font-variant-position"
      | "font-variant-east-asian"
      | "font-variant-numeric"
      | "font-variant-ligatures"
      | "font-variant-emoji"
      | "font-feature-settings"
      | "font-variation-settings"
      | "font-optical-sizing"
      | "font-language-override"
      | "font-synthesis"
      | "font-synthesis-weight"
      | "font-synthesis-style"
      | "font-synthesis-small-caps"
      | "font-synthesis-position"
      | "font-kerning"
      | "line-height"
      | "text-align"
      | "text-align-all"
      | "text-align-last"
      | "text-justify"
      | "text-wrap"
      | "text-rendering"
      | "text-indent"
      | "text-decoration-skip-ink"
      | "text-underline-offset"
      | "text-underline-position"
      | "text-emphasis-style"
      | "text-emphasis-color"
      | "text-emphasis-position"
      | "text-transform"
      | "text-combine-upright"
      | "text-orientation"
      | "letter-spacing"
      | "word-spacing"
      | "white-space"
      | "line-break"
      | "break-before"
      | "break-after"
      | "break-inside"
      | "widows"
      | "orphans"
      | "tab-size"
      | "hyphens"
      | "word-break"
      | "overflow-anchor"
      | "overflow-wrap"
      | "text-emphasis"
      | "justify-items"
      | "caption-side"
      | "empty-cells"
      | "list-style-type"
      | "list-style-position"
      | "list-style-image"
      | "list-style"
      | "quotes"
  )
}

fn global_keyword_source<'a>(
  keyword: GlobalKeyword,
  property: &str,
  parent: &'a ComputedStyle,
  defaults: &'a ComputedStyle,
  revert_base: &'a ComputedStyle,
  revert_layer_base: Option<&'a ComputedStyle>,
) -> Option<&'a ComputedStyle> {
  match keyword {
    GlobalKeyword::Inherit => Some(parent),
    GlobalKeyword::Initial => Some(defaults),
    GlobalKeyword::Unset => {
      if is_inherited_property(property) {
        Some(parent)
      } else {
        Some(defaults)
      }
    }
    GlobalKeyword::Revert => Some(revert_base),
    GlobalKeyword::RevertLayer => revert_layer_base.or(Some(revert_base)),
  }
}

fn inset_for_side(style: &ComputedStyle, side: crate::style::PhysicalSide) -> Option<Length> {
  match side {
    crate::style::PhysicalSide::Top => style.top,
    crate::style::PhysicalSide::Right => style.right,
    crate::style::PhysicalSide::Bottom => style.bottom,
    crate::style::PhysicalSide::Left => style.left,
  }
}

fn margin_for_side(style: &ComputedStyle, side: crate::style::PhysicalSide) -> Option<Length> {
  match side {
    crate::style::PhysicalSide::Top => style.margin_top,
    crate::style::PhysicalSide::Right => style.margin_right,
    crate::style::PhysicalSide::Bottom => style.margin_bottom,
    crate::style::PhysicalSide::Left => style.margin_left,
  }
}

fn padding_for_side(style: &ComputedStyle, side: crate::style::PhysicalSide) -> Length {
  match side {
    crate::style::PhysicalSide::Top => style.padding_top,
    crate::style::PhysicalSide::Right => style.padding_right,
    crate::style::PhysicalSide::Bottom => style.padding_bottom,
    crate::style::PhysicalSide::Left => style.padding_left,
  }
}

fn border_width_for_side(style: &ComputedStyle, side: crate::style::PhysicalSide) -> Length {
  match side {
    crate::style::PhysicalSide::Top => style.border_top_width,
    crate::style::PhysicalSide::Right => style.border_right_width,
    crate::style::PhysicalSide::Bottom => style.border_bottom_width,
    crate::style::PhysicalSide::Left => style.border_left_width,
  }
}

fn border_style_for_side(style: &ComputedStyle, side: crate::style::PhysicalSide) -> BorderStyle {
  match side {
    crate::style::PhysicalSide::Top => style.border_top_style,
    crate::style::PhysicalSide::Right => style.border_right_style,
    crate::style::PhysicalSide::Bottom => style.border_bottom_style,
    crate::style::PhysicalSide::Left => style.border_left_style,
  }
}

fn border_color_for_side(style: &ComputedStyle, side: crate::style::PhysicalSide) -> Rgba {
  match side {
    crate::style::PhysicalSide::Top => style.border_top_color,
    crate::style::PhysicalSide::Right => style.border_right_color,
    crate::style::PhysicalSide::Bottom => style.border_bottom_color,
    crate::style::PhysicalSide::Left => style.border_left_color,
  }
}

fn radius_for_corner(style: &ComputedStyle, corner: PhysicalCorner) -> BorderCornerRadius {
  match corner {
    PhysicalCorner::TopLeft => style.border_top_left_radius,
    PhysicalCorner::TopRight => style.border_top_right_radius,
    PhysicalCorner::BottomRight => style.border_bottom_right_radius,
    PhysicalCorner::BottomLeft => style.border_bottom_left_radius,
  }
}

fn apply_property_from_source(
  styles: &mut ComputedStyle,
  source: &ComputedStyle,
  property: &str,
  order: i32,
) -> bool {
  let inline_sides = inline_physical_sides(source);
  let block_sides = block_physical_sides(source);
  match property {
    "display" => styles.display = source.display,
    "visibility" => styles.visibility = source.visibility,
    "float" => styles.float = source.float,
    "clear" => styles.clear = source.clear,
    "overflow" => {
      styles.overflow_x = source.overflow_x;
      styles.overflow_y = source.overflow_y;
    }
    "overflow-x" => styles.overflow_x = source.overflow_x,
    "overflow-y" => styles.overflow_y = source.overflow_y,
    "position" => styles.position = source.position,
    "appearance" => styles.appearance = source.appearance.clone(),
    "resize" => styles.resize = source.resize,
    "box-sizing" => styles.box_sizing = source.box_sizing,
    "top" => set_inset_side(styles, crate::style::PhysicalSide::Top, source.top, order),
    "right" => set_inset_side(
      styles,
      crate::style::PhysicalSide::Right,
      source.right,
      order,
    ),
    "bottom" => set_inset_side(
      styles,
      crate::style::PhysicalSide::Bottom,
      source.bottom,
      order,
    ),
    "left" => set_inset_side(styles, crate::style::PhysicalSide::Left, source.left, order),
    "inset" => {
      set_inset_side(styles, crate::style::PhysicalSide::Top, source.top, order);
      set_inset_side(
        styles,
        crate::style::PhysicalSide::Right,
        source.right,
        order,
      );
      set_inset_side(
        styles,
        crate::style::PhysicalSide::Bottom,
        source.bottom,
        order,
      );
      set_inset_side(styles, crate::style::PhysicalSide::Left, source.left, order);
    }
    "inset-inline-start" => {
      let start_side = inline_sides.0;
      let value = inset_for_side(source, start_side);
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "inset-inline-end" => {
      let end_side = inline_sides.1;
      let value = inset_for_side(source, end_side);
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "inset-block-start" => {
      let start_side = block_sides.0;
      let value = inset_for_side(source, start_side);
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Block,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "inset-block-end" => {
      let end_side = block_sides.1;
      let value = inset_for_side(source, end_side);
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "inset-inline" => {
      let start = inset_for_side(source, inline_sides.0);
      let end = inset_for_side(source, inline_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(start),
          end: Some(end),
        },
        order,
      );
    }
    "inset-block" => {
      let start = inset_for_side(source, block_sides.0);
      let end = inset_for_side(source, block_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Block,
          start: Some(start),
          end: Some(end),
        },
        order,
      );
    }
    "z-index" => styles.z_index = source.z_index,
    "outline-color" => styles.outline_color = source.outline_color,
    "outline-style" => styles.outline_style = source.outline_style,
    "outline-width" => styles.outline_width = source.outline_width,
    "outline-offset" => styles.outline_offset = source.outline_offset,
    "outline" => {
      styles.outline_color = source.outline_color;
      styles.outline_style = source.outline_style;
      styles.outline_width = source.outline_width;
      styles.outline_offset = source.outline_offset;
    }
    "width" => set_length_with_order(
      &mut styles.width,
      &mut styles.logical.width_order,
      source.width,
      order,
    ),
    "height" => set_length_with_order(
      &mut styles.height,
      &mut styles.logical.height_order,
      source.height,
      order,
    ),
    "min-width" => set_length_with_order(
      &mut styles.min_width,
      &mut styles.logical.min_width_order,
      sanitize_min_length(source.min_width),
      order,
    ),
    "min-height" => set_length_with_order(
      &mut styles.min_height,
      &mut styles.logical.min_height_order,
      sanitize_min_length(source.min_height),
      order,
    ),
    "max-width" => set_length_with_order(
      &mut styles.max_width,
      &mut styles.logical.max_width_order,
      sanitize_max_length(source.max_width),
      order,
    ),
    "max-height" => {
      styles.max_height_is_max_content = source.max_height_is_max_content;
      set_length_with_order(
        &mut styles.max_height,
        &mut styles.logical.max_height_order,
        sanitize_max_length(source.max_height),
        order,
      );
    }
    "inline-size" => {
      let value = if inline_axis_is_horizontal(source.writing_mode) {
        source.width
      } else {
        source.height
      };
      set_axis_dimension(styles, crate::style::LogicalAxis::Inline, value, order);
    }
    "block-size" => {
      let value = if block_axis_is_horizontal(source.writing_mode) {
        source.width
      } else {
        source.height
      };
      set_axis_dimension(styles, crate::style::LogicalAxis::Block, value, order);
    }
    "min-inline-size" => {
      let value = if inline_axis_is_horizontal(source.writing_mode) {
        source.min_width
      } else {
        source.min_height
      };
      set_axis_min_dimension(styles, crate::style::LogicalAxis::Inline, value, order);
    }
    "min-block-size" => {
      let value = if block_axis_is_horizontal(source.writing_mode) {
        source.min_width
      } else {
        source.min_height
      };
      set_axis_min_dimension(styles, crate::style::LogicalAxis::Block, value, order);
    }
    "max-inline-size" => {
      let value = if inline_axis_is_horizontal(source.writing_mode) {
        source.max_width
      } else {
        source.max_height
      };
      set_axis_max_dimension(styles, crate::style::LogicalAxis::Inline, value, order);
    }
    "max-block-size" => {
      let value = if block_axis_is_horizontal(source.writing_mode) {
        source.max_width
      } else {
        source.max_height
      };
      set_axis_max_dimension(styles, crate::style::LogicalAxis::Block, value, order);
    }
    "margin" => {
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.margin_top,
        order,
      );
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Right,
        source.margin_right,
        order,
      );
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Bottom,
        source.margin_bottom,
        order,
      );
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Left,
        source.margin_left,
        order,
      );
    }
    "margin-top" => set_margin_side(
      styles,
      crate::style::PhysicalSide::Top,
      source.margin_top,
      order,
    ),
    "margin-right" => set_margin_side(
      styles,
      crate::style::PhysicalSide::Right,
      source.margin_right,
      order,
    ),
    "margin-bottom" => set_margin_side(
      styles,
      crate::style::PhysicalSide::Bottom,
      source.margin_bottom,
      order,
    ),
    "margin-left" => set_margin_side(
      styles,
      crate::style::PhysicalSide::Left,
      source.margin_left,
      order,
    ),
    "margin-inline-start" => {
      let value = margin_for_side(source, inline_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "margin-inline-end" => {
      let value = margin_for_side(source, inline_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "margin-block-start" => {
      let value = margin_for_side(source, block_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Block,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "margin-block-end" => {
      let value = margin_for_side(source, block_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "margin-inline" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(margin_for_side(source, inline_sides.0)),
          end: Some(margin_for_side(source, inline_sides.1)),
        },
        order,
      );
    }
    "margin-block" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Block,
          start: Some(margin_for_side(source, block_sides.0)),
          end: Some(margin_for_side(source, block_sides.1)),
        },
        order,
      );
    }
    "padding" => {
      set_padding_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.padding_top,
        order,
      );
      set_padding_side(
        styles,
        crate::style::PhysicalSide::Right,
        source.padding_right,
        order,
      );
      set_padding_side(
        styles,
        crate::style::PhysicalSide::Bottom,
        source.padding_bottom,
        order,
      );
      set_padding_side(
        styles,
        crate::style::PhysicalSide::Left,
        source.padding_left,
        order,
      );
    }
    "padding-top" => set_padding_side(
      styles,
      crate::style::PhysicalSide::Top,
      source.padding_top,
      order,
    ),
    "padding-right" => set_padding_side(
      styles,
      crate::style::PhysicalSide::Right,
      source.padding_right,
      order,
    ),
    "padding-bottom" => set_padding_side(
      styles,
      crate::style::PhysicalSide::Bottom,
      source.padding_bottom,
      order,
    ),
    "padding-left" => set_padding_side(
      styles,
      crate::style::PhysicalSide::Left,
      source.padding_left,
      order,
    ),
    "padding-inline-start" => {
      let value = padding_for_side(source, inline_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::Padding {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "padding-inline-end" => {
      let value = padding_for_side(source, inline_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::Padding {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "padding-block-start" => {
      let value = padding_for_side(source, block_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::Padding {
          axis: crate::style::LogicalAxis::Block,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "padding-block-end" => {
      let value = padding_for_side(source, block_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::Padding {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "padding-inline" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Padding {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(padding_for_side(source, inline_sides.0)),
          end: Some(padding_for_side(source, inline_sides.1)),
        },
        order,
      );
    }
    "padding-block" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Padding {
          axis: crate::style::LogicalAxis::Block,
          start: Some(padding_for_side(source, block_sides.0)),
          end: Some(padding_for_side(source, block_sides.1)),
        },
        order,
      );
    }
    "border-width" => {
      set_border_width_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.border_top_width,
        order,
      );
      set_border_width_side(
        styles,
        crate::style::PhysicalSide::Right,
        source.border_right_width,
        order,
      );
      set_border_width_side(
        styles,
        crate::style::PhysicalSide::Bottom,
        source.border_bottom_width,
        order,
      );
      set_border_width_side(
        styles,
        crate::style::PhysicalSide::Left,
        source.border_left_width,
        order,
      );
    }
    "border-top-width" => {
      set_border_width_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.border_top_width,
        order,
      );
    }
    "border-right-width" => set_border_width_side(
      styles,
      crate::style::PhysicalSide::Right,
      source.border_right_width,
      order,
    ),
    "border-bottom-width" => set_border_width_side(
      styles,
      crate::style::PhysicalSide::Bottom,
      source.border_bottom_width,
      order,
    ),
    "border-left-width" => set_border_width_side(
      styles,
      crate::style::PhysicalSide::Left,
      source.border_left_width,
      order,
    ),
    "border-inline-start-width" => {
      let value = border_width_for_side(source, inline_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderWidth {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "border-inline-end-width" => {
      let value = border_width_for_side(source, inline_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderWidth {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "border-block-start-width" => {
      let value = border_width_for_side(source, block_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderWidth {
          axis: crate::style::LogicalAxis::Block,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "border-block-end-width" => {
      let value = border_width_for_side(source, block_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderWidth {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "border-inline-width" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderWidth {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(border_width_for_side(source, inline_sides.0)),
          end: Some(border_width_for_side(source, inline_sides.1)),
        },
        order,
      );
    }
    "border-block-width" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderWidth {
          axis: crate::style::LogicalAxis::Block,
          start: Some(border_width_for_side(source, block_sides.0)),
          end: Some(border_width_for_side(source, block_sides.1)),
        },
        order,
      );
    }
    "border-color" => {
      set_border_color_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.border_top_color,
        order,
      );
      set_border_color_side(
        styles,
        crate::style::PhysicalSide::Right,
        source.border_right_color,
        order,
      );
      set_border_color_side(
        styles,
        crate::style::PhysicalSide::Bottom,
        source.border_bottom_color,
        order,
      );
      set_border_color_side(
        styles,
        crate::style::PhysicalSide::Left,
        source.border_left_color,
        order,
      );
    }
    "border-top-color" => {
      set_border_color_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.border_top_color,
        order,
      );
    }
    "border-right-color" => set_border_color_side(
      styles,
      crate::style::PhysicalSide::Right,
      source.border_right_color,
      order,
    ),
    "border-bottom-color" => set_border_color_side(
      styles,
      crate::style::PhysicalSide::Bottom,
      source.border_bottom_color,
      order,
    ),
    "border-left-color" => set_border_color_side(
      styles,
      crate::style::PhysicalSide::Left,
      source.border_left_color,
      order,
    ),
    "border-inline-start-color" => {
      let value = border_color_for_side(source, inline_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderColor {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "border-inline-end-color" => {
      let value = border_color_for_side(source, inline_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderColor {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "border-block-start-color" => {
      let value = border_color_for_side(source, block_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderColor {
          axis: crate::style::LogicalAxis::Block,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "border-block-end-color" => {
      let value = border_color_for_side(source, block_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderColor {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "border-inline-color" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderColor {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(border_color_for_side(source, inline_sides.0)),
          end: Some(border_color_for_side(source, inline_sides.1)),
        },
        order,
      );
    }
    "border-block-color" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderColor {
          axis: crate::style::LogicalAxis::Block,
          start: Some(border_color_for_side(source, block_sides.0)),
          end: Some(border_color_for_side(source, block_sides.1)),
        },
        order,
      );
    }
    "border-style" => {
      set_border_style_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.border_top_style,
        order,
      );
      set_border_style_side(
        styles,
        crate::style::PhysicalSide::Right,
        source.border_right_style,
        order,
      );
      set_border_style_side(
        styles,
        crate::style::PhysicalSide::Bottom,
        source.border_bottom_style,
        order,
      );
      set_border_style_side(
        styles,
        crate::style::PhysicalSide::Left,
        source.border_left_style,
        order,
      );
    }
    "border-top-style" => {
      set_border_style_side(
        styles,
        crate::style::PhysicalSide::Top,
        source.border_top_style,
        order,
      );
    }
    "border-right-style" => set_border_style_side(
      styles,
      crate::style::PhysicalSide::Right,
      source.border_right_style,
      order,
    ),
    "border-bottom-style" => set_border_style_side(
      styles,
      crate::style::PhysicalSide::Bottom,
      source.border_bottom_style,
      order,
    ),
    "border-left-style" => set_border_style_side(
      styles,
      crate::style::PhysicalSide::Left,
      source.border_left_style,
      order,
    ),
    "border-inline-start-style" => {
      let value = border_style_for_side(source, inline_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderStyle {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "border-inline-end-style" => {
      let value = border_style_for_side(source, inline_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderStyle {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "border-block-start-style" => {
      let value = border_style_for_side(source, block_sides.0);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderStyle {
          axis: crate::style::LogicalAxis::Block,
          start: Some(value),
          end: None,
        },
        order,
      );
    }
    "border-block-end-style" => {
      let value = border_style_for_side(source, block_sides.1);
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderStyle {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(value),
        },
        order,
      );
    }
    "border-inline-style" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderStyle {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(border_style_for_side(source, inline_sides.0)),
          end: Some(border_style_for_side(source, inline_sides.1)),
        },
        order,
      );
    }
    "border-block-style" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::BorderStyle {
          axis: crate::style::LogicalAxis::Block,
          start: Some(border_style_for_side(source, block_sides.0)),
          end: Some(border_style_for_side(source, block_sides.1)),
        },
        order,
      );
    }
    "border" => {
      styles.border_top_width = source.border_top_width;
      styles.border_right_width = source.border_right_width;
      styles.border_bottom_width = source.border_bottom_width;
      styles.border_left_width = source.border_left_width;
      styles.border_top_color = source.border_top_color;
      styles.border_right_color = source.border_right_color;
      styles.border_bottom_color = source.border_bottom_color;
      styles.border_left_color = source.border_left_color;
      styles.border_top_style = source.border_top_style;
      styles.border_right_style = source.border_right_style;
      styles.border_bottom_style = source.border_bottom_style;
      styles.border_left_style = source.border_left_style;
      styles.logical.border_width_orders = source.logical.border_width_orders.clone();
      styles.logical.border_style_orders = source.logical.border_style_orders.clone();
      styles.logical.border_color_orders = source.logical.border_color_orders.clone();
    }
    "border-inline" => {
      let start_side = inline_sides.0;
      let end_side = inline_sides.1;
      set_border_width_side(
        styles,
        start_side,
        border_width_for_side(source, start_side),
        order,
      );
      set_border_width_side(
        styles,
        end_side,
        border_width_for_side(source, end_side),
        order,
      );
      set_border_color_side(
        styles,
        start_side,
        border_color_for_side(source, start_side),
        order,
      );
      set_border_color_side(
        styles,
        end_side,
        border_color_for_side(source, end_side),
        order,
      );
      set_border_style_side(
        styles,
        start_side,
        border_style_for_side(source, start_side),
        order,
      );
      set_border_style_side(
        styles,
        end_side,
        border_style_for_side(source, end_side),
        order,
      );
    }
    "border-block" => {
      let start_side = block_sides.0;
      let end_side = block_sides.1;
      set_border_width_side(
        styles,
        start_side,
        border_width_for_side(source, start_side),
        order,
      );
      set_border_width_side(
        styles,
        end_side,
        border_width_for_side(source, end_side),
        order,
      );
      set_border_color_side(
        styles,
        start_side,
        border_color_for_side(source, start_side),
        order,
      );
      set_border_color_side(
        styles,
        end_side,
        border_color_for_side(source, end_side),
        order,
      );
      set_border_style_side(
        styles,
        start_side,
        border_style_for_side(source, start_side),
        order,
      );
      set_border_style_side(
        styles,
        end_side,
        border_style_for_side(source, end_side),
        order,
      );
    }
    "border-radius" => {
      styles.border_top_left_radius = source.border_top_left_radius;
      styles.border_top_right_radius = source.border_top_right_radius;
      styles.border_bottom_left_radius = source.border_bottom_left_radius;
      styles.border_bottom_right_radius = source.border_bottom_right_radius;
      styles.logical.corner_orders = source.logical.corner_orders.clone();
    }
    "border-top-left-radius" => set_corner_radius(
      styles,
      PhysicalCorner::TopLeft,
      Some(source.border_top_left_radius),
      order,
    ),
    "border-top-right-radius" => set_corner_radius(
      styles,
      PhysicalCorner::TopRight,
      Some(source.border_top_right_radius),
      order,
    ),
    "border-bottom-left-radius" => set_corner_radius(
      styles,
      PhysicalCorner::BottomLeft,
      Some(source.border_bottom_left_radius),
      order,
    ),
    "border-bottom-right-radius" => set_corner_radius(
      styles,
      PhysicalCorner::BottomRight,
      Some(source.border_bottom_right_radius),
      order,
    ),
    "border-start-start-radius" => {
      let target_corner =
        corner_from_logical_sides(block_sides.0, inline_sides.0).unwrap_or(PhysicalCorner::TopLeft);
      let source_corner = corner_from_logical_sides(
        block_physical_sides(source).0,
        inline_physical_sides(source).0,
      )
      .unwrap_or(PhysicalCorner::TopLeft);
      set_corner_radius(
        styles,
        target_corner,
        Some(radius_for_corner(source, source_corner)),
        order,
      );
    }
    "border-start-end-radius" => {
      let target_corner = corner_from_logical_sides(block_sides.0, inline_sides.1)
        .unwrap_or(PhysicalCorner::TopRight);
      let source_corner = corner_from_logical_sides(
        block_physical_sides(source).0,
        inline_physical_sides(source).1,
      )
      .unwrap_or(PhysicalCorner::TopRight);
      set_corner_radius(
        styles,
        target_corner,
        Some(radius_for_corner(source, source_corner)),
        order,
      );
    }
    "border-end-start-radius" => {
      let target_corner = corner_from_logical_sides(block_sides.1, inline_sides.0)
        .unwrap_or(PhysicalCorner::BottomLeft);
      let source_corner = corner_from_logical_sides(
        block_physical_sides(source).1,
        inline_physical_sides(source).0,
      )
      .unwrap_or(PhysicalCorner::BottomLeft);
      set_corner_radius(
        styles,
        target_corner,
        Some(radius_for_corner(source, source_corner)),
        order,
      );
    }
    "border-end-end-radius" => {
      let target_corner = corner_from_logical_sides(block_sides.1, inline_sides.1)
        .unwrap_or(PhysicalCorner::BottomRight);
      let source_corner = corner_from_logical_sides(
        block_physical_sides(source).1,
        inline_physical_sides(source).1,
      )
      .unwrap_or(PhysicalCorner::BottomRight);
      set_corner_radius(
        styles,
        target_corner,
        Some(radius_for_corner(source, source_corner)),
        order,
      );
    }
    "flex-direction" => styles.flex_direction = source.flex_direction,
    "flex-wrap" => styles.flex_wrap = source.flex_wrap,
    "flex-flow" => {
      styles.flex_direction = source.flex_direction;
      styles.flex_wrap = source.flex_wrap;
    }
    "justify-content" => styles.justify_content = source.justify_content,
    "align-items" => styles.align_items = source.align_items,
    "align-self" => styles.align_self = source.align_self,
    "align-content" => styles.align_content = source.align_content,
    "order" => styles.order = source.order,
    "justify-items" => styles.justify_items = source.justify_items,
    "justify-self" => styles.justify_self = source.justify_self,
    "place-items" => {
      styles.align_items = source.align_items;
      styles.justify_items = source.justify_items;
    }
    "place-self" => {
      styles.align_self = source.align_self;
      styles.justify_self = source.justify_self;
    }
    "place-content" => {
      styles.align_content = source.align_content;
      styles.justify_content = source.justify_content;
    }
    "flex-grow" => styles.flex_grow = source.flex_grow,
    "flex-shrink" => styles.flex_shrink = source.flex_shrink,
    "flex-basis" => styles.flex_basis = source.flex_basis.clone(),
    "grid-template-columns" => {
      styles.grid_template_columns = source.grid_template_columns.clone();
      styles.grid_column_line_names = source.grid_column_line_names.clone();
      styles.grid_column_names = source.grid_column_names.clone();
      styles.grid_column_subgrid = source.grid_column_subgrid;
      styles.subgrid_column_line_names = source.subgrid_column_line_names.clone();
    }
    "grid-template-rows" => {
      styles.grid_template_rows = source.grid_template_rows.clone();
      styles.grid_row_line_names = source.grid_row_line_names.clone();
      styles.grid_row_names = source.grid_row_names.clone();
      styles.grid_row_subgrid = source.grid_row_subgrid;
      styles.subgrid_row_line_names = source.subgrid_row_line_names.clone();
    }
    "grid-template-areas" => styles.grid_template_areas = source.grid_template_areas.clone(),
    "grid-template" => {
      styles.grid_template_columns = source.grid_template_columns.clone();
      styles.grid_template_rows = source.grid_template_rows.clone();
      styles.grid_template_areas = source.grid_template_areas.clone();
      styles.grid_column_line_names = source.grid_column_line_names.clone();
      styles.grid_row_line_names = source.grid_row_line_names.clone();
      styles.grid_column_names = source.grid_column_names.clone();
      styles.grid_row_names = source.grid_row_names.clone();
      styles.grid_column_subgrid = source.grid_column_subgrid;
      styles.grid_row_subgrid = source.grid_row_subgrid;
      styles.subgrid_column_line_names = source.subgrid_column_line_names.clone();
      styles.subgrid_row_line_names = source.subgrid_row_line_names.clone();
    }
    "grid" => {
      styles.grid_template_columns = source.grid_template_columns.clone();
      styles.grid_template_rows = source.grid_template_rows.clone();
      styles.grid_template_areas = source.grid_template_areas.clone();
      styles.grid_auto_rows = source.grid_auto_rows.clone();
      styles.grid_auto_columns = source.grid_auto_columns.clone();
      styles.grid_auto_flow = source.grid_auto_flow;
      styles.grid_row_gap = source.grid_row_gap;
      styles.grid_column_gap = source.grid_column_gap;
      styles.grid_gap = source.grid_gap;
      styles.grid_column_line_names = source.grid_column_line_names.clone();
      styles.grid_row_line_names = source.grid_row_line_names.clone();
      styles.grid_column_names = source.grid_column_names.clone();
      styles.grid_row_names = source.grid_row_names.clone();
      styles.grid_column_subgrid = source.grid_column_subgrid;
      styles.grid_row_subgrid = source.grid_row_subgrid;
      styles.subgrid_column_line_names = source.subgrid_column_line_names.clone();
      styles.subgrid_row_line_names = source.subgrid_row_line_names.clone();
    }
    "grid-auto-rows" => styles.grid_auto_rows = source.grid_auto_rows.clone(),
    "grid-auto-columns" => styles.grid_auto_columns = source.grid_auto_columns.clone(),
    "grid-auto-flow" => styles.grid_auto_flow = source.grid_auto_flow,
    "grid-column" => {
      styles.grid_column_start = source.grid_column_start;
      styles.grid_column_end = source.grid_column_end;
      styles.grid_column_raw = source.grid_column_raw.clone();
    }
    "grid-row" => {
      styles.grid_row_start = source.grid_row_start;
      styles.grid_row_end = source.grid_row_end;
      styles.grid_row_raw = source.grid_row_raw.clone();
    }
    "grid-column-start" => styles.grid_column_start = source.grid_column_start,
    "grid-column-end" => styles.grid_column_end = source.grid_column_end,
    "grid-row-start" => styles.grid_row_start = source.grid_row_start,
    "grid-row-end" => styles.grid_row_end = source.grid_row_end,
    "grid-area" => {
      styles.grid_row_start = source.grid_row_start;
      styles.grid_row_end = source.grid_row_end;
      styles.grid_column_start = source.grid_column_start;
      styles.grid_column_end = source.grid_column_end;
      styles.grid_row_raw = source.grid_row_raw.clone();
      styles.grid_column_raw = source.grid_column_raw.clone();
    }
    "column-count" => styles.column_count = source.column_count,
    "column-width" => styles.column_width = source.column_width,
    "column-gap" => {
      styles.column_gap = source.column_gap;
      styles.grid_column_gap = source.grid_column_gap;
    }
    "column-rule-width" => styles.column_rule_width = source.column_rule_width,
    "column-rule-style" => styles.column_rule_style = source.column_rule_style,
    "column-rule-color" => styles.column_rule_color = source.column_rule_color,
    "column-rule" => {
      styles.column_rule_width = source.column_rule_width;
      styles.column_rule_style = source.column_rule_style;
      styles.column_rule_color = source.column_rule_color;
    }
    "column-fill" => styles.column_fill = source.column_fill,
    "column-span" => styles.column_span = source.column_span,
    "columns" => {
      styles.column_count = source.column_count;
      styles.column_width = source.column_width;
    }
    "font" => {
      styles.font_style = source.font_style;
      styles.font_weight = source.font_weight;
      styles.font_variant = source.font_variant.clone();
      styles.font_variant_caps = source.font_variant_caps;
      styles.font_variant_alternates = source.font_variant_alternates.clone();
      styles.font_variant_numeric = source.font_variant_numeric;
      styles.font_variant_east_asian = source.font_variant_east_asian;
      styles.font_variant_ligatures = source.font_variant_ligatures;
      styles.font_variant_position = source.font_variant_position;
      styles.font_size = source.font_size;
      styles.line_height = source.line_height.clone();
      styles.font_family = source.font_family.clone();
      styles.font_stretch = source.font_stretch;
      styles.font_kerning = source.font_kerning;
      styles.font_synthesis = source.font_synthesis;
      styles.font_feature_settings = source.font_feature_settings.clone();
      styles.font_variation_settings = source.font_variation_settings.clone();
      styles.font_optical_sizing = source.font_optical_sizing;
      styles.font_language_override = source.font_language_override.clone();
      styles.font_variant_emoji = source.font_variant_emoji;
      styles.font_size_adjust = source.font_size_adjust;
      styles.root_font_size = source.root_font_size;
    }
    "font-family" => styles.font_family = source.font_family.clone(),
    "font-size" => {
      styles.font_size = source.font_size;
      styles.root_font_size = source.root_font_size;
    }
    "font-size-adjust" => styles.font_size_adjust = source.font_size_adjust,
    "font-weight" => styles.font_weight = source.font_weight,
    "font-style" => styles.font_style = source.font_style,
    "font-variant" => styles.font_variant = source.font_variant.clone(),
    "font-variant-caps" => styles.font_variant_caps = source.font_variant_caps,
    "font-variant-alternates" => {
      styles.font_variant_alternates = source.font_variant_alternates.clone()
    }
    "font-variant-position" => styles.font_variant_position = source.font_variant_position,
    "font-variant-east-asian" => styles.font_variant_east_asian = source.font_variant_east_asian,
    "font-variant-numeric" => styles.font_variant_numeric = source.font_variant_numeric,
    "font-variant-ligatures" => styles.font_variant_ligatures = source.font_variant_ligatures,
    "font-feature-settings" => styles.font_feature_settings = source.font_feature_settings.clone(),
    "font-optical-sizing" => styles.font_optical_sizing = source.font_optical_sizing,
    "font-language-override" => {
      styles.font_language_override = source.font_language_override.clone()
    }
    "font-variant-emoji" => styles.font_variant_emoji = source.font_variant_emoji,
    "font-variation-settings" => {
      styles.font_variation_settings = source.font_variation_settings.clone()
    }
    "font-stretch" => styles.font_stretch = source.font_stretch,
    "font-kerning" => styles.font_kerning = source.font_kerning,
    "font-synthesis" => styles.font_synthesis = source.font_synthesis,
    "font-synthesis-weight" => styles.font_synthesis.weight = source.font_synthesis.weight,
    "font-synthesis-style" => styles.font_synthesis.style = source.font_synthesis.style,
    "font-synthesis-small-caps" => {
      styles.font_synthesis.small_caps = source.font_synthesis.small_caps
    }
    "font-synthesis-position" => styles.font_synthesis.position = source.font_synthesis.position,
    "line-height" => styles.line_height = source.line_height.clone(),
    "table-layout" => styles.table_layout = source.table_layout,
    "empty-cells" => styles.empty_cells = source.empty_cells,
    "caption-side" => styles.caption_side = source.caption_side,
    "scroll-behavior" => styles.scroll_behavior = source.scroll_behavior,
    "overscroll-behavior" => {
      styles.overscroll_behavior_x = source.overscroll_behavior_x;
      styles.overscroll_behavior_y = source.overscroll_behavior_y;
    }
    "overscroll-behavior-x" => styles.overscroll_behavior_x = source.overscroll_behavior_x,
    "overscroll-behavior-y" => styles.overscroll_behavior_y = source.overscroll_behavior_y,
    "scroll-snap-type" => styles.scroll_snap_type = source.scroll_snap_type,
    "scroll-snap-align" => styles.scroll_snap_align = source.scroll_snap_align,
    "scroll-snap-stop" => styles.scroll_snap_stop = source.scroll_snap_stop,
    "scroll-timeline" => styles.scroll_timelines = source.scroll_timelines.clone(),
    "view-timeline" => styles.view_timelines = source.view_timelines.clone(),
    "animation-timeline" => styles.animation_timelines = source.animation_timelines.clone(),
    "animation-range" => styles.animation_ranges = source.animation_ranges.clone(),
    "animation-name" => styles.animation_names = source.animation_names.clone(),
    "scroll-padding" => {
      styles.scroll_padding_top = source.scroll_padding_top;
      styles.scroll_padding_right = source.scroll_padding_right;
      styles.scroll_padding_bottom = source.scroll_padding_bottom;
      styles.scroll_padding_left = source.scroll_padding_left;
    }
    "scroll-padding-top" => styles.scroll_padding_top = source.scroll_padding_top,
    "scroll-padding-right" => styles.scroll_padding_right = source.scroll_padding_right,
    "scroll-padding-bottom" => styles.scroll_padding_bottom = source.scroll_padding_bottom,
    "scroll-padding-left" => styles.scroll_padding_left = source.scroll_padding_left,
    "scroll-margin" => {
      styles.scroll_margin_top = source.scroll_margin_top;
      styles.scroll_margin_right = source.scroll_margin_right;
      styles.scroll_margin_bottom = source.scroll_margin_bottom;
      styles.scroll_margin_left = source.scroll_margin_left;
    }
    "scroll-margin-top" => styles.scroll_margin_top = source.scroll_margin_top,
    "scroll-margin-right" => styles.scroll_margin_right = source.scroll_margin_right,
    "scroll-margin-bottom" => styles.scroll_margin_bottom = source.scroll_margin_bottom,
    "scroll-margin-left" => styles.scroll_margin_left = source.scroll_margin_left,
    "scrollbar-gutter" => styles.scrollbar_gutter = source.scrollbar_gutter,
    "overflow-anchor" => styles.overflow_anchor = source.overflow_anchor,
    "pointer-events" => styles.pointer_events = source.pointer_events,
    "touch-action" => styles.touch_action = source.touch_action,
    "user-select" => styles.user_select = source.user_select,
    "scrollbar-width" => styles.scrollbar_width = source.scrollbar_width,
    "scrollbar-color" => styles.scrollbar_color = source.scrollbar_color,
    "vertical-align" => {
      styles.vertical_align = source.vertical_align;
      styles.vertical_align_specified = source.vertical_align_specified;
    }
    "text-align" => {
      styles.text_align = source.text_align;
      styles.text_align_last = source.text_align_last;
    }
    "text-align-all" => {
      styles.text_align = source.text_align;
      styles.text_align_last = source.text_align_last;
    }
    "text-align-last" => styles.text_align_last = source.text_align_last,
    "text-justify" => styles.text_justify = source.text_justify,
    "text-rendering" => styles.text_rendering = source.text_rendering,
    "text-size-adjust" => styles.text_size_adjust = source.text_size_adjust,
    "text-wrap" => styles.text_wrap = source.text_wrap,
    "text-orientation" => {
      styles.text_orientation = match source.text_orientation {
        crate::style::types::TextOrientation::SidewaysRight => {
          crate::style::types::TextOrientation::Sideways
        }
        other => other,
      }
    }
    "text-combine-upright" => styles.text_combine_upright = source.text_combine_upright,
    "text-indent" => styles.text_indent = source.text_indent.clone(),
    "text-overflow" => styles.text_overflow = source.text_overflow.clone(),
    "direction" => styles.direction = source.direction,
    "text-decoration-line" => {
      styles.text_decoration.lines = source.text_decoration.lines.clone();
      styles.text_decoration_line_specified = source.text_decoration_line_specified;
    }
    "text-decoration-style" => styles.text_decoration.style = source.text_decoration.style,
    "text-decoration-color" => styles.text_decoration.color = source.text_decoration.color,
    "text-decoration-thickness" => {
      styles.text_decoration.thickness = source.text_decoration.thickness.clone()
    }
    "text-decoration-skip-ink" => styles.text_decoration_skip_ink = source.text_decoration_skip_ink,
    "text-underline-offset" => styles.text_underline_offset = source.text_underline_offset,
    "text-underline-position" => styles.text_underline_position = source.text_underline_position,
    "text-emphasis-style" => styles.text_emphasis_style = source.text_emphasis_style.clone(),
    "text-emphasis-color" => styles.text_emphasis_color = source.text_emphasis_color,
    "text-emphasis-position" => styles.text_emphasis_position = source.text_emphasis_position,
    "ruby-position" => styles.ruby_position = source.ruby_position,
    "ruby-align" => styles.ruby_align = source.ruby_align,
    "ruby-merge" => styles.ruby_merge = source.ruby_merge,
    "text-emphasis" => {
      styles.text_emphasis_style = source.text_emphasis_style.clone();
      styles.text_emphasis_color = source.text_emphasis_color;
      styles.text_emphasis_position = source.text_emphasis_position;
    }
    "text-decoration" => {
      styles.text_decoration = source.text_decoration.clone();
      styles.text_decoration_line_specified = source.text_decoration_line_specified;
    }
    "list-style-type" => styles.list_style_type = source.list_style_type.clone(),
    "list-style-position" => styles.list_style_position = source.list_style_position,
    "list-style-image" => styles.list_style_image = source.list_style_image.clone(),
    "list-style" => {
      styles.list_style_type = source.list_style_type.clone();
      styles.list_style_position = source.list_style_position;
      styles.list_style_image = source.list_style_image.clone();
    }
    "counter-reset" => styles.counters.counter_reset = source.counters.counter_reset.clone(),
    "counter-increment" => {
      styles.counters.counter_increment = source.counters.counter_increment.clone()
    }
    "counter-set" => styles.counters.counter_set = source.counters.counter_set.clone(),
    "text-transform" => styles.text_transform = source.text_transform,
    "letter-spacing" => styles.letter_spacing = source.letter_spacing,
    "word-spacing" => styles.word_spacing = source.word_spacing,
    "white-space" => styles.white_space = source.white_space,
    "line-break" => styles.line_break = source.line_break,
    "break-before" => styles.break_before = source.break_before,
    "break-after" => styles.break_after = source.break_after,
    "break-inside" => styles.break_inside = source.break_inside,
    "widows" => styles.widows = source.widows,
    "orphans" => styles.orphans = source.orphans,
    "tab-size" => styles.tab_size = source.tab_size.clone(),
    "hyphens" => styles.hyphens = source.hyphens,
    "word-break" => styles.word_break = source.word_break,
    "unicode-bidi" => styles.unicode_bidi = source.unicode_bidi,
    "writing-mode" => styles.writing_mode = source.writing_mode,
    "cursor" => {
      styles.cursor = source.cursor;
      styles.cursor_images = source.cursor_images.clone();
    }
    "forced-color-adjust" => styles.forced_color_adjust = source.forced_color_adjust,
    "accent-color" => styles.accent_color = source.accent_color,
    "caret-color" => styles.caret_color = source.caret_color,
    "color-scheme" => styles.color_scheme = source.color_scheme.clone(),
    "color" => styles.color = source.color,
    "background-color" => styles.background_color = source.background_color,
    "background-image" => {
      styles.background_images = source.background_images.clone();
      styles.rebuild_background_layers();
    }
    "background-size" => {
      styles.background_sizes = source.background_sizes.clone();
      styles.rebuild_background_layers();
    }
    "background-repeat" => {
      styles.background_repeats = source.background_repeats.clone();
      styles.rebuild_background_layers();
    }
    "image-resolution" => styles.image_resolution = source.image_resolution,
    "background-position" => {
      styles.background_positions = source.background_positions.clone();
      styles.rebuild_background_layers();
    }
    "background-position-x" => {
      let mut xs: Vec<_> = source
        .background_positions
        .iter()
        .map(|p| match p {
          BackgroundPosition::Position { x, .. } => *x,
        })
        .collect();
      if xs.is_empty() {
        xs.push(match BackgroundLayer::default().position {
          BackgroundPosition::Position { x, .. } => x,
        });
      }
      styles.ensure_background_lists();
      let layer_count = xs.len().max(styles.background_positions.len()).max(1);
      let mut positions = Vec::with_capacity(layer_count);
      for idx in 0..layer_count {
        let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
        let source_pos = styles
          .background_positions
          .get(source_idx)
          .copied()
          .unwrap_or_else(|| BackgroundLayer::default().position);
        let x_comp = xs.get(idx).copied().unwrap_or_else(|| *xs.last().unwrap());
        let BackgroundPosition::Position { y, .. } = source_pos;
        positions.push(BackgroundPosition::Position { x: x_comp, y });
      }
      styles.background_positions = positions;
      styles.rebuild_background_layers();
    }
    "background-position-y" => {
      let mut ys: Vec<_> = source
        .background_positions
        .iter()
        .map(|p| match p {
          BackgroundPosition::Position { y, .. } => *y,
        })
        .collect();
      if ys.is_empty() {
        ys.push(match BackgroundLayer::default().position {
          BackgroundPosition::Position { y, .. } => y,
        });
      }
      styles.ensure_background_lists();
      let layer_count = ys.len().max(styles.background_positions.len()).max(1);
      let mut positions = Vec::with_capacity(layer_count);
      for idx in 0..layer_count {
        let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
        let source_pos = styles
          .background_positions
          .get(source_idx)
          .copied()
          .unwrap_or_else(|| BackgroundLayer::default().position);
        let y_comp = ys.get(idx).copied().unwrap_or_else(|| *ys.last().unwrap());
        let BackgroundPosition::Position { x, .. } = source_pos;
        positions.push(BackgroundPosition::Position { x, y: y_comp });
      }
      styles.background_positions = positions;
      styles.rebuild_background_layers();
    }
    "background-attachment" => {
      styles.background_attachments = source.background_attachments.clone();
      styles.rebuild_background_layers();
    }
    "border-image-source" => styles.border_image.source = source.border_image.source.clone(),
    "border-image-slice" => styles.border_image.slice = source.border_image.slice.clone(),
    "border-image-width" => styles.border_image.width = source.border_image.width.clone(),
    "border-image-outset" => styles.border_image.outset = source.border_image.outset.clone(),
    "border-image-repeat" => styles.border_image.repeat = source.border_image.repeat,
    "border-image" => {
      styles.border_image = source.border_image.clone();
    }
    "background-origin" => {
      styles.background_origins = source.background_origins.clone();
      styles.rebuild_background_layers();
    }
    "background-clip" => {
      styles.background_clips = source.background_clips.clone();
      styles.rebuild_background_layers();
    }
    "background-blend-mode" => {
      styles.background_blend_modes = source.background_blend_modes.clone();
      styles.rebuild_background_layers();
    }
    "background" => {
      styles.background_color = source.background_color;
      styles.background_images = source.background_images.clone();
      styles.background_positions = source.background_positions.clone();
      styles.background_sizes = source.background_sizes.clone();
      styles.background_repeats = source.background_repeats.clone();
      styles.background_attachments = source.background_attachments.clone();
      styles.background_origins = source.background_origins.clone();
      styles.background_clips = source.background_clips.clone();
      styles.background_blend_modes = source.background_blend_modes.clone();
      styles.rebuild_background_layers();
    }
    "opacity" => styles.opacity = source.opacity,
    "box-shadow" => styles.box_shadow = source.box_shadow.clone(),
    "text-shadow" => styles.text_shadow = source.text_shadow.clone(),
    "transform" => styles.transform = source.transform.clone(),
    "transform-box" => styles.transform_box = source.transform_box,
    "transform-style" => styles.transform_style = source.transform_style,
    "perspective" => styles.perspective = source.perspective,
    "perspective-origin" => styles.perspective_origin = source.perspective_origin,
    "backface-visibility" => styles.backface_visibility = source.backface_visibility,
    "filter" => styles.filter = source.filter.clone(),
    "backdrop-filter" => styles.backdrop_filter = source.backdrop_filter.clone(),
    "clip-path" => styles.clip_path = source.clip_path.clone(),
    "clip" => styles.clip = source.clip.clone(),
    "transform-origin" => styles.transform_origin = source.transform_origin.clone(),
    "mix-blend-mode" => styles.mix_blend_mode = source.mix_blend_mode,
    "isolation" => styles.isolation = source.isolation,
    "will-change" => styles.will_change = source.will_change.clone(),
    "contain" => styles.containment = source.containment,
    "border-collapse" => styles.border_collapse = source.border_collapse,
    "border-spacing" => {
      styles.border_spacing_horizontal = source.border_spacing_horizontal;
      styles.border_spacing_vertical = source.border_spacing_vertical;
    }
    "content" => {
      styles.content_value = source.content_value.clone();
      styles.content = source.content.clone();
    }
    "quotes" => styles.quotes = source.quotes.clone(),
    "image-orientation" => styles.image_orientation = source.image_orientation,
    "image-rendering" => styles.image_rendering = source.image_rendering,
    "aspect-ratio" => styles.aspect_ratio = source.aspect_ratio,
    "object-fit" => styles.object_fit = source.object_fit,
    "object-position" => styles.object_position = source.object_position.clone(),
    _ => return false,
  }
  true
}

fn apply_global_keyword(
  styles: &mut ComputedStyle,
  parent: &ComputedStyle,
  defaults: &ComputedStyle,
  revert_base: &ComputedStyle,
  revert_layer_base: Option<&ComputedStyle>,
  property: &str,
  keyword: GlobalKeyword,
  order: i32,
) -> bool {
  let Some(source) = global_keyword_source(
    keyword,
    property,
    parent,
    defaults,
    revert_base,
    revert_layer_base,
  ) else {
    return false;
  };
  apply_property_from_source(styles, source, property, order)
}

fn parse_font_variant_caps_tokens(
  tokens: &[&str],
  base_variant: FontVariant,
) -> Option<(FontVariantCaps, FontVariant)> {
  if tokens.is_empty() {
    return None;
  }
  if tokens.len() != 1 {
    return None;
  }

  let mut variant = base_variant;
  let caps = match tokens[0] {
    "normal" => FontVariantCaps::Normal,
    "small-caps" => {
      variant = FontVariant::SmallCaps;
      FontVariantCaps::SmallCaps
    }
    "all-small-caps" => FontVariantCaps::AllSmallCaps,
    "petite-caps" => FontVariantCaps::PetiteCaps,
    "all-petite-caps" => FontVariantCaps::AllPetiteCaps,
    "unicase" => FontVariantCaps::Unicase,
    "titling-caps" => FontVariantCaps::TitlingCaps,
    _ => return None,
  };

  Some((caps, variant))
}

fn parse_font_variant_ligatures_tokens(tokens: &[&str]) -> Option<FontVariantLigatures> {
  if tokens.is_empty() {
    return None;
  }

  if tokens.len() == 1 {
    return match tokens[0] {
      "normal" => Some(FontVariantLigatures::default()),
      "none" => Some(FontVariantLigatures {
        common: false,
        discretionary: false,
        historical: false,
        contextual: false,
      }),
      _ => None,
    };
  }

  let mut lig = FontVariantLigatures::default();
  let mut seen_common = false;
  let mut seen_disc = false;
  let mut seen_hist = false;
  let mut seen_ctx = false;

  for tok in tokens {
    match *tok {
      "common-ligatures" => {
        if seen_common {
          return None;
        }
        lig.common = true;
        seen_common = true;
      }
      "no-common-ligatures" => {
        if seen_common {
          return None;
        }
        lig.common = false;
        seen_common = true;
      }
      "discretionary-ligatures" => {
        if seen_disc {
          return None;
        }
        lig.discretionary = true;
        seen_disc = true;
      }
      "no-discretionary-ligatures" => {
        if seen_disc {
          return None;
        }
        lig.discretionary = false;
        seen_disc = true;
      }
      "historical-ligatures" => {
        if seen_hist {
          return None;
        }
        lig.historical = true;
        seen_hist = true;
      }
      "no-historical-ligatures" => {
        if seen_hist {
          return None;
        }
        lig.historical = false;
        seen_hist = true;
      }
      "contextual" => {
        if seen_ctx {
          return None;
        }
        lig.contextual = true;
        seen_ctx = true;
      }
      "no-contextual" => {
        if seen_ctx {
          return None;
        }
        lig.contextual = false;
        seen_ctx = true;
      }
      _ => return None,
    }
  }

  Some(lig)
}

fn parse_font_variant_numeric_tokens(tokens: &[&str]) -> Option<FontVariantNumeric> {
  if tokens.is_empty() {
    return None;
  }
  if tokens.len() == 1 && tokens[0] == "normal" {
    return Some(FontVariantNumeric::default());
  }

  let mut numeric = FontVariantNumeric::default();
  let mut seen_figure = false;
  let mut seen_spacing = false;
  let mut seen_fraction = false;

  for tok in tokens {
    match *tok {
      "lining-nums" => {
        if seen_figure {
          return None;
        }
        numeric.figure = NumericFigure::Lining;
        seen_figure = true;
      }
      "oldstyle-nums" => {
        if seen_figure {
          return None;
        }
        numeric.figure = NumericFigure::Oldstyle;
        seen_figure = true;
      }
      "proportional-nums" => {
        if seen_spacing {
          return None;
        }
        numeric.spacing = NumericSpacing::Proportional;
        seen_spacing = true;
      }
      "tabular-nums" => {
        if seen_spacing {
          return None;
        }
        numeric.spacing = NumericSpacing::Tabular;
        seen_spacing = true;
      }
      "diagonal-fractions" => {
        if seen_fraction {
          return None;
        }
        numeric.fraction = NumericFraction::Diagonal;
        seen_fraction = true;
      }
      "stacked-fractions" => {
        if seen_fraction {
          return None;
        }
        numeric.fraction = NumericFraction::Stacked;
        seen_fraction = true;
      }
      "ordinal" => numeric.ordinal = true,
      "slashed-zero" => numeric.slashed_zero = true,
      _ => return None,
    }
  }

  Some(numeric)
}

fn parse_font_variant_east_asian_tokens(tokens: &[&str]) -> Option<FontVariantEastAsian> {
  if tokens.is_empty() {
    return None;
  }
  if tokens.len() == 1 && tokens[0] == "normal" {
    return Some(FontVariantEastAsian::default());
  }

  let mut east = FontVariantEastAsian::default();
  let mut seen_variant = false;
  let mut seen_width = false;

  for tok in tokens {
    match *tok {
      "jis78" => {
        if seen_variant {
          return None;
        }
        east.variant = Some(EastAsianVariant::Jis78);
        seen_variant = true;
      }
      "jis83" => {
        if seen_variant {
          return None;
        }
        east.variant = Some(EastAsianVariant::Jis83);
        seen_variant = true;
      }
      "jis90" => {
        if seen_variant {
          return None;
        }
        east.variant = Some(EastAsianVariant::Jis90);
        seen_variant = true;
      }
      "jis04" => {
        if seen_variant {
          return None;
        }
        east.variant = Some(EastAsianVariant::Jis04);
        seen_variant = true;
      }
      "simplified" => {
        if seen_variant {
          return None;
        }
        east.variant = Some(EastAsianVariant::Simplified);
        seen_variant = true;
      }
      "traditional" => {
        if seen_variant {
          return None;
        }
        east.variant = Some(EastAsianVariant::Traditional);
        seen_variant = true;
      }
      "full-width" => {
        if seen_width {
          return None;
        }
        east.width = Some(EastAsianWidth::FullWidth);
        seen_width = true;
      }
      "proportional-width" => {
        if seen_width {
          return None;
        }
        east.width = Some(EastAsianWidth::ProportionalWidth);
        seen_width = true;
      }
      "ruby" => east.ruby = true,
      _ => return None,
    }
  }

  Some(east)
}

fn parse_font_variant_alternates_tokens(tokens: &[&str]) -> Option<FontVariantAlternates> {
  if tokens.is_empty() {
    return None;
  }

  let mut alt = FontVariantAlternates::default();
  let mut seen_stylistic = false;
  let mut seen_swash = false;
  let mut seen_ornaments = false;
  let mut seen_annotation = false;

  let parse_num = |s: &str| s.trim().parse::<u8>().ok().filter(|n| *n > 0 && *n <= 99);

  for token in tokens {
    if *token == "historical-forms" {
      alt.historical_forms = true;
      continue;
    }

    if let Some(inner) = token
      .strip_prefix("stylistic(")
      .and_then(|s| s.strip_suffix(')'))
    {
      if seen_stylistic {
        return None;
      }
      if let Some(n) = parse_num(inner) {
        alt.stylistic = Some(n);
        seen_stylistic = true;
        continue;
      }
      return None;
    }

    if let Some(inner) = token
      .strip_prefix("styleset(")
      .and_then(|s| s.strip_suffix(')'))
    {
      for part in inner
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
      {
        if let Some(n) = parse_num(part) {
          alt.stylesets.push(n);
        } else {
          return None;
        }
      }
      continue;
    }

    if let Some(inner) = token
      .strip_prefix("character-variant(")
      .and_then(|s| s.strip_suffix(')'))
    {
      for part in inner
        .split(|c: char| c == ',' || c.is_whitespace())
        .filter(|s| !s.is_empty())
      {
        if let Some(n) = parse_num(part) {
          alt.character_variants.push(n);
        } else {
          return None;
        }
      }
      continue;
    }

    if let Some(inner) = token
      .strip_prefix("swash(")
      .and_then(|s| s.strip_suffix(')'))
    {
      if seen_swash {
        return None;
      }
      if let Some(n) = parse_num(inner) {
        alt.swash = Some(n);
        seen_swash = true;
        continue;
      }
      return None;
    }

    if let Some(inner) = token
      .strip_prefix("ornaments(")
      .and_then(|s| s.strip_suffix(')'))
    {
      if seen_ornaments {
        return None;
      }
      if let Some(n) = parse_num(inner) {
        alt.ornaments = Some(n);
        seen_ornaments = true;
        continue;
      }
      return None;
    }

    if let Some(inner) = token
      .strip_prefix("annotation(")
      .and_then(|s| s.strip_suffix(')'))
    {
      if seen_annotation {
        return None;
      }
      if !inner.trim().is_empty() {
        alt.annotation = Some(inner.trim().to_string());
        seen_annotation = true;
        continue;
      }
      return None;
    }

    return None;
  }

  Some(alt)
}

fn parse_font_variant_position_tokens(
  tokens: &[&str],
  base_position: FontVariantPosition,
) -> Option<FontVariantPosition> {
  if tokens.is_empty() {
    return Some(base_position);
  }
  if tokens.len() != 1 {
    return None;
  }

  match tokens[0] {
    "normal" => Some(FontVariantPosition::Normal),
    "sub" => Some(FontVariantPosition::Sub),
    "super" => Some(FontVariantPosition::Super),
    _ => None,
  }
}

fn split_font_variant_tokens(input: &str) -> Vec<String> {
  let mut tokens = Vec::new();
  let mut current = String::new();
  let mut paren_depth = 0usize;

  for ch in input.chars() {
    if ch.is_whitespace() && paren_depth == 0 {
      if !current.is_empty() {
        tokens.push(current.clone());
        current.clear();
      }
      continue;
    }

    if ch == '(' {
      paren_depth = paren_depth.saturating_add(1);
    } else if ch == ')' && paren_depth > 0 {
      paren_depth -= 1;
    }

    current.push(ch);
  }

  if !current.is_empty() {
    tokens.push(current);
  }

  tokens
}

pub fn apply_declaration(
  styles: &mut ComputedStyle,
  decl: &Declaration,
  parent_styles: &ComputedStyle,
  parent_font_size: f32,
  root_font_size: f32,
) {
  apply_declaration_with_viewport(
    styles,
    decl,
    parent_styles,
    parent_font_size,
    root_font_size,
    DEFAULT_VIEWPORT,
  );
}

/// Applies a declaration using an explicit viewport for resolving viewport-relative units.
pub fn apply_declaration_with_viewport(
  styles: &mut ComputedStyle,
  decl: &Declaration,
  parent_styles: &ComputedStyle,
  parent_font_size: f32,
  root_font_size: f32,
  viewport: Size,
) {
  let defaults = ComputedStyle::default();
  apply_declaration_with_base(
    styles,
    decl,
    parent_styles,
    &defaults,
    None,
    parent_font_size,
    root_font_size,
    viewport,
  );
}

pub fn apply_declaration_with_base(
  styles: &mut ComputedStyle,
  decl: &Declaration,
  parent_styles: &ComputedStyle,
  revert_base: &ComputedStyle,
  revert_layer_base: Option<&ComputedStyle>,
  parent_font_size: f32,
  root_font_size: f32,
  viewport: crate::geometry::Size,
) {
  // Handle CSS Custom Properties (--*)
  if decl.property.starts_with("--") {
    // Preserve the raw custom property value verbatim.
    styles
      .custom_properties
      .insert(decl.property.clone(), decl.raw_value.clone());
    return;
  }

  // Resolve var() references in the value
  let resolved_value =
    match resolve_var_for_property(&decl.value, &styles.custom_properties, &decl.property) {
      VarResolutionResult::Resolved(v) => *v,
      // Unresolved or invalid at computed-value time -> declaration is ignored per spec.
      _ => return,
    };
  let order = styles.logical.next_order();
  if let Some(global) = global_keyword(&resolved_value) {
    let defaults = ComputedStyle::default();
    if apply_global_keyword(
      styles,
      parent_styles,
      &defaults,
      revert_base,
      revert_layer_base,
      decl.property.as_str(),
      global,
      order,
    ) {
      return;
    }
  }

  let resolve_color_value = |value: &PropertyValue| -> Option<Rgba> {
    match value {
      PropertyValue::Color(c) => Some(c.to_rgba(styles.color)),
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => Some(styles.color),
      _ => None,
    }
  };

  match decl.property.as_str() {
    "all" => {
      let Some(global) = global_keyword(&resolved_value) else {
        return;
      };
      let defaults = ComputedStyle::default();
      let Some(source) = global_keyword_source(
        global,
        "all",
        parent_styles,
        &defaults,
        revert_base,
        revert_layer_base,
      ) else {
        return;
      };
      let prev_direction = styles.direction;
      let prev_unicode_bidi = styles.unicode_bidi;
      let next_order = styles.logical.next_order_value();

      *styles = source.clone();
      styles.direction = prev_direction;
      styles.unicode_bidi = prev_unicode_bidi;
      styles.logical.reset();
      set_all_logical_orders(&mut styles.logical, order, next_order);
      return;
    }
    // Display
    "display" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Ok(display) = Display::parse(kw) {
          styles.display = display;
        }
      }
    }
    "visibility" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.visibility = match kw.as_str() {
          "visible" => crate::style::computed::Visibility::Visible,
          "hidden" => crate::style::computed::Visibility::Hidden,
          "collapse" => crate::style::computed::Visibility::Collapse,
          _ => styles.visibility,
        };
      }
    }
    "float" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Ok(value) = Float::parse(kw) {
          styles.float = value;
        }
      }
    }
    "clear" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Ok(value) = Clear::parse(kw) {
          styles.clear = value;
        }
      }
    }
    "overflow" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let overflow = match kw.as_str() {
          "visible" => Overflow::Visible,
          "hidden" => Overflow::Hidden,
          "scroll" => Overflow::Scroll,
          "auto" => Overflow::Auto,
          "clip" => Overflow::Clip,
          _ => styles.overflow_x,
        };
        styles.overflow_x = overflow;
        styles.overflow_y = overflow;
      }
    }
    "overflow-x" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.overflow_x = match kw.as_str() {
          "visible" => Overflow::Visible,
          "hidden" => Overflow::Hidden,
          "scroll" => Overflow::Scroll,
          "auto" => Overflow::Auto,
          "clip" => Overflow::Clip,
          _ => styles.overflow_x,
        };
      }
    }
    "overflow-y" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.overflow_y = match kw.as_str() {
          "visible" => Overflow::Visible,
          "hidden" => Overflow::Hidden,
          "scroll" => Overflow::Scroll,
          "auto" => Overflow::Auto,
          "clip" => Overflow::Clip,
          _ => styles.overflow_y,
        };
      }
    }

    // Position
    "position" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Ok(position) = Position::parse(kw) {
          styles.position = position;
        }
      }
    }
    "box-sizing" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        match kw.as_str() {
          _ if kw.eq_ignore_ascii_case("content-box") => styles.box_sizing = BoxSizing::ContentBox,
          _ if kw.eq_ignore_ascii_case("border-box") => styles.box_sizing = BoxSizing::BorderBox,
          _ => {}
        }
      }
    }

    "top" => set_inset_side(
      styles,
      crate::style::PhysicalSide::Top,
      extract_length(&resolved_value),
      order,
    ),
    "right" => set_inset_side(
      styles,
      crate::style::PhysicalSide::Right,
      extract_length(&resolved_value),
      order,
    ),
    "bottom" => set_inset_side(
      styles,
      crate::style::PhysicalSide::Bottom,
      extract_length(&resolved_value),
      order,
    ),
    "left" => set_inset_side(
      styles,
      crate::style::PhysicalSide::Left,
      extract_length(&resolved_value),
      order,
    ),
    "inset" => {
      if let Some(lengths) = extract_margin_values(&resolved_value) {
        let mut top = styles.top;
        let mut right = styles.right;
        let mut bottom = styles.bottom;
        let mut left = styles.left;
        apply_margin_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
        set_inset_side(styles, crate::style::PhysicalSide::Top, top, order);
        set_inset_side(styles, crate::style::PhysicalSide::Right, right, order);
        set_inset_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
        set_inset_side(styles, crate::style::PhysicalSide::Left, left, order);
      }
    }
    "inset-inline-start" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(extract_length(&resolved_value)),
          end: None,
        },
        order,
      );
    }
    "inset-inline-end" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "inset-block-start" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Block,
          start: Some(extract_length(&resolved_value)),
          end: None,
        },
        order,
      );
    }
    "inset-block-end" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Inset {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "inset-inline" => {
      if let Some(values) = extract_margin_values(&resolved_value) {
        let start = values.first().copied().flatten();
        let end = values.get(1).copied().flatten().or(start);
        push_logical(
          styles,
          crate::style::LogicalProperty::Inset {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }
    "inset-block" => {
      if let Some(values) = extract_margin_values(&resolved_value) {
        let start = values.first().copied().flatten();
        let end = values.get(1).copied().flatten().or(start);
        push_logical(
          styles,
          crate::style::LogicalProperty::Inset {
            axis: crate::style::LogicalAxis::Block,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }
    "z-index" => match resolved_value {
      PropertyValue::Number(n) => styles.z_index = Some(n as i32),
      PropertyValue::Keyword(ref kw) if kw.eq_ignore_ascii_case("auto") => styles.z_index = None,
      _ => {}
    },
    "outline-color" => match &resolved_value {
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => {
        styles.outline_color = OutlineColor::CurrentColor;
      }
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("invert") => {
        styles.outline_color = OutlineColor::Invert;
      }
      PropertyValue::Color(c) => {
        styles.outline_color = OutlineColor::Color(c.to_rgba(styles.color));
      }
      _ => {}
    },
    "outline-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(style) = parse_outline_style(kw) {
          styles.outline_style = style;
        }
      }
    }
    "outline-width" => {
      if let Some(width) = parse_outline_width(&resolved_value) {
        styles.outline_width = width;
      }
    }
    "outline-offset" => {
      if let Some(len) = extract_length(&resolved_value) {
        styles.outline_offset = len;
      }
    }
    "outline" => {
      apply_outline_shorthand(styles, &resolved_value);
    }

    // Width and height
    "width" => set_length_with_order(
      &mut styles.width,
      &mut styles.logical.width_order,
      extract_length(&resolved_value),
      order,
    ),
    "height" => {
      styles.max_height_is_max_content = false;
      set_length_with_order(
        &mut styles.height,
        &mut styles.logical.height_order,
        extract_length(&resolved_value),
        order,
      );
    }
    "min-width" => set_length_with_order(
      &mut styles.min_width,
      &mut styles.logical.min_width_order,
      sanitize_min_length(extract_length(&resolved_value)),
      order,
    ),
    "min-height" => set_length_with_order(
      &mut styles.min_height,
      &mut styles.logical.min_height_order,
      sanitize_min_length(extract_length(&resolved_value)),
      order,
    ),
    "max-width" => set_length_with_order(
      &mut styles.max_width,
      &mut styles.logical.max_width_order,
      sanitize_max_length(extract_length(&resolved_value)),
      order,
    ),
    "max-height" => {
      if let PropertyValue::Keyword(ref kw) = resolved_value {
        if kw.eq_ignore_ascii_case("max-content")
          || kw.eq_ignore_ascii_case("-webkit-max-content")
          || kw.eq_ignore_ascii_case("-moz-max-content")
        {
          if order >= styles.logical.max_height_order {
            styles.max_height_is_max_content = true;
            styles.logical.max_height_order = order;
            styles.max_height = Some(Length::px(f32::INFINITY));
          }
          return;
        }
      }
      styles.max_height_is_max_content = false;
      set_length_with_order(
        &mut styles.max_height,
        &mut styles.logical.max_height_order,
        sanitize_max_length(extract_length(&resolved_value)),
        order,
      );
    }
    "inline-size" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::InlineSize {
          value: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "block-size" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::BlockSize {
          value: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "min-inline-size" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::MinInlineSize {
          value: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "min-block-size" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::MinBlockSize {
          value: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "max-inline-size" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::MaxInlineSize {
          value: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "max-block-size" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::MaxBlockSize {
          value: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }

    // Margin
    "margin" => {
      if let Some(lengths) = extract_margin_values(&resolved_value) {
        let mut top = styles.margin_top;
        let mut right = styles.margin_right;
        let mut bottom = styles.margin_bottom;
        let mut left = styles.margin_left;
        apply_margin_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
        set_margin_side(styles, crate::style::PhysicalSide::Top, top, order);
        set_margin_side(styles, crate::style::PhysicalSide::Right, right, order);
        set_margin_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
        set_margin_side(styles, crate::style::PhysicalSide::Left, left, order);
      }
    }
    "margin-top" => {
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Top,
        extract_length(&resolved_value),
        order,
      );
    }
    "margin-right" => {
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Right,
        extract_length(&resolved_value),
        order,
      );
    }
    "margin-bottom" => {
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Bottom,
        extract_length(&resolved_value),
        order,
      );
    }
    "margin-left" => {
      set_margin_side(
        styles,
        crate::style::PhysicalSide::Left,
        extract_length(&resolved_value),
        order,
      );
    }
    "margin-inline-start" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Inline,
          start: Some(extract_length(&resolved_value)),
          end: None,
        },
        order,
      );
    }
    "margin-inline-end" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Inline,
          start: None,
          end: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "margin-block-start" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Block,
          start: Some(extract_length(&resolved_value)),
          end: None,
        },
        order,
      );
    }
    "margin-block-end" => {
      push_logical(
        styles,
        crate::style::LogicalProperty::Margin {
          axis: crate::style::LogicalAxis::Block,
          start: None,
          end: Some(extract_length(&resolved_value)),
        },
        order,
      );
    }
    "margin-inline" => {
      if let Some(values) = extract_margin_values(&resolved_value) {
        let start = values.first().copied().flatten();
        let end = values.get(1).copied().flatten().or(start);
        push_logical(
          styles,
          crate::style::LogicalProperty::Margin {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }
    "margin-block" => {
      if let Some(values) = extract_margin_values(&resolved_value) {
        let start = values.first().copied().flatten();
        let end = values.get(1).copied().flatten().or(start);
        push_logical(
          styles,
          crate::style::LogicalProperty::Margin {
            axis: crate::style::LogicalAxis::Block,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }

    // Padding
    "padding" => {
      if let Some(lengths) = extract_box_values(&resolved_value) {
        let mut top = styles.padding_top;
        let mut right = styles.padding_right;
        let mut bottom = styles.padding_bottom;
        let mut left = styles.padding_left;
        apply_box_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
        set_padding_side(styles, crate::style::PhysicalSide::Top, top, order);
        set_padding_side(styles, crate::style::PhysicalSide::Right, right, order);
        set_padding_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
        set_padding_side(styles, crate::style::PhysicalSide::Left, left, order);
      }
    }
    "padding-top" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_padding_side(styles, crate::style::PhysicalSide::Top, len, order);
      }
    }
    "padding-right" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_padding_side(styles, crate::style::PhysicalSide::Right, len, order);
      }
    }
    "padding-bottom" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_padding_side(styles, crate::style::PhysicalSide::Bottom, len, order);
      }
    }
    "padding-left" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_padding_side(styles, crate::style::PhysicalSide::Left, len, order);
      }
    }
    "padding-inline-start" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::Padding {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(len),
            end: None,
          },
          order,
        );
      }
    }
    "padding-inline-end" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::Padding {
            axis: crate::style::LogicalAxis::Inline,
            start: None,
            end: Some(len),
          },
          order,
        );
      }
    }
    "padding-block-start" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::Padding {
            axis: crate::style::LogicalAxis::Block,
            start: Some(len),
            end: None,
          },
          order,
        );
      }
    }
    "padding-block-end" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::Padding {
            axis: crate::style::LogicalAxis::Block,
            start: None,
            end: Some(len),
          },
          order,
        );
      }
    }
    "padding-inline" => {
      if let Some((start, end)) = extract_length_pair(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::Padding {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }
    "padding-block" => {
      if let Some((start, end)) = extract_length_pair(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::Padding {
            axis: crate::style::LogicalAxis::Block,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }

    // Border width
    "border-width" => {
      if let Some(lengths) = extract_box_values(&resolved_value) {
        let mut top = styles.border_top_width;
        let mut right = styles.border_right_width;
        let mut bottom = styles.border_bottom_width;
        let mut left = styles.border_left_width;
        apply_box_values(&mut top, &mut right, &mut bottom, &mut left, lengths);
        set_border_width_side(styles, crate::style::PhysicalSide::Top, top, order);
        set_border_width_side(styles, crate::style::PhysicalSide::Right, right, order);
        set_border_width_side(styles, crate::style::PhysicalSide::Bottom, bottom, order);
        set_border_width_side(styles, crate::style::PhysicalSide::Left, left, order);
      }
    }
    "border-top-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_border_width_side(styles, crate::style::PhysicalSide::Top, len, order);
      }
    }
    "border-right-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_border_width_side(styles, crate::style::PhysicalSide::Right, len, order);
      }
    }
    "border-bottom-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_border_width_side(styles, crate::style::PhysicalSide::Bottom, len, order);
      }
    }
    "border-left-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        set_border_width_side(styles, crate::style::PhysicalSide::Left, len, order);
      }
    }
    "border-inline-start-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderWidth {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(len),
            end: None,
          },
          order,
        );
      }
    }
    "border-inline-end-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderWidth {
            axis: crate::style::LogicalAxis::Inline,
            start: None,
            end: Some(len),
          },
          order,
        );
      }
    }
    "border-block-start-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderWidth {
            axis: crate::style::LogicalAxis::Block,
            start: Some(len),
            end: None,
          },
          order,
        );
      }
    }
    "border-block-end-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderWidth {
            axis: crate::style::LogicalAxis::Block,
            start: None,
            end: Some(len),
          },
          order,
        );
      }
    }
    "border-inline-width" => {
      if let Some((start, end)) = extract_length_pair(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderWidth {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }
    "border-block-width" => {
      if let Some((start, end)) = extract_length_pair(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderWidth {
            axis: crate::style::LogicalAxis::Block,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }

    // Border color
    "border-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        set_border_color_side(styles, crate::style::PhysicalSide::Top, c, order);
        set_border_color_side(styles, crate::style::PhysicalSide::Right, c, order);
        set_border_color_side(styles, crate::style::PhysicalSide::Bottom, c, order);
        set_border_color_side(styles, crate::style::PhysicalSide::Left, c, order);
      }
    }
    "border-top-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        set_border_color_side(styles, crate::style::PhysicalSide::Top, c, order);
      }
    }
    "border-right-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        set_border_color_side(styles, crate::style::PhysicalSide::Right, c, order);
      }
    }
    "border-bottom-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        set_border_color_side(styles, crate::style::PhysicalSide::Bottom, c, order);
      }
    }
    "border-left-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        set_border_color_side(styles, crate::style::PhysicalSide::Left, c, order);
      }
    }
    "border-inline-start-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderColor {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(c),
            end: None,
          },
          order,
        );
      }
    }
    "border-inline-end-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderColor {
            axis: crate::style::LogicalAxis::Inline,
            start: None,
            end: Some(c),
          },
          order,
        );
      }
    }
    "border-block-start-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderColor {
            axis: crate::style::LogicalAxis::Block,
            start: Some(c),
            end: None,
          },
          order,
        );
      }
    }
    "border-block-end-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderColor {
            axis: crate::style::LogicalAxis::Block,
            start: None,
            end: Some(c),
          },
          order,
        );
      }
    }
    "border-inline-color" => {
      if let Some((start, end)) = extract_color_pair_with(&resolved_value, &resolve_color_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderColor {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }
    "border-block-color" => {
      if let Some((start, end)) = extract_color_pair_with(&resolved_value, &resolve_color_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderColor {
            axis: crate::style::LogicalAxis::Block,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }

    // Border style
    "border-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let style = parse_border_style(kw);
        set_border_style_side(styles, crate::style::PhysicalSide::Top, style, order);
        set_border_style_side(styles, crate::style::PhysicalSide::Right, style, order);
        set_border_style_side(styles, crate::style::PhysicalSide::Bottom, style, order);
        set_border_style_side(styles, crate::style::PhysicalSide::Left, style, order);
      }
    }
    "border-top-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        set_border_style_side(
          styles,
          crate::style::PhysicalSide::Top,
          parse_border_style(kw),
          order,
        );
      }
    }
    "border-right-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        set_border_style_side(
          styles,
          crate::style::PhysicalSide::Right,
          parse_border_style(kw),
          order,
        );
      }
    }
    "border-bottom-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        set_border_style_side(
          styles,
          crate::style::PhysicalSide::Bottom,
          parse_border_style(kw),
          order,
        );
      }
    }
    "border-left-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        set_border_style_side(
          styles,
          crate::style::PhysicalSide::Left,
          parse_border_style(kw),
          order,
        );
      }
    }
    "border-inline-start-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderStyle {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(parse_border_style(kw)),
            end: None,
          },
          order,
        );
      }
    }
    "border-inline-end-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderStyle {
            axis: crate::style::LogicalAxis::Inline,
            start: None,
            end: Some(parse_border_style(kw)),
          },
          order,
        );
      }
    }
    "border-block-start-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderStyle {
            axis: crate::style::LogicalAxis::Block,
            start: Some(parse_border_style(kw)),
            end: None,
          },
          order,
        );
      }
    }
    "border-block-end-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderStyle {
            axis: crate::style::LogicalAxis::Block,
            start: None,
            end: Some(parse_border_style(kw)),
          },
          order,
        );
      }
    }
    "border-inline-style" => {
      let styles_list: Vec<BorderStyle> = match &resolved_value {
        PropertyValue::Keyword(kw) => vec![parse_border_style(kw)],
        PropertyValue::Multiple(values) => values
          .iter()
          .filter_map(|v| {
            if let PropertyValue::Keyword(kw) = v {
              Some(parse_border_style(kw))
            } else {
              None
            }
          })
          .collect(),
        _ => Vec::new(),
      };
      if !styles_list.is_empty() {
        let start = styles_list[0];
        let end = *styles_list.get(1).unwrap_or(&start);
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderStyle {
            axis: crate::style::LogicalAxis::Inline,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }
    "border-block-style" => {
      let styles_list: Vec<BorderStyle> = match &resolved_value {
        PropertyValue::Keyword(kw) => vec![parse_border_style(kw)],
        PropertyValue::Multiple(values) => values
          .iter()
          .filter_map(|v| {
            if let PropertyValue::Keyword(kw) = v {
              Some(parse_border_style(kw))
            } else {
              None
            }
          })
          .collect(),
        _ => Vec::new(),
      };
      if !styles_list.is_empty() {
        let start = styles_list[0];
        let end = *styles_list.get(1).unwrap_or(&start);
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderStyle {
            axis: crate::style::LogicalAxis::Block,
            start: Some(start),
            end: Some(end),
          },
          order,
        );
      }
    }

    // Border (shorthand)
    "border" => {
      let current_color = styles.color;
      let mut apply_shorthand = |values: &[PropertyValue]| {
        let mut width: Option<Length> = None;
        let mut style_val: Option<BorderStyle> = None;
        let mut color: Option<Rgba> = None;

        for val in values {
          match val {
            PropertyValue::Length(len) => width = Some(*len),
            PropertyValue::Number(n) => width = Some(Length::px(*n)),
            PropertyValue::Keyword(kw) => {
              if kw.eq_ignore_ascii_case("currentcolor") {
                color = Some(current_color);
              } else {
                style_val = Some(parse_border_style(kw));
              }
            }
            PropertyValue::Color(c) => color = Some(c.to_rgba(current_color)),
            _ => {}
          }
        }

        if let Some(BorderStyle::None | BorderStyle::Hidden) = style_val {
          width = Some(Length::px(0.0));
        }

        // Shorthand resets unspecified subproperties to their initial values.
        if style_val.is_none() {
          style_val = Some(BorderStyle::None);
        }
        if color.is_none() {
          color = Some(current_color);
        }

        if let Some(w) = width {
          set_border_width_side(styles, crate::style::PhysicalSide::Top, w, order);
          set_border_width_side(styles, crate::style::PhysicalSide::Right, w, order);
          set_border_width_side(styles, crate::style::PhysicalSide::Bottom, w, order);
          set_border_width_side(styles, crate::style::PhysicalSide::Left, w, order);
        }
        if let Some(st) = style_val {
          set_border_style_side(styles, crate::style::PhysicalSide::Top, st, order);
          set_border_style_side(styles, crate::style::PhysicalSide::Right, st, order);
          set_border_style_side(styles, crate::style::PhysicalSide::Bottom, st, order);
          set_border_style_side(styles, crate::style::PhysicalSide::Left, st, order);
        }
        if let Some(c) = color {
          set_border_color_side(styles, crate::style::PhysicalSide::Top, c, order);
          set_border_color_side(styles, crate::style::PhysicalSide::Right, c, order);
          set_border_color_side(styles, crate::style::PhysicalSide::Bottom, c, order);
          set_border_color_side(styles, crate::style::PhysicalSide::Left, c, order);
        }
      };

      match &resolved_value {
        PropertyValue::Multiple(values) => apply_shorthand(values),
        other => apply_shorthand(std::slice::from_ref(other)),
      }
    }
    "border-inline" => {
      if let PropertyValue::Multiple(values) = &resolved_value {
        let mut width: Option<Length> = None;
        let mut style_val: Option<BorderStyle> = None;
        let mut color: Option<Rgba> = None;
        for val in values {
          match val {
            PropertyValue::Length(len) => width = Some(*len),
            PropertyValue::Keyword(kw) => style_val = Some(parse_border_style(kw)),
            _ => {
              if let Some(c) = resolve_color_value(val) {
                color = Some(c);
              }
            }
          }
        }
        if let Some(w) = width {
          push_logical(
            styles,
            crate::style::LogicalProperty::BorderWidth {
              axis: crate::style::LogicalAxis::Inline,
              start: Some(w),
              end: Some(w),
            },
            order,
          );
        }
        if let Some(st) = style_val {
          push_logical(
            styles,
            crate::style::LogicalProperty::BorderStyle {
              axis: crate::style::LogicalAxis::Inline,
              start: Some(st),
              end: Some(st),
            },
            order,
          );
        }
        if let Some(c) = color {
          push_logical(
            styles,
            crate::style::LogicalProperty::BorderColor {
              axis: crate::style::LogicalAxis::Inline,
              start: Some(c),
              end: Some(c),
            },
            order,
          );
        }
      }
    }
    "border-block" => {
      if let PropertyValue::Multiple(values) = &resolved_value {
        let mut width: Option<Length> = None;
        let mut style_val: Option<BorderStyle> = None;
        let mut color: Option<Rgba> = None;
        for val in values {
          match val {
            PropertyValue::Length(len) => width = Some(*len),
            PropertyValue::Keyword(kw) => style_val = Some(parse_border_style(kw)),
            _ => {
              if let Some(c) = resolve_color_value(val) {
                color = Some(c);
              }
            }
          }
        }
        if let Some(w) = width {
          push_logical(
            styles,
            crate::style::LogicalProperty::BorderWidth {
              axis: crate::style::LogicalAxis::Block,
              start: Some(w),
              end: Some(w),
            },
            order,
          );
        }
        if let Some(st) = style_val {
          push_logical(
            styles,
            crate::style::LogicalProperty::BorderStyle {
              axis: crate::style::LogicalAxis::Block,
              start: Some(st),
              end: Some(st),
            },
            order,
          );
        }
        if let Some(c) = color {
          push_logical(
            styles,
            crate::style::LogicalProperty::BorderColor {
              axis: crate::style::LogicalAxis::Block,
              start: Some(c),
              end: Some(c),
            },
            order,
          );
        }
      }
    }

    // Border radius
    "border-radius" => {
      if let Some(radii) = parse_border_radius_shorthand(&resolved_value) {
        set_corner_radius(styles, PhysicalCorner::TopLeft, Some(radii[0]), order);
        set_corner_radius(styles, PhysicalCorner::TopRight, Some(radii[1]), order);
        set_corner_radius(styles, PhysicalCorner::BottomLeft, Some(radii[3]), order);
        set_corner_radius(styles, PhysicalCorner::BottomRight, Some(radii[2]), order);
      }
    }
    "border-top-left-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        set_corner_radius(styles, PhysicalCorner::TopLeft, Some(radius), order);
      }
    }
    "border-top-right-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        set_corner_radius(styles, PhysicalCorner::TopRight, Some(radius), order);
      }
    }
    "border-bottom-left-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        set_corner_radius(styles, PhysicalCorner::BottomLeft, Some(radius), order);
      }
    }
    "border-bottom-right-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        set_corner_radius(styles, PhysicalCorner::BottomRight, Some(radius), order);
      }
    }
    "border-start-start-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderCorner {
            block_start: true,
            inline_start: true,
            value: Some(radius),
          },
          order,
        );
      }
    }
    "border-start-end-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderCorner {
            block_start: true,
            inline_start: false,
            value: Some(radius),
          },
          order,
        );
      }
    }
    "border-end-start-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderCorner {
            block_start: false,
            inline_start: true,
            value: Some(radius),
          },
          order,
        );
      }
    }
    "border-end-end-radius" => {
      if let Some(radius) = parse_single_corner_radius(&resolved_value) {
        push_logical(
          styles,
          crate::style::LogicalProperty::BorderCorner {
            block_start: false,
            inline_start: false,
            value: Some(radius),
          },
          order,
        );
      }
    }

    // Flexbox
    "flex-direction" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.flex_direction = match kw.as_str() {
          "row" => FlexDirection::Row,
          "row-reverse" => FlexDirection::RowReverse,
          "column" => FlexDirection::Column,
          "column-reverse" => FlexDirection::ColumnReverse,
          _ => styles.flex_direction,
        };
      }
    }
    "flex-wrap" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.flex_wrap = match kw.as_str() {
          "nowrap" => FlexWrap::NoWrap,
          "wrap" => FlexWrap::Wrap,
          "wrap-reverse" => FlexWrap::WrapReverse,
          _ => styles.flex_wrap,
        };
      }
    }
    "flex-flow" => {
      let tokens: Vec<String> = match &resolved_value {
        PropertyValue::Keyword(kw) => kw
          .split_whitespace()
          .map(|s| s.to_ascii_lowercase())
          .collect(),
        PropertyValue::Multiple(values) => values
          .iter()
          .flat_map(|v| match v {
            PropertyValue::Keyword(kw) => kw
              .split_whitespace()
              .map(|s| s.to_ascii_lowercase())
              .collect(),
            _ => Vec::new(),
          })
          .collect(),
        _ => Vec::new(),
      };

      if !tokens.is_empty() && tokens.len() <= 2 {
        let mut direction: Option<FlexDirection> = None;
        let mut wrap: Option<FlexWrap> = None;
        let mut valid = true;

        for token in tokens {
          match token.as_str() {
            "row" => {
              if direction.replace(FlexDirection::Row).is_some() {
                valid = false;
                break;
              }
            }
            "row-reverse" => {
              if direction.replace(FlexDirection::RowReverse).is_some() {
                valid = false;
                break;
              }
            }
            "column" => {
              if direction.replace(FlexDirection::Column).is_some() {
                valid = false;
                break;
              }
            }
            "column-reverse" => {
              if direction.replace(FlexDirection::ColumnReverse).is_some() {
                valid = false;
                break;
              }
            }
            "nowrap" => {
              if wrap.replace(FlexWrap::NoWrap).is_some() {
                valid = false;
                break;
              }
            }
            "wrap" => {
              if wrap.replace(FlexWrap::Wrap).is_some() {
                valid = false;
                break;
              }
            }
            "wrap-reverse" => {
              if wrap.replace(FlexWrap::WrapReverse).is_some() {
                valid = false;
                break;
              }
            }
            _ => {
              valid = false;
              break;
            }
          }
        }

        if valid && (direction.is_some() || wrap.is_some()) {
          styles.flex_direction = direction.unwrap_or(FlexDirection::Row);
          styles.flex_wrap = wrap.unwrap_or(FlexWrap::NoWrap);
        }
      }
    }
    "justify-content" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.justify_content = match kw.as_str() {
          "flex-start" | "start" => JustifyContent::FlexStart,
          "flex-end" | "end" => JustifyContent::FlexEnd,
          "center" => JustifyContent::Center,
          "space-between" => JustifyContent::SpaceBetween,
          "space-around" => JustifyContent::SpaceAround,
          "space-evenly" => JustifyContent::SpaceEvenly,
          _ => styles.justify_content,
        };
      }
    }
    "align-items" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(value) = parse_align_keyword(kw) {
          styles.align_items = value;
        }
      }
    }
    "align-self" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.align_self = match kw.as_str() {
          "auto" => None,
          _ => parse_align_keyword(kw),
        }
        .or(styles.align_self);
      }
    }
    "align-content" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.align_content = match kw.as_str() {
          "flex-start" | "start" => AlignContent::FlexStart,
          "flex-end" | "end" => AlignContent::FlexEnd,
          "center" => AlignContent::Center,
          "space-between" => AlignContent::SpaceBetween,
          "space-evenly" => AlignContent::SpaceEvenly,
          "space-around" => AlignContent::SpaceAround,
          "stretch" => AlignContent::Stretch,
          _ => styles.align_content,
        };
      }
    }
    "justify-items" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        match kw.as_str() {
          "auto" => styles.justify_items = AlignItems::Stretch,
          _ => {
            if let Some(value) = parse_align_keyword(kw) {
              styles.justify_items = value;
            }
          }
        }
      }
    }
    "justify-self" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.justify_self = match kw.as_str() {
          "auto" => None,
          _ => parse_align_keyword(kw),
        }
        .or(styles.justify_self);
      }
    }
    "place-items" => {
      if let Some((align, justify)) = parse_place_pair(&resolved_value) {
        styles.align_items = align;
        styles.justify_items = justify;
      }
    }
    "place-self" => {
      if let Some((align, justify)) = parse_place_pair(&resolved_value) {
        styles.align_self = Some(align);
        styles.justify_self = Some(justify);
      }
    }
    "place-content" => {
      if let Some((align, justify)) = parse_place_content_pair(&resolved_value) {
        styles.align_content = align;
        styles.justify_content = justify;
      }
    }
    "order" => {
      if let PropertyValue::Number(n) = resolved_value {
        if n.is_finite() && (n.fract() == 0.0) {
          // CSS order is an integer; ignore non-integers.
          let int = n as i64;
          if let Ok(val) = i32::try_from(int) {
            styles.order = val;
          }
        }
      }
    }
    "flex-grow" => {
      if let PropertyValue::Number(n) = resolved_value {
        if n.is_finite() && n >= 0.0 {
          styles.flex_grow = n;
        }
      }
    }
    "flex-shrink" => {
      if let PropertyValue::Number(n) = resolved_value {
        if n.is_finite() && n >= 0.0 {
          styles.flex_shrink = n;
        }
      }
    }
    "flex" => {
      if let Some((grow, shrink, basis)) = parse_flex_shorthand(&resolved_value) {
        styles.flex_grow = grow;
        styles.flex_shrink = shrink;
        styles.flex_basis = basis;
      }
    }
    "flex-basis" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if kw == "auto" {
          styles.flex_basis = FlexBasis::Auto;
        }
      } else if let Some(len) = extract_length(&resolved_value) {
        styles.flex_basis = FlexBasis::Length(len);
      }
    }

    // Grid
    "grid-template-columns" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(line_names) = parse_subgrid_line_names(kw) {
          styles.grid_column_subgrid = true;
          styles.subgrid_column_line_names = line_names.clone();
          styles.grid_template_columns.clear();
          styles.grid_column_line_names = line_names;
          styles.grid_column_names.clear();
          return;
        }
        let (tracks, named_lines, line_names) = parse_grid_tracks_with_names(kw);
        styles.grid_template_columns = tracks;
        styles.grid_column_names = named_lines;
        styles.grid_column_line_names = line_names;
        styles.grid_column_subgrid = false;
        styles.subgrid_column_line_names.clear();
      }
    }
    "grid-template-rows" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(line_names) = parse_subgrid_line_names(kw) {
          styles.grid_row_subgrid = true;
          styles.subgrid_row_line_names = line_names.clone();
          styles.grid_template_rows.clear();
          styles.grid_row_line_names = line_names;
          styles.grid_row_names.clear();
          return;
        }
        let (tracks, named_lines, line_names) = parse_grid_tracks_with_names(kw);
        styles.grid_template_rows = tracks;
        styles.grid_row_names = named_lines;
        styles.grid_row_line_names = line_names;
        styles.grid_row_subgrid = false;
        styles.subgrid_row_line_names.clear();
      }
    }
    "grid-template-areas" => match &resolved_value {
      PropertyValue::Keyword(kw) | PropertyValue::String(kw) => {
        if let Some(areas) = parse_grid_template_areas(kw) {
          let row_count = areas.len();
          let col_count = areas.first().map(|r| r.len()).unwrap_or(0);
          if col_count == 0 {
            return;
          }

          // CSS requires area matrices to align with explicit track counts if specified.
          if (!styles.grid_template_columns.is_empty()
            && styles.grid_template_columns.len() != col_count)
            || (!styles.grid_template_rows.is_empty()
              && styles.grid_template_rows.len() != row_count)
          {
            return;
          }

          styles.grid_template_areas = areas;

          if styles.grid_template_columns.is_empty() {
            styles.grid_template_columns = vec![GridTrack::Auto; col_count];
            styles.grid_column_line_names = vec![Vec::new(); col_count + 1];
          }
          if styles.grid_template_rows.is_empty() {
            styles.grid_template_rows = vec![GridTrack::Auto; row_count];
            styles.grid_row_line_names = vec![Vec::new(); row_count + 1];
          }
          styles.grid_row_subgrid = false;
          styles.grid_column_subgrid = false;
          styles.subgrid_row_line_names.clear();
          styles.subgrid_column_line_names.clear();
          synthesize_area_line_names(styles);
        }
      }
      _ => {}
    },
    "grid-template" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if kw.to_ascii_lowercase().contains("subgrid") {
          let mut parts = kw.split('/');
          if let Some(row_part) = parts.next() {
            if row_part.to_ascii_lowercase().contains("subgrid") {
              if let Some(names) = parse_subgrid_line_names(row_part) {
                styles.grid_row_subgrid = true;
                styles.subgrid_row_line_names = names.clone();
                styles.grid_row_line_names = names;
                styles.grid_template_rows.clear();
                styles.grid_row_names.clear();
              }
            } else if let Some(parsed) = parse_grid_template_shorthand(row_part) {
              if let Some((rows, row_line_names)) = parsed.row_tracks {
                styles.grid_template_rows = rows;
                styles.grid_row_line_names = row_line_names;
                styles.grid_row_subgrid = false;
                styles.subgrid_row_line_names.clear();
              }
              if let Some(areas) = parsed.areas {
                styles.grid_template_areas = areas;
              }
            }
          }

          if let Some(col_part) = parts.next() {
            if col_part.to_ascii_lowercase().contains("subgrid") {
              if let Some(names) = parse_subgrid_line_names(col_part) {
                styles.grid_column_subgrid = true;
                styles.subgrid_column_line_names = names.clone();
                styles.grid_column_line_names = names;
                styles.grid_template_columns.clear();
                styles.grid_column_names.clear();
              }
            } else {
              let ParsedTracks {
                tracks, line_names, ..
              } = parse_track_list(col_part);
              if !tracks.is_empty() {
                styles.grid_template_columns = tracks;
                styles.grid_column_line_names = line_names;
                styles.grid_column_subgrid = false;
                styles.subgrid_column_line_names.clear();
              }
            }
          }

          synthesize_area_line_names(styles);
          return;
        }
        if let Some(parsed) = parse_grid_template_shorthand(kw) {
          if let Some((rows, row_line_names)) = parsed.row_tracks {
            styles.grid_template_rows = rows;
            styles.grid_row_line_names = row_line_names;
            styles.grid_row_subgrid = false;
            styles.subgrid_row_line_names.clear();
          }
          if let Some((cols, col_line_names)) = parsed.column_tracks {
            styles.grid_template_columns = cols;
            styles.grid_column_line_names = col_line_names;
            styles.grid_column_subgrid = false;
            styles.subgrid_column_line_names.clear();
          }
          if let Some(areas) = parsed.areas {
            styles.grid_template_areas = areas;
          }

          synthesize_area_line_names(styles);
        }
      }
    }
    "grid" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(parsed) = parse_grid_shorthand(kw) {
          if let Some(template) = parsed.template {
            styles.grid_template_areas = template.areas.unwrap_or_default();
            if let Some((rows, row_names)) = template.row_tracks {
              styles.grid_template_rows = rows;
              styles.grid_row_line_names = row_names;
              styles.grid_row_subgrid = false;
              styles.subgrid_row_line_names.clear();
            }
            if let Some((cols, col_names)) = template.column_tracks {
              styles.grid_template_columns = cols;
              styles.grid_column_line_names = col_names;
              styles.grid_column_subgrid = false;
              styles.subgrid_column_line_names.clear();
            }
          }
          if let Some(rows) = parsed.auto_rows {
            styles.grid_auto_rows = rows;
          }
          if let Some(cols) = parsed.auto_columns {
            styles.grid_auto_columns = cols;
          }
          if let Some(flow) = parsed.auto_flow {
            styles.grid_auto_flow = flow;
          }

          synthesize_area_line_names(styles);
        }
      }
    }
    "grid-auto-rows" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let ParsedTracks { tracks, .. } = parse_track_list(kw);
        if !tracks.is_empty() {
          styles.grid_auto_rows = tracks;
        }
      }
    }
    "grid-auto-columns" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let ParsedTracks { tracks, .. } = parse_track_list(kw);
        if !tracks.is_empty() {
          styles.grid_auto_columns = tracks;
        }
      }
    }
    "grid-auto-flow" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let lower = kw.to_ascii_lowercase();
        let dense = lower.contains("dense");
        let primary = if lower.contains("column") {
          "column"
        } else if lower.contains("row") {
          "row"
        } else {
          "row"
        };
        styles.grid_auto_flow = match (primary, dense) {
          ("row", false) => GridAutoFlow::Row,
          ("row", true) => GridAutoFlow::RowDense,
          ("column", false) => GridAutoFlow::Column,
          ("column", true) => GridAutoFlow::ColumnDense,
          _ => GridAutoFlow::Row,
        };
      }
    }
    "grid-gap" | "gap" => {
      if let Some((row, column)) = parse_gap_lengths(&resolved_value) {
        styles.grid_gap = row;
        styles.grid_row_gap = row;
        styles.grid_column_gap = column;
        styles.column_gap = column;
      }
    }
    "grid-row-gap" | "row-gap" => {
      if let Some(len) = parse_single_gap_length(&resolved_value) {
        styles.grid_row_gap = len;
      }
    }
    "grid-column-gap" | "column-gap" => {
      if let Some(len) = parse_single_gap_length(&resolved_value) {
        styles.grid_column_gap = len;
        styles.column_gap = len;
      }
    }
    "column-count" => match &resolved_value {
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => {
        styles.column_count = None;
      }
      PropertyValue::Number(n) if *n >= 1.0 => {
        styles.column_count = Some(n.round().max(1.0) as u32);
      }
      _ => {}
    },
    "column-width" => match &resolved_value {
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => {
        styles.column_width = None;
      }
      _ => {
        if let Some(len) = extract_length(&resolved_value) {
          styles.column_width = Some(len);
        }
      }
    },
    "column-rule-width" => {
      if let Some(len) = extract_length(&resolved_value) {
        styles.column_rule_width = len;
      }
    }
    "column-rule-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.column_rule_style = parse_border_style(kw);
      }
    }
    "column-rule-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        styles.column_rule_color = Some(c);
      }
    }
    "column-rule" => {
      let mut width = None;
      let mut style_val = None;
      let mut color = None;
      match &resolved_value {
        PropertyValue::Multiple(values) => {
          for val in values {
            match val {
              PropertyValue::Keyword(kw) => {
                if let Some(len) = parse_border_width_keyword(kw) {
                  width = Some(len);
                  continue;
                }
                let style_parsed = parse_border_style(kw);
                if style_parsed != BorderStyle::None || kw.eq_ignore_ascii_case("none") {
                  style_val = Some(style_parsed);
                  continue;
                }
                if color.is_none() {
                  if let Some(c) = resolve_color_value(&PropertyValue::Keyword(kw.clone())) {
                    color = Some(c);
                  }
                }
              }
              PropertyValue::Length(l) => width = Some(*l),
              PropertyValue::Color(_) => {
                if let Some(c) = resolve_color_value(val) {
                  color = Some(c);
                }
              }
              _ => {}
            }
          }
        }
        PropertyValue::Keyword(kw) => {
          if let Some(len) = parse_border_width_keyword(kw) {
            width = Some(len);
          }
          let style_parsed = parse_border_style(kw);
          if style_parsed != BorderStyle::None || kw.eq_ignore_ascii_case("none") {
            style_val = Some(style_parsed);
          }
          if color.is_none() {
            if let Some(c) = resolve_color_value(&resolved_value) {
              color = Some(c);
            }
          }
        }
        _ => {}
      }

      if let Some(w) = width {
        styles.column_rule_width = w;
      }
      if let Some(s) = style_val {
        styles.column_rule_style = s;
      }
      if let Some(c) = color {
        styles.column_rule_color = Some(c);
      }
    }
    "columns" => {
      let mut count: Option<u32> = None;
      let mut width: Option<Length> = None;

      let parse_token = |tok: &str, count_ref: &mut Option<u32>, width_ref: &mut Option<Length>| {
        let lower = tok.trim();
        if lower.is_empty() || lower.eq_ignore_ascii_case("auto") {
          return;
        }
        if let Some(len) = parse_length(lower) {
          *width_ref = Some(len);
          return;
        }
        if let Ok(n) = lower.parse::<f32>() {
          if n >= 1.0 {
            *count_ref = Some(n.round() as u32);
          }
        }
      };

      match &resolved_value {
        PropertyValue::Multiple(values) => {
          for v in values {
            match v {
              PropertyValue::Keyword(kw) => parse_token(kw, &mut count, &mut width),
              PropertyValue::Length(l) => width = Some(*l),
              PropertyValue::Number(n) if *n >= 1.0 => count = Some(n.round() as u32),
              _ => {}
            }
          }
        }
        PropertyValue::Keyword(kw) => {
          for tok in kw.split_whitespace() {
            parse_token(tok, &mut count, &mut width);
          }
        }
        PropertyValue::Length(l) => width = Some(*l),
        PropertyValue::Number(n) if *n >= 1.0 => count = Some(n.round() as u32),
        _ => {}
      }

      styles.column_count = count;
      styles.column_width = width;
    }
    "column-fill" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.column_fill = match kw.to_ascii_lowercase().as_str() {
          "auto" => ColumnFill::Auto,
          "balance" => ColumnFill::Balance,
          _ => styles.column_fill,
        };
      }
    }
    "column-span" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.column_span = match kw.to_ascii_lowercase().as_str() {
          "all" => ColumnSpan::All,
          _ => ColumnSpan::None,
        };
      }
    }
    "grid-column" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        // Store raw value for later resolution (after grid-template-columns is set)
        styles.grid_column_raw = Some(kw.clone());
      }
    }
    "grid-row" => {
      match &resolved_value {
        PropertyValue::Keyword(kw) => {
          // Store raw value for later resolution (after grid-template-rows is set)
          styles.grid_row_raw = Some(kw.clone());
        }
        PropertyValue::Number(n) => {
          // Handle numeric values like "2"
          styles.grid_row_raw = Some(n.to_string());
        }
        _ => {}
      }
    }
    "grid-column-start" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        // For explicit start/end, we can parse immediately if numeric
        if let Ok(n) = kw.parse::<i32>() {
          styles.grid_column_start = n;
        } else {
          // Store in grid_column_raw for deferred resolution
          let current_end = styles
            .grid_column_raw
            .as_ref()
            .and_then(|s| s.split_once('/').map(|(_, e)| e.trim()))
            .unwrap_or("auto");
          styles.grid_column_raw = Some(format!("{} / {}", kw, current_end));
        }
      }
    }
    "grid-column-end" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Ok(n) = kw.parse::<i32>() {
          styles.grid_column_end = n;
        } else {
          let current_start = styles
            .grid_column_raw
            .as_ref()
            .and_then(|s| s.split_once('/').map(|(s, _)| s.trim()))
            .unwrap_or("auto");
          styles.grid_column_raw = Some(format!("{} / {}", current_start, kw));
        }
      }
    }
    "grid-row-start" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Ok(n) = kw.parse::<i32>() {
          styles.grid_row_start = n;
        } else {
          let current_end = styles
            .grid_row_raw
            .as_ref()
            .and_then(|s| s.split_once('/').map(|(_, e)| e.trim()))
            .unwrap_or("auto");
          styles.grid_row_raw = Some(format!("{} / {}", kw, current_end));
        }
      }
    }
    "grid-row-end" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Ok(n) = kw.parse::<i32>() {
          styles.grid_row_end = n;
        } else {
          let current_start = styles
            .grid_row_raw
            .as_ref()
            .and_then(|s| s.split_once('/').map(|(s, _)| s.trim()))
            .unwrap_or("auto");
          styles.grid_row_raw = Some(format!("{} / {}", current_start, kw));
        }
      }
    }
    "grid-area" => {
      if let PropertyValue::Keyword(kw) | PropertyValue::String(kw) = &resolved_value {
        let parts: Vec<&str> = kw
          .split('/')
          .map(|s| s.trim())
          .filter(|s| !s.is_empty())
          .collect();
        if parts.is_empty() {
          return;
        }
        let mut row_start = parts.first().copied().unwrap_or("auto").to_string();
        let mut col_start = parts.get(1).copied().unwrap_or("auto").to_string();
        let mut row_end = parts.get(2).copied().unwrap_or("auto").to_string();
        let col_end = if parts.len() == 1 {
          // Single area name: map to area start/end
          row_start = format!("{}-start", parts[0]);
          row_end = format!("{}-end", parts[0]);
          col_start = row_start.clone();
          row_end.clone()
        } else {
          parts.get(3).copied().unwrap_or("auto").to_string()
        };
        styles.grid_row_raw = Some(format!("{} / {}", row_start, row_end));
        styles.grid_column_raw = Some(format!("{} / {}", col_start, col_end));
      }
    }

    // Typography
    "font" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        if let Some((
          font_style,
          font_weight,
          font_variant,
          font_stretch,
          font_size,
          line_height,
          families,
        )) = parse_font_shorthand(raw, parent_font_size, root_font_size, viewport)
        {
          styles.font_variant_ligatures = FontVariantLigatures::default();
          styles.font_variant_caps = FontVariantCaps::Normal;
          styles.font_variant_alternates = FontVariantAlternates::default();
          styles.font_variant_numeric = FontVariantNumeric::default();
          styles.font_variant_east_asian = FontVariantEastAsian::default();
          styles.font_variant_position = FontVariantPosition::Normal;
          styles.font_variant_emoji = FontVariantEmoji::Normal;
          styles.font_size_adjust = FontSizeAdjust::None;
          styles.font_synthesis = FontSynthesis::default();
          styles.font_kerning = FontKerning::Auto;
          styles.font_optical_sizing = FontOpticalSizing::Auto;
          // font-feature-settings, font-variation-settings, and font-language-override are not reset by the font shorthand.
          styles.font_style = font_style;
          styles.font_weight = font_weight;
          styles.font_variant = font_variant;
          if matches!(font_variant, FontVariant::SmallCaps) {
            styles.font_variant_caps = FontVariantCaps::SmallCaps;
          }
          styles.font_stretch = font_stretch;
          styles.font_size = font_size;
          styles.line_height = line_height;
          styles.font_family = families;
        }
      }
    }
    "font-family" => {
      if let PropertyValue::FontFamily(families) = &resolved_value {
        styles.font_family = families.clone();
      }
    }
    "font-size" => match &resolved_value {
      PropertyValue::Keyword(kw) => {
        if let Some(size) = parse_font_size_keyword(kw, parent_font_size) {
          styles.font_size = size;
        }
      }
      PropertyValue::Length(len) => {
        if len.value >= 0.0 {
          if let Some(size) =
            resolve_font_size_length(*len, parent_font_size, root_font_size, viewport)
          {
            styles.font_size = size;
          }
        }
      }
      PropertyValue::Percentage(p) => {
        if *p >= 0.0 {
          styles.font_size = (p / 100.0) * parent_font_size;
        }
      }
      _ => {}
    },
    "font-size-adjust" => match &resolved_value {
      PropertyValue::Keyword(kw) => match kw.as_str() {
        "none" => styles.font_size_adjust = FontSizeAdjust::None,
        "from-font" => styles.font_size_adjust = FontSizeAdjust::FromFont,
        _ => {}
      },
      PropertyValue::Number(n) if *n >= 0.0 => {
        styles.font_size_adjust = FontSizeAdjust::Number(*n);
      }
      _ => {}
    },
    "font-weight" => match &resolved_value {
      PropertyValue::Keyword(kw) => {
        styles.font_weight = match kw.as_str() {
          "normal" => FontWeight::Normal,
          "bold" => FontWeight::Bold,
          "lighter" => FontWeight::Lighter,
          "bolder" => FontWeight::Bolder,
          _ => styles.font_weight,
        };
      }
      PropertyValue::Number(n) => {
        if (1.0..=1000.0).contains(n) {
          styles.font_weight = FontWeight::Number((*n as u16).clamp(1, 1000));
        }
      }
      _ => {}
    },
    "font-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(fs) = parse_font_style_keyword(kw) {
          styles.font_style = fs;
        }
      }
    }
    "font-variant" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let trimmed = kw.trim();
        // CSS Fonts shorthand for font-variant subproperties.
        if trimmed == "normal" {
          styles.font_variant = FontVariant::Normal;
          styles.font_variant_caps = FontVariantCaps::Normal;
          styles.font_variant_alternates = FontVariantAlternates::default();
          styles.font_variant_ligatures = FontVariantLigatures::default();
          styles.font_variant_numeric = FontVariantNumeric::default();
          styles.font_variant_east_asian = FontVariantEastAsian::default();
          styles.font_variant_position = FontVariantPosition::Normal;
        } else {
          let raw_tokens = split_font_variant_tokens(trimmed);
          if raw_tokens.is_empty() {
            return;
          }
          let tokens: Vec<&str> = raw_tokens.iter().map(String::as_str).collect();

          let mut caps_tokens: Vec<&str> = Vec::new();
          let mut ligature_tokens: Vec<&str> = Vec::new();
          let mut numeric_tokens: Vec<&str> = Vec::new();
          let mut east_asian_tokens: Vec<&str> = Vec::new();
          let mut alternate_tokens: Vec<&str> = Vec::new();
          let mut position_tokens: Vec<&str> = Vec::new();
          let mut invalid = false;

          for tok in tokens {
            match tok {
              "small-caps" | "all-small-caps" | "petite-caps" | "all-petite-caps" | "unicase"
              | "titling-caps" => caps_tokens.push(tok),
              // The shorthand grammar omits "normal"/"none" for ligatures; those are invalid here.
              "common-ligatures"
              | "no-common-ligatures"
              | "discretionary-ligatures"
              | "no-discretionary-ligatures"
              | "historical-ligatures"
              | "no-historical-ligatures"
              | "contextual"
              | "no-contextual" => ligature_tokens.push(tok),
              "lining-nums" | "oldstyle-nums" | "proportional-nums" | "tabular-nums"
              | "diagonal-fractions" | "stacked-fractions" | "ordinal" | "slashed-zero" => {
                numeric_tokens.push(tok);
              }
              "jis78" | "jis83" | "jis90" | "jis04" | "simplified" | "traditional"
              | "full-width" | "proportional-width" | "ruby" => east_asian_tokens.push(tok),
              tok
                if tok.starts_with("stylistic(")
                  || tok.starts_with("styleset(")
                  || tok.starts_with("character-variant(")
                  || tok.starts_with("swash(")
                  || tok.starts_with("ornaments(")
                  || tok.starts_with("annotation(")
                  || tok == "historical-forms" =>
              {
                alternate_tokens.push(tok);
              }
              "sub" | "super" => position_tokens.push(tok),
              _ => {
                invalid = true;
                break;
              }
            }
          }

          if invalid {
            return;
          }

          let mut variant = FontVariant::Normal;
          let mut caps = FontVariantCaps::Normal;
          if !caps_tokens.is_empty() {
            if let Some((parsed_caps, parsed_variant)) =
              parse_font_variant_caps_tokens(&caps_tokens, FontVariant::Normal)
            {
              caps = parsed_caps;
              variant = parsed_variant;
            } else {
              return;
            }
          }

          let mut ligatures = FontVariantLigatures::default();
          if !ligature_tokens.is_empty() {
            if let Some(parsed) = parse_font_variant_ligatures_tokens(&ligature_tokens) {
              ligatures = parsed;
            } else {
              return;
            }
          }

          let mut numeric = FontVariantNumeric::default();
          if !numeric_tokens.is_empty() {
            if let Some(parsed) = parse_font_variant_numeric_tokens(&numeric_tokens) {
              numeric = parsed;
            } else {
              return;
            }
          }

          let mut east_asian = FontVariantEastAsian::default();
          if !east_asian_tokens.is_empty() {
            if let Some(parsed) = parse_font_variant_east_asian_tokens(&east_asian_tokens) {
              east_asian = parsed;
            } else {
              return;
            }
          }

          let mut alternates = FontVariantAlternates::default();
          if !alternate_tokens.is_empty() {
            if let Some(parsed) = parse_font_variant_alternates_tokens(&alternate_tokens) {
              alternates = parsed;
            } else {
              return;
            }
          }

          let mut position = FontVariantPosition::Normal;
          if !position_tokens.is_empty() {
            if let Some(parsed) =
              parse_font_variant_position_tokens(&position_tokens, FontVariantPosition::Normal)
            {
              position = parsed;
            } else {
              return;
            }
          }

          styles.font_variant = variant;
          styles.font_variant_caps = caps;
          styles.font_variant_alternates = alternates;
          styles.font_variant_ligatures = ligatures;
          styles.font_variant_numeric = numeric;
          styles.font_variant_east_asian = east_asian;
          styles.font_variant_position = position;
        }
      }
    }
    "font-variant-caps" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let raw_tokens = split_font_variant_tokens(kw);
        let tokens: Vec<&str> = raw_tokens.iter().map(String::as_str).collect();
        if let Some((caps, variant)) = parse_font_variant_caps_tokens(&tokens, styles.font_variant)
        {
          styles.font_variant_caps = caps;
          styles.font_variant = variant;
        }
      }
    }
    "font-variant-alternates" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let raw_tokens = split_font_variant_tokens(kw);
        let tokens: Vec<&str> = raw_tokens.iter().map(String::as_str).collect();
        if tokens.len() == 1 && tokens[0] == "normal" {
          styles.font_variant_alternates = FontVariantAlternates::default();
        } else if let Some(alt) = parse_font_variant_alternates_tokens(&tokens) {
          styles.font_variant_alternates = alt;
        }
      }
    }
    "font-variant-position" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let raw_tokens = split_font_variant_tokens(kw);
        let tokens: Vec<&str> = raw_tokens.iter().map(String::as_str).collect();
        if let Some(position) =
          parse_font_variant_position_tokens(&tokens, styles.font_variant_position)
        {
          styles.font_variant_position = position;
        }
      }
    }
    "font-variant-east-asian" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let tokens: Vec<&str> = kw.split_whitespace().collect();
        if let Some(east) = parse_font_variant_east_asian_tokens(&tokens) {
          styles.font_variant_east_asian = east;
        }
      }
    }
    "font-variant-numeric" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let tokens: Vec<&str> = kw.split_whitespace().collect();
        if let Some(numeric) = parse_font_variant_numeric_tokens(&tokens) {
          styles.font_variant_numeric = numeric;
        }
      }
    }
    "font-variant-ligatures" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let tokens: Vec<&str> = kw.split_whitespace().collect();
        if let Some(ligatures) = parse_font_variant_ligatures_tokens(&tokens) {
          styles.font_variant_ligatures = ligatures;
        }
      }
    }
    "font-feature-settings" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        let trimmed = raw.trim();
        if trimmed.eq_ignore_ascii_case("normal") {
          styles.font_feature_settings.clear();
        } else {
          let mut input = ParserInput::new(trimmed);
          let mut parser = Parser::new(&mut input);
          if let Ok(features) = parser.parse_comma_separated(|p| parse_feature_setting(p)) {
            styles.font_feature_settings = features;
          }
        }
      }
    }
    "font-optical-sizing" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        styles.font_optical_sizing = match raw.to_ascii_lowercase().as_str() {
          "auto" => FontOpticalSizing::Auto,
          "none" => FontOpticalSizing::None,
          _ => styles.font_optical_sizing,
        };
      }
    }
    "font-language-override" => match &resolved_value {
      PropertyValue::Keyword(raw) if raw.eq_ignore_ascii_case("normal") => {
        styles.font_language_override = FontLanguageOverride::Normal;
      }
      PropertyValue::Keyword(raw) | PropertyValue::String(raw) => {
        let tag = raw.trim_matches('"').trim();
        if (1..=4).contains(&tag.len()) && tag.is_ascii() {
          styles.font_language_override = FontLanguageOverride::Override(tag.to_string());
        }
      }
      _ => {}
    },
    "font-variant-emoji" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        styles.font_variant_emoji = match raw.to_ascii_lowercase().as_str() {
          "emoji" => crate::style::types::FontVariantEmoji::Emoji,
          "text" => crate::style::types::FontVariantEmoji::Text,
          "unicode" => crate::style::types::FontVariantEmoji::Unicode,
          "normal" => crate::style::types::FontVariantEmoji::Normal,
          _ => styles.font_variant_emoji,
        };
      }
    }
    "font-variation-settings" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        let trimmed = raw.trim();
        if trimmed.eq_ignore_ascii_case("normal") {
          styles.font_variation_settings.clear();
        } else {
          let mut input = ParserInput::new(trimmed);
          let mut parser = Parser::new(&mut input);
          if let Ok(vars) = parser.parse_comma_separated(|p| parse_variation_setting(p)) {
            styles.font_variation_settings = vars;
          }
        }
      }
    }
    "font-stretch" => match &resolved_value {
      PropertyValue::Keyword(kw) => {
        if let Some(stretch) = parse_font_stretch_keyword(kw) {
          styles.font_stretch = stretch;
        }
      }
      PropertyValue::Percentage(p) => {
        styles.font_stretch = FontStretch::from_percentage(*p);
      }
      _ => {}
    },
    "font-kerning" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.font_kerning = match kw.as_str() {
          "auto" => FontKerning::Auto,
          "normal" => FontKerning::Normal,
          "none" => FontKerning::None,
          _ => styles.font_kerning,
        };
      }
    }
    "font-synthesis" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        let tokens: Vec<&str> = raw.split_whitespace().collect();
        if tokens.len() == 1 && tokens[0] == "none" {
          styles.font_synthesis = FontSynthesis {
            weight: false,
            style: false,
            small_caps: false,
            position: false,
          };
        } else if !tokens.is_empty() {
          let mut synth = FontSynthesis {
            weight: false,
            style: false,
            small_caps: false,
            position: false,
          };
          for tok in tokens {
            match tok {
              "weight" => synth.weight = true,
              "style" => synth.style = true,
              "small-caps" => synth.small_caps = true,
              "position" => synth.position = true,
              _ => {}
            }
          }
          styles.font_synthesis = synth;
        }
      }
    }
    "font-synthesis-weight" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        styles.font_synthesis.weight = !raw.eq_ignore_ascii_case("none");
      }
    }
    "font-synthesis-style" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        styles.font_synthesis.style = !raw.eq_ignore_ascii_case("none");
      }
    }
    "font-synthesis-small-caps" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        styles.font_synthesis.small_caps = !raw.eq_ignore_ascii_case("none");
      }
    }
    "font-synthesis-position" => {
      if let PropertyValue::Keyword(raw) = &resolved_value {
        styles.font_synthesis.position = !raw.eq_ignore_ascii_case("none");
      }
    }
    "line-height" => match &resolved_value {
      PropertyValue::Keyword(kw) if kw == "normal" => {
        styles.line_height = LineHeight::Normal;
      }
      PropertyValue::Number(n) => {
        if *n >= 0.0 {
          styles.line_height = LineHeight::Number(*n);
        }
      }
      PropertyValue::Length(len) => {
        if len.unit == LengthUnit::Percent {
          if len.value >= 0.0 {
            styles.line_height = LineHeight::Percentage(len.value);
          }
        } else if len.value >= 0.0 {
          styles.line_height = LineHeight::Length(*len);
        }
      }
      PropertyValue::Percentage(pct) => {
        if *pct >= 0.0 {
          styles.line_height = LineHeight::Percentage(*pct);
        }
      }
      _ => {}
    },
    "table-layout" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.table_layout = match kw.as_str() {
          "auto" => TableLayout::Auto,
          "fixed" => TableLayout::Fixed,
          _ => styles.table_layout,
        };
      }
    }
    "empty-cells" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.empty_cells = match kw.as_str() {
          "show" => EmptyCells::Show,
          "hide" => EmptyCells::Hide,
          _ => styles.empty_cells,
        };
      }
    }
    "caption-side" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.caption_side = match kw.as_str() {
          "top" => CaptionSide::Top,
          "bottom" => CaptionSide::Bottom,
          _ => styles.caption_side,
        };
      }
    }
    "vertical-align" => match &resolved_value {
      PropertyValue::Keyword(kw) => {
        styles.vertical_align = match kw.as_str() {
          "baseline" => VerticalAlign::Baseline,
          "sub" => VerticalAlign::Sub,
          "super" => VerticalAlign::Super,
          "text-top" => VerticalAlign::TextTop,
          "text-bottom" => VerticalAlign::TextBottom,
          "middle" => VerticalAlign::Middle,
          "top" => VerticalAlign::Top,
          "bottom" => VerticalAlign::Bottom,
          _ => styles.vertical_align,
        };
        styles.vertical_align_specified = true;
      }
      PropertyValue::Length(len) => {
        styles.vertical_align = VerticalAlign::Length(*len);
        styles.vertical_align_specified = true;
      }
      PropertyValue::Percentage(pct) => {
        styles.vertical_align = VerticalAlign::Percentage(*pct);
        styles.vertical_align_specified = true;
      }
      _ => {}
    },
    "text-align" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let parsed = match kw.as_str() {
          "start" => Some(TextAlign::Start),
          "end" => Some(TextAlign::End),
          "left" => Some(TextAlign::Left),
          "right" => Some(TextAlign::Right),
          "center" => Some(TextAlign::Center),
          "justify" => Some(TextAlign::Justify),
          "justify-all" => Some(TextAlign::JustifyAll),
          "match-parent" => Some(TextAlign::MatchParent),
          _ => None,
        };
        if let Some(value) = parsed {
          styles.text_align = value;
        }
      }
    }
    "text-align-all" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let parsed = match kw.as_str() {
          "start" => Some(TextAlign::Start),
          "end" => Some(TextAlign::End),
          "left" => Some(TextAlign::Left),
          "right" => Some(TextAlign::Right),
          "center" => Some(TextAlign::Center),
          "justify" => Some(TextAlign::Justify),
          "justify-all" => Some(TextAlign::JustifyAll),
          "match-parent" => Some(TextAlign::MatchParent),
          _ => None,
        };
        if let Some(value) = parsed {
          styles.text_align = value;
        }
      }
    }
    "text-align-last" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.text_align_last = match kw.as_str() {
          "auto" => TextAlignLast::Auto,
          "start" => TextAlignLast::Start,
          "end" => TextAlignLast::End,
          "left" => TextAlignLast::Left,
          "right" => TextAlignLast::Right,
          "center" => TextAlignLast::Center,
          "justify" => TextAlignLast::Justify,
          "match-parent" => TextAlignLast::MatchParent,
          _ => styles.text_align_last,
        };
      }
    }
    "text-rendering" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.text_rendering = match kw.to_ascii_lowercase().as_str() {
          "auto" => TextRendering::Auto,
          "optimizespeed" => TextRendering::OptimizeSpeed,
          "optimizelegibility" => TextRendering::OptimizeLegibility,
          "geometricprecision" => TextRendering::GeometricPrecision,
          _ => styles.text_rendering,
        };
      }
    }
    "text-justify" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.text_justify = match kw.as_str() {
          "auto" => TextJustify::Auto,
          "none" => TextJustify::None,
          "inter-word" => TextJustify::InterWord,
          "inter-character" => TextJustify::InterCharacter,
          "distribute" => TextJustify::Distribute,
          _ => styles.text_justify,
        };
      }
    }
    "text-size-adjust" => {
      styles.text_size_adjust = match &resolved_value {
        PropertyValue::Keyword(kw) => match kw.to_ascii_lowercase().as_str() {
          "auto" => TextSizeAdjust::Auto,
          "none" => TextSizeAdjust::None,
          _ => styles.text_size_adjust,
        },
        PropertyValue::Percentage(p) if *p >= 0.0 => TextSizeAdjust::Percentage(*p),
        _ => styles.text_size_adjust,
      };
    }
    "text-wrap" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.text_wrap = match kw.as_str() {
          "wrap" | "auto" | "normal" => TextWrap::Auto,
          "nowrap" => TextWrap::NoWrap,
          "balance" => TextWrap::Balance,
          "pretty" => TextWrap::Pretty,
          "stable" => TextWrap::Stable,
          _ => styles.text_wrap,
        };
      }
    }
    "text-orientation" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let kw = kw.to_ascii_lowercase();
        styles.text_orientation = match kw.as_str() {
          "mixed" => TextOrientation::Mixed,
          "upright" => TextOrientation::Upright,
          "sideways" => TextOrientation::Sideways,
          "sideways-left" => TextOrientation::SidewaysLeft,
          "sideways-right" => TextOrientation::Sideways,
          _ => styles.text_orientation,
        };
      }
    }
    "text-combine-upright" => {
      if let Some(value) = parse_text_combine_upright(&resolved_value) {
        styles.text_combine_upright = value;
      }
    }
    "text-indent" => {
      let mut length = styles.text_indent.length;
      let mut hanging = false;
      let mut each_line = false;

      let mut apply_component = |value: &PropertyValue| match value {
        PropertyValue::Length(len) => length = *len,
        PropertyValue::Percentage(pct) => length = Length::percent(*pct),
        PropertyValue::Keyword(kw) => match kw.as_str() {
          "hanging" => hanging = true,
          "each-line" => each_line = true,
          _ => {}
        },
        _ => {}
      };

      match &resolved_value {
        PropertyValue::Multiple(values) => {
          for v in values {
            apply_component(v);
          }
        }
        other => apply_component(other),
      }

      styles.text_indent = TextIndent {
        length,
        hanging,
        each_line,
      };
    }
    "direction" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.direction = match kw.as_str() {
          "ltr" => Direction::Ltr,
          "rtl" => Direction::Rtl,
          _ => styles.direction,
        };
      }
    }
    "text-decoration-line" => {
      if let Some(lines) = parse_text_decoration_line(&resolved_value) {
        styles.text_decoration_line_specified = true;
        styles.text_decoration.lines = lines;
      }
    }
    "text-decoration-style" => {
      if let Some(style) = parse_text_decoration_style(&resolved_value) {
        styles.text_decoration.style = style;
      }
    }
    "text-decoration-color" => {
      if let Some(color) = parse_text_decoration_color(&resolved_value, styles.color) {
        styles.text_decoration.color = color;
      }
    }
    "text-decoration-thickness" => {
      if let Some(thick) =
        parse_text_decoration_thickness(&resolved_value, parent_font_size, root_font_size)
      {
        styles.text_decoration.thickness = thick;
      }
    }
    "text-decoration-skip-ink" => {
      if let Some(skip) = parse_text_decoration_skip_ink(&resolved_value) {
        styles.text_decoration_skip_ink = skip;
      }
    }
    "text-underline-offset" => {
      if let Some(offset) = parse_text_underline_offset(&resolved_value) {
        styles.text_underline_offset = offset;
      }
    }
    "text-underline-position" => {
      if let Some(pos) = parse_text_underline_position(&resolved_value) {
        styles.text_underline_position = pos;
      }
    }
    "text-emphasis-style" => {
      if let Some(emph) = parse_text_emphasis_style(&resolved_value) {
        styles.text_emphasis_style = emph;
      }
    }
    "text-emphasis-color" => {
      if let Some(color) = parse_text_emphasis_color(&resolved_value, styles.color) {
        styles.text_emphasis_color = color;
      }
    }
    "text-emphasis-position" => {
      if let Some(pos) = parse_text_emphasis_position(&resolved_value) {
        styles.text_emphasis_position = pos;
      }
    }
    "text-emphasis" => {
      if let Some((style_val, color_val)) =
        parse_text_emphasis_shorthand(&resolved_value, styles.color)
      {
        if let Some(emph_style) = style_val {
          styles.text_emphasis_style = emph_style;
        } else {
          styles.text_emphasis_style = TextEmphasisStyle::None;
        }
        if let Some(color) = color_val {
          styles.text_emphasis_color = color;
        } else {
          styles.text_emphasis_color = None;
        }
      }
    }
    "ruby-position" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.ruby_position = match kw.as_str() {
          "over" => RubyPosition::Over,
          "under" => RubyPosition::Under,
          "inter-character" => RubyPosition::InterCharacter,
          "alternate" => RubyPosition::Alternate,
          _ => styles.ruby_position,
        };
      }
    }
    "ruby-align" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.ruby_align = match kw.as_str() {
          "auto" => RubyAlign::Auto,
          "start" => RubyAlign::Start,
          "center" => RubyAlign::Center,
          "space-between" => RubyAlign::SpaceBetween,
          "space-around" => RubyAlign::SpaceAround,
          _ => styles.ruby_align,
        };
      }
    }
    "ruby-merge" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.ruby_merge = match kw.as_str() {
          "separate" => RubyMerge::Separate,
          "collapse" => RubyMerge::Collapse,
          "auto" => RubyMerge::Auto,
          _ => styles.ruby_merge,
        };
      }
    }
    "text-decoration" => {
      let tokens: Vec<PropertyValue> = match resolved_value {
        PropertyValue::Multiple(ref values) => values.clone(),
        _ => vec![resolved_value.clone()],
      };

      if tokens.is_empty() {
        return;
      }

      // Reset to initial values per shorthand rules.
      let mut decoration = TextDecoration::default();
      styles.text_decoration_line_specified = true;
      for token in tokens {
        if let Some(lines) = parse_text_decoration_line(&token) {
          decoration.lines = lines;
          continue;
        }
        if let Some(style) = parse_text_decoration_style(&token) {
          decoration.style = style;
          continue;
        }
        if let Some(color) = parse_text_decoration_color(&token, styles.color) {
          decoration.color = color;
          continue;
        }
        if let Some(thick) =
          parse_text_decoration_thickness(&token, parent_font_size, root_font_size)
        {
          decoration.thickness = thick;
        }
      }

      styles.text_decoration = decoration;
    }
    "list-style-type" => {
      if let Some(t) = parse_list_style_type(&resolved_value) {
        styles.list_style_type = t;
      }
    }
    "list-style-position" => {
      if let Some(p) = parse_list_style_position(&resolved_value) {
        styles.list_style_position = p;
      }
    }
    "list-style-image" => {
      if let Some(img) = parse_list_style_image(&resolved_value) {
        styles.list_style_image = img;
      }
    }
    "list-style" => {
      let tokens: Vec<PropertyValue> = match resolved_value {
        PropertyValue::Multiple(ref values) => values.clone(),
        _ => vec![resolved_value.clone()],
      };

      if tokens.is_empty() {
        return;
      }

      // Reset to initial values
      let mut list_type = ListStyleType::Disc;
      let mut list_pos = ListStylePosition::Outside;
      let mut list_image = ListStyleImage::None;

      for token in tokens {
        if matches!(&token, PropertyValue::Keyword(kw) if kw == "none") {
          list_type = ListStyleType::None;
          list_image = ListStyleImage::None;
          continue;
        }

        if let Some(p) = parse_list_style_position(&token) {
          list_pos = p;
          continue;
        }
        if let Some(t) = parse_list_style_type(&token) {
          list_type = t;
          continue;
        }
        if let Some(img) = parse_list_style_image(&token) {
          list_image = img;
        }
      }

      styles.list_style_type = list_type;
      styles.list_style_position = list_pos;
      styles.list_style_image = list_image;
    }
    "counter-reset" => {
      if let Some(parsed) = parse_counter_property(&resolved_value, CounterPropertyKind::Reset) {
        styles.counters.counter_reset = Some(parsed);
      }
    }
    "counter-increment" => {
      if let Some(parsed) = parse_counter_property(&resolved_value, CounterPropertyKind::Increment)
      {
        styles.counters.counter_increment = Some(parsed);
      }
    }
    "counter-set" => {
      if let Some(parsed) = parse_counter_property(&resolved_value, CounterPropertyKind::Set) {
        styles.counters.counter_set = Some(parsed);
      }
    }
    "text-transform" => {
      if let Some(parsed) = parse_text_transform(&resolved_value) {
        styles.text_transform = parsed;
      }
    }
    "letter-spacing" => {
      let font_size = if styles.font_size.is_finite() && styles.font_size > 0.0 {
        styles.font_size
      } else {
        parent_font_size
      };
      if let Some(len) = parse_spacing_value(&resolved_value, font_size, root_font_size, false) {
        styles.letter_spacing = len;
      }
    }
    "word-spacing" => {
      let font_size = if styles.font_size.is_finite() && styles.font_size > 0.0 {
        styles.font_size
      } else {
        parent_font_size
      };
      if let Some(len) = parse_spacing_value(&resolved_value, font_size, root_font_size, true) {
        styles.word_spacing = len;
      }
    }
    "white-space" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.white_space = match kw.as_str() {
          "normal" => WhiteSpace::Normal,
          "nowrap" => WhiteSpace::Nowrap,
          "pre" => WhiteSpace::Pre,
          "pre-wrap" => WhiteSpace::PreWrap,
          "pre-line" => WhiteSpace::PreLine,
          "break-spaces" => WhiteSpace::BreakSpaces,
          _ => styles.white_space,
        };
      }
    }
    "line-break" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.line_break = match kw.as_str() {
          "auto" => LineBreak::Auto,
          "loose" => LineBreak::Loose,
          "normal" => LineBreak::Normal,
          "strict" => LineBreak::Strict,
          "anywhere" => LineBreak::Anywhere,
          _ => styles.line_break,
        };
      }
    }
    "break-before" | "break-after" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let value = match kw.as_str() {
          "auto" => BreakBetween::Auto,
          "avoid" => BreakBetween::Avoid,
          "always" => BreakBetween::Always,
          "column" => BreakBetween::Column,
          "page" => BreakBetween::Page,
          _ => BreakBetween::Auto,
        };
        if decl.property == "break-before" {
          styles.break_before = value;
        } else {
          styles.break_after = value;
        }
      }
    }
    "break-inside" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.break_inside = match kw.as_str() {
          "auto" => BreakInside::Auto,
          "avoid" => BreakInside::Avoid,
          _ => styles.break_inside,
        };
      }
    }
    "page" => match &resolved_value {
      PropertyValue::Keyword(kw) => {
        if kw.eq_ignore_ascii_case("auto") {
          styles.page = None;
        } else {
          styles.page = Some(kw.clone());
        }
      }
      PropertyValue::String(s) => {
        styles.page = Some(s.clone());
      }
      _ => {}
    },
    "widows" => {
      if let PropertyValue::Number(n) = &resolved_value {
        styles.widows = n.max(1.0).floor() as usize;
      }
    }
    "orphans" => {
      if let PropertyValue::Number(n) = &resolved_value {
        styles.orphans = n.max(1.0).floor() as usize;
      }
    }
    "tab-size" => match &resolved_value {
      PropertyValue::Number(n) => {
        styles.tab_size = TabSize::Number(n.max(0.0));
      }
      PropertyValue::Length(len) if !len.value.is_nan() => {
        styles.tab_size = TabSize::Length(*len);
      }
      _ => {}
    },
    "hyphens" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.hyphens = match kw.as_str() {
          "none" => HyphensMode::None,
          "manual" => HyphensMode::Manual,
          "auto" => HyphensMode::Auto,
          _ => styles.hyphens,
        };
      }
    }
    "word-break" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.word_break = match kw.as_str() {
          "normal" => WordBreak::Normal,
          "break-all" => WordBreak::BreakAll,
          "keep-all" => WordBreak::KeepAll,
          "anywhere" => WordBreak::Anywhere,
          "break-word" => WordBreak::BreakWord,
          _ => styles.word_break,
        };
      }
    }
    "overflow-wrap" | "word-wrap" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.overflow_wrap = match kw.as_str() {
          "normal" => OverflowWrap::Normal,
          "break-word" => OverflowWrap::BreakWord,
          "anywhere" => OverflowWrap::Anywhere,
          _ => styles.overflow_wrap,
        };
      }
    }
    "overflow-anchor" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.overflow_anchor = match kw.as_str() {
          "auto" => OverflowAnchor::Auto,
          "none" => OverflowAnchor::None,
          _ => styles.overflow_anchor,
        };
      }
    }
    "forced-color-adjust" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.forced_color_adjust = match kw.to_ascii_lowercase().as_str() {
          "auto" => ForcedColorAdjust::Auto,
          "none" => ForcedColorAdjust::None,
          "preserve-parent-color" => ForcedColorAdjust::PreserveParentColor,
          _ => styles.forced_color_adjust,
        };
      }
    }
    "appearance" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if kw.eq_ignore_ascii_case("auto") {
          styles.appearance = Appearance::Auto;
        } else if kw.eq_ignore_ascii_case("none") {
          styles.appearance = Appearance::None;
        } else {
          styles.appearance = Appearance::Keyword(kw.to_ascii_lowercase());
        }
      }
    }
    "text-overflow" => {
      let parse_side = |value: &PropertyValue| -> Option<TextOverflowSide> {
        match value {
          PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("clip") => {
            Some(TextOverflowSide::Clip)
          }
          PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("ellipsis") => {
            Some(TextOverflowSide::Ellipsis)
          }
          PropertyValue::String(s) => Some(TextOverflowSide::String(s.clone())),
          _ => None,
        }
      };

      let mut sides: Vec<TextOverflowSide> = match &resolved_value {
        PropertyValue::Multiple(values) => values.iter().filter_map(parse_side).collect(),
        other => parse_side(other).into_iter().collect(),
      };

      if sides.len() == 1 {
        sides.push(sides[0].clone());
      }

      if sides.len() == 2 {
        styles.text_overflow = TextOverflow {
          inline_start: sides[0].clone(),
          inline_end: sides[1].clone(),
        };
      }
    }
    "unicode-bidi" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.unicode_bidi = match kw.as_str() {
          "normal" => UnicodeBidi::Normal,
          "embed" => UnicodeBidi::Embed,
          "bidi-override" => UnicodeBidi::BidiOverride,
          "isolate" => UnicodeBidi::Isolate,
          "isolate-override" => UnicodeBidi::IsolateOverride,
          "plaintext" => UnicodeBidi::Plaintext,
          _ => styles.unicode_bidi,
        };
      }
    }
    "writing-mode" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        let kw = kw.to_ascii_lowercase();
        styles.writing_mode = match kw.as_str() {
          "horizontal-tb" => WritingMode::HorizontalTb,
          "vertical-rl" => WritingMode::VerticalRl,
          "vertical-lr" => WritingMode::VerticalLr,
          "sideways-rl" => WritingMode::SidewaysRl,
          "sideways-lr" => WritingMode::SidewaysLr,
          _ => styles.writing_mode,
        };
      }
    }
    "cursor" => {
      if let Some((images, keyword)) = parse_cursor(&resolved_value) {
        styles.cursor_images = images;
        styles.cursor = keyword;
      }
    }
    "caret-color" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if kw.eq_ignore_ascii_case("auto") {
          styles.caret_color = CaretColor::Auto;
        } else if let Some(c) = resolve_color_value(&PropertyValue::Keyword(kw.clone())) {
          styles.caret_color = CaretColor::Color(c);
        } else if let Ok(parsed) = Color::parse(kw) {
          styles.caret_color = CaretColor::Color(parsed.to_rgba(styles.color));
        }
      } else if let Some(c) = resolve_color_value(&resolved_value) {
        styles.caret_color = CaretColor::Color(c);
      }
    }
    "accent-color" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if kw.eq_ignore_ascii_case("auto") {
          styles.accent_color = AccentColor::Auto;
        } else if let Some(c) = resolve_color_value(&PropertyValue::Keyword(kw.clone())) {
          styles.accent_color = AccentColor::Color(c);
        } else if let Ok(parsed) = Color::parse(kw) {
          styles.accent_color = AccentColor::Color(parsed.to_rgba(styles.color));
        }
      } else if let Some(c) = resolve_color_value(&resolved_value) {
        styles.accent_color = AccentColor::Color(c);
      }
    }
    "scroll-behavior" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.scroll_behavior = match kw.as_str() {
          "auto" => ScrollBehavior::Auto,
          "smooth" => ScrollBehavior::Smooth,
          _ => styles.scroll_behavior,
        }
      }
    }
    "overscroll-behavior" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(val) = parse_overscroll_keyword(kw) {
          styles.overscroll_behavior_x = val;
          styles.overscroll_behavior_y = val;
        }
      }
    }
    "overscroll-behavior-x" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(val) = parse_overscroll_keyword(kw) {
          styles.overscroll_behavior_x = val;
        }
      }
    }
    "overscroll-behavior-y" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(val) = parse_overscroll_keyword(kw) {
          styles.overscroll_behavior_y = val;
        }
      }
    }
    "scroll-padding" => {
      if let Some(values) = extract_scroll_padding_values(&resolved_value) {
        let mut top = styles.scroll_padding_top;
        let mut right = styles.scroll_padding_right;
        let mut bottom = styles.scroll_padding_bottom;
        let mut left = styles.scroll_padding_left;
        apply_box_values(&mut top, &mut right, &mut bottom, &mut left, values);
        styles.scroll_padding_top = top;
        styles.scroll_padding_right = right;
        styles.scroll_padding_bottom = bottom;
        styles.scroll_padding_left = left;
      }
    }
    "scroll-padding-top" => {
      if let Some(len) = extract_scroll_padding_length(&resolved_value) {
        styles.scroll_padding_top = len;
      }
    }
    "scroll-padding-right" => {
      if let Some(len) = extract_scroll_padding_length(&resolved_value) {
        styles.scroll_padding_right = len;
      }
    }
    "scroll-padding-bottom" => {
      if let Some(len) = extract_scroll_padding_length(&resolved_value) {
        styles.scroll_padding_bottom = len;
      }
    }
    "scroll-padding-left" => {
      if let Some(len) = extract_scroll_padding_length(&resolved_value) {
        styles.scroll_padding_left = len;
      }
    }
    "scroll-margin" => {
      if let Some(values) = extract_box_values(&resolved_value) {
        let mut top = styles.scroll_margin_top;
        let mut right = styles.scroll_margin_right;
        let mut bottom = styles.scroll_margin_bottom;
        let mut left = styles.scroll_margin_left;
        apply_box_values(&mut top, &mut right, &mut bottom, &mut left, values);
        styles.scroll_margin_top = top;
        styles.scroll_margin_right = right;
        styles.scroll_margin_bottom = bottom;
        styles.scroll_margin_left = left;
      }
    }
    "scroll-margin-top" => {
      if let Some(len) = extract_length(&resolved_value) {
        styles.scroll_margin_top = len;
      }
    }
    "scroll-margin-right" => {
      if let Some(len) = extract_length(&resolved_value) {
        styles.scroll_margin_right = len;
      }
    }
    "scroll-margin-bottom" => {
      if let Some(len) = extract_length(&resolved_value) {
        styles.scroll_margin_bottom = len;
      }
    }
    "scroll-margin-left" => {
      if let Some(len) = extract_length(&resolved_value) {
        styles.scroll_margin_left = len;
      }
    }
    "scroll-snap-type" => {
      let mut parts = Vec::new();
      match &resolved_value {
        PropertyValue::Keyword(kw) => parts.extend(kw.split_whitespace().map(|s| s.to_string())),
        PropertyValue::Multiple(tokens) => {
          for token in tokens {
            if let PropertyValue::Keyword(k) = token {
              if k == "," {
                continue;
              }
              parts.push(k.to_ascii_lowercase());
            }
          }
        }
        _ => {}
      }
      if let Some(val) = parse_scroll_snap_type_keywords(&parts) {
        styles.scroll_snap_type = val;
      }
    }
    "scroll-snap-align" => {
      let mut parts = Vec::new();
      match &resolved_value {
        PropertyValue::Keyword(kw) => parts.extend(kw.split_whitespace().map(|s| s.to_string())),
        PropertyValue::Multiple(tokens) => {
          for token in tokens {
            if let PropertyValue::Keyword(k) = token {
              if k == "," {
                continue;
              }
              parts.push(k.to_ascii_lowercase());
            }
          }
        }
        _ => {}
      }
      if let Some(val) = parse_scroll_snap_align_keywords(&parts) {
        styles.scroll_snap_align = val;
      }
    }
    "scroll-snap-stop" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.scroll_snap_stop = match kw.to_ascii_lowercase().as_str() {
          "normal" => ScrollSnapStop::Normal,
          "always" => ScrollSnapStop::Always,
          _ => styles.scroll_snap_stop,
        };
      }
    }
    "scroll-timeline" => {
      styles.scroll_timelines = parse_scroll_timeline_list(&decl.raw_value);
    }
    "view-timeline" => {
      styles.view_timelines = parse_view_timeline_list(&decl.raw_value);
    }
    "animation-timeline" => {
      styles.animation_timelines = parse_animation_timeline_list(&decl.raw_value);
    }
    "animation-range" => {
      styles.animation_ranges = parse_animation_range_list(&decl.raw_value);
    }
    "animation-name" => {
      styles.animation_names = parse_animation_names(&decl.raw_value);
    }
    "scrollbar-gutter" => {
      let mut stable = false;
      let mut both_edges = false;
      let mut seen = false;
      let tokens: Vec<String> = match &resolved_value {
        PropertyValue::Keyword(kw) => kw
          .split_whitespace()
          .map(|s| s.to_ascii_lowercase())
          .collect(),
        PropertyValue::Multiple(values) => values
          .iter()
          .filter_map(|v| match v {
            PropertyValue::Keyword(k) if k != "," => Some(k.to_ascii_lowercase()),
            _ => None,
          })
          .collect(),
        _ => Vec::new(),
      };
      for token in tokens {
        seen = true;
        match token.as_str() {
          "auto" => {}
          "stable" => stable = true,
          "both-edges" => both_edges = true,
          _ => {
            seen = false;
            break;
          }
        }
      }
      if seen {
        styles.scrollbar_gutter = ScrollbarGutter { stable, both_edges };
      }
    }
    "pointer-events" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.pointer_events = match kw.to_ascii_lowercase().as_str() {
          "auto" => PointerEvents::Auto,
          "none" => PointerEvents::None,
          "visiblepainted" => PointerEvents::VisiblePainted,
          "visiblefill" => PointerEvents::VisibleFill,
          "visiblestroke" => PointerEvents::VisibleStroke,
          "visible" => PointerEvents::Visible,
          "painted" => PointerEvents::Painted,
          "fill" => PointerEvents::Fill,
          "stroke" => PointerEvents::Stroke,
          "all" => PointerEvents::All,
          _ => styles.pointer_events,
        }
      }
    }
    "user-select" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.user_select = match kw.to_ascii_lowercase().as_str() {
          "auto" => UserSelect::Auto,
          "text" => UserSelect::Text,
          "none" => UserSelect::None,
          "all" => UserSelect::All,
          "contain" => UserSelect::Contain,
          _ => styles.user_select,
        };
      }
    }
    "touch-action" => match &resolved_value {
      PropertyValue::Keyword(kw) => {
        if let Some(val) = parse_touch_action_keywords(&[kw.to_ascii_lowercase()]) {
          styles.touch_action = val;
        }
      }
      PropertyValue::Multiple(tokens) => {
        let mut parts = Vec::new();
        for token in tokens {
          if let PropertyValue::Keyword(k) = token {
            if k == "," {
              continue;
            }
            parts.push(k.to_ascii_lowercase());
          }
        }
        if let Some(val) = parse_touch_action_keywords(&parts) {
          styles.touch_action = val;
        }
      }
      _ => {}
    },
    "scrollbar-width" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.scrollbar_width = match kw.to_ascii_lowercase().as_str() {
          "auto" => ScrollbarWidth::Auto,
          "thin" => ScrollbarWidth::Thin,
          "none" => ScrollbarWidth::None,
          _ => styles.scrollbar_width,
        }
      }
    }
    "scrollbar-color" => {
      if let Some((thumb, track)) = extract_color_pair_with(&resolved_value, &resolve_color_value) {
        styles.scrollbar_color = ScrollbarColor::Colors { thumb, track };
      } else if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.scrollbar_color = match kw.to_ascii_lowercase().as_str() {
          "auto" => ScrollbarColor::Auto,
          "dark" => ScrollbarColor::Dark,
          "light" => ScrollbarColor::Light,
          _ => styles.scrollbar_color,
        };
      }
    }
    "resize" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.resize = match kw.to_ascii_lowercase().as_str() {
          "none" => Resize::None,
          "both" => Resize::Both,
          "horizontal" => Resize::Horizontal,
          "vertical" => Resize::Vertical,
          "block" => Resize::Block,
          "inline" => Resize::Inline,
          _ => styles.resize,
        };
      }
    }

    // Color
    "color-scheme" => {
      if let Some(pref) = parse_color_scheme(&resolved_value) {
        styles.color_scheme = pref;
      }
    }
    "color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        styles.color = c;
      }
    }
    "background-color" => {
      if let Some(c) = resolve_color_value(&resolved_value) {
        styles.background_color = c;
      }
    }

    // Background
    "background-image" => {
      if let Some(images) = parse_background_image_list(&resolved_value) {
        styles.background_images = images;
        styles.rebuild_background_layers();
      }
    }
    "background-size" => {
      if let Some(sizes) = parse_layer_list(&resolved_value, parse_background_size) {
        styles.background_sizes = sizes;
        styles.rebuild_background_layers();
      }
    }
    "background-size-inline" => {
      if let Some(values) = parse_layer_list(&resolved_value, parse_background_size_component) {
        styles.ensure_background_lists();
        let horizontal_inline = inline_axis_is_horizontal(styles.writing_mode);
        let default = BackgroundLayer::default().size;
        let layer_count = values.len().max(styles.background_sizes.len()).max(1);
        let mut sizes = Vec::with_capacity(layer_count);
        for idx in 0..layer_count {
          let source_idx = styles.background_sizes.len().saturating_sub(1).min(idx);
          let source = styles
            .background_sizes
            .get(source_idx)
            .copied()
            .unwrap_or_else(|| default.clone());
          let inline_value = values
            .get(idx)
            .copied()
            .unwrap_or_else(|| values.last().copied().unwrap());
          let explicit = match source {
            BackgroundSize::Explicit(mut x, mut y) => {
              if horizontal_inline {
                x = inline_value;
              } else {
                y = inline_value;
              }
              BackgroundSize::Explicit(x, y)
            }
            BackgroundSize::Keyword(_) => {
              if horizontal_inline {
                BackgroundSize::Explicit(inline_value, BackgroundSizeComponent::Auto)
              } else {
                BackgroundSize::Explicit(BackgroundSizeComponent::Auto, inline_value)
              }
            }
          };
          sizes.push(explicit);
        }
        styles.background_sizes = sizes;
        styles.rebuild_background_layers();
      }
    }
    "background-size-block" => {
      if let Some(values) = parse_layer_list(&resolved_value, parse_background_size_component) {
        styles.ensure_background_lists();
        let horizontal_block = block_axis_is_horizontal(styles.writing_mode);
        let default = BackgroundLayer::default().size;
        let layer_count = values.len().max(styles.background_sizes.len()).max(1);
        let mut sizes = Vec::with_capacity(layer_count);
        for idx in 0..layer_count {
          let source_idx = styles.background_sizes.len().saturating_sub(1).min(idx);
          let source = styles
            .background_sizes
            .get(source_idx)
            .copied()
            .unwrap_or(default);
          let block_value = values
            .get(idx)
            .copied()
            .unwrap_or_else(|| values.last().copied().unwrap());
          let explicit = match source {
            BackgroundSize::Explicit(mut x, mut y) => {
              if horizontal_block {
                x = block_value;
              } else {
                y = block_value;
              }
              BackgroundSize::Explicit(x, y)
            }
            BackgroundSize::Keyword(_) => {
              if horizontal_block {
                BackgroundSize::Explicit(block_value, BackgroundSizeComponent::Auto)
              } else {
                BackgroundSize::Explicit(BackgroundSizeComponent::Auto, block_value)
              }
            }
          };
          sizes.push(explicit);
        }
        styles.background_sizes = sizes;
        styles.rebuild_background_layers();
      }
    }
    "background-repeat" => {
      if let Some(repeats) = parse_layer_list(&resolved_value, parse_background_repeat) {
        styles.background_repeats = repeats;
        styles.rebuild_background_layers();
      }
    }
    "background-position" => {
      if let Some(positions) = parse_layer_list(&resolved_value, parse_background_position) {
        styles.background_positions = positions;
        styles.rebuild_background_layers();
      }
    }
    "background-position-x" => {
      if let Some(xs) = parse_layer_list(&resolved_value, parse_background_position_component_x) {
        styles.ensure_background_lists();
        let BackgroundPosition::Position { x: default, .. } = BackgroundLayer::default().position;
        let layer_count = xs.len().max(styles.background_positions.len()).max(1);
        let mut positions = Vec::with_capacity(layer_count);
        for idx in 0..layer_count {
          let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
          let source = styles
            .background_positions
            .get(source_idx)
            .copied()
            .unwrap_or_else(|| BackgroundLayer::default().position);
          let x_comp = xs
            .get(idx)
            .copied()
            .unwrap_or_else(|| xs.last().copied().unwrap_or(default));
          let BackgroundPosition::Position { y, .. } = source;
          positions.push(BackgroundPosition::Position { x: x_comp, y });
        }
        styles.background_positions = positions;
        styles.rebuild_background_layers();
      }
    }
    "background-position-y" => {
      if let Some(ys) = parse_layer_list(&resolved_value, parse_background_position_component_y) {
        styles.ensure_background_lists();
        let BackgroundPosition::Position { y: default, .. } = BackgroundLayer::default().position;
        let layer_count = ys.len().max(styles.background_positions.len()).max(1);
        let mut positions = Vec::with_capacity(layer_count);
        for idx in 0..layer_count {
          let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
          let source = styles
            .background_positions
            .get(source_idx)
            .copied()
            .unwrap_or_else(|| BackgroundLayer::default().position);
          let y_comp = ys
            .get(idx)
            .copied()
            .unwrap_or_else(|| ys.last().copied().unwrap_or(default));
          let BackgroundPosition::Position { x, .. } = source;
          positions.push(BackgroundPosition::Position { x, y: y_comp });
        }
        styles.background_positions = positions;
        styles.rebuild_background_layers();
      }
    }
    "background-position-inline" => {
      if let Some(values) = parse_layer_list(&resolved_value, parse_background_position_component_x)
      {
        styles.ensure_background_lists();
        let horizontal_inline = inline_axis_is_horizontal(styles.writing_mode);
        let default = BackgroundLayer::default().position;
        let layer_count = values.len().max(styles.background_positions.len()).max(1);
        let mut positions = Vec::with_capacity(layer_count);
        for idx in 0..layer_count {
          let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
          let source = styles
            .background_positions
            .get(source_idx)
            .copied()
            .unwrap_or(default);
          let inline_value = values
            .get(idx)
            .copied()
            .unwrap_or_else(|| values.last().copied().unwrap());
          let BackgroundPosition::Position { mut x, mut y } = source;
          if horizontal_inline {
            x = inline_value;
          } else {
            y = inline_value;
          }
          positions.push(BackgroundPosition::Position { x, y });
        }
        styles.background_positions = positions;
        styles.rebuild_background_layers();
      }
    }
    "background-position-block" => {
      if let Some(values) = parse_layer_list(&resolved_value, parse_background_position_component_y)
      {
        styles.ensure_background_lists();
        let horizontal_block = block_axis_is_horizontal(styles.writing_mode);
        let default = BackgroundLayer::default().position;
        let layer_count = values.len().max(styles.background_positions.len()).max(1);
        let mut positions = Vec::with_capacity(layer_count);
        for idx in 0..layer_count {
          let source_idx = styles.background_positions.len().saturating_sub(1).min(idx);
          let source = styles
            .background_positions
            .get(source_idx)
            .copied()
            .unwrap_or(default);
          let block_value = values
            .get(idx)
            .copied()
            .unwrap_or_else(|| values.last().copied().unwrap());
          let BackgroundPosition::Position { mut x, mut y } = source;
          if horizontal_block {
            x = block_value;
          } else {
            y = block_value;
          }
          positions.push(BackgroundPosition::Position { x, y });
        }
        styles.background_positions = positions;
        styles.rebuild_background_layers();
      }
    }
    "background-attachment" => {
      let parse = |value: &PropertyValue| match value {
        PropertyValue::Keyword(kw) => match kw.as_str() {
          "scroll" => Some(BackgroundAttachment::Scroll),
          "fixed" => Some(BackgroundAttachment::Fixed),
          "local" => Some(BackgroundAttachment::Local),
          _ => None,
        },
        _ => None,
      };
      if let Some(attachments) = parse_layer_list(&resolved_value, parse) {
        styles.background_attachments = attachments;
        styles.rebuild_background_layers();
      }
    }
    "background-origin" => {
      if let Some(origins) = parse_layer_list(&resolved_value, parse_background_box) {
        styles.background_origins = origins;
        styles.rebuild_background_layers();
      }
    }
    "background-clip" => {
      if let Some(clips) = parse_layer_list(&resolved_value, parse_background_box) {
        styles.background_clips = clips;
        styles.rebuild_background_layers();
      }
    }
    "background-blend-mode" => {
      let parse = |value: &PropertyValue| match value {
        PropertyValue::Keyword(kw) => parse_mix_blend_mode(kw),
        _ => None,
      };
      if let Some(modes) = parse_layer_list(&resolved_value, parse) {
        styles.background_blend_modes = modes;
        styles.rebuild_background_layers();
      }
    }

    // Mask
    "mask-image" => {
      if let Some(images) = parse_background_image_list(&resolved_value) {
        styles.mask_images = images;
        styles.rebuild_mask_layers();
      }
    }
    "mask-position" => {
      if let Some(positions) = parse_layer_list(&resolved_value, parse_background_position) {
        styles.mask_positions = positions;
        styles.rebuild_mask_layers();
      }
    }
    "mask-size" => {
      if let Some(sizes) = parse_layer_list(&resolved_value, parse_background_size) {
        styles.mask_sizes = sizes;
        styles.rebuild_mask_layers();
      }
    }
    "mask-repeat" => {
      if let Some(repeats) = parse_layer_list(&resolved_value, parse_background_repeat) {
        styles.mask_repeats = repeats;
        styles.rebuild_mask_layers();
      }
    }
    "mask-mode" => {
      if let Some(modes) = parse_layer_list(&resolved_value, parse_mask_mode) {
        styles.mask_modes = modes;
        styles.rebuild_mask_layers();
      }
    }
    "mask-origin" => {
      if let Some(origins) = parse_layer_list(&resolved_value, parse_mask_origin) {
        styles.mask_origins = origins;
        styles.rebuild_mask_layers();
      }
    }
    "mask-clip" => {
      if let Some(clips) = parse_layer_list(&resolved_value, parse_mask_clip) {
        styles.mask_clips = clips;
        styles.rebuild_mask_layers();
      }
    }
    "mask-composite" => {
      if let Some(ops) = parse_layer_list(&resolved_value, parse_mask_composite) {
        styles.mask_composites = ops;
        styles.rebuild_mask_layers();
      }
    }
    "mask" => {
      if let Some(images) = parse_background_image_list(&resolved_value) {
        styles.mask_images = images;
        styles.rebuild_mask_layers();
      }
    }

    // Shorthand: background
    "background" => {
      let tokens: Vec<PropertyValue> = match resolved_value {
        PropertyValue::Multiple(ref parts) => parts.clone(),
        _ => vec![resolved_value.clone()],
      };

      if tokens.is_empty() {
        return;
      }

      let layers = split_layers(&tokens);
      let mut parsed_layers = Vec::new();
      for layer_tokens in layers {
        if let Some(parsed) = parse_background_shorthand(&layer_tokens, styles.color) {
          parsed_layers.push(parsed);
        }
      }
      if parsed_layers.is_empty() {
        return;
      }

      styles.background_color = parsed_layers
        .last()
        .and_then(|p| p.color)
        .unwrap_or(Rgba::TRANSPARENT);

      let mut layers = Vec::new();
      for parsed in parsed_layers {
        let mut layer = BackgroundLayer::default();
        layer.image = parsed.image;
        if let Some(rep) = parsed.repeat {
          layer.repeat = rep;
        }
        if let Some(pos) = parsed.position {
          layer.position = pos;
        }
        if let Some(size) = parsed.size {
          layer.size = size;
        }
        if let Some(att) = parsed.attachment {
          layer.attachment = att;
        }
        if let Some(origin) = parsed.origin {
          layer.origin = origin;
        }
        if let Some(clip) = parsed.clip {
          layer.clip = clip;
        }
        layers.push(layer);
      }
      styles.set_background_layers(layers);
      styles.rebuild_background_layers();
    }

    // Visual effects
    "opacity" => {
      if let PropertyValue::Number(n) = resolved_value {
        styles.opacity = n.clamp(0.0, 1.0);
      }
    }
    "box-shadow" => match &resolved_value {
      PropertyValue::BoxShadow(shadows) => {
        styles.box_shadow = shadows.clone();
      }
      PropertyValue::Keyword(kw) if kw == "none" => {
        styles.box_shadow.clear();
      }
      _ => {}
    },
    "text-shadow" => match &resolved_value {
      PropertyValue::TextShadow(shadows) => {
        styles.text_shadow = shadows.clone();
      }
      PropertyValue::Keyword(kw) if kw == "none" => {
        styles.text_shadow.clear();
      }
      _ => {}
    },
    "transform" => {
      if let PropertyValue::Transform(transforms) = &resolved_value {
        styles.transform = transforms.clone();
      } else if let PropertyValue::Keyword(kw) = &resolved_value {
        if kw.eq_ignore_ascii_case("none") {
          styles.transform.clear();
        } else if let Some(ts) = crate::css::properties::parse_transform_list(kw) {
          styles.transform = ts;
        }
      }
    }
    "offset-path" => {
      if let Some(path) = parse_offset_path_value(&resolved_value) {
        styles.offset_path = path;
      }
    }
    "offset-distance" => match &resolved_value {
      PropertyValue::Length(len) => {
        styles.offset_distance = *len;
      }
      PropertyValue::Percentage(p) => {
        styles.offset_distance = Length::percent(*p);
      }
      PropertyValue::Number(n) if (*n).abs() < f32::EPSILON => {
        styles.offset_distance = Length::px(0.0);
      }
      _ => {}
    },
    "offset-rotate" => {
      if let Some(value) = parse_offset_rotate(&resolved_value) {
        styles.offset_rotate = value;
      }
    }
    "offset-anchor" => {
      if let Some(anchor) = parse_offset_anchor(&resolved_value) {
        styles.offset_anchor = anchor;
      }
    }
    "transform-box" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(value) = parse_transform_box(kw) {
          styles.transform_box = value;
        }
      }
    }
    "transform-style" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(value) = parse_transform_style(kw) {
          styles.transform_style = value;
        }
      }
    }
    "perspective" => match &resolved_value {
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("none") => {
        styles.perspective = None;
      }
      PropertyValue::Length(len) if len.value > 0.0 && !len.unit.is_percentage() => {
        styles.perspective = Some(len.clone());
      }
      _ => {}
    },
    "perspective-origin" => {
      if let Some(origin) = parse_transform_origin(&resolved_value) {
        styles.perspective_origin = origin;
      }
    }
    "backface-visibility" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(value) = parse_backface_visibility(kw) {
          styles.backface_visibility = value;
        }
      }
    }
    "filter" => {
      if let Some(filters) = parse_filter_list(&resolved_value) {
        styles.filter = filters;
      }
    }
    "backdrop-filter" => {
      if let Some(filters) = parse_filter_list(&resolved_value) {
        styles.backdrop_filter = filters;
      }
    }
    "clip-path" => {
      if let Some(path) = parse_clip_path_value(&resolved_value) {
        styles.clip_path = path;
      }
    }
    "clip" => {
      if let Some(value) = parse_clip_value(&resolved_value) {
        styles.clip = value;
      }
    }
    "transform-origin" => {
      if let Some(origin) = parse_transform_origin(&resolved_value) {
        styles.transform_origin = origin;
      }
    }
    "mix-blend-mode" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(mode) = parse_mix_blend_mode(kw) {
          styles.mix_blend_mode = mode;
        }
      }
    }
    "isolation" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.isolation = match kw.as_str() {
          "isolate" => Isolation::Isolate,
          _ => Isolation::Auto,
        };
      }
    }
    "will-change" => {
      if let Some(value) = parse_will_change(&resolved_value) {
        styles.will_change = value;
      }
    }
    "container-type" => {
      if let Some(ct) = parse_container_type_value(&resolved_value) {
        styles.container_type = ct;
      }
    }
    "container-name" => {
      if let Some(names) = parse_container_names(&resolved_value) {
        styles.container_name = names;
      }
    }
    "container" => {
      if let Some((names, ty)) = parse_container_shorthand(&resolved_value) {
        styles.container_name = names;
        if let Some(ct) = ty {
          styles.container_type = ct;
        }
      }
    }
    "contain" => {
      if let Some(value) = parse_containment(&resolved_value) {
        styles.containment = value;
      }
    }

    "border-collapse" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        styles.border_collapse = match kw.as_str() {
          "collapse" => crate::style::types::BorderCollapse::Collapse,
          "separate" => crate::style::types::BorderCollapse::Separate,
          _ => styles.border_collapse,
        };
      }
    }
    "border-spacing" => {
      if let Some((mut h, mut v)) = extract_length_pair(&resolved_value) {
        // border-spacing only accepts non-negative lengths; percentages are invalid.
        if h.has_percentage() || v.has_percentage() {
          return;
        }

        h = sanitize_non_negative_length(h);
        v = sanitize_non_negative_length(v);

        styles.border_spacing_horizontal = h;
        styles.border_spacing_vertical = v;
      }
    }

    // Content property (for ::before and ::after pseudo-elements)
    "content" => {
      if let Some(parsed) = content_value_from_property(&resolved_value) {
        styles.content_value = parsed.clone();
        // Keep legacy string storage for marker tests and existing code paths.
        styles.content = match &resolved_value {
          PropertyValue::String(s) => s.clone(),
          PropertyValue::Keyword(k) => k.clone(),
          PropertyValue::Multiple(tokens) => tokens
            .iter()
            .map(|t| match t {
              PropertyValue::String(s) => format!("\"{}\"", s),
              PropertyValue::Keyword(k) => k.clone(),
              PropertyValue::Url(u) => format!("url({})", u),
              PropertyValue::Number(n) => n.to_string(),
              PropertyValue::Percentage(p) => format!("{}%", p),
              _ => String::new(),
            })
            .collect::<Vec<_>>()
            .join(" "),
          PropertyValue::Url(u) => format!("url({})", u),
          _ => String::new(),
        };
      }
    }
    "quotes" => match &resolved_value {
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("none") => {
        styles.quotes.clear();
      }
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => {
        styles.quotes = crate::style::content::default_quotes();
      }
      PropertyValue::Multiple(list)
        if list.iter().all(|v| matches!(v, PropertyValue::String(_))) =>
      {
        let strings: Vec<String> = list
          .iter()
          .filter_map(|v| {
            if let PropertyValue::String(s) = v {
              Some(s.clone())
            } else {
              None
            }
          })
          .collect();
        if strings.len() % 2 == 0 && !strings.is_empty() {
          styles.quotes = strings
            .chunks(2)
            .map(|pair| (pair[0].clone(), pair[1].clone()))
            .collect();
        }
      }
      _ => {}
    },
    "image-orientation" => {
      if let Some(orientation) = parse_image_orientation(&resolved_value) {
        styles.image_orientation = orientation;
      }
    }
    "image-resolution" => {
      if let Some(res) = parse_image_resolution(&resolved_value) {
        styles.image_resolution = res;
      }
    }
    "border-image-source" => {
      if let Some(src) = parse_border_image_source(&resolved_value) {
        styles.border_image.source = src;
      }
    }
    "border-image-slice" => {
      if let Some(slice) = parse_border_image_slice(&resolved_value) {
        styles.border_image.slice = slice;
      }
    }
    "border-image-width" => {
      if let Some(width) = parse_border_image_width(&resolved_value) {
        styles.border_image.width = width;
      }
    }
    "border-image-outset" => {
      if let Some(outset) = parse_border_image_outset(&resolved_value) {
        styles.border_image.outset = outset;
      }
    }
    "border-image-repeat" => {
      if let Some(rep) = parse_border_image_repeat(&resolved_value) {
        styles.border_image.repeat = rep;
      }
    }
    "border-image" => {
      if let Some(img) = parse_border_image_shorthand(&resolved_value) {
        styles.border_image = img;
      }
    }
    "image-rendering" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(rendering) = parse_image_rendering(kw) {
          styles.image_rendering = rendering;
        }
      }
    }
    "aspect-ratio" => {
      if let Some(ratio) = parse_aspect_ratio(&resolved_value) {
        styles.aspect_ratio = ratio;
      }
    }
    "object-fit" => {
      if let PropertyValue::Keyword(kw) = &resolved_value {
        if let Some(fit) = parse_object_fit(kw) {
          styles.object_fit = fit;
        }
      }
    }
    "object-position" => {
      if let Some(pos) = parse_object_position(&resolved_value) {
        styles.object_position = pos;
      }
    }

    _ => {
      // Ignore unknown properties
    }
  }
}

pub fn extract_length(value: &PropertyValue) -> Option<Length> {
  match value {
    PropertyValue::Length(len) => Some(*len),
    PropertyValue::Number(n) if *n == 0.0 => Some(Length::px(0.0)),
    PropertyValue::Keyword(kw) if kw == "auto" => None,
    PropertyValue::Keyword(kw) => parse_length(kw),
    _ => None,
  }
}

pub fn extract_length_pair(value: &PropertyValue) -> Option<(Length, Length)> {
  match value {
    PropertyValue::Length(len) => Some((*len, *len)),
    PropertyValue::Multiple(values) => {
      let lengths: Vec<Length> = values.iter().filter_map(extract_length).collect();
      match lengths.len() {
        1 => Some((lengths[0], lengths[0])),
        l if l >= 2 => Some((lengths[0], lengths[1])),
        _ => None,
      }
    }
    _ => None,
  }
}

fn parse_gap_token(token: &str) -> Option<Length> {
  let t = token.trim();
  if t.is_empty() {
    return None;
  }
  if t.eq_ignore_ascii_case("normal") {
    return Some(Length::px(0.0));
  }
  parse_length(t)
}

fn length_from_value(value: &PropertyValue) -> Option<Length> {
  match value {
    PropertyValue::Length(l) => Some(*l),
    PropertyValue::Percentage(p) => Some(Length::new(*p, LengthUnit::Percent)),
    _ => None,
  }
}

fn parse_flex_shorthand(value: &PropertyValue) -> Option<(f32, f32, FlexBasis)> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "none" => return Some((0.0, 0.0, FlexBasis::Auto)),
      "auto" => return Some((1.0, 1.0, FlexBasis::Auto)),
      "initial" => return Some((0.0, 1.0, FlexBasis::Auto)),
      _ => {}
    },
    PropertyValue::Number(n) if n.is_finite() && *n >= 0.0 => {
      return Some((
        *n,
        1.0,
        FlexBasis::Length(Length::new(0.0, LengthUnit::Percent)),
      ));
    }
    PropertyValue::Length(len) => {
      return Some((1.0, 1.0, FlexBasis::Length(*len)));
    }
    PropertyValue::Percentage(p) => {
      return Some((
        1.0,
        1.0,
        FlexBasis::Length(Length::new(*p, LengthUnit::Percent)),
      ));
    }
    PropertyValue::Multiple(values) => {
      let mut grow: Option<f32> = None;
      let mut shrink: Option<f32> = None;
      let mut basis: Option<FlexBasis> = None;

      for v in values {
        match v {
          PropertyValue::Number(n) if n.is_finite() && *n >= 0.0 => {
            if grow.is_none() {
              grow = Some(*n);
            } else if shrink.is_none() {
              shrink = Some(*n);
            }
          }
          PropertyValue::Number(n) if n.is_finite() && *n < 0.0 => {
            // Negative grow/shrink values make the declaration invalid.
            return None;
          }
          PropertyValue::Keyword(kw) if kw == "auto" || kw == "content" => {
            if basis.is_none() {
              basis = Some(FlexBasis::Auto);
            }
          }
          _ => {
            if basis.is_none() {
              if let Some(len) = length_from_value(v) {
                basis = Some(FlexBasis::Length(len));
              }
            }
          }
        }
      }

      if grow.is_none() && shrink.is_none() && basis.is_none() {
        return None;
      }

      let grow = grow.unwrap_or(1.0);
      let shrink = shrink.unwrap_or(1.0);
      let basis = basis.unwrap_or(FlexBasis::Length(Length::new(0.0, LengthUnit::Percent)));
      return Some((grow, shrink, basis));
    }
    _ => {}
  }

  None
}

fn extract_gap_component(value: &PropertyValue) -> Option<Length> {
  match value {
    PropertyValue::Length(len) => Some(*len),
    PropertyValue::Percentage(p) => Some(Length::percent(*p)),
    PropertyValue::Number(n) if *n == 0.0 => Some(Length::px(0.0)),
    PropertyValue::Keyword(kw) => parse_gap_token(kw),
    _ => None,
  }
}

fn parse_gap_lengths(value: &PropertyValue) -> Option<(Length, Length)> {
  match value {
    PropertyValue::Multiple(values) => {
      let lengths: Vec<Length> = values.iter().filter_map(extract_gap_component).collect();
      match lengths.as_slice() {
        [first] => Some((*first, *first)),
        [first, second, ..] => Some((*first, *second)),
        _ => None,
      }
    }
    PropertyValue::Keyword(kw) => {
      let tokens: Vec<&str> = kw.split_whitespace().filter(|s| !s.is_empty()).collect();
      if tokens.is_empty() {
        return None;
      }
      let mut lengths = Vec::new();
      for token in tokens.iter().take(2) {
        if let Some(len) = parse_gap_token(token) {
          lengths.push(len);
        }
      }
      match lengths.as_slice() {
        [first] => Some((*first, *first)),
        [first, second] => Some((*first, *second)),
        _ => None,
      }
    }
    PropertyValue::Length(len) => Some((*len, *len)),
    PropertyValue::Percentage(p) => Some((Length::percent(*p), Length::percent(*p))),
    PropertyValue::Number(n) if *n == 0.0 => Some((Length::px(0.0), Length::px(0.0))),
    _ => None,
  }
}

fn parse_single_gap_length(value: &PropertyValue) -> Option<Length> {
  parse_gap_lengths(value).map(|(first, _)| first)
}

fn parse_align_keyword(kw: &str) -> Option<AlignItems> {
  match kw {
    "start" => Some(AlignItems::Start),
    "end" => Some(AlignItems::End),
    "self-start" => Some(AlignItems::SelfStart),
    "self-end" => Some(AlignItems::SelfEnd),
    "flex-start" => Some(AlignItems::FlexStart),
    "flex-end" => Some(AlignItems::FlexEnd),
    "center" => Some(AlignItems::Center),
    "baseline" => Some(AlignItems::Baseline),
    "stretch" => Some(AlignItems::Stretch),
    "normal" => Some(AlignItems::Stretch),
    "left" => Some(AlignItems::Start),
    "right" => Some(AlignItems::End),
    _ => None,
  }
}

fn parse_place_pair(value: &PropertyValue) -> Option<(AlignItems, AlignItems)> {
  let tokens: Vec<String> = match value {
    PropertyValue::Multiple(values) => values
      .iter()
      .filter_map(|v| match v {
        PropertyValue::Keyword(k) => Some(k.clone()),
        _ => None,
      })
      .collect(),
    PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
    _ => Vec::new(),
  };
  if tokens.is_empty() {
    return None;
  }
  if tokens.len() == 1 {
    if let Some(val) = parse_align_keyword(&tokens[0]) {
      return Some((val, val));
    }
    return None;
  }
  let first = parse_align_keyword(&tokens[0])?;
  let second = parse_align_keyword(&tokens[1])?;
  Some((first, second))
}

fn parse_place_content_pair(value: &PropertyValue) -> Option<(AlignContent, JustifyContent)> {
  fn to_align_content(kw: &str) -> Option<AlignContent> {
    match kw {
      "start" | "flex-start" => Some(AlignContent::FlexStart),
      "end" | "flex-end" => Some(AlignContent::FlexEnd),
      "center" => Some(AlignContent::Center),
      "stretch" | "normal" => Some(AlignContent::Stretch),
      "space-between" => Some(AlignContent::SpaceBetween),
      "space-around" => Some(AlignContent::SpaceAround),
      "space-evenly" => Some(AlignContent::SpaceEvenly),
      _ => None,
    }
  }

  fn to_justify_content(kw: &str) -> Option<JustifyContent> {
    match kw {
      "flex-start" | "start" => Some(JustifyContent::FlexStart),
      "flex-end" | "end" => Some(JustifyContent::FlexEnd),
      "center" => Some(JustifyContent::Center),
      "space-between" => Some(JustifyContent::SpaceBetween),
      "space-around" => Some(JustifyContent::SpaceAround),
      "space-evenly" => Some(JustifyContent::SpaceEvenly),
      _ => None,
    }
  }

  let tokens: Vec<String> = match value {
    PropertyValue::Multiple(values) => values
      .iter()
      .filter_map(|v| match v {
        PropertyValue::Keyword(k) => Some(k.clone()),
        _ => None,
      })
      .collect(),
    PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
    _ => Vec::new(),
  };
  if tokens.is_empty() {
    return None;
  }
  if tokens.len() == 1 {
    if let (Some(a), Some(j)) = (to_align_content(&tokens[0]), to_justify_content(&tokens[0])) {
      return Some((a, j));
    }
    return None;
  }
  let first = to_align_content(&tokens[0])?;
  let second = to_justify_content(&tokens[1])?;
  Some((first, second))
}

fn parse_spacing_value(
  value: &PropertyValue,
  font_size: f32,
  root_font_size: f32,
  allow_percentage: bool,
) -> Option<f32> {
  match value {
    PropertyValue::Keyword(kw) if kw == "normal" => Some(0.0),
    PropertyValue::Number(n) if *n == 0.0 => Some(0.0),
    PropertyValue::Length(len) => resolve_font_relative_length(*len, font_size, root_font_size),
    PropertyValue::Percentage(pct) if allow_percentage => Some((pct / 100.0) * font_size),
    _ => None,
  }
}

fn resolve_font_relative_length(len: Length, font_size: f32, root_font_size: f32) -> Option<f32> {
  Some(match len.unit {
    u if u.is_absolute() => len.to_px(),
    LengthUnit::Em => len.value * font_size,
    LengthUnit::Ex => len.value * font_size * 0.5,
    LengthUnit::Ch => len.value * font_size * 0.5,
    LengthUnit::Rem => len.value * root_font_size,
    LengthUnit::Percent => (len.value / 100.0) * font_size,
    // Fallback: keep the raw author value when we cannot resolve viewport-relative or unknown units here.
    _ => len.value,
  })
}

fn parse_object_fit(kw: &str) -> Option<ObjectFit> {
  match kw {
    "fill" => Some(ObjectFit::Fill),
    "contain" => Some(ObjectFit::Contain),
    "cover" => Some(ObjectFit::Cover),
    "none" => Some(ObjectFit::None),
    "scale-down" => Some(ObjectFit::ScaleDown),
    _ => None,
  }
}

fn parse_image_rendering(kw: &str) -> Option<ImageRendering> {
  match kw {
    "auto" => Some(ImageRendering::Auto),
    "smooth" | "high-quality" | "optimizequality" => Some(ImageRendering::Smooth),
    "crisp-edges" | "crispedges" | "optimize-contrast" | "optimizecontrast" => {
      Some(ImageRendering::CrispEdges)
    }
    "pixelated" => Some(ImageRendering::Pixelated),
    "optimizespeed" => Some(ImageRendering::CrispEdges),
    _ => None,
  }
}

fn quantize_quarter_turns(angle_deg: f32) -> u8 {
  if !angle_deg.is_finite() {
    return 0;
  }
  let quarters = angle_deg / 90.0;
  let base = quarters.floor();
  let frac = quarters - base;
  let rounded = if (frac - 0.5).abs() < 1e-6 {
    base + 1.0 // ties round toward +∞
  } else if frac < 0.5 {
    base
  } else {
    base + 1.0
  };
  let mut turns = (rounded as i32) % 4;
  if turns < 0 {
    turns += 4;
  }
  turns as u8
}

fn parse_image_orientation_tokens(tokens: &[&str]) -> Option<ImageOrientation> {
  if tokens.is_empty() {
    return None;
  }

  if tokens.len() == 1 {
    let kw = tokens[0].to_ascii_lowercase();
    return match kw.as_str() {
      "from-image" => Some(ImageOrientation::FromImage),
      "none" => Some(ImageOrientation::None),
      "flip" => Some(ImageOrientation::Angle {
        quarter_turns: 0,
        flip: true,
      }),
      _ => parse_angle_from_str(&kw).map(|angle| ImageOrientation::Angle {
        quarter_turns: quantize_quarter_turns(angle),
        flip: false,
      }),
    };
  }

  let mut angle: Option<f32> = None;
  let mut flip = false;
  for token in tokens {
    let lower = token.to_ascii_lowercase();
    match lower.as_str() {
      "flip" if !flip => {
        flip = true;
      }
      _ if angle.is_none() => {
        if let Some(a) = parse_angle_from_str(&lower) {
          angle = Some(a);
        } else {
          return None;
        }
      }
      _ => return None,
    }
  }

  Some(ImageOrientation::Angle {
    quarter_turns: quantize_quarter_turns(angle.unwrap_or(0.0)),
    flip,
  })
}

fn parse_image_orientation(value: &PropertyValue) -> Option<ImageOrientation> {
  match value {
    PropertyValue::Keyword(kw) => parse_image_orientation_tokens(&[kw.as_str()]),
    PropertyValue::Multiple(values) => {
      let mut tokens = Vec::new();
      for v in values {
        match v {
          PropertyValue::Keyword(kw) => tokens.push(kw.as_str()),
          _ => return None,
        }
      }
      parse_image_orientation_tokens(&tokens)
    }
    _ => None,
  }
}

fn parse_resolution_token(token: &str) -> Option<f32> {
  let lower = token.trim().to_ascii_lowercase();
  if let Some(rest) = lower.strip_suffix("dppx") {
    return rest.parse::<f32>().ok().filter(|v| *v > 0.0);
  }
  if let Some(rest) = lower.strip_suffix("dpi") {
    return rest
      .parse::<f32>()
      .ok()
      .filter(|v| *v > 0.0)
      .map(|dpi| dpi / 96.0);
  }
  if let Some(rest) = lower.strip_suffix("dpcm") {
    return rest
      .parse::<f32>()
      .ok()
      .filter(|v| *v > 0.0)
      .map(|dpcm| (dpcm * 2.54) / 96.0);
  }
  if let Some(rest) = lower.strip_suffix('x') {
    return rest.parse::<f32>().ok().filter(|v| *v > 0.0);
  }
  None
}

fn parse_image_resolution_tokens(tokens: &[&str]) -> Option<ImageResolution> {
  if tokens.is_empty() {
    return None;
  }

  let mut from_image = false;
  let mut snap = false;
  let mut resolution: Option<f32> = None;

  for token in tokens {
    let lower = token.to_ascii_lowercase();
    match lower.as_str() {
      "from-image" if !from_image => from_image = true,
      "from-image" => return None,
      "snap" if !snap => snap = true,
      "snap" => return None,
      _ => {
        if resolution.is_some() {
          return None;
        }
        resolution = Some(parse_resolution_token(&lower)?);
      }
    }
  }

  Some(ImageResolution {
    from_image,
    specified: resolution,
    snap,
  })
}

fn parse_image_resolution(value: &PropertyValue) -> Option<ImageResolution> {
  match value {
    PropertyValue::Keyword(kw) => parse_image_resolution_tokens(&[kw.as_str()]),
    PropertyValue::Multiple(values) => {
      let mut tokens = Vec::with_capacity(values.len());
      for v in values {
        match v {
          PropertyValue::Keyword(kw) => tokens.push(kw.as_str()),
          _ => return None,
        }
      }
      parse_image_resolution_tokens(&tokens)
    }
    _ => None,
  }
}

fn parse_border_image_source(value: &PropertyValue) -> Option<BorderImageSource> {
  if let PropertyValue::Keyword(kw) = value {
    if kw.eq_ignore_ascii_case("none") {
      return Some(BorderImageSource::None);
    }
  }
  parse_background_image_value(value).map(|img| BorderImageSource::Image(Box::new(img)))
}

fn parse_border_image_slice(value: &PropertyValue) -> Option<BorderImageSlice> {
  let tokens: Vec<PropertyValue> = match value {
    PropertyValue::Multiple(v) => v.clone(),
    other => vec![other.clone()],
  };
  parse_border_image_slice_values(&tokens)
}

fn parse_border_image_slice_values(values: &[PropertyValue]) -> Option<BorderImageSlice> {
  if values.is_empty() {
    return None;
  }
  let mut fill = false;
  let mut numeric: Vec<BorderImageSliceValue> = Vec::new();
  for v in values {
    match v {
      PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("fill") => fill = true,
      PropertyValue::Number(n) if *n >= 0.0 => numeric.push(BorderImageSliceValue::Number(*n)),
      PropertyValue::Percentage(p) if *p >= 0.0 => {
        numeric.push(BorderImageSliceValue::Percentage(*p))
      }
      _ => return None,
    }
  }
  if numeric.is_empty() {
    return None;
  }
  let expand = |vals: &[BorderImageSliceValue]| -> [BorderImageSliceValue; 4] {
    match vals.len() {
      1 => [vals[0], vals[0], vals[0], vals[0]],
      2 => [vals[0], vals[1], vals[0], vals[1]],
      3 => [vals[0], vals[1], vals[2], vals[1]],
      _ => [vals[0], vals[1], vals[2], vals[3]],
    }
  };
  let expanded = expand(&numeric);
  Some(BorderImageSlice {
    top: expanded[0],
    right: expanded[1],
    bottom: expanded[2],
    left: expanded[3],
    fill,
  })
}

fn parse_border_image_width(value: &PropertyValue) -> Option<BorderImageWidth> {
  let tokens: Vec<PropertyValue> = match value {
    PropertyValue::Multiple(v) => v.clone(),
    other => vec![other.clone()],
  };
  parse_border_image_width_list(&tokens)
}

fn parse_border_image_width_list(values: &[PropertyValue]) -> Option<BorderImageWidth> {
  if values.is_empty() {
    return None;
  }
  let mut widths: Vec<BorderImageWidthValue> = Vec::new();
  for v in values {
    if let PropertyValue::Keyword(kw) = v {
      if kw.eq_ignore_ascii_case("auto") {
        widths.push(BorderImageWidthValue::Auto);
        continue;
      }
    }
    match v {
      PropertyValue::Number(n) if *n >= 0.0 => widths.push(BorderImageWidthValue::Number(*n)),
      PropertyValue::Length(len) if len.value >= 0.0 => {
        if len.unit == LengthUnit::Percent {
          widths.push(BorderImageWidthValue::Percentage(len.value));
        } else {
          widths.push(BorderImageWidthValue::Length(*len));
        }
      }
      PropertyValue::Percentage(p) if *p >= 0.0 => {
        widths.push(BorderImageWidthValue::Percentage(*p))
      }
      _ => return None,
    }
  }
  let expand = |vals: &[BorderImageWidthValue]| -> [BorderImageWidthValue; 4] {
    match vals.len() {
      1 => [vals[0], vals[0], vals[0], vals[0]],
      2 => [vals[0], vals[1], vals[0], vals[1]],
      3 => [vals[0], vals[1], vals[2], vals[1]],
      _ => [vals[0], vals[1], vals[2], vals[3]],
    }
  };
  let expanded = expand(&widths);
  Some(BorderImageWidth {
    top: expanded[0],
    right: expanded[1],
    bottom: expanded[2],
    left: expanded[3],
  })
}

fn parse_border_image_outset(value: &PropertyValue) -> Option<BorderImageOutset> {
  let tokens: Vec<PropertyValue> = match value {
    PropertyValue::Multiple(v) => v.clone(),
    other => vec![other.clone()],
  };
  parse_border_image_outset_list(&tokens)
}

fn parse_border_image_outset_list(values: &[PropertyValue]) -> Option<BorderImageOutset> {
  if values.is_empty() {
    return None;
  }
  let mut outsets: Vec<BorderImageOutsetValue> = Vec::new();
  for v in values {
    match v {
      PropertyValue::Number(n) if *n >= 0.0 => outsets.push(BorderImageOutsetValue::Number(*n)),
      PropertyValue::Length(len) if len.value >= 0.0 => {
        outsets.push(BorderImageOutsetValue::Length(*len))
      }
      _ => return None,
    }
  }
  let expand = |vals: &[BorderImageOutsetValue]| -> [BorderImageOutsetValue; 4] {
    match vals.len() {
      1 => [vals[0], vals[0], vals[0], vals[0]],
      2 => [vals[0], vals[1], vals[0], vals[1]],
      3 => [vals[0], vals[1], vals[2], vals[1]],
      _ => [vals[0], vals[1], vals[2], vals[3]],
    }
  };
  let expanded = expand(&outsets);
  Some(BorderImageOutset {
    top: expanded[0],
    right: expanded[1],
    bottom: expanded[2],
    left: expanded[3],
  })
}

fn parse_border_image_repeat(
  value: &PropertyValue,
) -> Option<(BorderImageRepeat, BorderImageRepeat)> {
  let tokens: Vec<String> = match value {
    PropertyValue::Multiple(v) => v
      .iter()
      .filter_map(|p| match p {
        PropertyValue::Keyword(k) => Some(k.clone()),
        _ => None,
      })
      .collect(),
    PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
    _ => Vec::new(),
  };
  if tokens.is_empty() {
    return None;
  }
  let parse = |kw: &str| -> Option<BorderImageRepeat> {
    match kw {
      "stretch" => Some(BorderImageRepeat::Stretch),
      "repeat" => Some(BorderImageRepeat::Repeat),
      "round" => Some(BorderImageRepeat::Round),
      "space" => Some(BorderImageRepeat::Space),
      _ => None,
    }
  };
  if tokens.len() == 1 {
    parse(&tokens[0]).map(|r| (r, r))
  } else {
    match (parse(&tokens[0]), parse(&tokens[1])) {
      (Some(a), Some(b)) => Some((a, b)),
      _ => None,
    }
  }
}

fn parse_border_image_shorthand(value: &PropertyValue) -> Option<BorderImage> {
  let mut img = BorderImage::default();
  let tokens: Vec<PropertyValue> = match value {
    PropertyValue::Multiple(v) => v.clone(),
    other => vec![other.clone()],
  };
  if tokens.is_empty() {
    return None;
  }

  let mut segments: Vec<Vec<PropertyValue>> = Vec::new();
  let mut current = Vec::new();
  for t in tokens {
    if matches!(&t, PropertyValue::Keyword(k) if k == "/") {
      segments.push(current);
      current = Vec::new();
    } else {
      current.push(t);
    }
  }
  segments.push(current);

  // First segment: source, slice, repeat
  if let Some(first) = segments.first() {
    let mut items = first.clone();
    // Extract repeat keywords from the end (1 or 2 tokens)
    let mut rep_tokens: Vec<PropertyValue> = Vec::new();
    while let Some(PropertyValue::Keyword(k)) = items.last() {
      if ["stretch", "repeat", "round", "space"].contains(&k.as_str()) {
        rep_tokens.push(items.pop().unwrap());
      } else {
        break;
      }
      if rep_tokens.len() == 2 {
        break;
      }
    }
    rep_tokens.reverse();
    if !rep_tokens.is_empty() {
      if let Some(rep) = parse_border_image_repeat(&PropertyValue::Multiple(rep_tokens)) {
        img.repeat = rep;
      }
    }

    if let Some(src) = items.first().and_then(|t| parse_border_image_source(t)) {
      img.source = src;
      items.remove(0);
    }
    if let Some(slice) = parse_border_image_slice_values(&items) {
      img.slice = slice;
    }
  }

  if let Some(seg) = segments.get(1) {
    if let Some(width) = parse_border_image_width_list(seg) {
      img.width = width;
    }
  }
  if let Some(seg) = segments.get(2) {
    if let Some(outset) = parse_border_image_outset_list(seg) {
      img.outset = outset;
    }
  }

  Some(img)
}

fn parse_aspect_ratio(value: &PropertyValue) -> Option<AspectRatio> {
  match value {
    PropertyValue::Keyword(kw) => {
      if kw.eq_ignore_ascii_case("auto") {
        return Some(AspectRatio::Auto);
      }
      parse_ratio_string(kw).map(AspectRatio::Ratio)
    }
    PropertyValue::Number(n) => (*n > 0.0).then_some(AspectRatio::Ratio(*n)),
    PropertyValue::Multiple(values) => {
      let mut nums: Vec<f32> = Vec::new();
      for v in values {
        match v {
          PropertyValue::Number(n) if *n > 0.0 => nums.push(*n),
          PropertyValue::Keyword(kw) => {
            if let Ok(n) = kw.parse::<f32>() {
              if n > 0.0 {
                nums.push(n);
              }
            }
          }
          _ => {}
        }
      }
      match nums.as_slice() {
        [num] => Some(AspectRatio::Ratio(*num)),
        [num, denom, ..] if *denom > 0.0 => Some(AspectRatio::Ratio(*num / *denom)),
        _ => None,
      }
    }
    _ => None,
  }
}

fn parse_ratio_string(raw: &str) -> Option<f32> {
  let trimmed = raw.trim();
  if trimmed.is_empty() {
    return None;
  }
  if let Ok(val) = trimmed.parse::<f32>() {
    return (val > 0.0).then_some(val);
  }
  if let Some((num_str, denom_str)) = trimmed.split_once('/') {
    let num = num_str.trim().parse::<f32>().ok()?;
    let denom = denom_str.trim().parse::<f32>().ok()?;
    if num > 0.0 && denom > 0.0 {
      return Some(num / denom);
    }
  }
  None
}

fn parse_object_position(value: &PropertyValue) -> Option<ObjectPosition> {
  use crate::style::types::PositionComponent as PC;
  use crate::style::types::PositionKeyword as PK;

  #[derive(Copy, Clone, Eq, PartialEq)]
  enum Axis {
    Horizontal,
    Vertical,
  }

  fn parse_component(value: &PropertyValue) -> Option<(PC, Option<Axis>)> {
    match value {
      PropertyValue::Length(len) => Some((PC::Length(*len), None)),
      PropertyValue::Percentage(pct) => Some((PC::Percentage(*pct / 100.0), None)),
      PropertyValue::Keyword(kw) => match kw.as_str() {
        "left" => Some((PC::Keyword(PK::Start), Some(Axis::Horizontal))),
        "right" => Some((PC::Keyword(PK::End), Some(Axis::Horizontal))),
        "center" => Some((PC::Keyword(PK::Center), None)),
        "top" => Some((PC::Keyword(PK::Start), Some(Axis::Vertical))),
        "bottom" => Some((PC::Keyword(PK::End), Some(Axis::Vertical))),
        _ => None,
      },
      _ => None,
    }
  }

  let values: Vec<&PropertyValue> = match value {
    PropertyValue::Multiple(list) if !list.is_empty() => list.iter().collect(),
    _ => vec![value],
  };

  let mut parsed: Vec<(PC, Option<Axis>)> =
    values.iter().filter_map(|v| parse_component(v)).collect();
  if parsed.is_empty() {
    return None;
  }

  let default = PC::Keyword(PK::Center);
  if parsed.len() == 1 {
    let (comp, axis) = parsed.remove(0);
    let (x, y) = match axis {
      Some(Axis::Vertical) => (default, comp),
      _ => (comp, default),
    };
    return Some(ObjectPosition { x, y });
  }

  let (first, first_axis) = parsed.first().copied().unwrap_or((default, None));
  let (second, second_axis) = parsed.get(1).copied().unwrap_or((default, None));

  let (x, y) = match (first_axis, second_axis) {
    (Some(Axis::Vertical), Some(Axis::Horizontal)) => (second, first),
    (Some(Axis::Vertical), None) => (second, first),
    _ => (first, second),
  };

  Some(ObjectPosition { x, y })
}

pub fn parse_background_size_component(value: &PropertyValue) -> Option<BackgroundSizeComponent> {
  match value {
    PropertyValue::Keyword(kw) if kw == "auto" => Some(BackgroundSizeComponent::Auto),
    PropertyValue::Length(len) => Some(BackgroundSizeComponent::Length(*len)),
    PropertyValue::Number(n) if *n == 0.0 => Some(BackgroundSizeComponent::Length(Length::px(0.0))),
    PropertyValue::Percentage(p) => Some(BackgroundSizeComponent::Length(Length::percent(*p))),
    _ => None,
  }
}

fn parse_background_size(value: &PropertyValue) -> Option<BackgroundSize> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "cover" => Some(BackgroundSize::Keyword(BackgroundSizeKeyword::Cover)),
      "contain" => Some(BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)),
      "auto" => Some(BackgroundSize::Explicit(
        BackgroundSizeComponent::Auto,
        BackgroundSizeComponent::Auto,
      )),
      _ => None,
    },
    PropertyValue::Multiple(values) => {
      if values.len() == 1 {
        if let Some(single) = parse_background_size(&values[0]) {
          return Some(single);
        }
      }
      let components: Vec<BackgroundSizeComponent> = values
        .iter()
        .filter_map(parse_background_size_component)
        .collect();
      match components.len() {
        0 => None,
        1 => Some(BackgroundSize::Explicit(
          components[0],
          BackgroundSizeComponent::Auto,
        )),
        _ => Some(BackgroundSize::Explicit(components[0], components[1])),
      }
    }
    _ => parse_background_size_component(value)
      .map(|c| BackgroundSize::Explicit(c, BackgroundSizeComponent::Auto)),
  }
}

fn parse_transform_origin(value: &PropertyValue) -> Option<TransformOrigin> {
  #[derive(Clone, Copy, PartialEq, Eq)]
  enum AxisHint {
    Horizontal,
    Vertical,
    Either,
  }

  fn keyword_to_length(kw: &str) -> Option<(Length, AxisHint)> {
    match kw {
      "left" => Some((Length::percent(0.0), AxisHint::Horizontal)),
      "right" => Some((Length::percent(100.0), AxisHint::Horizontal)),
      "top" => Some((Length::percent(0.0), AxisHint::Vertical)),
      "bottom" => Some((Length::percent(100.0), AxisHint::Vertical)),
      "center" => Some((Length::percent(50.0), AxisHint::Either)),
      _ => None,
    }
  }

  fn push_component(
    target_x: &mut Option<Length>,
    target_y: &mut Option<Length>,
    component: Length,
    hint: AxisHint,
  ) {
    match hint {
      AxisHint::Horizontal => {
        if target_x.is_none() {
          *target_x = Some(component);
        }
      }
      AxisHint::Vertical => {
        if target_y.is_none() {
          *target_y = Some(component);
        }
      }
      AxisHint::Either => {
        if target_x.is_none() {
          *target_x = Some(component);
        } else if target_y.is_none() {
          *target_y = Some(component);
        }
      }
    }
  }

  let components: Vec<&PropertyValue> = match value {
    PropertyValue::Multiple(values) if !values.is_empty() => values.iter().collect(),
    _ => vec![value],
  };

  let mut x: Option<Length> = None;
  let mut y: Option<Length> = None;

  for comp in components.iter().take(3) {
    match *comp {
      PropertyValue::Length(len) => push_component(&mut x, &mut y, *len, AxisHint::Either),
      PropertyValue::Percentage(pct) => {
        push_component(&mut x, &mut y, Length::percent(*pct), AxisHint::Either)
      }
      PropertyValue::Keyword(kw) => {
        if let Some((len, hint)) = keyword_to_length(kw) {
          push_component(&mut x, &mut y, len, hint);
        }
      }
      PropertyValue::Number(n) if *n == 0.0 => {
        push_component(&mut x, &mut y, Length::px(0.0), AxisHint::Either)
      }
      _ => {}
    }
  }

  let x = x.unwrap_or_else(|| Length::percent(50.0));
  let y = y.unwrap_or_else(|| Length::percent(50.0));
  Some(TransformOrigin { x, y })
}

fn parse_transform_box(kw: &str) -> Option<TransformBox> {
  match kw.to_ascii_lowercase().as_str() {
    "border-box" => Some(TransformBox::BorderBox),
    "content-box" => Some(TransformBox::ContentBox),
    "fill-box" => Some(TransformBox::FillBox),
    "stroke-box" => Some(TransformBox::StrokeBox),
    "view-box" => Some(TransformBox::ViewBox),
    _ => None,
  }
}

fn parse_transform_style(kw: &str) -> Option<TransformStyle> {
  match kw.to_ascii_lowercase().as_str() {
    "flat" => Some(TransformStyle::Flat),
    "preserve-3d" => Some(TransformStyle::Preserve3d),
    _ => None,
  }
}

fn parse_angle_value<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<f32, cssparser::ParseError<'i, ()>> {
  let token = input.next()?;
  match token {
    Token::Dimension {
      value, ref unit, ..
    } => {
      let u = unit.to_ascii_lowercase();
      let deg = match u.as_str() {
        "deg" => *value,
        "grad" => *value * (360.0 / 400.0),
        "rad" => value.to_degrees(),
        "turn" => *value * 360.0,
        _ => {
          return Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Basic(BasicParseErrorKind::UnexpectedToken(
              token.clone(),
            )),
            location: input.current_source_location(),
          })
        }
      };
      Ok(deg)
    }
    Token::Number { value, .. } if *value == 0.0 => Ok(0.0),
    _ => Err(cssparser::ParseError {
      kind: cssparser::ParseErrorKind::Basic(BasicParseErrorKind::UnexpectedToken(token.clone())),
      location: input.current_source_location(),
    }),
  }
}

fn parse_offset_path_value(value: &PropertyValue) -> Option<OffsetPath> {
  match value {
    PropertyValue::Keyword(kw) => parse_offset_path_str(kw),
    PropertyValue::Multiple(tokens) => {
      let mut combined = String::new();
      for (idx, token) in tokens.iter().enumerate() {
        let PropertyValue::Keyword(part) = token else {
          return None;
        };
        if idx > 0 {
          combined.push(' ');
        }
        combined.push_str(part);
      }
      parse_offset_path_str(combined.trim())
    }
    _ => None,
  }
}

fn parse_offset_path_str(input: &str) -> Option<OffsetPath> {
  let trimmed = input.trim();
  if trimmed.eq_ignore_ascii_case("none") {
    return Some(OffsetPath::None);
  }

  if let Some(path_data) = trimmed
    .strip_prefix("path(")
    .and_then(|s| s.strip_suffix(')'))
  {
    let inner = path_data.trim().trim_matches(&['"', '\''][..]);
    if let Some(commands) = parse_svg_motion_path(inner) {
      return Some(OffsetPath::Path(commands));
    }
    return None;
  }

  let mut input = ParserInput::new(trimmed);
  let mut parser = Parser::new(&mut input);

  if let Ok(shape) = parser.try_parse(parse_basic_shape) {
    parser.expect_exhausted().ok()?;
    return Some(OffsetPath::BasicShape(Box::new(shape)));
  }

  if parser
    .try_parse(|p| p.expect_function_matching("ray"))
    .is_ok()
  {
    let ray = parser.parse_nested_block(parse_ray).ok()?;
    parser.expect_exhausted().ok()?;
    return Some(OffsetPath::Ray(ray));
  }

  None
}

fn parse_svg_motion_path(data: &str) -> Option<Vec<MotionPathCommand>> {
  let mut commands = Vec::new();
  let mut current = (0.0f32, 0.0f32);
  let mut subpath_start = (0.0f32, 0.0f32);
  let mut last_cubic_ctrl: Option<(f32, f32)> = None;
  let mut last_quad_ctrl: Option<(f32, f32)> = None;

  for segment in PathParser::from(data) {
    let seg = segment.ok()?;
    match seg {
      svgtypes::PathSegment::MoveTo { abs, x, y } => {
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        current = (nx, ny);
        subpath_start = current;
        commands.push(MotionPathCommand::MoveTo(MotionPosition {
          x: Length::px(nx),
          y: Length::px(ny),
        }));
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      svgtypes::PathSegment::LineTo { abs, x, y } => {
        let (nx, ny) = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        current = (nx, ny);
        commands.push(MotionPathCommand::LineTo(MotionPosition {
          x: Length::px(nx),
          y: Length::px(ny),
        }));
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      svgtypes::PathSegment::HorizontalLineTo { abs, x } => {
        let nx = if abs { x as f32 } else { current.0 + x as f32 };
        current.0 = nx;
        commands.push(MotionPathCommand::LineTo(MotionPosition {
          x: Length::px(nx),
          y: Length::px(current.1),
        }));
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      svgtypes::PathSegment::VerticalLineTo { abs, y } => {
        let ny = if abs { y as f32 } else { current.1 + y as f32 };
        current.1 = ny;
        commands.push(MotionPathCommand::LineTo(MotionPosition {
          x: Length::px(current.0),
          y: Length::px(ny),
        }));
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      svgtypes::PathSegment::CurveTo {
        abs,
        x1,
        y1,
        x2,
        y2,
        x,
        y,
      } => {
        let start = current;
        let ctrl1 = if abs {
          (x1 as f32, y1 as f32)
        } else {
          (current.0 + x1 as f32, current.1 + y1 as f32)
        };
        let ctrl2 = if abs {
          (x2 as f32, y2 as f32)
        } else {
          (current.0 + x2 as f32, current.1 + y2 as f32)
        };
        let end = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        flatten_cubic_bezier(start, ctrl1, ctrl2, end, &mut commands);
        current = end;
        last_cubic_ctrl = Some(ctrl2);
        last_quad_ctrl = None;
      }
      svgtypes::PathSegment::SmoothCurveTo { abs, x2, y2, x, y } => {
        let start = current;
        let ctrl1 = match last_cubic_ctrl {
          Some((px, py)) => (2.0 * current.0 - px, 2.0 * current.1 - py),
          None => current,
        };
        let ctrl2 = if abs {
          (x2 as f32, y2 as f32)
        } else {
          (current.0 + x2 as f32, current.1 + y2 as f32)
        };
        let end = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        flatten_cubic_bezier(start, ctrl1, ctrl2, end, &mut commands);
        current = end;
        last_cubic_ctrl = Some(ctrl2);
        last_quad_ctrl = None;
      }
      svgtypes::PathSegment::Quadratic { abs, x1, y1, x, y } => {
        let start = current;
        let ctrl = if abs {
          (x1 as f32, y1 as f32)
        } else {
          (current.0 + x1 as f32, current.1 + y1 as f32)
        };
        let end = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        flatten_quadratic_bezier(start, ctrl, end, &mut commands);
        current = end;
        last_quad_ctrl = Some(ctrl);
        last_cubic_ctrl = None;
      }
      svgtypes::PathSegment::SmoothQuadratic { abs, x, y } => {
        let start = current;
        let ctrl = match last_quad_ctrl {
          Some((px, py)) => (2.0 * current.0 - px, 2.0 * current.1 - py),
          None => current,
        };
        let end = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        flatten_quadratic_bezier(start, ctrl, end, &mut commands);
        current = end;
        last_quad_ctrl = Some(ctrl);
        last_cubic_ctrl = None;
      }
      svgtypes::PathSegment::EllipticalArc { abs, x, y, .. } => {
        let end = if abs {
          (x as f32, y as f32)
        } else {
          (current.0 + x as f32, current.1 + y as f32)
        };
        // Approximate arcs as straight lines for motion path purposes.
        commands.push(MotionPathCommand::LineTo(MotionPosition {
          x: Length::px(end.0),
          y: Length::px(end.1),
        }));
        current = end;
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
      svgtypes::PathSegment::ClosePath { .. } => {
        commands.push(MotionPathCommand::ClosePath);
        current = subpath_start;
        last_cubic_ctrl = None;
        last_quad_ctrl = None;
      }
    }
  }

  if commands.is_empty() {
    None
  } else {
    Some(commands)
  }
}

fn flatten_cubic_bezier(
  start: (f32, f32),
  ctrl1: (f32, f32),
  ctrl2: (f32, f32),
  end: (f32, f32),
  out: &mut Vec<MotionPathCommand>,
) {
  const STEPS: usize = 20;
  for step in 1..=STEPS {
    let t = step as f32 / STEPS as f32;
    let mt = 1.0 - t;
    let x = mt * mt * mt * start.0
      + 3.0 * mt * mt * t * ctrl1.0
      + 3.0 * mt * t * t * ctrl2.0
      + t * t * t * end.0;
    let y = mt * mt * mt * start.1
      + 3.0 * mt * mt * t * ctrl1.1
      + 3.0 * mt * t * t * ctrl2.1
      + t * t * t * end.1;
    out.push(MotionPathCommand::LineTo(MotionPosition {
      x: Length::px(x),
      y: Length::px(y),
    }));
  }
}

fn flatten_quadratic_bezier(
  start: (f32, f32),
  ctrl: (f32, f32),
  end: (f32, f32),
  out: &mut Vec<MotionPathCommand>,
) {
  const STEPS: usize = 20;
  for step in 1..=STEPS {
    let t = step as f32 / STEPS as f32;
    let mt = 1.0 - t;
    let x = mt * mt * start.0 + 2.0 * mt * t * ctrl.0 + t * t * end.0;
    let y = mt * mt * start.1 + 2.0 * mt * t * ctrl.1 + t * t * end.1;
    out.push(MotionPathCommand::LineTo(MotionPosition {
      x: Length::px(x),
      y: Length::px(y),
    }));
  }
}

fn parse_ray<'i, 't>(input: &mut Parser<'i, 't>) -> Result<Ray, cssparser::ParseError<'i, ()>> {
  let angle = parse_angle_value(input)
    .map_err(|_| input.new_error(BasicParseErrorKind::QualifiedRuleInvalid))?;
  let mut length: Option<Length> = None;
  let mut contain = false;

  while !input.is_exhausted() {
    if input
      .try_parse(|p| p.expect_ident_matching("contain"))
      .is_ok()
    {
      contain = true;
      continue;
    }

    if length.is_none() {
      if let Ok(len) = input.try_parse(parse_length_component) {
        length = Some(len);
        continue;
      }
    }

    let _ = input.try_parse(|p| p.expect_comma());
    break;
  }

  Ok(Ray {
    angle,
    length,
    contain,
  })
}

fn parse_offset_rotate(value: &PropertyValue) -> Option<OffsetRotate> {
  let raw = property_value_to_string(value)?;
  let mut input = ParserInput::new(&raw);
  let mut parser = Parser::new(&mut input);
  let mut auto = false;
  let mut reverse = false;

  if parser
    .try_parse(|p| p.expect_ident_matching("reverse"))
    .is_ok()
  {
    auto = true;
    reverse = true;
  } else if parser
    .try_parse(|p| p.expect_ident_matching("auto"))
    .is_ok()
  {
    auto = true;
  }

  // Allow optional second keyword when first was reverse
  if !auto
    && parser
      .try_parse(|p| p.expect_ident_matching("reverse"))
      .is_ok()
  {
    auto = true;
    reverse = true;
  } else if auto && !reverse {
    if parser
      .try_parse(|p| p.expect_ident_matching("reverse"))
      .is_ok()
    {
      reverse = true;
    }
  }

  let angle = parser.try_parse(parse_angle_value).unwrap_or(0.0);
  parser.expect_exhausted().ok()?;

  if auto {
    Some(OffsetRotate::Auto { angle, reverse })
  } else {
    Some(OffsetRotate::Angle(angle))
  }
}

fn parse_offset_anchor(value: &PropertyValue) -> Option<OffsetAnchor> {
  if let PropertyValue::Keyword(kw) = value {
    if kw.eq_ignore_ascii_case("auto") {
      return Some(OffsetAnchor::Auto);
    }
  }

  parse_transform_origin(value).map(|origin| OffsetAnchor::Position {
    x: origin.x,
    y: origin.y,
  })
}

fn parse_backface_visibility(kw: &str) -> Option<BackfaceVisibility> {
  match kw.to_ascii_lowercase().as_str() {
    "visible" => Some(BackfaceVisibility::Visible),
    "hidden" => Some(BackfaceVisibility::Hidden),
    _ => None,
  }
}

fn parse_will_change(value: &PropertyValue) -> Option<WillChange> {
  let text = will_change_value_as_string(value)?;
  parse_will_change_from_str(&text)
}

fn property_value_to_string(value: &PropertyValue) -> Option<String> {
  match value {
    PropertyValue::Keyword(kw) | PropertyValue::String(kw) => Some(kw.clone()),
    PropertyValue::Multiple(values) if !values.is_empty() => {
      let mut out = String::new();
      for token in values {
        match token {
          PropertyValue::Keyword(k) | PropertyValue::String(k) => {
            if !out.is_empty() && k != "," && k != "/" {
              out.push(' ');
            }
            out.push_str(k);
            if k == "," {
              out.push(' ');
            }
          }
          _ => return None,
        }
      }
      let trimmed = out.trim();
      if trimmed.is_empty() {
        None
      } else {
        Some(trimmed.to_string())
      }
    }
    _ => None,
  }
}

fn parse_container_type_keyword(text: &str) -> Option<ContainerType> {
  match text.to_ascii_lowercase().as_str() {
    "none" => Some(ContainerType::None),
    "normal" => Some(ContainerType::Normal),
    "size" => Some(ContainerType::Size),
    "inline-size" => Some(ContainerType::InlineSize),
    _ => None,
  }
}

fn parse_container_type_value(value: &PropertyValue) -> Option<ContainerType> {
  let text = property_value_to_string(value)?;
  let trimmed = text.trim();
  if trimmed.is_empty() {
    return None;
  }
  parse_container_type_keyword(trimmed)
}

fn parse_container_names_from_str(input: &str) -> Option<Vec<String>> {
  let trimmed = input.trim();
  if trimmed.is_empty() {
    return None;
  }
  if trimmed.eq_ignore_ascii_case("none") {
    return Some(Vec::new());
  }

  let mut names = Vec::new();
  for part in trimmed.split(',') {
    for name in part.split_whitespace() {
      let candidate = name.trim();
      if !candidate.is_empty() {
        names.push(candidate.to_string());
      }
    }
  }

  if names.is_empty() {
    None
  } else if names.iter().any(|n| n.eq_ignore_ascii_case("none")) {
    None
  } else {
    Some(names)
  }
}

fn parse_container_names(value: &PropertyValue) -> Option<Vec<String>> {
  let text = property_value_to_string(value)?;
  parse_container_names_from_str(&text)
}

fn parse_container_shorthand(
  value: &PropertyValue,
) -> Option<(Vec<String>, Option<ContainerType>)> {
  let text = property_value_to_string(value)?;
  let trimmed = text.trim();
  if trimmed.is_empty() {
    return None;
  }
  if trimmed.eq_ignore_ascii_case("none") {
    return Some((Vec::new(), Some(ContainerType::None)));
  }

  let mut parts = trimmed.splitn(2, '/');
  let left = parts.next().unwrap_or("").trim();
  let right = parts.next().map(|s| s.trim());

  let names = if left.is_empty() {
    Vec::new()
  } else {
    parse_container_names_from_str(left)?
  };
  let mut container_type = None;

  if let Some(right_part) = right {
    if !right_part.is_empty() {
      container_type = parse_container_type_keyword(right_part);
      container_type?;
    }
  } else if names.is_empty() && !left.is_empty() {
    container_type = parse_container_type_keyword(left);
    container_type?;
  }

  if names.is_empty() && container_type.is_none() {
    None
  } else {
    Some((names, container_type))
  }
}

fn parse_containment(value: &PropertyValue) -> Option<Containment> {
  let text = match value {
    PropertyValue::Keyword(k) | PropertyValue::String(k) => Some(k.clone()),
    PropertyValue::Multiple(values) if !values.is_empty() => {
      let mut out = String::new();
      for token in values {
        match token {
          PropertyValue::Keyword(k) => {
            if !out.is_empty() {
              out.push(' ');
            }
            out.push_str(k);
          }
          PropertyValue::String(s) => {
            if !out.is_empty() {
              out.push(' ');
            }
            out.push_str(s);
          }
          _ => return None,
        }
      }
      Some(out)
    }
    _ => None,
  }?;

  let trimmed = text.trim();
  if trimmed.is_empty() {
    return None;
  }

  let mut input = ParserInput::new(trimmed);
  let mut parser = Parser::new(&mut input);

  if parser
    .try_parse(|p| p.expect_ident_matching("none"))
    .is_ok()
  {
    parser.skip_whitespace();
    return if parser.is_exhausted() {
      Some(Containment::none())
    } else {
      None
    };
  }
  if parser
    .try_parse(|p| p.expect_ident_matching("strict"))
    .is_ok()
  {
    parser.skip_whitespace();
    return if parser.is_exhausted() {
      Some(Containment::strict())
    } else {
      None
    };
  }
  if parser
    .try_parse(|p| p.expect_ident_matching("content"))
    .is_ok()
  {
    parser.skip_whitespace();
    return if parser.is_exhausted() {
      Some(Containment::content())
    } else {
      None
    };
  }

  let mut size = false;
  let mut inline_size = false;
  let mut layout = false;
  let mut style = false;
  let mut paint = false;
  let mut saw_any = false;

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    let ident =
      match parser.try_parse(|p| p.expect_ident().map(|i| i.as_ref().to_ascii_lowercase())) {
        Ok(i) => i,
        Err(_) => return None,
      };
    match ident.as_str() {
      "size" => {
        size = true;
        saw_any = true;
      }
      "inline-size" => {
        inline_size = true;
        saw_any = true;
      }
      "layout" => {
        layout = true;
        saw_any = true;
      }
      "style" => {
        style = true;
        saw_any = true;
      }
      "paint" => {
        paint = true;
        saw_any = true;
      }
      _ => return None,
    }
    parser.skip_whitespace();
  }

  if saw_any {
    Some(Containment::with_flags(
      size,
      inline_size,
      layout,
      style,
      paint,
    ))
  } else {
    None
  }
}

fn will_change_value_as_string(value: &PropertyValue) -> Option<String> {
  match value {
    PropertyValue::Keyword(kw) | PropertyValue::String(kw) => Some(kw.clone()),
    PropertyValue::Multiple(values) => {
      let mut out = String::new();
      for token in values {
        match token {
          PropertyValue::Keyword(k) => {
            if !out.is_empty() && k != "," {
              out.push(' ');
            }
            out.push_str(k);
            if k == "," {
              out.push(' ');
            }
          }
          PropertyValue::String(s) => {
            if !out.is_empty() {
              out.push(' ');
            }
            out.push_str(s);
          }
          _ => return None,
        }
      }
      let trimmed = out.trim();
      if trimmed.is_empty() {
        None
      } else {
        Some(trimmed.to_string())
      }
    }
    _ => None,
  }
}

fn parse_will_change_from_str(text: &str) -> Option<WillChange> {
  let trimmed = text.trim();
  if trimmed.is_empty() {
    return None;
  }

  let mut input = ParserInput::new(trimmed);
  let mut parser = Parser::new(&mut input);

  if parser
    .try_parse(|p| p.expect_ident_matching("auto"))
    .is_ok()
  {
    parser.skip_whitespace();
    return if parser.is_exhausted() {
      Some(WillChange::Auto)
    } else {
      None
    };
  }

  let mut hints = Vec::new();
  while !parser.is_exhausted() {
    parser.skip_whitespace();
    let ident = parser.expect_ident().ok()?;
    let ident_lower = ident.as_ref().to_ascii_lowercase();
    match ident_lower.as_str() {
      "auto" => return None,
      "scroll-position" => hints.push(WillChangeHint::ScrollPosition),
      "contents" => hints.push(WillChangeHint::Contents),
      other => hints.push(WillChangeHint::Property(other.to_string())),
    }
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }
    parser.expect_comma().ok()?;
  }

  if hints.is_empty() {
    None
  } else {
    Some(WillChange::Hints(hints))
  }
}

fn parse_filter_list(value: &PropertyValue) -> Option<Vec<FilterFunction>> {
  let text = match value {
    PropertyValue::Keyword(kw) => kw.as_str(),
    PropertyValue::String(s) => s.as_str(),
    PropertyValue::Multiple(values) if values.len() == 1 => match &values[0] {
      PropertyValue::Keyword(kw) => kw.as_str(),
      PropertyValue::String(s) => s.as_str(),
      _ => return None,
    },
    _ => return None,
  };

  let trimmed = text.trim();
  if trimmed.eq_ignore_ascii_case("none") {
    return Some(Vec::new());
  }

  let mut input = ParserInput::new(trimmed);
  let mut parser = Parser::new(&mut input);
  let mut filters = Vec::new();

  while !parser.is_exhausted() {
    parser.skip_whitespace();
    if parser.is_exhausted() {
      break;
    }

    if let Ok(url) = parser.try_parse(|p| p.expect_url()) {
      filters.push(FilterFunction::Url(url.as_ref().to_string()));
      continue;
    }

    let func_name = match parser.next() {
      Ok(Token::Function(name)) => name.as_ref().to_ascii_lowercase(),
      _ => return None,
    };

    let parsed = parser
      .parse_nested_block(|block| parse_filter_function(&func_name, block))
      .ok()?;
    filters.push(parsed);
    parser.skip_whitespace();
  }

  Some(filters)
}

fn parse_filter_function<'i, 't>(
  name: &str,
  input: &mut Parser<'i, 't>,
) -> Result<FilterFunction, cssparser::ParseError<'i, ()>> {
  fn parse_number_or_percentage_with_default<'i, 't>(
    input: &mut Parser<'i, 't>,
    default: f32,
  ) -> Result<f32, cssparser::ParseError<'i, ()>> {
    input.skip_whitespace();
    if input.is_exhausted() {
      return Ok(default);
    }
    let v = parse_number_or_percentage(input)?;
    if v < 0.0 {
      return Err(input.new_custom_error(()));
    }
    input.skip_whitespace();
    if !input.is_exhausted() {
      return Err(input.new_custom_error(()));
    }
    Ok(v)
  }

  fn parse_filter_length<'i, 't>(
    input: &mut Parser<'i, 't>,
  ) -> Result<Length, cssparser::ParseError<'i, ()>> {
    let len = parse_length_component(input)?;
    if matches!(len.unit, LengthUnit::Percent) {
      return Err(input.new_custom_error(()));
    }
    Ok(len)
  }

  fn parse_non_negative_filter_length<'i, 't>(
    input: &mut Parser<'i, 't>,
  ) -> Result<Length, cssparser::ParseError<'i, ()>> {
    let len = parse_filter_length(input)?;
    if len.value < 0.0 {
      return Err(input.new_custom_error(()));
    }
    Ok(len)
  }

  match name {
    "url" => {
      input.skip_whitespace();
      let url = input
        .try_parse(|p| p.expect_url().map(|u| u.as_ref().to_string()))
        .or_else(|_| input.try_parse(|p| p.expect_string().map(|s| s.as_ref().to_string())))
        .or_else(|_| input.try_parse(|p| p.expect_ident_cloned().map(|s| s.to_string())))
        .map_err(|_| input.new_custom_error(()))?;
      input.skip_whitespace();
      if !input.is_exhausted() {
        return Err(input.new_custom_error(()));
      }
      Ok(FilterFunction::Url(url))
    }
    "blur" => {
      input.skip_whitespace();
      let len = if input.is_exhausted() {
        Length::px(0.0)
      } else {
        let len = parse_non_negative_filter_length(input)?;
        input.skip_whitespace();
        if !input.is_exhausted() {
          return Err(input.new_custom_error(()));
        }
        len
      };
      Ok(FilterFunction::Blur(len))
    }
    "brightness" => {
      let v = parse_number_or_percentage_with_default(input, 1.0)?;
      Ok(FilterFunction::Brightness(v))
    }
    "contrast" => {
      let v = parse_number_or_percentage_with_default(input, 1.0)?;
      Ok(FilterFunction::Contrast(v))
    }
    "grayscale" => {
      let v = parse_number_or_percentage_with_default(input, 1.0)?;
      Ok(FilterFunction::Grayscale(v))
    }
    "sepia" => {
      let v = parse_number_or_percentage_with_default(input, 1.0)?;
      Ok(FilterFunction::Sepia(v))
    }
    "saturate" => {
      let v = parse_number_or_percentage_with_default(input, 1.0)?;
      Ok(FilterFunction::Saturate(v))
    }
    "hue-rotate" => {
      input.skip_whitespace();
      let v = if input.is_exhausted() {
        0.0
      } else {
        let v = parse_angle_degrees(input)?;
        input.skip_whitespace();
        if !input.is_exhausted() {
          return Err(input.new_custom_error(()));
        }
        v
      };
      Ok(FilterFunction::HueRotate(v))
    }
    "invert" => {
      let v = parse_number_or_percentage_with_default(input, 1.0)?;
      Ok(FilterFunction::Invert(v))
    }
    "opacity" => {
      let v = parse_number_or_percentage_with_default(input, 1.0)?;
      Ok(FilterFunction::Opacity(v))
    }
    "drop-shadow" => parse_drop_shadow(input),
    _ => Err(input.new_custom_error(())),
  }
}

fn parse_number_or_percentage<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<f32, cssparser::ParseError<'i, ()>> {
  let location = input.current_source_location();
  match input.next()? {
    Token::Number { value, .. } => Ok(*value),
    Token::Percentage { unit_value, .. } => Ok(*unit_value),
    _ => Err(location.new_custom_error(())),
  }
}

fn validate_oblique_angle(angle: f32) -> Option<f32> {
  // CSS Fonts: oblique angles must be between -90deg and 90deg.
  if angle >= -90.0 && angle <= 90.0 {
    Some(angle)
  } else {
    None
  }
}

fn parse_angle_token<'i, 't>(input: &mut Parser<'i, 't>) -> Option<f32> {
  parse_angle_degrees(input)
    .ok()
    .and_then(validate_oblique_angle)
}

fn parse_font_style_keyword(raw: &str) -> Option<FontStyle> {
  let lower = raw.trim().to_ascii_lowercase();
  if lower == "normal" {
    return Some(FontStyle::Normal);
  }
  if lower == "italic" {
    return Some(FontStyle::Italic);
  }
  if lower.starts_with("oblique") {
    let angle_part = lower.trim_start_matches("oblique").trim();
    if angle_part.is_empty() {
      return Some(FontStyle::Oblique(None));
    }
    return parse_angle_from_str(angle_part).map(|angle| FontStyle::Oblique(Some(angle)));
  }
  None
}

fn parse_angle_from_str(s: &str) -> Option<f32> {
  let mut input = ParserInput::new(s.trim());
  let mut parser = Parser::new(&mut input);
  parse_angle_degrees(&mut parser)
    .ok()
    .and_then(validate_oblique_angle)
}

fn parse_font_stretch_keyword(kw: &str) -> Option<FontStretch> {
  match kw {
    "ultra-condensed" => Some(FontStretch::UltraCondensed),
    "extra-condensed" => Some(FontStretch::ExtraCondensed),
    "condensed" => Some(FontStretch::Condensed),
    "semi-condensed" => Some(FontStretch::SemiCondensed),
    "normal" => Some(FontStretch::Normal),
    "semi-expanded" => Some(FontStretch::SemiExpanded),
    "expanded" => Some(FontStretch::Expanded),
    "extra-expanded" => Some(FontStretch::ExtraExpanded),
    "ultra-expanded" => Some(FontStretch::UltraExpanded),
    _ => None,
  }
}

fn parse_font_shorthand(
  value: &str,
  parent_font_size: f32,
  root_font_size: f32,
  viewport: crate::geometry::Size,
) -> Option<(
  FontStyle,
  FontWeight,
  FontVariant,
  FontStretch,
  f32,
  LineHeight,
  Vec<String>,
)> {
  let trimmed = value.trim();
  if trimmed.is_empty() {
    return None;
  }

  // `inherit` leaves inherited values in place (styles are inherited before declarations apply)
  if trimmed.eq_ignore_ascii_case("inherit") {
    return None;
  }

  // `initial`/`revert` -> reset to initial font values
  if trimmed.eq_ignore_ascii_case("initial") || trimmed.eq_ignore_ascii_case("revert") {
    let defaults = ComputedStyle::default();
    return Some((
      defaults.font_style,
      defaults.font_weight,
      defaults.font_variant,
      defaults.font_stretch,
      defaults.font_size,
      defaults.line_height.clone(),
      defaults.font_family.clone(),
    ));
  }

  let mut input = ParserInput::new(trimmed);
  let mut parser = Parser::new(&mut input);

  enum Phase {
    PreSize,
    AfterSize,
    AfterSlash,
  }

  let mut phase = Phase::PreSize;
  let mut font_style: Option<FontStyle> = None;
  let mut font_weight: Option<FontWeight> = None;
  let mut font_variant: Option<FontVariant> = None;
  let mut font_stretch: Option<FontStretch> = None;
  let mut font_size: Option<f32> = None;
  let mut line_height: Option<LineHeight> = None;
  let mut families: Vec<String> = Vec::new();
  let mut current_family: Vec<String> = Vec::new();

  loop {
    let token = match parser.next() {
      Ok(t) => t,
      Err(_) => break,
    };
    match phase {
      Phase::PreSize => {
        if matches!(token, Token::Delim('/')) {
          return None; // slash before size is invalid
        }

        if font_size.is_none() {
          if font_size.is_none() {
            if let Token::Function(ref name) = token {
              if name.eq_ignore_ascii_case("calc")
                || name.eq_ignore_ascii_case("min")
                || name.eq_ignore_ascii_case("max")
                || name.eq_ignore_ascii_case("clamp")
              {
                let parsed = parse_calc_function_length(&mut parser);
                match parsed {
                  Ok(len) if len.value >= 0.0 => {
                    if let Some(sz) =
                      resolve_font_size_length(len, parent_font_size, root_font_size, viewport)
                    {
                      font_size = Some(sz);
                      phase = Phase::AfterSize;
                      continue;
                    } else {
                      return None;
                    }
                  }
                  _ => return None,
                }
              }
            }

            if let Some(sz) =
              parse_font_size_token(&token, parent_font_size, root_font_size, viewport)
            {
              font_size = Some(sz);
              phase = Phase::AfterSize;
              continue;
            }
          }
        }

        if let Token::Ident(ref ident) = token {
          let ident = ident.as_ref();
          match ident {
            "normal" => {
              if font_style.is_none() {
                font_style = Some(FontStyle::Normal);
              } else if font_weight.is_none() {
                font_weight = Some(FontWeight::Normal);
              } else if font_stretch.is_none() {
                font_stretch = Some(FontStretch::Normal);
              }
            }
            "small-caps" => font_variant = Some(FontVariant::SmallCaps),
            "italic" => font_style = Some(FontStyle::Italic),
            "oblique" => {
              font_style = Some(FontStyle::Oblique(None));
              match parser.try_parse(|p| Ok::<_, cssparser::ParseError<()>>(parse_angle_token(p))) {
                Ok(Some(angle)) => font_style = Some(FontStyle::Oblique(Some(angle))),
                Ok(None) => return None, // invalid angle token consumed
                Err(_) => {}
              }
            }
            "bold" => font_weight = Some(FontWeight::Bold),
            "bolder" => font_weight = Some(FontWeight::Bolder),
            "lighter" => font_weight = Some(FontWeight::Lighter),
            _ => {
              if font_stretch.is_none() {
                if let Some(stretch) = parse_font_stretch_keyword(ident) {
                  font_stretch = Some(stretch);
                }
              }
              if font_variant.is_none() {
                if ident == "normal" {
                  font_variant = Some(FontVariant::Normal);
                }
              }
            }
          }
        } else if let Token::Number { value, .. } = token {
          if font_weight.is_none() && *value >= 1.0 && *value <= 1000.0 {
            font_weight = Some(FontWeight::Number((*value as u16).clamp(1, 1000)));
          }
        } else if let Token::Percentage { unit_value, .. } = token {
          if font_stretch.is_none() {
            font_stretch = Some(FontStretch::from_percentage(*unit_value * 100.0));
          }
        } else if let Token::Dimension { ref unit, .. } = token {
          // Oblique angles are allowed; ignore them for now.
          let u = unit.as_ref();
          if matches!(u, "deg" | "grad" | "rad" | "turn") {
            continue;
          }
        }
      }
      Phase::AfterSlash => match token {
        Token::Function(ref name)
          if name.eq_ignore_ascii_case("calc")
            || name.eq_ignore_ascii_case("min")
            || name.eq_ignore_ascii_case("max")
            || name.eq_ignore_ascii_case("clamp") =>
        {
          // Parse calc()/min()/max()/clamp() as a length/percentage line-height.
          let parsed = parse_calc_function_length(&mut parser);
          match parsed {
            Ok(len) if len.value >= 0.0 => {
              if len.unit == LengthUnit::Percent {
                line_height = Some(LineHeight::Percentage(len.value));
              } else {
                line_height = Some(LineHeight::Length(len));
              }
              phase = Phase::AfterSize;
            }
            _ => return None,
          }
        }
        _ => {
          if let Some(parsed) = parse_line_height_token(&token) {
            line_height = Some(parsed);
            phase = Phase::AfterSize;
          } else {
            return None;
          }
        }
      },
      Phase::AfterSize => match token {
        Token::Delim('/') => {
          if line_height.is_some() {
            return None; // multiple slashes not allowed
          }
          phase = Phase::AfterSlash;
        }
        Token::Comma => {
          if !current_family.is_empty() {
            families.push(current_family.join(" "));
            current_family.clear();
          }
        }
        Token::QuotedString(ref s) => {
          if !current_family.is_empty() {
            families.push(current_family.join(" "));
            current_family.clear();
          }
          families.push(s.as_ref().to_string());
        }
        Token::Ident(ref ident) => {
          current_family.push(ident.as_ref().to_string());
        }
        _ => {}
      },
    }
  }

  if matches!(phase, Phase::AfterSlash) && line_height.is_none() {
    return None;
  }

  if !current_family.is_empty() {
    families.push(current_family.join(" "));
  }

  if font_size.is_none() || families.is_empty() {
    return None;
  }

  Some((
    font_style.unwrap_or(FontStyle::Normal),
    font_weight.unwrap_or(FontWeight::Normal),
    font_variant.unwrap_or(FontVariant::Normal),
    font_stretch.unwrap_or(FontStretch::Normal),
    font_size.unwrap_or(parent_font_size),
    line_height.unwrap_or(LineHeight::Normal),
    families,
  ))
}

fn parse_font_size_token(
  token: &Token,
  parent_font_size: f32,
  root_font_size: f32,
  viewport: crate::geometry::Size,
) -> Option<f32> {
  if let Token::Ident(ref ident) = token {
    if let Some(size) = parse_font_size_keyword(ident.as_ref(), parent_font_size) {
      return Some(size);
    }
  }

  if let Some(len) = length_from_token(token) {
    return resolve_font_size_length(len, parent_font_size, root_font_size, viewport);
  }

  None
}

fn parse_line_height_token(token: &Token) -> Option<LineHeight> {
  match token {
    Token::Ident(ref ident) if ident.as_ref().eq_ignore_ascii_case("normal") => {
      Some(LineHeight::Normal)
    }
    Token::Number { value, .. } if *value >= 0.0 => Some(LineHeight::Number(*value)),
    Token::Percentage { unit_value, .. } if *unit_value >= 0.0 => {
      Some(LineHeight::Percentage(*unit_value * 100.0))
    }
    _ => length_from_token(token).and_then(|len| {
      if len.value < 0.0 {
        return None;
      }
      if len.unit == LengthUnit::Percent {
        Some(LineHeight::Percentage(len.value))
      } else {
        Some(LineHeight::Length(len))
      }
    }),
  }
}

fn parse_font_size_keyword(keyword: &str, parent_font_size: f32) -> Option<f32> {
  let scale: f32 = 1.2;
  let medium = 16.0;

  match keyword {
    "xx-small" => Some(medium / scale.powi(3)),
    "x-small" => Some(medium / scale.powi(2)),
    "small" => Some(medium / scale),
    "medium" => Some(medium),
    "large" => Some(medium * scale),
    "x-large" => Some(medium * scale.powi(2)),
    "xx-large" => Some(medium * scale.powi(3)),
    "xxx-large" => Some(medium * scale.powi(4)),
    "larger" => Some(parent_font_size * scale),
    "smaller" => Some(parent_font_size / scale),
    _ => None,
  }
}

fn resolve_font_size_length(
  len: Length,
  parent_font_size: f32,
  root_font_size: f32,
  viewport: crate::geometry::Size,
) -> Option<f32> {
  if len.value < 0.0 {
    return None;
  }
  if len.unit.is_absolute() {
    return Some(len.to_px());
  }
  if len.unit == LengthUnit::Em {
    return Some(len.value * parent_font_size);
  }
  if len.unit == LengthUnit::Rem {
    return Some(len.value * root_font_size);
  }
  if len.unit == LengthUnit::Percent {
    return Some((len.value / 100.0) * parent_font_size);
  }
  if len.unit.is_viewport_relative() {
    return len.resolve_with_viewport(viewport.width, viewport.height);
  }
  if len.unit == LengthUnit::Ex {
    return Some(len.value * parent_font_size * 0.5);
  }
  if len.unit == LengthUnit::Ch {
    return Some(len.value * parent_font_size * 0.5);
  }
  None
}

fn length_from_token(token: &Token) -> Option<Length> {
  match token {
    Token::Dimension {
      value, ref unit, ..
    } => match unit.as_ref() {
      "px" => Some(Length::px(*value)),
      "em" => Some(Length::em(*value)),
      "rem" => Some(Length::rem(*value)),
      "pt" => Some(Length::pt(*value)),
      "pc" => Some(Length::pc(*value)),
      "in" => Some(Length::inches(*value)),
      "cm" => Some(Length::cm(*value)),
      "mm" => Some(Length::mm(*value)),
      "vh" => Some(Length::new(*value, LengthUnit::Vh)),
      "vw" => Some(Length::new(*value, LengthUnit::Vw)),
      "vmin" => Some(Length::new(*value, LengthUnit::Vmin)),
      "vmax" => Some(Length::new(*value, LengthUnit::Vmax)),
      "dvh" => Some(Length::new(*value, LengthUnit::Dvh)),
      "dvw" => Some(Length::new(*value, LengthUnit::Dvw)),
      "dvmin" => Some(Length::new(*value, LengthUnit::Dvmin)),
      "dvmax" => Some(Length::new(*value, LengthUnit::Dvmax)),
      "%" => Some(Length::percent(*value)),
      _ => None,
    },
    Token::Percentage { unit_value, .. } => Some(Length::percent(*unit_value * 100.0)),
    Token::Number { value, .. } if *value == 0.0 => Some(Length::px(0.0)),
    _ => None,
  }
}

fn parse_angle_degrees<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<f32, cssparser::ParseError<'i, ()>> {
  crate::css::properties::parse_angle_component(input).map_err(|_| input.new_custom_error(()))
}

fn parse_length_component<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<Length, cssparser::ParseError<'i, ()>> {
  let location = input.current_source_location();
  match input.next()? {
    Token::Dimension {
      value, ref unit, ..
    } => {
      let unit = unit.as_ref().to_ascii_lowercase();
      let len = match unit.as_str() {
        "px" => Length::px(*value),
        "em" => Length::em(*value),
        "rem" => Length::rem(*value),
        "ex" => Length::ex(*value),
        "ch" => Length::ch(*value),
        "pt" => Length::pt(*value),
        "pc" => Length::pc(*value),
        "in" => Length::inches(*value),
        "cm" => Length::cm(*value),
        "mm" => Length::mm(*value),
        "q" => Length::q(*value),
        "vw" => Length::new(*value, LengthUnit::Vw),
        "vh" => Length::new(*value, LengthUnit::Vh),
        "vmin" => Length::new(*value, LengthUnit::Vmin),
        "vmax" => Length::new(*value, LengthUnit::Vmax),
        _ => return Err(location.new_custom_error(())),
      };
      Ok(len)
    }
    Token::Function(ref name) if name.eq_ignore_ascii_case("calc") => {
      crate::css::properties::parse_calc_function_length(input)
    }
    Token::Function(ref name) if name.eq_ignore_ascii_case("min") => {
      crate::css::properties::parse_min_max_function_length(
        input,
        crate::css::properties::MathFn::Min,
      )
    }
    Token::Function(ref name) if name.eq_ignore_ascii_case("max") => {
      crate::css::properties::parse_min_max_function_length(
        input,
        crate::css::properties::MathFn::Max,
      )
    }
    Token::Function(ref name) if name.eq_ignore_ascii_case("clamp") => {
      crate::css::properties::parse_clamp_function_length(input)
    }
    Token::Percentage { unit_value, .. } => Ok(Length::percent(*unit_value * 100.0)),
    Token::Number { value, .. } if *value == 0.0 => Ok(Length::px(0.0)),
    _ => Err(location.new_custom_error(())),
  }
}

fn parse_css_color_value<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<FilterColor, cssparser::ParseError<'i, ()>> {
  let location = input.current_source_location();
  let token = input.next()?;
  let raw = match token {
    Token::Ident(ref ident) => ident.as_ref().to_string(),
    Token::Hash(ref value) | Token::IDHash(ref value) => format!("#{}", value),
    Token::Function(ref name) => {
      let func = name.as_ref().to_string();
      let inner =
        input.parse_nested_block(|block| Ok(block.slice_from(block.position()).to_string()))?;
      format!("{}({})", func, inner)
    }
    _ => return Err(location.new_custom_error(())),
  };

  if raw.eq_ignore_ascii_case("currentcolor") {
    return Ok(FilterColor::CurrentColor);
  }

  let parsed =
    crate::style::color::Color::parse(&raw).map_err(|_| location.new_custom_error(()))?;
  Ok(FilterColor::Color(parsed.to_rgba(Rgba::BLACK)))
}

fn parse_drop_shadow<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<FilterFunction, cssparser::ParseError<'i, ()>> {
  let mut lengths = Vec::new();
  let mut color: Option<FilterColor> = None;

  while !input.is_exhausted() {
    input.skip_whitespace();
    if input.is_exhausted() {
      break;
    }

    if color.is_none() {
      if let Ok(c) = input.try_parse(parse_css_color_value) {
        color = Some(c);
        continue;
      }
    }

    if let Ok(len) = input.try_parse(parse_filter_function_length) {
      lengths.push(len);
      continue;
    }

    // Unexpected token
    let _ = input.next();
    return Err(input.new_custom_error(()));
  }

  if lengths.len() < 2 {
    return Err(input.new_custom_error(()));
  }

  if lengths.len() > 4 {
    return Err(input.new_custom_error(()));
  }

  let blur = lengths.get(2).copied().unwrap_or_else(|| Length::px(0.0));
  let spread = lengths.get(3).copied().unwrap_or_else(|| Length::px(0.0));
  if blur.value < 0.0 {
    return Err(input.new_custom_error(()));
  }

  Ok(FilterFunction::DropShadow(Box::new(FilterShadow {
    offset_x: lengths[0],
    offset_y: lengths[1],
    blur_radius: blur,
    spread,
    color: color.unwrap_or(FilterColor::CurrentColor),
  })))
}

fn parse_filter_function_length<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<Length, cssparser::ParseError<'i, ()>> {
  let len = parse_length_component(input)?;
  if matches!(len.unit, LengthUnit::Percent) {
    return Err(input.new_custom_error(()));
  }
  Ok(len)
}

fn parse_mix_blend_mode(kw: &str) -> Option<MixBlendMode> {
  match kw {
    "normal" => Some(MixBlendMode::Normal),
    "multiply" => Some(MixBlendMode::Multiply),
    "screen" => Some(MixBlendMode::Screen),
    "overlay" => Some(MixBlendMode::Overlay),
    "darken" => Some(MixBlendMode::Darken),
    "lighten" => Some(MixBlendMode::Lighten),
    "color-dodge" => Some(MixBlendMode::ColorDodge),
    "color-burn" => Some(MixBlendMode::ColorBurn),
    "hard-light" => Some(MixBlendMode::HardLight),
    "soft-light" => Some(MixBlendMode::SoftLight),
    "difference" => Some(MixBlendMode::Difference),
    "exclusion" => Some(MixBlendMode::Exclusion),
    "hue" => Some(MixBlendMode::Hue),
    "saturation" => Some(MixBlendMode::Saturation),
    "color" => Some(MixBlendMode::Color),
    "luminosity" => Some(MixBlendMode::Luminosity),
    "plus-lighter" => Some(MixBlendMode::PlusLighter),
    "plus-darker" => Some(MixBlendMode::PlusDarker),
    "hue-hsv" | "hue-hsb" => Some(MixBlendMode::HueHsv),
    "saturation-hsv" | "saturation-hsb" => Some(MixBlendMode::SaturationHsv),
    "color-hsv" | "color-hsb" => Some(MixBlendMode::ColorHsv),
    "luminosity-hsv" | "luminosity-hsb" => Some(MixBlendMode::LuminosityHsv),
    "hue-oklch" => Some(MixBlendMode::HueOklch),
    "saturation-oklch" | "chroma-oklch" => Some(MixBlendMode::ChromaOklch),
    "color-oklch" => Some(MixBlendMode::ColorOklch),
    "luminosity-oklch" | "lightness-oklch" => Some(MixBlendMode::LuminosityOklch),
    _ => None,
  }
}

fn parse_background_box(value: &PropertyValue) -> Option<BackgroundBox> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "border-box" => Some(BackgroundBox::BorderBox),
      "padding-box" => Some(BackgroundBox::PaddingBox),
      "content-box" => Some(BackgroundBox::ContentBox),
      _ => None,
    },
    _ => None,
  }
}

fn parse_repeat_keyword(kw: &str) -> Option<BackgroundRepeatKeyword> {
  match kw {
    "repeat" => Some(BackgroundRepeatKeyword::Repeat),
    "space" => Some(BackgroundRepeatKeyword::Space),
    "round" => Some(BackgroundRepeatKeyword::Round),
    "no-repeat" => Some(BackgroundRepeatKeyword::NoRepeat),
    _ => None,
  }
}

fn parse_background_repeat(value: &PropertyValue) -> Option<BackgroundRepeat> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "repeat-x" => Some(BackgroundRepeat::repeat_x()),
      "repeat-y" => Some(BackgroundRepeat::repeat_y()),
      _ => parse_repeat_keyword(kw).map(|k| BackgroundRepeat { x: k, y: k }),
    },
    PropertyValue::Multiple(values) if values.len() == 2 => {
      if let (PropertyValue::Keyword(x_kw), PropertyValue::Keyword(y_kw)) = (&values[0], &values[1])
      {
        let x = parse_repeat_keyword(x_kw)?;
        let y = parse_repeat_keyword(y_kw)?;
        Some(BackgroundRepeat { x, y })
      } else {
        None
      }
    }
    _ => None,
  }
}

fn parse_mask_mode(value: &PropertyValue) -> Option<MaskMode> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "alpha" => Some(MaskMode::Alpha),
      "luminance" => Some(MaskMode::Luminance),
      "match-source" => Some(MaskMode::Alpha),
      _ => None,
    },
    _ => None,
  }
}

fn parse_mask_origin(value: &PropertyValue) -> Option<MaskOrigin> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "border-box" => Some(MaskOrigin::BorderBox),
      "padding-box" => Some(MaskOrigin::PaddingBox),
      "content-box" => Some(MaskOrigin::ContentBox),
      _ => None,
    },
    _ => None,
  }
}

fn parse_mask_clip(value: &PropertyValue) -> Option<MaskClip> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "border-box" => Some(MaskClip::BorderBox),
      "padding-box" => Some(MaskClip::PaddingBox),
      "content-box" => Some(MaskClip::ContentBox),
      "text" => Some(MaskClip::Text),
      "no-clip" => Some(MaskClip::NoClip),
      _ => None,
    },
    _ => None,
  }
}

fn parse_mask_composite(value: &PropertyValue) -> Option<MaskComposite> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "add" | "source-over" | "src-over" => Some(MaskComposite::Add),
      "subtract" | "source-out" | "src-out" => Some(MaskComposite::Subtract),
      "intersect" | "source-in" | "src-in" => Some(MaskComposite::Intersect),
      "exclude" | "xor" => Some(MaskComposite::Exclude),
      _ => None,
    },
    _ => None,
  }
}

fn parse_background_position(value: &PropertyValue) -> Option<BackgroundPosition> {
  #[derive(Clone, Copy, PartialEq, Eq)]
  enum AxisKind {
    Horizontal,
    Vertical,
    Either, // center
  }

  #[derive(Clone, Copy)]
  enum Part {
    Keyword(AxisKind, f32),
    Offset(Length),
  }

  impl Part {
    fn as_offset(&self) -> Option<Length> {
      if let Part::Offset(l) = self {
        Some(*l)
      } else {
        None
      }
    }
  }

  fn classify(value: &PropertyValue) -> Option<Part> {
    match value {
      PropertyValue::Keyword(kw) => match kw.as_str() {
        "left" => Some(Part::Keyword(AxisKind::Horizontal, 0.0)),
        "right" => Some(Part::Keyword(AxisKind::Horizontal, 1.0)),
        "top" => Some(Part::Keyword(AxisKind::Vertical, 0.0)),
        "bottom" => Some(Part::Keyword(AxisKind::Vertical, 1.0)),
        "center" => Some(Part::Keyword(AxisKind::Either, 0.5)),
        _ => None,
      },
      PropertyValue::Length(l) => Some(Part::Offset(*l)),
      PropertyValue::Percentage(p) => Some(Part::Offset(Length::percent(*p))),
      PropertyValue::Number(n) if *n == 0.0 => Some(Part::Offset(Length::px(0.0))),
      _ => None,
    }
  }

  fn component_from_keyword(align: f32, offset: Option<Length>) -> BackgroundPositionComponent {
    // Percent-based zero so default offsets participate in percentage resolution where relevant.
    let mut off = offset.unwrap_or_else(|| Length::percent(0.0));
    if (align - 1.0).abs() < 1e-6 {
      off.value = -off.value;
    }
    BackgroundPositionComponent {
      alignment: align,
      offset: off,
    }
  }

  fn component_from_single(part: &Part, axis: AxisKind) -> Option<BackgroundPositionComponent> {
    match (part, axis) {
      (Part::Keyword(kind, align), _) if *kind == AxisKind::Either => {
        Some(component_from_keyword(*align, None))
      }
      (Part::Keyword(kind, align), AxisKind::Horizontal) if *kind == AxisKind::Horizontal => {
        Some(component_from_keyword(*align, None))
      }
      (Part::Keyword(kind, align), AxisKind::Vertical) if *kind == AxisKind::Vertical => {
        Some(component_from_keyword(*align, None))
      }
      (Part::Offset(len), AxisKind::Horizontal | AxisKind::Vertical) => {
        Some(component_from_keyword(0.0, Some(*len)))
      }
      _ => None,
    }
  }

  let parts: Vec<Part> = match value {
    PropertyValue::Multiple(values) if !values.is_empty() => {
      values.iter().filter_map(classify).collect()
    }
    other => classify(other).into_iter().collect(),
  };

  if parts.is_empty() || parts.len() > 4 {
    return None;
  }

  let mut x: Option<BackgroundPositionComponent> = None;
  let mut y: Option<BackgroundPositionComponent> = None;

  match parts.len() {
    1 => match parts[0] {
      Part::Keyword(AxisKind::Horizontal, align) => {
        x = Some(component_from_keyword(align, None));
        y = Some(component_from_keyword(0.5, None));
      }
      Part::Keyword(AxisKind::Vertical, align) => {
        y = Some(component_from_keyword(align, None));
        x = Some(component_from_keyword(0.5, None));
      }
      Part::Keyword(AxisKind::Either, align) => {
        x = Some(component_from_keyword(align, None));
        y = Some(component_from_keyword(align, None));
      }
      Part::Offset(len) => {
        x = Some(component_from_keyword(0.0, Some(len)));
        y = Some(component_from_keyword(0.5, None));
      }
    },
    2 => {
      // Two-value syntax: first is horizontal, second vertical; if order is vertical+horizontal, swap.
      let (first, second) = (&parts[0], &parts[1]);

      let first_is_vertical = matches!(first, Part::Keyword(AxisKind::Vertical, _));
      let second_is_horizontal = matches!(second, Part::Keyword(AxisKind::Horizontal, _));
      // center + horizontal keyword => treat first as vertical center, second as horizontal
      if matches!(first, Part::Keyword(AxisKind::Either, _)) && second_is_horizontal {
        x = component_from_single(second, AxisKind::Horizontal);
        y = Some(component_from_keyword(0.5, None));
      // center + vertical keyword => treat first as horizontal center, second as vertical
      } else if matches!(first, Part::Keyword(AxisKind::Either, _))
        && matches!(second, Part::Keyword(AxisKind::Vertical, _))
      {
        x = Some(component_from_keyword(0.5, None));
        y = component_from_single(second, AxisKind::Vertical);
      } else if first_is_vertical && second_is_horizontal {
        y = component_from_single(first, AxisKind::Vertical);
        x = component_from_single(second, AxisKind::Horizontal);
      } else {
        let first_h = component_from_single(first, AxisKind::Horizontal);
        let first_v = component_from_single(first, AxisKind::Vertical);
        let second_h = component_from_single(second, AxisKind::Horizontal);
        let second_v = component_from_single(second, AxisKind::Vertical);

        // Ambiguous center handling: if first is center and second is a horizontal-only keyword,
        // treat the first as vertical center and second as horizontal (center left -> left center).
        if first_v.is_none() && first_h.is_some() && second_h.is_some() && second_v.is_none() {
          x = second_h;
          y = Some(first_v.unwrap_or_else(|| component_from_keyword(0.5, None)));
        } else if first_v.is_some() && second_h.is_none() && second_v.is_some() {
          // first vertical only, second vertical or center -> treat first vertical, second becomes horizontal center.
          y = first_v;
          x = Some(second_h.unwrap_or_else(|| component_from_keyword(0.5, None)));
        } else {
          x = first_h.or(second_h);
          y = second_v.or(first_v);
        }
      }
      if x.is_none() {
        x = Some(component_from_keyword(0.5, None));
      }
      if y.is_none() {
        y = Some(component_from_keyword(0.5, None));
      }
    }
    3 => {
      // One axis has keyword + offset, the other a single component.
      let a = &parts[0];
      let b = &parts[1];
      let c = &parts[2];

      // Horizontal pair first
      if matches!(a, Part::Keyword(AxisKind::Horizontal | AxisKind::Either, _))
        && matches!(b, Part::Offset(_))
      {
        if let Part::Keyword(_, align) = a {
          x = Some(component_from_keyword(*align, b.as_offset()));
        }
        y = component_from_single(c, AxisKind::Vertical);
      }

      // Vertical pair first
      if x.is_none()
        && matches!(a, Part::Keyword(AxisKind::Vertical | AxisKind::Either, _))
        && matches!(b, Part::Offset(_))
      {
        if let Part::Keyword(_, align) = a {
          y = Some(component_from_keyword(*align, b.as_offset()));
        }
        x = component_from_single(c, AxisKind::Horizontal);
      }
    }
    4 => {
      let a = &parts[0];
      let b = &parts[1];
      let c = &parts[2];
      let d = &parts[3];

      // horizontal pair then vertical pair
      if matches!(a, Part::Keyword(AxisKind::Horizontal | AxisKind::Either, _))
        && matches!(b, Part::Offset(_))
        && matches!(c, Part::Keyword(AxisKind::Vertical | AxisKind::Either, _))
        && matches!(d, Part::Offset(_))
      {
        if let Part::Keyword(_, align) = a {
          x = Some(component_from_keyword(*align, b.as_offset()));
        }
        if let Part::Keyword(_, align) = c {
          y = Some(component_from_keyword(*align, d.as_offset()));
        }
      } else if matches!(a, Part::Keyword(AxisKind::Vertical | AxisKind::Either, _))
        && matches!(b, Part::Offset(_))
        && matches!(c, Part::Keyword(AxisKind::Horizontal | AxisKind::Either, _))
        && matches!(d, Part::Offset(_))
      {
        if let Part::Keyword(_, align) = a {
          y = Some(component_from_keyword(*align, b.as_offset()));
        }
        if let Part::Keyword(_, align) = c {
          x = Some(component_from_keyword(*align, d.as_offset()));
        }
      }
    }
    _ => {}
  }

  let x = x.unwrap_or_else(|| component_from_keyword(0.5, None));
  let y = y.unwrap_or_else(|| component_from_keyword(0.5, None));

  Some(BackgroundPosition::Position { x, y })
}

fn parse_background_position_component_x(
  value: &PropertyValue,
) -> Option<BackgroundPositionComponent> {
  match value {
    PropertyValue::Keyword(kw) => match kw.to_ascii_lowercase().as_str() {
      "left" => Some(BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::px(0.0),
      }),
      "right" => Some(BackgroundPositionComponent {
        alignment: 1.0,
        offset: Length::px(0.0),
      }),
      "center" => Some(BackgroundPositionComponent {
        alignment: 0.5,
        offset: Length::px(0.0),
      }),
      _ => None,
    },
    PropertyValue::Length(len) => Some(BackgroundPositionComponent {
      alignment: 0.0,
      offset: *len,
    }),
    PropertyValue::Percentage(p) => Some(BackgroundPositionComponent {
      alignment: 0.0,
      offset: Length::percent(*p),
    }),
    PropertyValue::Number(n) if *n == 0.0 => Some(BackgroundPositionComponent {
      alignment: 0.0,
      offset: Length::px(0.0),
    }),
    _ => None,
  }
}

fn parse_background_position_component_y(
  value: &PropertyValue,
) -> Option<BackgroundPositionComponent> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "top" => Some(BackgroundPositionComponent {
        alignment: 0.0,
        offset: Length::px(0.0),
      }),
      "bottom" => Some(BackgroundPositionComponent {
        alignment: 1.0,
        offset: Length::px(0.0),
      }),
      "center" => Some(BackgroundPositionComponent {
        alignment: 0.5,
        offset: Length::px(0.0),
      }),
      _ => None,
    },
    PropertyValue::Length(len) => Some(BackgroundPositionComponent {
      alignment: 0.0,
      offset: *len,
    }),
    PropertyValue::Percentage(p) => Some(BackgroundPositionComponent {
      alignment: 0.0,
      offset: Length::percent(*p),
    }),
    PropertyValue::Number(n) if *n == 0.0 => Some(BackgroundPositionComponent {
      alignment: 0.0,
      offset: Length::px(0.0),
    }),
    _ => None,
  }
}

fn parse_clip_path_value(value: &PropertyValue) -> Option<ClipPath> {
  match value {
    PropertyValue::Keyword(kw) => parse_clip_path_str(kw),
    PropertyValue::Multiple(parts) => {
      let mut joined = String::new();
      for (idx, part) in parts.iter().enumerate() {
        let token = match part {
          PropertyValue::Keyword(k) => k.as_str(),
          _ => return None,
        };
        if idx > 0 {
          joined.push(' ');
        }
        joined.push_str(token);
      }
      if joined.is_empty() {
        None
      } else {
        parse_clip_path_str(&joined)
      }
    }
    _ => None,
  }
}

#[allow(clippy::option_option)]
fn parse_clip_value(value: &PropertyValue) -> Option<Option<ClipRect>> {
  match value {
    PropertyValue::Keyword(raw) => {
      let mut input = ParserInput::new(raw);
      let mut parser = Parser::new(&mut input);

      if parser
        .try_parse(|p| p.expect_ident_matching("auto"))
        .is_ok()
      {
        return Some(None);
      }

      if parser
        .try_parse(|p| p.expect_function_matching("rect"))
        .is_ok()
      {
        return parser
          .parse_nested_block(|p| -> Result<_, cssparser::ParseError<'_, ()>> {
            let top = match parse_clip_component(p) {
              Some(c) => c,
              None => return Ok(None),
            };
            let _ = p.try_parse(|p| p.expect_comma());
            let right = match parse_clip_component(p) {
              Some(c) => c,
              None => return Ok(None),
            };
            let _ = p.try_parse(|p| p.expect_comma());
            let bottom = match parse_clip_component(p) {
              Some(c) => c,
              None => return Ok(None),
            };
            let _ = p.try_parse(|p| p.expect_comma());
            let left = match parse_clip_component(p) {
              Some(c) => c,
              None => return Ok(None),
            };
            Ok(Some(ClipRect {
              top,
              right,
              bottom,
              left,
            }))
          })
          .ok()
          .flatten()
          .map(Some);
      } else {
        None
      }
    }
    _ => None,
  }
}

fn parse_clip_component(parser: &mut Parser<'_, '_>) -> Option<ClipComponent> {
  if let Ok(len) = parser.try_parse(|p| parse_length_component(p)) {
    return Some(ClipComponent::Length(len));
  }

  if let Ok(ident) = parser.try_parse(|p| p.expect_ident_cloned()) {
    if ident.eq_ignore_ascii_case("auto") {
      return Some(ClipComponent::Auto);
    }
  }

  None
}

fn parse_clip_path_str(input_str: &str) -> Option<ClipPath> {
  let mut input = ParserInput::new(input_str);
  let mut parser = Parser::new(&mut input);

  if parser
    .try_parse(|p| p.expect_ident_matching("none"))
    .is_ok()
  {
    parser.expect_exhausted().ok()?;
    return Some(ClipPath::None);
  }

  if let Ok(shape) = parser.try_parse(parse_basic_shape) {
    let reference = parser.try_parse(parse_reference_box).ok();
    parser.expect_exhausted().ok()?;
    return Some(ClipPath::BasicShape(Box::new(shape), reference));
  }

  if let Ok(reference) = parser.parse_entirely(parse_reference_box) {
    return Some(ClipPath::Box(reference));
  }

  None
}

fn parse_reference_box<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<ReferenceBox, cssparser::ParseError<'i, ()>> {
  let ident = input.expect_ident()?;
  match &*ident.to_ascii_lowercase() {
    "border-box" => Ok(ReferenceBox::BorderBox),
    "padding-box" => Ok(ReferenceBox::PaddingBox),
    "content-box" => Ok(ReferenceBox::ContentBox),
    "margin-box" => Ok(ReferenceBox::MarginBox),
    "fill-box" => Ok(ReferenceBox::FillBox),
    "stroke-box" => Ok(ReferenceBox::StrokeBox),
    "view-box" => Ok(ReferenceBox::ViewBox),
    _ => Err(cssparser::ParseError {
      kind: cssparser::ParseErrorKind::Custom(()),
      location: input.current_source_location(),
    }),
  }
}

fn parse_basic_shape<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<BasicShape, cssparser::ParseError<'i, ()>> {
  if let Ok(inset) = input.try_parse(parse_inset_shape) {
    return Ok(inset);
  }
  if let Ok(circle) = input.try_parse(parse_circle_shape) {
    return Ok(circle);
  }
  if let Ok(ellipse) = input.try_parse(parse_ellipse_shape) {
    return Ok(ellipse);
  }
  if let Ok(polygon) = input.try_parse(parse_polygon_shape) {
    return Ok(polygon);
  }

  Err(cssparser::ParseError {
    kind: cssparser::ParseErrorKind::Custom(()),
    location: input.current_source_location(),
  })
}

fn parse_inset_shape<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<BasicShape, cssparser::ParseError<'i, ()>> {
  input.expect_function_matching("inset")?;
  input.parse_nested_block(|nested| {
    let mut offsets = Vec::new();
    for _ in 0..4 {
      if let Ok(len) = nested.try_parse(parse_length_percentage_component) {
        offsets.push(len);
      } else {
        break;
      }
    }
    if offsets.is_empty() {
      return Err(cssparser::ParseError {
        kind: cssparser::ParseErrorKind::Custom(()),
        location: nested.current_source_location(),
      });
    }
    let offsets = match offsets.len() {
      1 => vec![offsets[0]; 4],
      2 => vec![offsets[0], offsets[1], offsets[0], offsets[1]],
      3 => vec![offsets[0], offsets[1], offsets[2], offsets[1]],
      _ => offsets.into_iter().take(4).collect(),
    };

    let radii = if nested
      .try_parse(|p| p.expect_ident_matching("round"))
      .is_ok()
    {
      Some(parse_clip_radii(nested)?)
    } else {
      None
    };

    nested.expect_exhausted()?;
    Ok(BasicShape::Inset {
      top: offsets[0],
      right: offsets[1],
      bottom: offsets[2],
      left: offsets[3],
      border_radius: Box::new(radii),
    })
  })
}

fn parse_circle_shape<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<BasicShape, cssparser::ParseError<'i, ()>> {
  input.expect_function_matching("circle")?;
  input.parse_nested_block(|nested| {
    let radius = nested
      .try_parse(parse_shape_radius)
      .unwrap_or(ShapeRadius::ClosestSide);

    let position = parse_clip_position(nested)?;
    nested.expect_exhausted()?;
    Ok(BasicShape::Circle { radius, position })
  })
}

fn parse_ellipse_shape<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<BasicShape, cssparser::ParseError<'i, ()>> {
  input.expect_function_matching("ellipse")?;
  input.parse_nested_block(|nested| {
    let radius_x = nested.try_parse(parse_shape_radius).ok();
    let radius_y = if let Ok(second) = nested.try_parse(parse_shape_radius) {
      Some(second)
    } else {
      radius_x
    };

    let position = parse_clip_position(nested)?;
    nested.expect_exhausted()?;
    Ok(BasicShape::Ellipse {
      radius_x: radius_x.unwrap_or(ShapeRadius::ClosestSide),
      radius_y: radius_y.unwrap_or(ShapeRadius::ClosestSide),
      position,
    })
  })
}

fn parse_polygon_shape<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<BasicShape, cssparser::ParseError<'i, ()>> {
  input.expect_function_matching("polygon")?;
  input.parse_nested_block(|nested| {
    let state = nested.state();
    let mut fill_rule = FillRule::NonZero;
    if let Ok(rule) = nested.try_parse(|p| p.expect_ident_cloned()) {
      match rule.to_ascii_lowercase().as_str() {
        "evenodd" => {
          fill_rule = FillRule::EvenOdd;
          let _ = nested.try_parse(|p| p.expect_comma());
        }
        "nonzero" => {
          fill_rule = FillRule::NonZero;
          let _ = nested.try_parse(|p| p.expect_comma());
        }
        _ => nested.reset(&state),
      }
    }

    let mut points = Vec::new();
    while !nested.is_exhausted() {
      let x = parse_length_percentage_component(nested)?;
      let y = parse_length_percentage_component(nested)?;
      points.push((x, y));
      if nested.try_parse(|p| p.expect_comma()).is_err() {
        break;
      }
    }

    if points.is_empty() {
      return Err(cssparser::ParseError {
        kind: cssparser::ParseErrorKind::Custom(()),
        location: nested.current_source_location(),
      });
    }
    nested.expect_exhausted()?;
    Ok(BasicShape::Polygon {
      fill: fill_rule,
      points,
    })
  })
}

fn parse_shape_radius<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<ShapeRadius, cssparser::ParseError<'i, ()>> {
  if let Ok(len) = input.try_parse(parse_length_percentage_component) {
    return Ok(ShapeRadius::Length(len));
  }

  let ident = input.expect_ident()?;
  match &*ident.to_ascii_lowercase() {
    "closest-side" => Ok(ShapeRadius::ClosestSide),
    "farthest-side" => Ok(ShapeRadius::FarthestSide),
    _ => Err(cssparser::ParseError {
      kind: cssparser::ParseErrorKind::Custom(()),
      location: input.current_source_location(),
    }),
  }
}

fn parse_clip_position<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<BackgroundPosition, cssparser::ParseError<'i, ()>> {
  let mut position_tokens = Vec::new();
  if input.try_parse(|p| p.expect_ident_matching("at")).is_ok() {
    while !input.is_exhausted() {
      let token = input.next()?;
      let pv = match token {
        Token::Ident(ident) => PropertyValue::Keyword(ident.to_string()),
        Token::Percentage { unit_value, .. } => PropertyValue::Percentage(unit_value * 100.0),
        Token::Dimension {
          value, ref unit, ..
        } => {
          let Some(unit) = length_unit_from_str(unit) else {
            return Err(cssparser::ParseError {
              kind: cssparser::ParseErrorKind::Custom(()),
              location: input.current_source_location(),
            });
          };
          PropertyValue::Length(Length::new(*value, unit))
        }
        Token::Number { value, .. } if *value == 0.0 => PropertyValue::Length(Length::px(0.0)),
        Token::Function(ref name) if name.eq_ignore_ascii_case("calc") => {
          let len = input.parse_nested_block(parse_calc_function_length)?;
          PropertyValue::Length(len)
        }
        _ => {
          return Err(cssparser::ParseError {
            kind: cssparser::ParseErrorKind::Custom(()),
            location: input.current_source_location(),
          })
        }
      };
      position_tokens.push(pv);
    }
  }

  if position_tokens.is_empty() {
    return Ok(BackgroundPosition::Position {
      x: BackgroundPositionComponent {
        alignment: 0.5,
        offset: Length::px(0.0),
      },
      y: BackgroundPositionComponent {
        alignment: 0.5,
        offset: Length::px(0.0),
      },
    });
  }

  parse_background_position(&PropertyValue::Multiple(position_tokens)).ok_or(
    cssparser::ParseError {
      kind: cssparser::ParseErrorKind::Custom(()),
      location: input.current_source_location(),
    },
  )
}

fn parse_clip_radii<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<ClipRadii, cssparser::ParseError<'i, ()>> {
  let mut horizontal = Vec::new();
  for _ in 0..4 {
    if let Ok(len) = input.try_parse(parse_length_percentage_component) {
      horizontal.push(len);
    } else {
      break;
    }
  }

  if horizontal.is_empty() {
    return Err(cssparser::ParseError {
      kind: cssparser::ParseErrorKind::Custom(()),
      location: input.current_source_location(),
    });
  }

  let mut vertical = Vec::new();
  if input.try_parse(|p| p.expect_delim('/')).is_ok() {
    for _ in 0..4 {
      if let Ok(len) = input.try_parse(parse_length_percentage_component) {
        vertical.push(len);
      } else {
        break;
      }
    }
    if vertical.is_empty() {
      return Err(cssparser::ParseError {
        kind: cssparser::ParseErrorKind::Custom(()),
        location: input.current_source_location(),
      });
    }
  }

  if vertical.is_empty() {
    vertical = horizontal.clone();
  }

  let h = expand_corner_list(&horizontal).ok_or(cssparser::ParseError {
    kind: cssparser::ParseErrorKind::Custom(()),
    location: input.current_source_location(),
  })?;
  let v = expand_corner_list(&vertical).ok_or(cssparser::ParseError {
    kind: cssparser::ParseErrorKind::Custom(()),
    location: input.current_source_location(),
  })?;

  Ok(ClipRadii {
    top_left: BorderCornerRadius { x: h[0], y: v[0] },
    top_right: BorderCornerRadius { x: h[1], y: v[1] },
    bottom_right: BorderCornerRadius { x: h[2], y: v[2] },
    bottom_left: BorderCornerRadius { x: h[3], y: v[3] },
  })
}

fn parse_length_percentage_component<'i, 't>(
  input: &mut Parser<'i, 't>,
) -> Result<Length, cssparser::ParseError<'i, ()>> {
  let token = input.next()?;
  match token {
    Token::Percentage { unit_value, .. } => Ok(Length::percent(unit_value * 100.0)),
    Token::Dimension {
      value, ref unit, ..
    } => {
      let Some(unit) = length_unit_from_str(unit) else {
        return Err(cssparser::ParseError {
          kind: cssparser::ParseErrorKind::Custom(()),
          location: input.current_source_location(),
        });
      };
      Ok(Length::new(*value, unit))
    }
    Token::Number { value, .. } if *value == 0.0 => Ok(Length::px(0.0)),
    Token::Function(ref name) => {
      let owned = name.to_string();
      parse_function_length_token(&owned, input)
    }
    _ => Err(cssparser::ParseError {
      kind: cssparser::ParseErrorKind::Custom(()),
      location: input.current_source_location(),
    }),
  }
}

fn parse_function_length_token<'i, 't>(
  name: &str,
  input: &mut Parser<'i, 't>,
) -> Result<Length, cssparser::ParseError<'i, ()>> {
  if name.eq_ignore_ascii_case("calc") {
    input.parse_nested_block(parse_calc_function_length)
  } else {
    Err(cssparser::ParseError {
      kind: cssparser::ParseErrorKind::Custom(()),
      location: input.current_source_location(),
    })
  }
}

fn length_unit_from_str(unit: &str) -> Option<LengthUnit> {
  match unit.to_ascii_lowercase().as_str() {
    "px" => Some(LengthUnit::Px),
    "pt" => Some(LengthUnit::Pt),
    "pc" => Some(LengthUnit::Pc),
    "in" => Some(LengthUnit::In),
    "cm" => Some(LengthUnit::Cm),
    "mm" => Some(LengthUnit::Mm),
    "q" => Some(LengthUnit::Q),
    "em" => Some(LengthUnit::Em),
    "rem" => Some(LengthUnit::Rem),
    "ex" => Some(LengthUnit::Ex),
    "ch" => Some(LengthUnit::Ch),
    "vw" => Some(LengthUnit::Vw),
    "vh" => Some(LengthUnit::Vh),
    "vmin" => Some(LengthUnit::Vmin),
    "vmax" => Some(LengthUnit::Vmax),
    "dvw" => Some(LengthUnit::Dvw),
    "dvh" => Some(LengthUnit::Dvh),
    "dvmin" => Some(LengthUnit::Dvmin),
    "dvmax" => Some(LengthUnit::Dvmax),
    _ => None,
  }
}

fn parse_text_decoration_line(value: &PropertyValue) -> Option<TextDecorationLine> {
  let components: Vec<&PropertyValue> = match value {
    PropertyValue::Multiple(values) if !values.is_empty() => values.iter().collect(),
    _ => vec![value],
  };

  let mut lines = TextDecorationLine::NONE;
  let mut saw_none = false;

  for comp in components {
    if let PropertyValue::Keyword(kw) = comp {
      let kw = kw.to_ascii_lowercase();
      match kw.as_str() {
        "none" => {
          saw_none = true;
          lines = TextDecorationLine::NONE;
          break;
        }
        "underline" => lines.insert(TextDecorationLine::UNDERLINE),
        "overline" => lines.insert(TextDecorationLine::OVERLINE),
        "line-through" => lines.insert(TextDecorationLine::LINE_THROUGH),
        _ => {}
      }
    }
  }

  if saw_none {
    Some(TextDecorationLine::NONE)
  } else if lines.is_empty() {
    None
  } else {
    Some(lines)
  }
}

fn parse_text_decoration_style(value: &PropertyValue) -> Option<TextDecorationStyle> {
  match value {
    PropertyValue::Keyword(kw) => match kw.to_ascii_lowercase().as_str() {
      "solid" => Some(TextDecorationStyle::Solid),
      "double" => Some(TextDecorationStyle::Double),
      "dotted" => Some(TextDecorationStyle::Dotted),
      "dashed" => Some(TextDecorationStyle::Dashed),
      "wavy" => Some(TextDecorationStyle::Wavy),
      _ => None,
    },
    _ => None,
  }
}

#[allow(clippy::option_option)]
fn parse_text_decoration_color(value: &PropertyValue, current_color: Rgba) -> Option<Option<Rgba>> {
  match value {
    PropertyValue::Color(c) => Some(Some(c.to_rgba(current_color))),
    PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => Some(None),
    _ => None,
  }
}

fn parse_text_underline_offset(value: &PropertyValue) -> Option<TextUnderlineOffset> {
  match value {
    PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => {
      Some(TextUnderlineOffset::Auto)
    }
    PropertyValue::Length(l) => Some(TextUnderlineOffset::Length(*l)),
    PropertyValue::Percentage(p) => Some(TextUnderlineOffset::Length(Length::percent(*p))),
    _ => None,
  }
}

fn parse_text_underline_position(value: &PropertyValue) -> Option<TextUnderlinePosition> {
  enum Side {
    Left,
    Right,
  }

  let mut under = false;
  let mut side: Option<Side> = None;
  let mut from_font = false;
  let mut auto = false;

  let mut handle_keyword = |kw: &str| -> Option<()> {
    match kw {
      "auto" => {
        if auto || from_font || under || side.is_some() {
          return None;
        }
        auto = true;
      }
      "from-font" => {
        if auto || from_font || under || side.is_some() {
          return None;
        }
        from_font = true;
      }
      "under" => {
        if auto || from_font || under {
          return None;
        }
        under = true;
      }
      "left" => {
        if auto || from_font || side.is_some() {
          return None;
        }
        side = Some(Side::Left);
      }
      "right" => {
        if auto || from_font || side.is_some() {
          return None;
        }
        side = Some(Side::Right);
      }
      _ => return None,
    }
    Some(())
  };

  let keywords: Vec<String> = match value {
    PropertyValue::Multiple(values) => {
      let mut kws = Vec::new();
      for v in values {
        if let PropertyValue::Keyword(kw) = v {
          kws.extend(kw.split_whitespace().map(|s| s.to_ascii_lowercase()));
        } else {
          return None;
        }
      }
      kws
    }
    PropertyValue::Keyword(kw) => kw
      .split_whitespace()
      .map(|s| s.to_ascii_lowercase())
      .collect(),
    _ => return None,
  };

  if keywords.is_empty() {
    return None;
  }

  for kw in keywords {
    handle_keyword(&kw)?;
  }

  if auto {
    Some(TextUnderlinePosition::Auto)
  } else if from_font {
    Some(TextUnderlinePosition::FromFont)
  } else {
    match (under, side) {
      (true, Some(Side::Left)) => Some(TextUnderlinePosition::UnderLeft),
      (true, Some(Side::Right)) => Some(TextUnderlinePosition::UnderRight),
      (true, None) => Some(TextUnderlinePosition::Under),
      (false, Some(Side::Left)) => Some(TextUnderlinePosition::Left),
      (false, Some(Side::Right)) => Some(TextUnderlinePosition::Right),
      (false, None) => None,
    }
  }
}

fn parse_text_emphasis_style(value: &PropertyValue) -> Option<TextEmphasisStyle> {
  fn parse_keywords(values: &[&str]) -> Option<TextEmphasisStyle> {
    let mut fill = TextEmphasisFill::Filled;
    let mut shape: Option<TextEmphasisShape> = None;

    for kw in values {
      match *kw {
        "filled" => fill = TextEmphasisFill::Filled,
        "open" => fill = TextEmphasisFill::Open,
        "dot" => shape = Some(TextEmphasisShape::Dot),
        "circle" => shape = Some(TextEmphasisShape::Circle),
        "double-circle" => shape = Some(TextEmphasisShape::DoubleCircle),
        "triangle" => shape = Some(TextEmphasisShape::Triangle),
        "sesame" => shape = Some(TextEmphasisShape::Sesame),
        "none" => return Some(TextEmphasisStyle::None),
        _ => return None,
      }
    }

    shape.map(|s| TextEmphasisStyle::Mark { fill, shape: s })
  }

  match value {
    PropertyValue::Keyword(kw) => {
      let parts: Vec<&str> = kw.split_whitespace().collect();
      parse_keywords(&parts)
    }
    PropertyValue::Multiple(values) => {
      let mut parts = Vec::new();
      for v in values {
        match v {
          PropertyValue::Keyword(kw) => {
            parts.extend(kw.split_whitespace());
          }
          PropertyValue::String(s) if !s.is_empty() => {
            return Some(TextEmphasisStyle::String(s.clone()));
          }
          _ => return None,
        }
      }
      parse_keywords(&parts)
    }
    PropertyValue::String(s) if !s.is_empty() => Some(TextEmphasisStyle::String(s.clone())),
    _ => None,
  }
}

#[allow(clippy::option_option)]
fn parse_text_emphasis_color(value: &PropertyValue, current_color: Rgba) -> Option<Option<Rgba>> {
  match value {
    PropertyValue::Color(c) => Some(Some(c.to_rgba(current_color))),
    PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => Some(None),
    PropertyValue::Keyword(kw) => match crate::style::color::Color::parse(kw) {
      Ok(crate::style::color::Color::Rgba(c)) => Some(Some(c)),
      Ok(crate::style::color::Color::CurrentColor) => Some(None),
      _ => None,
    },
    _ => None,
  }
}

fn parse_text_emphasis_position(value: &PropertyValue) -> Option<TextEmphasisPosition> {
  #[derive(Default)]
  struct PositionParse {
    over_under: Option<bool>, // true over, false under
    side: Option<bool>,       // true right, false left
  }

  let mut state = PositionParse::default();

  let handle_kw = |kw: &str, state: &mut PositionParse| -> bool {
    match kw {
      "over" => {
        if state.over_under.is_some() {
          return false;
        }
        state.over_under = Some(true);
      }
      "under" => {
        if state.over_under.is_some() {
          return false;
        }
        state.over_under = Some(false);
      }
      "left" => {
        if state.side.is_some() {
          return false;
        }
        state.side = Some(false);
      }
      "right" => {
        if state.side.is_some() {
          return false;
        }
        state.side = Some(true);
      }
      "auto" => {
        if state.over_under.is_some() || state.side.is_some() {
          return false;
        }
        return true;
      }
      _ => return false,
    }
    true
  };

  let keywords: Vec<String> = match value {
    PropertyValue::Multiple(values) => {
      let mut kws = Vec::new();
      for v in values {
        if let PropertyValue::Keyword(kw) = v {
          kws.extend(kw.split_whitespace().map(|s| s.to_string()));
        } else {
          return None;
        }
      }
      kws
    }
    PropertyValue::Keyword(kw) => kw.split_whitespace().map(|s| s.to_string()).collect(),
    _ => return None,
  };

  if keywords.is_empty() {
    return None;
  }

  for kw in &keywords {
    if !handle_kw(kw, &mut state) {
      return None;
    }
  }

  match (state.over_under, state.side) {
    (None, None) => Some(TextEmphasisPosition::Auto),
    (Some(true), None) => Some(TextEmphasisPosition::Over),
    (Some(false), None) => Some(TextEmphasisPosition::Under),
    (Some(true), Some(false)) => Some(TextEmphasisPosition::OverLeft),
    (Some(true), Some(true)) => Some(TextEmphasisPosition::OverRight),
    (Some(false), Some(false)) => Some(TextEmphasisPosition::UnderLeft),
    (Some(false), Some(true)) => Some(TextEmphasisPosition::UnderRight),
    (None, Some(false)) => Some(TextEmphasisPosition::OverLeft),
    (None, Some(true)) => Some(TextEmphasisPosition::OverRight),
  }
}

#[allow(clippy::option_option)]
fn parse_text_emphasis_shorthand(
  value: &PropertyValue,
  current_color: Rgba,
) -> Option<(Option<TextEmphasisStyle>, Option<Option<Rgba>>)> {
  let values: Vec<&PropertyValue> = match value {
    PropertyValue::Multiple(vals) => vals.iter().collect(),
    other => vec![other],
  };

  if values.is_empty() {
    return None;
  }

  let mut style: Option<TextEmphasisStyle> = None;
  let mut color: Option<Option<Rgba>> = None;

  for v in values {
    if color.is_none() {
      if let Some(c) = parse_text_emphasis_color(v, current_color) {
        color = Some(c);
        continue;
      }
    }
    if style.is_none() {
      if let Some(s) = parse_text_emphasis_style(v) {
        style = Some(s);
        continue;
      }
    }
  }

  Some((style, color))
}

fn parse_text_decoration_thickness(
  value: &PropertyValue,
  _parent_font_size: f32,
  _root_font_size: f32,
) -> Option<TextDecorationThickness> {
  match value {
    PropertyValue::Keyword(kw) => match kw.to_ascii_lowercase().as_str() {
      "auto" => Some(TextDecorationThickness::Auto),
      "from-font" => Some(TextDecorationThickness::FromFont),
      _ => None,
    },
    PropertyValue::Length(l) => Some(TextDecorationThickness::Length(*l)),
    PropertyValue::Percentage(p) => Some(TextDecorationThickness::Length(Length::percent(*p))),
    _ => None,
  }
}

fn parse_text_decoration_skip_ink(value: &PropertyValue) -> Option<TextDecorationSkipInk> {
  if let PropertyValue::Keyword(kw) = value {
    return match kw.to_ascii_lowercase().as_str() {
      "auto" => Some(TextDecorationSkipInk::Auto),
      "none" => Some(TextDecorationSkipInk::None),
      "all" => Some(TextDecorationSkipInk::All),
      _ => None,
    };
  }
  None
}

fn parse_text_combine_upright(value: &PropertyValue) -> Option<TextCombineUpright> {
  match value {
    PropertyValue::Keyword(kw) => {
      let kw = kw.to_ascii_lowercase();
      match kw.as_str() {
        "none" => Some(TextCombineUpright::None),
        "all" => Some(TextCombineUpright::All),
        "digits" => Some(TextCombineUpright::Digits(2)),
        other if other.starts_with("digits") => {
          let tail = other["digits".len()..].trim();
          if tail.is_empty() {
            return Some(TextCombineUpright::Digits(2));
          }
          if let Ok(count) = tail.parse::<i32>() {
            if (2..=4).contains(&count) {
              return Some(TextCombineUpright::Digits(count as u8));
            }
          }
          None
        }
        _ => None,
      }
    }
    PropertyValue::Multiple(values) => {
      if values.is_empty() {
        return None;
      }
      if let PropertyValue::Keyword(first) = &values[0] {
        if first.eq_ignore_ascii_case("digits") {
          if values.len() == 1 {
            return Some(TextCombineUpright::Digits(2));
          }
          if values.len() == 2 {
            let parse_int = |v: &PropertyValue| -> Option<i32> {
              match v {
                PropertyValue::Number(n) if n.fract() == 0.0 => Some(*n as i32),
                PropertyValue::Keyword(kw) => kw.parse::<i32>().ok(),
                _ => None,
              }
            };
            if let Some(count) = parse_int(&values[1]) {
              if (2..=4).contains(&count) {
                return Some(TextCombineUpright::Digits(count as u8));
              }
            }
            return None;
          }
          return None;
        }
      }
      if values.len() == 1 {
        return parse_text_combine_upright(&values[0]);
      }
      None
    }
    _ => None,
  }
}

fn parse_text_transform(value: &PropertyValue) -> Option<TextTransform> {
  use crate::style::types::CaseTransform;

  let mut case = CaseTransform::None;
  let mut full_width = false;
  let mut full_size_kana = false;

  let mut apply_keyword = |kw: &str| -> Option<()> {
    match kw {
      "none" => {
        // none must be the only keyword present
        case = CaseTransform::None;
        full_width = false;
        full_size_kana = false;
      }
      "uppercase" => {
        if !matches!(case, CaseTransform::None | CaseTransform::Uppercase) {
          return None;
        }
        case = CaseTransform::Uppercase;
      }
      "lowercase" => {
        if !matches!(case, CaseTransform::None | CaseTransform::Lowercase) {
          return None;
        }
        case = CaseTransform::Lowercase;
      }
      "capitalize" => {
        if !matches!(case, CaseTransform::None | CaseTransform::Capitalize) {
          return None;
        }
        case = CaseTransform::Capitalize;
      }
      "full-width" => full_width = true,
      "full-size-kana" => full_size_kana = true,
      _ => return None,
    }
    Some(())
  };

  match value {
    PropertyValue::Keyword(kw) => apply_keyword(kw)?,
    PropertyValue::Multiple(list) if !list.is_empty() => {
      // 'none' cannot be combined with other keywords
      let has_none = list
        .iter()
        .any(|v| matches!(v, PropertyValue::Keyword(k) if k == "none"));
      if has_none && list.len() > 1 {
        return None;
      }
      for part in list {
        if let PropertyValue::Keyword(kw) = part {
          apply_keyword(kw)?;
        } else {
          return None;
        }
      }
    }
    _ => return None,
  }

  Some(TextTransform {
    case,
    full_width,
    full_size_kana,
  })
}

fn parse_list_style_type(value: &PropertyValue) -> Option<ListStyleType> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "disc" => Some(ListStyleType::Disc),
      "circle" => Some(ListStyleType::Circle),
      "square" => Some(ListStyleType::Square),
      "decimal" => Some(ListStyleType::Decimal),
      "decimal-leading-zero" => Some(ListStyleType::DecimalLeadingZero),
      "lower-roman" => Some(ListStyleType::LowerRoman),
      "upper-roman" => Some(ListStyleType::UpperRoman),
      "lower-alpha" | "lower-latin" => Some(ListStyleType::LowerAlpha),
      "upper-alpha" | "upper-latin" => Some(ListStyleType::UpperAlpha),
      "armenian" | "upper-armenian" => Some(ListStyleType::Armenian),
      "lower-armenian" => Some(ListStyleType::LowerArmenian),
      "georgian" => Some(ListStyleType::Georgian),
      "lower-greek" => Some(ListStyleType::LowerGreek),
      "disclosure-open" => Some(ListStyleType::DisclosureOpen),
      "disclosure-closed" => Some(ListStyleType::DisclosureClosed),
      "none" => Some(ListStyleType::None),
      _ if kw.contains('(') => None,
      _ => Some(ListStyleType::Custom(kw.to_ascii_lowercase())),
    },
    PropertyValue::String(s) => Some(ListStyleType::String(s.clone())),
    _ => None,
  }
}

fn parse_list_style_position(value: &PropertyValue) -> Option<ListStylePosition> {
  match value {
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "inside" => Some(ListStylePosition::Inside),
      "outside" => Some(ListStylePosition::Outside),
      _ => None,
    },
    _ => None,
  }
}

fn parse_list_style_image(value: &PropertyValue) -> Option<ListStyleImage> {
  match value {
    PropertyValue::Keyword(kw) if kw == "none" => Some(ListStyleImage::None),
    PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => {
      parse_image_set(kw).and_then(|img| match img {
        BackgroundImage::Url(url) => Some(ListStyleImage::Url(url)),
        _ => None,
      })
    }
    PropertyValue::Url(url) => Some(ListStyleImage::Url(url.clone())),
    _ => None,
  }
}

#[derive(Clone, Copy)]
enum CounterPropertyKind {
  Reset,
  Increment,
  Set,
}

fn parse_counter_property(value: &PropertyValue, kind: CounterPropertyKind) -> Option<CounterSet> {
  let input = counter_value_to_string(value)?;
  let parsed = match kind {
    CounterPropertyKind::Reset => CounterSet::parse_reset(&input)?,
    CounterPropertyKind::Increment => CounterSet::parse_increment(&input)?,
    CounterPropertyKind::Set => CounterSet::parse_set(&input)?,
  };

  Some(parsed)
}

fn counter_value_to_string(value: &PropertyValue) -> Option<String> {
  match value {
    PropertyValue::Keyword(kw) => Some(kw.trim().to_string()),
    PropertyValue::Multiple(parts) => {
      let mut tokens = Vec::new();
      for part in parts {
        match part {
          PropertyValue::Keyword(kw) => tokens.push(kw.clone()),
          PropertyValue::Number(num) if num.fract().abs() < f32::EPSILON => {
            tokens.push((*num as i32).to_string());
          }
          _ => return None,
        }
      }
      if tokens.is_empty() {
        None
      } else {
        Some(tokens.join(" "))
      }
    }
    _ => None,
  }
}

pub fn extract_margin_values(value: &PropertyValue) -> Option<Vec<Option<Length>>> {
  match value {
    PropertyValue::Length(len) => Some(vec![Some(*len)]),
    PropertyValue::Number(n) if *n == 0.0 => Some(vec![Some(Length::px(0.0))]),
    PropertyValue::Keyword(kw) if kw == "auto" => Some(vec![None]), // auto margins
    PropertyValue::Keyword(kw) => parse_length(kw).map(|len| vec![Some(len)]),
    PropertyValue::Multiple(values) => {
      let lengths: Vec<Option<Length>> = values.iter().map(extract_length).collect();
      if lengths.is_empty() {
        None
      } else {
        Some(lengths)
      }
    }
    _ => None,
  }
}

pub fn extract_scroll_padding_length(value: &PropertyValue) -> Option<Length> {
  match value {
    PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => Some(Length::px(0.0)),
    _ => extract_length(value),
  }
}

pub fn extract_scroll_padding_values(value: &PropertyValue) -> Option<Vec<Length>> {
  match value {
    PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("auto") => Some(vec![Length::px(0.0)]),
    PropertyValue::Length(_) | PropertyValue::Number(_) => {
      extract_scroll_padding_length(value).map(|v| vec![v])
    }
    PropertyValue::Multiple(values) => {
      let lengths: Vec<Length> = values
        .iter()
        .filter_map(extract_scroll_padding_length)
        .collect();
      if lengths.is_empty() {
        None
      } else {
        Some(lengths)
      }
    }
    _ => None,
  }
}

pub fn extract_box_values(value: &PropertyValue) -> Option<Vec<Length>> {
  match value {
    PropertyValue::Length(len) => Some(vec![*len]),
    PropertyValue::Number(n) if *n == 0.0 => Some(vec![Length::px(0.0)]),
    PropertyValue::Multiple(values) => {
      let lengths: Vec<Length> = values.iter().filter_map(extract_length).collect();
      if lengths.is_empty() {
        None
      } else {
        Some(lengths)
      }
    }
    _ => None,
  }
}

pub fn apply_margin_values(
  top: &mut Option<Length>,
  right: &mut Option<Length>,
  bottom: &mut Option<Length>,
  left: &mut Option<Length>,
  values: Vec<Option<Length>>,
) {
  match values.len() {
    1 => {
      *top = values[0];
      *right = values[0];
      *bottom = values[0];
      *left = values[0];
    }
    2 => {
      *top = values[0];
      *bottom = values[0];
      *right = values[1];
      *left = values[1];
    }
    3 => {
      *top = values[0];
      *right = values[1];
      *left = values[1];
      *bottom = values[2];
    }
    4 => {
      *top = values[0];
      *right = values[1];
      *bottom = values[2];
      *left = values[3];
    }
    _ => {}
  }
}

pub fn apply_box_values(
  top: &mut Length,
  right: &mut Length,
  bottom: &mut Length,
  left: &mut Length,
  values: Vec<Length>,
) {
  match values.len() {
    1 => {
      *top = values[0];
      *right = values[0];
      *bottom = values[0];
      *left = values[0];
    }
    2 => {
      *top = values[0];
      *bottom = values[0];
      *right = values[1];
      *left = values[1];
    }
    3 => {
      *top = values[0];
      *right = values[1];
      *left = values[1];
      *bottom = values[2];
    }
    4 => {
      *top = values[0];
      *right = values[1];
      *bottom = values[2];
      *left = values[3];
    }
    _ => {}
  }
}

pub fn parse_border_style(kw: &str) -> BorderStyle {
  match kw {
    "none" => BorderStyle::None,
    "hidden" => BorderStyle::Hidden,
    "solid" => BorderStyle::Solid,
    "dashed" => BorderStyle::Dashed,
    "dotted" => BorderStyle::Dotted,
    "double" => BorderStyle::Double,
    "groove" => BorderStyle::Groove,
    "ridge" => BorderStyle::Ridge,
    "inset" => BorderStyle::Inset,
    "outset" => BorderStyle::Outset,
    _ => BorderStyle::None,
  }
}

fn parse_outline_style(kw: &str) -> Option<OutlineStyle> {
  match kw {
    "none" => Some(OutlineStyle::None),
    "hidden" => Some(OutlineStyle::Hidden),
    "solid" => Some(OutlineStyle::Solid),
    "dashed" => Some(OutlineStyle::Dashed),
    "dotted" => Some(OutlineStyle::Dotted),
    "double" => Some(OutlineStyle::Double),
    "groove" => Some(OutlineStyle::Groove),
    "ridge" => Some(OutlineStyle::Ridge),
    "inset" => Some(OutlineStyle::Inset),
    "outset" => Some(OutlineStyle::Outset),
    "auto" => Some(OutlineStyle::Auto),
    _ => None,
  }
}

fn parse_outline_width(value: &PropertyValue) -> Option<Length> {
  match value {
    PropertyValue::Length(l) if l.value >= 0.0 => Some(*l),
    PropertyValue::Keyword(kw) => match kw.as_str() {
      "thin" => Some(Length::px(1.0)),
      "medium" => Some(Length::px(3.0)),
      "thick" => Some(Length::px(5.0)),
      _ => None,
    },
    PropertyValue::Number(n) if *n >= 0.0 => Some(Length::px(*n)),
    _ => None,
  }
}

fn parse_border_width_keyword(kw: &str) -> Option<Length> {
  match kw {
    "thin" => Some(Length::px(1.0)),
    "medium" => Some(Length::px(3.0)),
    "thick" => Some(Length::px(5.0)),
    _ => None,
  }
}

fn apply_outline_shorthand(styles: &mut ComputedStyle, value: &PropertyValue) {
  // The outline shorthand resets color/style/width to their initial values
  // before applying provided tokens (offset is not part of the shorthand).
  let defaults = ComputedStyle::default();
  styles.outline_style = defaults.outline_style;
  styles.outline_width = defaults.outline_width;
  styles.outline_color = defaults.outline_color;

  let mut color: Option<OutlineColor> = None;
  let mut style = None;
  let mut width = None;

  let tokens = match value {
    PropertyValue::Multiple(list) => list.clone(),
    other => vec![other.clone()],
  };

  for token in tokens {
    match token {
      PropertyValue::Color(c) => {
        color = Some(OutlineColor::Color(c.to_rgba(styles.color)));
      }
      PropertyValue::Keyword(ref kw) if kw.eq_ignore_ascii_case("currentcolor") => {
        color = Some(OutlineColor::CurrentColor);
      }
      PropertyValue::Keyword(ref kw) if kw.eq_ignore_ascii_case("invert") => {
        color = Some(OutlineColor::Invert);
      }
      PropertyValue::Keyword(ref kw) => {
        if let Some(parsed_style) = parse_outline_style(kw) {
          style = Some(parsed_style);
          continue;
        }
        match kw.as_str() {
          "thin" | "medium" | "thick" => {
            width = parse_outline_width(&PropertyValue::Keyword(kw.clone()));
          }
          _ => {}
        }
      }
      PropertyValue::Length(_) | PropertyValue::Number(_) => {
        if width.is_none() {
          width = parse_outline_width(&token);
        }
      }
      _ => {}
    }
  }

  if let Some(c) = color {
    styles.outline_color = c;
  }
  if let Some(s) = style {
    styles.outline_style = s;
  }
  if let Some(w) = width {
    styles.outline_width = w;
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::css::properties::parse_property_value;
  use crate::geometry::Size;
  use crate::style::types::AlignContent;
  use crate::style::types::AlignItems;
  use crate::style::types::AspectRatio;
  use crate::style::types::BackgroundRepeatKeyword;
  use crate::style::types::BorderCornerRadius;
  use crate::style::types::BoxSizing;
  use crate::style::types::CaseTransform;
  use crate::style::types::FlexDirection;
  use crate::style::types::FlexWrap;
  use crate::style::types::FontStretch;
  use crate::style::types::FontVariant;
  use crate::style::types::GridAutoFlow;
  use crate::style::types::GridTrack;
  use crate::style::types::ImageOrientation;
  use crate::style::types::ImageRendering;
  use crate::style::types::ImageResolution;
  use crate::style::types::JustifyContent;
  use crate::style::types::ListStylePosition;
  use crate::style::types::ListStyleType;
  use crate::style::types::MixBlendMode;
  use crate::style::types::OutlineColor;
  use crate::style::types::OutlineStyle;
  use crate::style::types::PositionComponent;
  use crate::style::types::PositionKeyword;
  use crate::style::types::ScrollSnapAlign;
  use crate::style::types::ScrollSnapAxis;
  use crate::style::types::ScrollSnapStop;
  use crate::style::types::ScrollSnapStrictness;
  use crate::style::types::TextCombineUpright;
  use crate::style::types::TextDecorationLine;
  use crate::style::types::TextDecorationStyle;
  use crate::style::types::TextDecorationThickness;
  use crate::style::types::TextEmphasisFill;
  use crate::style::types::TextEmphasisPosition;
  use crate::style::types::TextEmphasisShape;
  use crate::style::types::TextEmphasisStyle;
  use crate::style::types::TextOrientation;
  use crate::style::types::TextOverflowSide;
  use crate::style::types::TextSizeAdjust;
  use crate::style::types::TextTransform;
  use crate::style::types::TextWrap;
  use crate::style::types::TransformBox;
  use crate::style::types::WordBreak;
  use crate::style::types::WritingMode;
  use crate::style::values::CalcLength;
  use crate::style::values::LengthUnit;
  use cssparser::Parser;
  use cssparser::ParserInput;

  #[test]
  fn extract_margin_values_accepts_zero_number_and_calc_zero() {
    let num_zero = PropertyValue::Number(0.0);
    let out = extract_margin_values(&num_zero).expect("number zero should be accepted");
    assert_eq!(out.len(), 1);
    assert_eq!(out[0], Some(Length::px(0.0)));

    let calc_zero = CalcLength::single(LengthUnit::Px, 0.0);
    let calc_len = PropertyValue::Length(Length::calc(calc_zero));
    let out = extract_margin_values(&calc_len).expect("calc(0) should be accepted");
    assert_eq!(out.len(), 1);
    assert!(out[0].unwrap().is_zero());
  }

  #[test]
  fn parses_object_fit_keyword() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "object-fit".to_string(),
      value: PropertyValue::Keyword("cover".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.object_fit, ObjectFit::Cover);
  }

  #[test]
  fn parses_image_rendering_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-rendering".to_string(),
        value: PropertyValue::Keyword("pixelated".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.image_rendering, ImageRendering::Pixelated);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-rendering".to_string(),
        value: PropertyValue::Keyword("crisp-edges".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.image_rendering, ImageRendering::CrispEdges);
  }

  #[test]
  fn parses_text_wrap_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("nowrap".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.text_wrap, TextWrap::NoWrap);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("balance".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.text_wrap, TextWrap::Balance);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("pretty".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.text_wrap, TextWrap::Pretty);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("wrap".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.text_wrap, TextWrap::Auto);
  }

  #[test]
  fn parses_word_break_anywhere() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "word-break".to_string(),
        value: PropertyValue::Keyword("anywhere".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.word_break, WordBreak::Anywhere));
  }
  #[test]
  fn parses_flex_flow_shorthand() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex-flow".to_string(),
        value: PropertyValue::Keyword("column wrap-reverse".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_direction, FlexDirection::Column);
    assert_eq!(style.flex_wrap, FlexWrap::WrapReverse);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex-flow".to_string(),
        value: PropertyValue::Keyword("wrap row-reverse".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_direction, FlexDirection::RowReverse);
    assert_eq!(style.flex_wrap, FlexWrap::Wrap);
  }

  #[test]
  fn parses_flex_flow_wrap_only_defaults_direction() {
    let mut style = ComputedStyle::default();
    style.flex_direction = FlexDirection::ColumnReverse;
    style.flex_wrap = FlexWrap::NoWrap;

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex-flow".to_string(),
        value: PropertyValue::Keyword("wrap".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.flex_direction, FlexDirection::Row);
    assert_eq!(style.flex_wrap, FlexWrap::Wrap);
  }

  #[test]
  fn parses_flex_shorthand_keywords() {
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex".to_string(),
        value: PropertyValue::Keyword("none".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_grow, 0.0);
    assert_eq!(style.flex_shrink, 0.0);
    assert!(matches!(style.flex_basis, FlexBasis::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex".to_string(),
        value: PropertyValue::Keyword("auto".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_grow, 1.0);
    assert_eq!(style.flex_shrink, 1.0);
    assert!(matches!(style.flex_basis, FlexBasis::Auto));
  }

  #[test]
  fn parses_flex_shorthand_numbers_and_basis() {
    use crate::style::values::Length;
    use crate::style::values::LengthUnit;

    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Number(2.0),
          PropertyValue::Number(3.0),
          PropertyValue::Length(Length::px(10.0)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_grow, 2.0);
    assert_eq!(style.flex_shrink, 3.0);
    assert_eq!(style.flex_basis, FlexBasis::Length(Length::px(10.0)));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Number(4.0),
          PropertyValue::Number(5.0),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_grow, 4.0);
    assert_eq!(style.flex_shrink, 5.0);
    assert_eq!(
      style.flex_basis,
      FlexBasis::Length(Length::new(0.0, LengthUnit::Percent))
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex".to_string(),
        value: PropertyValue::Number(6.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_grow, 6.0);
    assert_eq!(style.flex_shrink, 1.0);
    assert_eq!(
      style.flex_basis,
      FlexBasis::Length(Length::new(0.0, LengthUnit::Percent))
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex".to_string(),
        value: PropertyValue::Length(Length::px(12.0)),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.flex_grow, 1.0);
    assert_eq!(style.flex_shrink, 1.0);
    assert_eq!(style.flex_basis, FlexBasis::Length(Length::px(12.0)));
  }

  #[test]
  fn flex_shorthand_ignores_negative_numbers() {
    let mut style = ComputedStyle::default();
    // Start with known non-defaults so failures are visible.
    style.flex_grow = 2.0;
    style.flex_shrink = 2.0;
    style.flex_basis = FlexBasis::Auto;

    apply_declaration(
      &mut style,
      &Declaration {
        property: "flex".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Number(-1.0),
          PropertyValue::Number(3.0),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    // Negative flex items are invalid and should leave the previous values intact.
    assert_eq!(style.flex_grow, 2.0);
    assert_eq!(style.flex_shrink, 2.0);
    assert!(matches!(style.flex_basis, FlexBasis::Auto));
  }

  #[test]
  fn parses_clip_path_basic_shapes() {
    let decl = Declaration {
      property: "clip-path".to_string(),
      value: PropertyValue::Keyword("inset(10px 20% round 5px)".to_string()),
      raw_value: String::new(),
      important: false,
    };
    let mut style = ComputedStyle::default();
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    match &style.clip_path {
      ClipPath::BasicShape(basic, None) => match basic.as_ref() {
        BasicShape::Inset {
          top,
          right,
          bottom,
          left,
          border_radius,
        } => {
          assert_eq!(*top, Length::px(10.0));
          assert_eq!(*bottom, Length::px(10.0));
          assert_eq!(*right, Length::percent(20.0));
          assert_eq!(*left, Length::percent(20.0));
          let radii = border_radius.as_ref().expect("radii present");
          assert_eq!(radii.top_left, BorderCornerRadius::uniform(Length::px(5.0)));
          assert_eq!(
            radii.bottom_right,
            BorderCornerRadius::uniform(Length::px(5.0))
          );
        }
        other => panic!("unexpected basic shape: {other:?}"),
      },
      other => panic!("unexpected clip-path parsed: {other:?}"),
    }
  }

  #[test]
  fn parses_clip_path_circle_position() {
    let decl = Declaration {
      property: "clip-path".to_string(),
      value: PropertyValue::Keyword("circle(30% at left 10px top 20%)".to_string()),
      raw_value: String::new(),
      important: false,
    };
    let mut style = ComputedStyle::default();
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    match &style.clip_path {
      ClipPath::BasicShape(basic, None) => {
        let (radius, position) = match basic.as_ref() {
          BasicShape::Circle { radius, position } => (radius, position),
          other => panic!("unexpected basic shape: {other:?}"),
        };
        match radius {
          ShapeRadius::Length(len) => {
            assert_eq!(len.unit, LengthUnit::Percent);
            assert!((len.value - 30.0).abs() < 1e-3);
          }
          _ => panic!("expected length radius"),
        }
        let BackgroundPosition::Position { x, y } = position;
        assert_eq!(x.alignment, 0.0);
        assert_eq!(y.alignment, 0.0);
        assert_eq!(x.offset, Length::px(10.0));
        assert_eq!(y.offset, Length::percent(20.0));
      }
      other => panic!("unexpected clip-path parsed: {other:?}"),
    }
  }

  #[test]
  fn parses_clip_rect_values() {
    let decl = Declaration {
      property: "clip".to_string(),
      value: PropertyValue::Keyword("rect(1px, 10px, 9px, 2px)".to_string()),
      raw_value: String::new(),
      important: false,
    };
    let mut style = ComputedStyle::default();
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    let rect = style.clip.as_ref().expect("clip parsed");
    assert_eq!(rect.top, ClipComponent::Length(Length::px(1.0)));
    assert_eq!(rect.right, ClipComponent::Length(Length::px(10.0)));
    assert_eq!(rect.bottom, ClipComponent::Length(Length::px(9.0)));
    assert_eq!(rect.left, ClipComponent::Length(Length::px(2.0)));

    let auto_decl = Declaration {
      property: "clip".to_string(),
      value: PropertyValue::Keyword("auto".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &auto_decl,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.clip.is_none());
  }

  #[test]
  fn parses_transform_box_keyword_and_inherit() {
    let mut style = ComputedStyle::default();
    let parent = ComputedStyle {
      transform_box: TransformBox::ContentBox,
      ..ComputedStyle::default()
    };

    apply_declaration(
      &mut style,
      &Declaration {
        property: "transform-box".to_string(),
        value: PropertyValue::Keyword("view-box".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.transform_box, TransformBox::ViewBox);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "transform-box".to_string(),
        value: PropertyValue::Keyword("inherit".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.transform_box, TransformBox::ContentBox);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "transform-box".to_string(),
        value: PropertyValue::Keyword("bogus".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.transform_box, TransformBox::ContentBox);
  }

  #[test]
  fn parses_transform_style_and_backface_visibility() {
    let mut style = ComputedStyle::default();
    let parent = ComputedStyle {
      transform_style: TransformStyle::Preserve3d,
      backface_visibility: BackfaceVisibility::Hidden,
      ..ComputedStyle::default()
    };

    apply_declaration(
      &mut style,
      &Declaration {
        property: "transform-style".to_string(),
        value: PropertyValue::Keyword("preserve-3d".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.transform_style, TransformStyle::Preserve3d);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "transform-style".to_string(),
        value: PropertyValue::Keyword("inherit".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.transform_style, TransformStyle::Preserve3d);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "transform-style".to_string(),
        value: PropertyValue::Keyword("flat".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.transform_style, TransformStyle::Flat);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "backface-visibility".to_string(),
        value: PropertyValue::Keyword("hidden".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.backface_visibility, BackfaceVisibility::Hidden);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "backface-visibility".to_string(),
        value: PropertyValue::Keyword("inherit".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.backface_visibility, BackfaceVisibility::Hidden);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "backface-visibility".to_string(),
        value: PropertyValue::Keyword("visible".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.backface_visibility, BackfaceVisibility::Visible);
  }

  #[test]
  fn transform_box_defaults_to_border_box_and_ignores_invalid() {
    let mut style = ComputedStyle::default();
    assert_eq!(style.transform_box, TransformBox::BorderBox);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "transform-box".to_string(),
        value: PropertyValue::Keyword("invalid-value".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.transform_box,
      TransformBox::BorderBox,
      "invalid keywords should be ignored"
    );
  }

  #[test]
  fn parses_image_resolution_values() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-resolution".to_string(),
        value: PropertyValue::Keyword("2dppx".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.image_resolution,
      ImageResolution {
        from_image: false,
        specified: Some(2.0),
        snap: false
      }
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-resolution".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("from-image".to_string()),
          PropertyValue::Keyword("192dpi".to_string()),
          PropertyValue::Keyword("snap".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.image_resolution.from_image);
    assert!(style.image_resolution.snap);
    assert!(
      (style.image_resolution.specified.unwrap() - 2.0).abs() < 1e-6,
      "192dpi should convert to 2dppx"
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-resolution".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("from-image".to_string()),
          PropertyValue::Keyword("2dppx".to_string()),
          PropertyValue::Keyword("3dppx".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(
      (style.image_resolution.specified.unwrap() - 2.0).abs() < 1e-6,
      "invalid duplicates should leave the value unchanged"
    );
  }

  #[test]
  fn image_resolution_snap_rounds_to_integer_device_mapping() {
    let res = ImageResolution {
      from_image: false,
      specified: Some(1.5),
      snap: true,
    };
    // device 2 / 1.5 = 1.333 -> round to 1 device px per image px => used resolution becomes 2dppx
    assert!((res.used_resolution(None, None, 2.0) - 2.0).abs() < 1e-6);

    let res = ImageResolution {
      from_image: false,
      specified: Some(1.6),
      snap: true,
    };
    // device 3 / 1.6 = 1.875 -> round to 2 => used resolution 1.5dppx
    assert!((res.used_resolution(None, None, 3.0) - 1.5).abs() < 1e-6);
  }

  #[test]
  fn image_resolution_prefers_override_and_ignores_metadata_by_default() {
    let default_res = ImageResolution::default();
    // Override (e.g., srcset density) should apply even when from-image is false.
    assert!((default_res.used_resolution(Some(2.0), Some(3.0), 1.0) - 2.0).abs() < 1e-6);
    // Metadata (e.g., EXIF DPI) is ignored unless from-image is set.
    assert!((default_res.used_resolution(None, Some(2.5), 1.0) - 1.0).abs() < 1e-6);

    let from_image = ImageResolution {
      from_image: true,
      specified: None,
      snap: false,
    };
    assert!((from_image.used_resolution(None, Some(2.5), 1.0) - 2.5).abs() < 1e-6);
  }

  #[test]
  fn parses_text_overflow_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-overflow".to_string(),
        value: PropertyValue::Keyword("ellipsis".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.text_overflow.inline_start,
      TextOverflowSide::Ellipsis
    ));
    assert!(matches!(
      style.text_overflow.inline_end,
      TextOverflowSide::Ellipsis
    ));

    let mut two_value = ComputedStyle::default();
    apply_declaration(
      &mut two_value,
      &Declaration {
        property: "text-overflow".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("clip".to_string()),
          PropertyValue::Keyword("ellipsis".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      two_value.text_overflow.inline_start,
      TextOverflowSide::Clip
    ));
    assert!(matches!(
      two_value.text_overflow.inline_end,
      TextOverflowSide::Ellipsis
    ));
  }

  #[test]
  fn parses_text_overflow_string_value() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-overflow".to_string(),
        value: PropertyValue::String("--".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
        style.text_overflow.inline_start,
        TextOverflowSide::String(ref s) if s == "--"
    ));
    assert!(matches!(
        style.text_overflow.inline_end,
        TextOverflowSide::String(ref s) if s == "--"
    ));
  }

  #[test]
  fn parses_text_shadow_layers() {
    use crate::css::properties::parse_property_value;

    let single = parse_property_value("text-shadow", "1px 2px 3px rgb(10 20 30)").unwrap();
    let PropertyValue::TextShadow(shadows) = single else {
      panic!("expected text-shadow value");
    };
    assert_eq!(shadows.len(), 1);
    assert_eq!(shadows[0].offset_x, Length::px(1.0));
    assert_eq!(shadows[0].offset_y, Length::px(2.0));
    assert_eq!(shadows[0].blur_radius, Length::px(3.0));
    assert_eq!(shadows[0].color, Some(Rgba::from_rgba8(10, 20, 30, 255)));

    let multiple =
      parse_property_value("text-shadow", "1px -1px red, 0 2px").expect("valid multi-shadow");
    let PropertyValue::TextShadow(shadows) = multiple else {
      panic!("expected text-shadow value");
    };
    assert_eq!(shadows.len(), 2);
    assert_eq!(shadows[0].offset_x, Length::px(1.0));
    assert_eq!(shadows[0].offset_y, Length::px(-1.0));
    assert_eq!(shadows[0].blur_radius, Length::px(0.0));
    assert_eq!(shadows[0].color, Some(Rgba::RED));
    assert_eq!(shadows[1].offset_x, Length::px(0.0));
    assert_eq!(shadows[1].offset_y, Length::px(2.0));
    assert_eq!(shadows[1].blur_radius, Length::px(0.0));
    assert_eq!(shadows[1].color, None); // defaults to currentColor
  }

  #[test]
  fn rejects_invalid_text_shadow() {
    use crate::css::properties::parse_property_value;
    assert!(parse_property_value("text-shadow", "1px").is_none());
    assert!(parse_property_value("text-shadow", ",").is_none());
    assert!(parse_property_value("text-shadow", "1px 2px 3px 4px").is_none());
  }

  #[test]
  fn parses_image_orientation_values() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-orientation".to_string(),
        value: PropertyValue::Keyword("none".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.image_orientation, ImageOrientation::None);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-orientation".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("90deg".into()),
          PropertyValue::Keyword("flip".into()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.image_orientation,
      ImageOrientation::Angle {
        quarter_turns: 1,
        flip: true
      }
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-orientation".to_string(),
        value: PropertyValue::Keyword("45deg".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.image_orientation,
      ImageOrientation::Angle {
        quarter_turns: 1,
        flip: false
      }
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "image-orientation".to_string(),
        value: PropertyValue::Keyword("flip".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.image_orientation,
      ImageOrientation::Angle {
        quarter_turns: 0,
        flip: true
      }
    );
  }

  #[test]
  fn parses_containment_values() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "contain".to_string(),
        value: PropertyValue::Keyword("paint".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.containment.paint);
    assert!(!style.containment.layout);
    assert!(!style.containment.size);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "contain".to_string(),
        value: PropertyValue::Keyword("strict".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.containment.paint && style.containment.layout && style.containment.size);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "contain".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("layout".into()),
          PropertyValue::Keyword("style".into()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.containment.layout && style.containment.style);
    assert!(!style.containment.paint);
  }

  #[test]
  fn parse_length_component_supports_all_units_case_insensitive() {
    let cases = [
      ("10PX", LengthUnit::Px),
      ("2rem", LengthUnit::Rem),
      ("3EM", LengthUnit::Em),
      ("1ex", LengthUnit::Ex),
      ("1ch", LengthUnit::Ch),
      ("5pt", LengthUnit::Pt),
      ("4pc", LengthUnit::Pc),
      ("2in", LengthUnit::In),
      ("1cm", LengthUnit::Cm),
      ("8mm", LengthUnit::Mm),
      ("6q", LengthUnit::Q),
      ("12vw", LengthUnit::Vw),
      ("14vh", LengthUnit::Vh),
      ("16vmin", LengthUnit::Vmin),
      ("18vmax", LengthUnit::Vmax),
    ];

    for (text, unit) in cases {
      let mut input = ParserInput::new(text);
      let mut parser = Parser::new(&mut input);
      let len = parse_length_component(&mut parser).expect("length");
      assert_eq!(len.unit, unit, "failed for {}", text);
    }
  }

  #[test]
  fn parse_length_component_handles_calc() {
    let mut input = ParserInput::new("calc(10px + 5px)");
    let mut parser = Parser::new(&mut input);
    let len = parse_length_component(&mut parser).expect("calc length");
    assert_eq!(len, Length::px(15.0));

    let mut input = ParserInput::new("calc(50% - 20%)");
    let mut parser = Parser::new(&mut input);
    let len = parse_length_component(&mut parser).expect("calc percent");
    assert_eq!(len, Length::percent(30.0));

    let mut input = ParserInput::new("min(2em, 3em)");
    let mut parser = Parser::new(&mut input);
    let len = parse_length_component(&mut parser).expect("min length");
    assert_eq!(len, Length::em(2.0));

    let mut input = ParserInput::new("clamp(10px, 12px, 15px)");
    let mut parser = Parser::new(&mut input);
    let len = parse_length_component(&mut parser).expect("clamp length");
    assert_eq!(len, Length::px(12.0));
  }

  #[test]
  fn parses_text_orientation_keywords() {
    let mut styles = ComputedStyle::default();
    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-orientation".into(),
        value: PropertyValue::Keyword("upright".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(styles.text_orientation, TextOrientation::Upright);

    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-orientation".into(),
        value: PropertyValue::Keyword("sideways-right".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(styles.text_orientation, TextOrientation::Sideways);

    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-orientation".into(),
        value: PropertyValue::Keyword("sideways-left".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(styles.text_orientation, TextOrientation::SidewaysLeft);
  }

  #[test]
  fn leaves_text_orientation_unchanged_on_unknown_keyword() {
    let mut styles = ComputedStyle::default();
    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-orientation".into(),
        value: PropertyValue::Keyword("upright".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-orientation".into(),
        value: PropertyValue::Keyword("invalid".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(styles.text_orientation, TextOrientation::Upright);
  }

  #[test]
  fn parses_text_combine_upright_values() {
    let mut styles = ComputedStyle::default();
    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-combine-upright".into(),
        value: PropertyValue::Keyword("digits".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(styles.text_combine_upright, TextCombineUpright::Digits(2));

    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-combine-upright".into(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("digits".into()),
          PropertyValue::Number(3.0),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(styles.text_combine_upright, TextCombineUpright::Digits(3));

    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-combine-upright".into(),
        value: PropertyValue::Keyword("all".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(styles.text_combine_upright, TextCombineUpright::All);
  }

  #[test]
  fn rejects_out_of_range_text_combine_digits() {
    let mut styles = ComputedStyle::default();
    styles.text_combine_upright = TextCombineUpright::Digits(2);

    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-combine-upright".into(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("digits".into()),
          PropertyValue::Number(5.0),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      styles.text_combine_upright,
      TextCombineUpright::Digits(2),
      "digits(5) should be invalid and leave the value unchanged"
    );

    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-combine-upright".into(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("digits".into()),
          PropertyValue::Number(1.0),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      styles.text_combine_upright,
      TextCombineUpright::Digits(2),
      "digits(1) should be invalid and leave the value unchanged"
    );

    apply_declaration(
      &mut styles,
      &Declaration {
        property: "text-combine-upright".into(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("digits".into()),
          PropertyValue::Number(2.5),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      styles.text_combine_upright,
      TextCombineUpright::Digits(2),
      "non-integer digits() should be invalid and leave the value unchanged"
    );
  }

  #[test]
  fn parses_text_transform_keyword() {
    let mut styles = ComputedStyle::default();
    let prop = Declaration {
      property: "text-transform".into(),
      value: PropertyValue::Keyword("uppercase".into()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut styles, &prop, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      styles.text_transform,
      TextTransform::with_case(CaseTransform::Uppercase)
    );
  }

  #[test]
  fn parses_aspect_ratio_keywords_and_numbers() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "aspect-ratio".to_string(),
        value: PropertyValue::Keyword("16/9".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.aspect_ratio, AspectRatio::Ratio(r) if (r - (16.0/9.0)).abs() < 0.0001));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "aspect-ratio".to_string(),
        value: PropertyValue::Number(2.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.aspect_ratio, AspectRatio::Ratio(r) if (r - 2.0).abs() < 0.0001));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "aspect-ratio".to_string(),
        value: PropertyValue::Keyword("auto".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.aspect_ratio, AspectRatio::Auto));
  }

  #[test]
  fn parses_color_scheme_lists_and_only() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "color-scheme".to_string(),
        value: PropertyValue::Keyword("light dark".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.color_scheme,
      ColorSchemePreference::Supported {
        schemes: vec![ColorSchemeEntry::Light, ColorSchemeEntry::Dark],
        only: false
      }
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "color-scheme".to_string(),
        value: PropertyValue::Keyword("only dark".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.color_scheme,
      ColorSchemePreference::Supported {
        schemes: vec![ColorSchemeEntry::Dark],
        only: true
      }
    );
  }

  #[test]
  fn rejects_color_scheme_without_schemes() {
    let mut style = ComputedStyle::default();
    style.color_scheme = ColorSchemePreference::Supported {
      schemes: vec![ColorSchemeEntry::Light],
      only: false,
    };

    apply_declaration(
      &mut style,
      &Declaration {
        property: "color-scheme".to_string(),
        value: PropertyValue::Keyword("only".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(
      style.color_scheme,
      ColorSchemePreference::Supported {
        schemes: vec![ColorSchemeEntry::Light],
        only: false
      }
    );
  }

  #[test]
  fn caret_color_parses_auto_and_colors() {
    let mut style = ComputedStyle::default();
    // auto leaves the value at Auto
    apply_declaration(
      &mut style,
      &Declaration {
        property: "caret-color".into(),
        value: PropertyValue::Keyword("auto".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.caret_color, CaretColor::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "caret-color".into(),
        value: PropertyValue::Keyword("red".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(
      matches!(style.caret_color, CaretColor::Color(c) if c.r == 255 && c.g == 0 && c.b == 0)
    );

    let mut with_color = ComputedStyle::default();
    with_color.color = Rgba::new(0, 128, 0, 1.0);
    apply_declaration(
      &mut with_color,
      &Declaration {
        property: "caret-color".into(),
        value: PropertyValue::Keyword("currentColor".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(
      matches!(with_color.caret_color, CaretColor::Color(c) if c.g == 128 && c.r == 0 && c.b == 0)
    );
  }

  #[test]
  fn caret_color_inherit_and_initial() {
    let parent = ComputedStyle {
      caret_color: CaretColor::Color(Rgba::new(0, 0, 255, 1.0)),
      ..ComputedStyle::default()
    };
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "caret-color".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.caret_color, parent.caret_color);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "caret-color".into(),
        value: PropertyValue::Keyword("initial".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.caret_color, CaretColor::Auto));
  }

  #[test]
  fn overflow_anchor_parses_and_defaults() {
    let mut style = ComputedStyle::default();
    assert!(matches!(style.overflow_anchor, OverflowAnchor::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "overflow-anchor".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.overflow_anchor, OverflowAnchor::None));
  }

  #[test]
  fn overflow_anchor_inherit_and_initial() {
    let parent = ComputedStyle {
      overflow_anchor: OverflowAnchor::None,
      ..ComputedStyle::default()
    };
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "overflow-anchor".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.overflow_anchor, parent.overflow_anchor);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "overflow-anchor".into(),
        value: PropertyValue::Keyword("initial".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.overflow_anchor, OverflowAnchor::Auto));
  }

  #[test]
  fn forced_color_adjust_parses_and_defaults() {
    let mut style = ComputedStyle::default();
    assert!(matches!(style.forced_color_adjust, ForcedColorAdjust::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.forced_color_adjust, ForcedColorAdjust::None));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".into(),
        value: PropertyValue::Keyword("preserve-parent-color".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.forced_color_adjust,
      ForcedColorAdjust::PreserveParentColor
    ));
  }

  #[test]
  fn forced_color_adjust_inherit_and_initial() {
    let parent = ComputedStyle {
      forced_color_adjust: ForcedColorAdjust::None,
      ..ComputedStyle::default()
    };
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.forced_color_adjust, parent.forced_color_adjust);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".into(),
        value: PropertyValue::Keyword("initial".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.forced_color_adjust, ForcedColorAdjust::Auto));
  }

  #[test]
  fn text_rendering_parses_keywords() {
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-rendering".into(),
        value: PropertyValue::Keyword("optimizeLegibility".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.text_rendering,
      TextRendering::OptimizeLegibility
    ));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-rendering".into(),
        value: PropertyValue::Keyword("geometricprecision".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.text_rendering,
      TextRendering::GeometricPrecision
    ));
  }

  #[test]
  fn text_rendering_inherit_and_initial() {
    let parent = ComputedStyle {
      text_rendering: TextRendering::OptimizeSpeed,
      ..ComputedStyle::default()
    };
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-rendering".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.text_rendering, TextRendering::OptimizeSpeed));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-rendering".into(),
        value: PropertyValue::Keyword("initial".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.text_rendering, TextRendering::Auto));
  }

  #[test]
  fn scroll_snap_type_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-snap-type".into(),
        value: PropertyValue::Keyword("x mandatory".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.scroll_snap_type.axis, ScrollSnapAxis::X);
    assert_eq!(
      style.scroll_snap_type.strictness,
      ScrollSnapStrictness::Mandatory
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-snap-type".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.scroll_snap_type.axis, ScrollSnapAxis::None);
  }

  #[test]
  fn scroll_snap_align_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-snap-align".into(),
        value: PropertyValue::Keyword("start end".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.scroll_snap_align.inline, ScrollSnapAlign::Start);
    assert_eq!(style.scroll_snap_align.block, ScrollSnapAlign::End);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-snap-align".into(),
        value: PropertyValue::Keyword("center".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.scroll_snap_align.inline, ScrollSnapAlign::Center);
    assert_eq!(style.scroll_snap_align.block, ScrollSnapAlign::Center);
  }

  #[test]
  fn scroll_snap_stop_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-snap-stop".into(),
        value: PropertyValue::Keyword("always".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.scroll_snap_stop, ScrollSnapStop::Always);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-snap-stop".into(),
        value: PropertyValue::Keyword("normal".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.scroll_snap_stop, ScrollSnapStop::Normal);
  }

  #[test]
  fn accent_color_parses_and_inherits() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "accent-color".into(),
        value: PropertyValue::Keyword("auto".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.accent_color, AccentColor::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "accent-color".into(),
        value: PropertyValue::Keyword("blue".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.accent_color, AccentColor::Color(c) if c.b == 255));

    let parent = ComputedStyle {
      accent_color: AccentColor::Color(Rgba::new(10, 20, 30, 1.0)),
      ..ComputedStyle::default()
    };
    apply_declaration(
      &mut style,
      &Declaration {
        property: "accent-color".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.accent_color, parent.accent_color);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "accent-color".into(),
        value: PropertyValue::Keyword("initial".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.accent_color, AccentColor::Auto));
  }

  #[test]
  fn appearance_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "appearance".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.appearance, Appearance::None));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "appearance".into(),
        value: PropertyValue::Keyword("auto".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.appearance, Appearance::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "appearance".into(),
        value: PropertyValue::Keyword("textfield".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.appearance, Appearance::Keyword(ref k) if k == "textfield"));
  }

  #[test]
  fn scroll_behavior_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-behavior".into(),
        value: PropertyValue::Keyword("smooth".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.scroll_behavior, ScrollBehavior::Smooth));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-behavior".into(),
        value: PropertyValue::Keyword("auto".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.scroll_behavior, ScrollBehavior::Auto));

    let parent = ComputedStyle {
      scroll_behavior: ScrollBehavior::Smooth,
      ..ComputedStyle::default()
    };
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-behavior".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.scroll_behavior, parent.scroll_behavior);
  }

  #[test]
  fn overscroll_behavior_parses_and_splits() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "overscroll-behavior".into(),
        value: PropertyValue::Keyword("contain".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.overscroll_behavior_x,
      OverscrollBehavior::Contain
    ));
    assert!(matches!(
      style.overscroll_behavior_y,
      OverscrollBehavior::Contain
    ));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "overscroll-behavior-x".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.overscroll_behavior_x,
      OverscrollBehavior::None
    ));
    assert!(matches!(
      style.overscroll_behavior_y,
      OverscrollBehavior::Contain
    ));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "overscroll-behavior-y".into(),
        value: PropertyValue::Keyword("auto".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.overscroll_behavior_y,
      OverscrollBehavior::Auto
    ));
  }

  #[test]
  fn resize_parses_keywords() {
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "resize".into(),
        value: PropertyValue::Keyword("both".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.resize, Resize::Both));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "resize".into(),
        value: PropertyValue::Keyword("inline".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.resize, Resize::Inline));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "resize".into(),
        value: PropertyValue::Keyword("invalid".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    // Should preserve previous value on invalid input
    assert!(matches!(style.resize, Resize::Inline));
  }

  #[test]
  fn pointer_events_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "pointer-events".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.pointer_events, PointerEvents::None));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "pointer-events".into(),
        value: PropertyValue::Keyword("visibleFill".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.pointer_events, PointerEvents::VisibleFill));

    let parent = ComputedStyle {
      pointer_events: PointerEvents::Painted,
      ..ComputedStyle::default()
    };
    apply_declaration(
      &mut style,
      &Declaration {
        property: "pointer-events".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.pointer_events, PointerEvents::Painted));
  }

  #[test]
  fn user_select_parses_keywords_and_inherit() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "user-select".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.user_select, UserSelect::None));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "user-select".into(),
        value: PropertyValue::Keyword("all".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.user_select, UserSelect::All));

    let parent = ComputedStyle {
      user_select: UserSelect::Contain,
      ..ComputedStyle::default()
    };
    apply_declaration(
      &mut style,
      &Declaration {
        property: "user-select".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.user_select, UserSelect::Contain));
  }

  #[test]
  fn touch_action_parses_keywords_and_lists() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "touch-action".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.touch_action.none);
    assert!(!style.touch_action.auto);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "touch-action".into(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("pan-x".into()),
          PropertyValue::Keyword("pan-y".into()),
          PropertyValue::Keyword("pinch-zoom".into()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.touch_action.pan_x);
    assert!(style.touch_action.pan_y);
    assert!(style.touch_action.pinch_zoom);
    assert!(!style.touch_action.none);
    assert!(!style.touch_action.auto);
  }

  #[test]
  fn scrollbar_width_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-width".into(),
        value: PropertyValue::Keyword("thin".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.scrollbar_width, ScrollbarWidth::Thin));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-width".into(),
        value: PropertyValue::Keyword("none".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.scrollbar_width, ScrollbarWidth::None));
  }

  #[test]
  fn scrollbar_gutter_parses_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-gutter".into(),
        value: PropertyValue::Keyword("stable".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.scrollbar_gutter.stable);
    assert!(!style.scrollbar_gutter.both_edges);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-gutter".into(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("both-edges".into()),
          PropertyValue::Keyword("stable".into()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.scrollbar_gutter.stable);
    assert!(style.scrollbar_gutter.both_edges);
  }

  #[test]
  fn scrollbar_width_honors_global_keywords() {
    let parent = ComputedStyle {
      scrollbar_width: ScrollbarWidth::Thin,
      ..ComputedStyle::default()
    };
    let revert_base = ComputedStyle {
      scrollbar_width: ScrollbarWidth::None,
      ..ComputedStyle::default()
    };
    let revert_layer_base = ComputedStyle {
      scrollbar_width: ScrollbarWidth::Auto,
      ..ComputedStyle::default()
    };

    let mut style = ComputedStyle {
      scrollbar_width: ScrollbarWidth::None,
      ..ComputedStyle::default()
    };

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-width".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.scrollbar_width, ScrollbarWidth::Thin);

    style.scrollbar_width = ScrollbarWidth::Thin;
    apply_declaration_with_base(
      &mut style,
      &Declaration {
        property: "scrollbar-width".into(),
        value: PropertyValue::Keyword("revert".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      &revert_base,
      None,
      16.0,
      16.0,
      DEFAULT_VIEWPORT,
    );
    assert_eq!(style.scrollbar_width, ScrollbarWidth::None);

    style.scrollbar_width = ScrollbarWidth::Thin;
    apply_declaration_with_base(
      &mut style,
      &Declaration {
        property: "scrollbar-width".into(),
        value: PropertyValue::Keyword("revert-layer".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      &revert_base,
      Some(&revert_layer_base),
      16.0,
      16.0,
      DEFAULT_VIEWPORT,
    );
    assert_eq!(style.scrollbar_width, ScrollbarWidth::Auto);

    style.scrollbar_width = ScrollbarWidth::None;
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-width".into(),
        value: PropertyValue::Keyword("unset".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.scrollbar_width, ScrollbarWidth::Auto);
  }

  #[test]
  fn scrollbar_gutter_honors_global_keywords() {
    let parent = ComputedStyle {
      scrollbar_gutter: ScrollbarGutter {
        stable: true,
        both_edges: true,
      },
      ..ComputedStyle::default()
    };
    let revert_base = ComputedStyle {
      scrollbar_gutter: ScrollbarGutter {
        stable: false,
        both_edges: true,
      },
      ..ComputedStyle::default()
    };
    let revert_layer_base = ComputedStyle::default();

    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-gutter".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.scrollbar_gutter, parent.scrollbar_gutter);

    style.scrollbar_gutter = ScrollbarGutter {
      stable: true,
      both_edges: false,
    };
    apply_declaration_with_base(
      &mut style,
      &Declaration {
        property: "scrollbar-gutter".into(),
        value: PropertyValue::Keyword("revert".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      &revert_base,
      None,
      16.0,
      16.0,
      DEFAULT_VIEWPORT,
    );
    assert_eq!(style.scrollbar_gutter, revert_base.scrollbar_gutter);

    apply_declaration_with_base(
      &mut style,
      &Declaration {
        property: "scrollbar-gutter".into(),
        value: PropertyValue::Keyword("revert-layer".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      &revert_base,
      Some(&revert_layer_base),
      16.0,
      16.0,
      DEFAULT_VIEWPORT,
    );
    assert_eq!(style.scrollbar_gutter, revert_layer_base.scrollbar_gutter);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-gutter".into(),
        value: PropertyValue::Keyword("unset".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.scrollbar_gutter, ScrollbarGutter::default());
  }

  #[test]
  fn scrollbar_color_parses_keywords_and_colors() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-color".into(),
        value: PropertyValue::Keyword("dark".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.scrollbar_color, ScrollbarColor::Dark));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-color".into(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Color(Color::Rgba(Rgba::from_rgba8(255, 0, 0, 255))),
          PropertyValue::Keyword("currentColor".into()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    match style.scrollbar_color {
      ScrollbarColor::Colors { thumb, track } => {
        assert_eq!(thumb, Rgba::from_rgba8(255, 0, 0, 255));
        assert_eq!(track, style.color);
      }
      other => panic!("Unexpected scrollbar color: {:?}", other),
    }
  }

  #[test]
  fn scrollbar_color_inherits() {
    let parent = ComputedStyle {
      scrollbar_color: ScrollbarColor::Colors {
        thumb: Rgba::from_rgba8(1, 2, 3, 255),
        track: Rgba::from_rgba8(4, 5, 6, 255),
      },
      ..ComputedStyle::default()
    };

    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-color".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );

    assert_eq!(style.scrollbar_color, parent.scrollbar_color);
  }

  #[test]
  fn scrollbar_color_ignores_single_value() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scrollbar-color".into(),
        value: PropertyValue::Color(Color::Rgba(Rgba::from_rgba8(10, 20, 30, 255))),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert!(matches!(style.scrollbar_color, ScrollbarColor::Auto));
  }

  #[test]
  fn color_scheme_inherit_and_initial() {
    let parent = ComputedStyle {
      color_scheme: ColorSchemePreference::Supported {
        schemes: vec![ColorSchemeEntry::Dark],
        only: false,
      },
      ..ComputedStyle::default()
    };

    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "color-scheme".into(),
        value: PropertyValue::Keyword("inherit".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.color_scheme, parent.color_scheme);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "color-scheme".into(),
        value: PropertyValue::Keyword("initial".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.color_scheme, ColorSchemePreference::Normal);
  }

  #[test]
  fn all_resets_color_scheme() {
    let parent = ComputedStyle::default();
    let mut style = ComputedStyle {
      color_scheme: ColorSchemePreference::Supported {
        schemes: vec![ColorSchemeEntry::Dark],
        only: true,
      },
      ..ComputedStyle::default()
    };

    apply_declaration(
      &mut style,
      &Declaration {
        property: "all".into(),
        value: PropertyValue::Keyword("initial".into()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );

    assert_eq!(style.color_scheme, ColorSchemePreference::Normal);
  }

  #[test]
  fn parses_alignment_keywords_with_start_variants() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "align-items".to_string(),
        value: PropertyValue::Keyword("start".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.align_items, AlignItems::Start);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "align-self".to_string(),
        value: PropertyValue::Keyword("self-end".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.align_self, Some(AlignItems::SelfEnd));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "justify-items".to_string(),
        value: PropertyValue::Keyword("right".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.justify_items, AlignItems::End);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "justify-self".to_string(),
        value: PropertyValue::Keyword("auto".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style.justify_self.is_none());

    apply_declaration(
      &mut style,
      &Declaration {
        property: "justify-self".to_string(),
        value: PropertyValue::Keyword("center".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.justify_self, Some(AlignItems::Center));
  }

  #[test]
  fn parses_place_shorthands() {
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "place-items".to_string(),
        value: PropertyValue::Keyword("center stretch".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.align_items, AlignItems::Center);
    assert_eq!(style.justify_items, AlignItems::Stretch);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "place-self".to_string(),
        value: PropertyValue::Keyword("end start".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.align_self, Some(AlignItems::End));
    assert_eq!(style.justify_self, Some(AlignItems::Start));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "place-content".to_string(),
        value: PropertyValue::Keyword("space-between center".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.align_content, AlignContent::SpaceBetween);
    assert_eq!(style.justify_content, JustifyContent::Center);
  }

  #[test]
  fn parses_writing_mode_keywords() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "writing-mode".to_string(),
        value: PropertyValue::Keyword("vertical-rl".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.writing_mode, WritingMode::VerticalRl);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "writing-mode".to_string(),
        value: PropertyValue::Keyword("sideways-lr".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.writing_mode, WritingMode::SidewaysLr);
  }

  #[test]
  fn inset_shorthand_accepts_calc_zero() {
    let mut style = ComputedStyle {
      top: Some(Length::px(5.0)),
      right: Some(Length::px(6.0)),
      bottom: Some(Length::px(7.0)),
      left: Some(Length::px(8.0)),
      ..ComputedStyle::default()
    };

    let value = parse_property_value("inset", "calc(0)").expect("inset calc(0)");
    apply_declaration(
      &mut style,
      &Declaration {
        property: "inset".to_string(),
        value,
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.top, Some(Length::px(0.0)));
    assert_eq!(style.right, Some(Length::px(0.0)));
    assert_eq!(style.bottom, Some(Length::px(0.0)));
    assert_eq!(style.left, Some(Length::px(0.0)));
  }

  #[test]
  fn margin_shorthand_accepts_calc_zero() {
    let mut style = ComputedStyle {
      margin_top: Some(Length::px(9.0)),
      margin_right: Some(Length::px(10.0)),
      margin_bottom: Some(Length::px(11.0)),
      margin_left: Some(Length::px(12.0)),
      ..ComputedStyle::default()
    };

    let value = parse_property_value("margin", "calc(0)").expect("margin calc(0)");
    apply_declaration(
      &mut style,
      &Declaration {
        property: "margin".to_string(),
        value,
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.margin_top, Some(Length::px(0.0)));
    assert_eq!(style.margin_right, Some(Length::px(0.0)));
    assert_eq!(style.margin_bottom, Some(Length::px(0.0)));
    assert_eq!(style.margin_left, Some(Length::px(0.0)));
  }

  #[test]
  fn padding_shorthand_accepts_calc_zero() {
    let mut style = ComputedStyle {
      padding_top: Length::px(3.0),
      padding_right: Length::px(4.0),
      padding_bottom: Length::px(5.0),
      padding_left: Length::px(6.0),
      ..ComputedStyle::default()
    };

    let value = parse_property_value("padding", "calc(0)").expect("padding calc(0)");
    apply_declaration(
      &mut style,
      &Declaration {
        property: "padding".to_string(),
        value,
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.padding_top, Length::px(0.0));
    assert_eq!(style.padding_right, Length::px(0.0));
    assert_eq!(style.padding_bottom, Length::px(0.0));
    assert_eq!(style.padding_left, Length::px(0.0));
  }

  #[test]
  fn scroll_margin_shorthand_accepts_calc_zero() {
    let mut style = ComputedStyle {
      scroll_margin_top: Length::px(1.0),
      scroll_margin_right: Length::px(2.0),
      scroll_margin_bottom: Length::px(3.0),
      scroll_margin_left: Length::px(4.0),
      ..ComputedStyle::default()
    };

    let value = parse_property_value("scroll-margin", "calc(0)").expect("scroll-margin calc(0)");
    apply_declaration(
      &mut style,
      &Declaration {
        property: "scroll-margin".to_string(),
        value,
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.scroll_margin_top, Length::px(0.0));
    assert_eq!(style.scroll_margin_right, Length::px(0.0));
    assert_eq!(style.scroll_margin_bottom, Length::px(0.0));
    assert_eq!(style.scroll_margin_left, Length::px(0.0));
  }

  #[test]
  fn gap_shorthands_accept_calc_zero() {
    let mut style = ComputedStyle {
      grid_gap: Length::px(5.0),
      grid_row_gap: Length::px(6.0),
      grid_column_gap: Length::px(7.0),
      ..ComputedStyle::default()
    };

    let gap_value = parse_property_value("gap", "calc(0)").expect("gap calc(0)");
    apply_declaration(
      &mut style,
      &Declaration {
        property: "gap".to_string(),
        value: gap_value,
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.grid_gap, Length::px(0.0));
    assert_eq!(style.grid_row_gap, Length::px(0.0));
    assert_eq!(style.grid_column_gap, Length::px(0.0));
    assert_eq!(style.column_gap, Length::px(0.0));

    let row_gap_value = parse_property_value("row-gap", "calc(0)").expect("row-gap calc(0)");
    apply_declaration(
      &mut style,
      &Declaration {
        property: "row-gap".to_string(),
        value: row_gap_value,
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_row_gap, Length::px(0.0));

    let column_gap_value =
      parse_property_value("column-gap", "calc(0)").expect("column-gap calc(0)");
    apply_declaration(
      &mut style,
      &Declaration {
        property: "column-gap".to_string(),
        value: column_gap_value,
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_column_gap, Length::px(0.0));
    assert_eq!(style.column_gap, Length::px(0.0));
  }

  #[test]
  fn parses_object_position_keywords() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "object-position".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("right".to_string()),
        PropertyValue::Keyword("bottom".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.object_position.x,
      PositionComponent::Keyword(PositionKeyword::End)
    ));
    assert!(matches!(
      style.object_position.y,
      PositionComponent::Keyword(PositionKeyword::End)
    ));
  }

  #[test]
  fn parses_box_sizing_keyword() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "box-sizing".to_string(),
        value: PropertyValue::Keyword("border-box".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert!(matches!(style.box_sizing, BoxSizing::BorderBox));
  }

  #[test]
  fn parses_grid_auto_rows_and_columns() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "grid-auto-rows".into(),
        value: PropertyValue::Keyword("10px minmax(0,1fr)".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_auto_rows.len(), 2);
    assert!(matches!(style.grid_auto_rows[0], GridTrack::Length(_)));
    apply_declaration(
      &mut style,
      &Declaration {
        property: "grid-auto-columns".into(),
        value: PropertyValue::Keyword("20%".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_auto_columns.len(), 1);
  }

  #[test]
  fn parses_grid_auto_flow() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "grid-auto-flow".into(),
        value: PropertyValue::Keyword("column dense".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_auto_flow, GridAutoFlow::ColumnDense);
  }

  #[test]
  fn parses_grid_area_named_area() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "grid-area".into(),
        value: PropertyValue::Keyword("hero".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_row_raw.as_deref(), Some("hero-start / hero-end"));
    assert_eq!(
      style.grid_column_raw.as_deref(),
      Some("hero-start / hero-end")
    );
  }

  #[test]
  fn parses_grid_shorthand_auto_flow_form() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "grid".into(),
        value: PropertyValue::Keyword("auto-flow 10px / 20px".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_auto_flow, GridAutoFlow::Row);
    assert_eq!(style.grid_auto_rows.len(), 1);
    assert_eq!(style.grid_auto_columns.len(), 1);
  }

  #[test]
  fn parses_grid_shorthand_template_form() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "grid".into(),
        value: PropertyValue::Keyword("\"a a\" \"b b\" / 1fr 2fr".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_template_rows.len(), 2);
    assert_eq!(style.grid_template_columns.len(), 2);
    assert_eq!(style.grid_template_areas.len(), 2);
  }

  #[test]
  fn gap_shorthand_parses_two_values_and_percent() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "gap".to_string(),
        value: PropertyValue::Keyword("10px 20%".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.grid_row_gap, Length::px(10.0));
    assert_eq!(style.grid_column_gap, Length::percent(20.0));
    assert_eq!(style.column_gap, Length::percent(20.0));
  }

  #[test]
  fn row_and_column_gap_accept_percent_and_normal() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "row-gap".to_string(),
        value: PropertyValue::Keyword("normal".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.grid_row_gap, Length::px(0.0));

    let mut style2 = ComputedStyle::default();
    apply_declaration(
      &mut style2,
      &Declaration {
        property: "column-gap".to_string(),
        value: PropertyValue::Percentage(15.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style2.grid_column_gap, Length::percent(15.0));
    assert_eq!(style2.column_gap, Length::percent(15.0));
  }

  #[test]
  fn parses_column_properties() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "column-count".to_string(),
        value: PropertyValue::Number(2.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    apply_declaration(
      &mut style,
      &Declaration {
        property: "column-width".to_string(),
        value: PropertyValue::Length(Length::px(120.0)),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    apply_declaration(
      &mut style,
      &Declaration {
        property: "column-fill".to_string(),
        value: PropertyValue::Keyword("auto".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    apply_declaration(
      &mut style,
      &Declaration {
        property: "column-span".to_string(),
        value: PropertyValue::Keyword("all".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    apply_declaration(
      &mut style,
      &Declaration {
        property: "column-rule".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Length(Length::px(5.0)),
          PropertyValue::Keyword("solid".to_string()),
          PropertyValue::Color(Color::Rgba(Rgba::BLUE)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.column_count, Some(2));
    assert_eq!(style.column_width, Some(Length::px(120.0)));
    assert_eq!(style.column_fill, ColumnFill::Auto);
    assert_eq!(style.column_span, ColumnSpan::All);
    assert_eq!(style.column_rule_style, BorderStyle::Solid);
    assert_eq!(style.column_rule_width, Length::px(5.0));
    assert_eq!(style.column_rule_color, Some(Rgba::BLUE));

    let mut shorthand = ComputedStyle::default();
    apply_declaration(
      &mut shorthand,
      &Declaration {
        property: "columns".to_string(),
        value: PropertyValue::Keyword("4 60px".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(shorthand.column_count, Some(4));
    assert_eq!(shorthand.column_width, Some(Length::px(60.0)));
  }

  #[test]
  fn outline_shorthand_resets_missing_parts() {
    let mut style = ComputedStyle::default();
    style.outline_style = OutlineStyle::Solid;
    style.outline_width = Length::px(8.0);
    style.outline_color = OutlineColor::Color(Rgba::GREEN);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "outline".to_string(),
        value: PropertyValue::Color(Color::Rgba(Rgba::RED)),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.outline_style, OutlineStyle::None);
    assert_eq!(style.outline_width, Length::px(3.0)); // medium
    assert_eq!(style.outline_color, OutlineColor::Color(Rgba::RED));
  }

  #[test]
  fn outline_shorthand_defaults_color_to_currentcolor() {
    let mut style = ComputedStyle::default();
    style.color = Rgba::BLUE;
    style.outline_color = OutlineColor::Color(Rgba::GREEN);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "outline".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("solid".to_string()),
          PropertyValue::Keyword("thin".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.outline_style, OutlineStyle::Solid);
    assert_eq!(style.outline_width, Length::px(1.0));
    assert_eq!(style.outline_color, OutlineColor::Invert);
    // currentColor resolution happens at paint time; initial value is invert per spec
  }

  #[test]
  fn negative_outline_width_is_ignored() {
    let mut style = ComputedStyle::default();
    style.outline_width = Length::px(5.0);
    apply_declaration(
      &mut style,
      &Declaration {
        property: "outline-width".to_string(),
        value: PropertyValue::Number(-3.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.outline_width, Length::px(5.0));
  }

  #[test]
  fn negative_padding_and_border_widths_clamp_to_zero() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "padding-left".to_string(),
        value: PropertyValue::Length(Length::px(-4.0)),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    apply_declaration(
      &mut style,
      &Declaration {
        property: "border-left-width".to_string(),
        value: PropertyValue::Number(-3.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.padding_left, Length::px(0.0));
    assert_eq!(style.border_left_width, Length::px(0.0));
  }

  #[test]
  fn negative_border_radius_clamps_to_zero() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "border-radius".to_string(),
        value: PropertyValue::Length(Length::px(-10.0)),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    let zero = BorderCornerRadius::default();
    assert_eq!(style.border_top_left_radius, zero);
    assert_eq!(style.border_top_right_radius, zero);
    assert_eq!(style.border_bottom_right_radius, zero);
    assert_eq!(style.border_bottom_left_radius, zero);
  }

  #[test]
  fn parses_elliptical_border_radius_single_pair() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "border-radius".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Length(Length::px(10.0)),
          PropertyValue::Keyword("/".to_string()),
          PropertyValue::Length(Length::px(20.0)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    let expected = BorderCornerRadius {
      x: Length::px(10.0),
      y: Length::px(20.0),
    };
    assert_eq!(style.border_top_left_radius, expected);
    assert_eq!(style.border_top_right_radius, expected);
    assert_eq!(style.border_bottom_right_radius, expected);
    assert_eq!(style.border_bottom_left_radius, expected);
  }

  #[test]
  fn parses_elliptical_border_radius_with_four_values() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "border-radius".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Length(Length::px(10.0)),
          PropertyValue::Length(Length::px(20.0)),
          PropertyValue::Keyword("/".to_string()),
          PropertyValue::Length(Length::px(30.0)),
          PropertyValue::Length(Length::px(40.0)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(
      style.border_top_left_radius,
      BorderCornerRadius {
        x: Length::px(10.0),
        y: Length::px(30.0),
      }
    );
    assert_eq!(
      style.border_top_right_radius,
      BorderCornerRadius {
        x: Length::px(20.0),
        y: Length::px(40.0),
      }
    );
    assert_eq!(
      style.border_bottom_right_radius,
      BorderCornerRadius {
        x: Length::px(10.0),
        y: Length::px(30.0),
      }
    );
    assert_eq!(
      style.border_bottom_left_radius,
      BorderCornerRadius {
        x: Length::px(20.0),
        y: Length::px(40.0),
      }
    );
  }

  #[test]
  fn overflow_clip_sets_both_axes() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "overflow".to_string(),
        value: PropertyValue::Keyword("clip".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.overflow_x, Overflow::Clip);
    assert_eq!(style.overflow_y, Overflow::Clip);
  }

  #[test]
  fn parses_background_position_keywords() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "background-position".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("left".to_string()),
        PropertyValue::Keyword("top".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 0.0).abs() < 0.01);
    assert!((y.alignment - 0.0).abs() < 0.01);
    assert!(x.offset.is_zero());
    assert!(y.offset.is_zero());
  }

  #[test]
  fn background_position_two_value_second_length_is_vertical() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("left".to_string()),
          PropertyValue::Percentage(20.0),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 0.0).abs() < 0.01);
    assert!(x.offset.is_zero());
    assert!((y.alignment - 0.0).abs() < 0.01);
    assert_eq!(y.offset, Length::percent(20.0));
  }

  #[test]
  fn background_position_two_keywords_swapped_axes() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("top".to_string()),
          PropertyValue::Keyword("right".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 1.0).abs() < 0.01);
    assert!(x.offset.is_zero());
    assert!((y.alignment - 0.0).abs() < 0.01);
  }

  #[test]
  fn background_position_two_keywords_vertical_first_with_percentage() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("top".to_string()),
          PropertyValue::Percentage(30.0),
          PropertyValue::Keyword("left".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((y.alignment - 0.0).abs() < 0.01);
    assert_eq!(y.offset, Length::percent(30.0));
    assert!((x.alignment - 0.0).abs() < 0.01);
    assert!(x.offset.is_zero());
  }

  #[test]
  fn background_position_center_left_swaps_axes() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("center".to_string()),
          PropertyValue::Keyword("left".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 0.0).abs() < 0.01);
    assert!((y.alignment - 0.5).abs() < 0.01);
  }

  #[test]
  fn background_position_center_bottom_keeps_vertical() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("center".to_string()),
          PropertyValue::Keyword("bottom".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 0.5).abs() < 0.01);
    assert!((y.alignment - 1.0).abs() < 0.01);
  }

  #[test]
  fn background_position_three_values_horizontal_pair() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("left".to_string()),
          PropertyValue::Length(Length::px(10.0)),
          PropertyValue::Keyword("bottom".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 0.0).abs() < 0.01);
    assert_eq!(x.offset, Length::px(10.0));
    assert!((y.alignment - 1.0).abs() < 0.01);
    assert!(y.offset.is_zero());
  }

  #[test]
  fn background_position_three_values_vertical_pair() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("bottom".to_string()),
          PropertyValue::Length(Length::px(5.0)),
          PropertyValue::Keyword("right".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((y.alignment - 1.0).abs() < 0.01);
    assert_eq!(y.offset, Length::px(-5.0));
    assert!((x.alignment - 1.0).abs() < 0.01);
    assert!(x.offset.is_zero());
  }

  #[test]
  fn background_position_four_values() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("right".to_string()),
          PropertyValue::Length(Length::px(20.0)),
          PropertyValue::Keyword("bottom".to_string()),
          PropertyValue::Length(Length::px(10.0)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 1.0).abs() < 0.01);
    assert_eq!(x.offset, Length::px(-20.0));
    assert!((y.alignment - 1.0).abs() < 0.01);
    assert_eq!(y.offset, Length::px(-10.0));
  }

  #[test]
  fn background_position_four_values_vertical_first() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("top".to_string()),
          PropertyValue::Length(Length::px(10.0)),
          PropertyValue::Keyword("left".to_string()),
          PropertyValue::Length(Length::px(5.0)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((y.alignment - 0.0).abs() < 0.01);
    assert_eq!(y.offset, Length::px(10.0));
    assert!((x.alignment - 0.0).abs() < 0.01);
    assert_eq!(x.offset, Length::px(5.0));
  }

  #[test]
  fn background_position_vertical_keyword_first_with_offsets() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("bottom".to_string()),
          PropertyValue::Length(Length::px(12.0)),
          PropertyValue::Keyword("right".to_string()),
          PropertyValue::Length(Length::px(8.0)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((y.alignment - 1.0).abs() < 0.01);
    // End offsets are stored as negative lengths.
    assert_eq!(y.offset, Length::px(-12.0));
    assert!((x.alignment - 1.0).abs() < 0.01);
    assert_eq!(x.offset, Length::px(-8.0));
  }

  #[test]
  fn background_position_x_and_y_longhands_merge_layers() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Url("a.png".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Url("b.png".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position-y".to_string(),
        value: PropertyValue::Keyword("bottom".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position-x".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("left".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Percentage(20.0),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    let BackgroundPosition::Position { x: x0, y: y0 } = style.background_layers[0].position;
    assert!((x0.alignment - 0.0).abs() < 0.01);
    assert!(x0.offset.is_zero());
    assert!((y0.alignment - 1.0).abs() < 0.01);

    let BackgroundPosition::Position { x: x1, y: y1 } = style.background_layers[1].position;
    assert_eq!(x1.offset, Length::percent(20.0));
    assert!((x1.alignment - 0.0).abs() < 0.01);
    assert!((y1.alignment - 1.0).abs() < 0.01);
  }

  #[test]
  fn background_position_y_longhand_repeats_for_layers() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Url("a.png".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Url("b.png".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position-y".to_string(),
        value: PropertyValue::Percentage(10.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    let BackgroundPosition::Position { y: y0, .. } = style.background_layers[0].position;
    let BackgroundPosition::Position { y: y1, .. } = style.background_layers[1].position;
    assert_eq!(y0.offset, Length::percent(10.0));
    assert_eq!(y1.offset, Length::percent(10.0));
  }

  #[test]
  fn parses_background_position_end_offsets() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "background-position".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("right".to_string()),
        PropertyValue::Percentage(10.0),
        PropertyValue::Keyword("bottom".to_string()),
        PropertyValue::Length(Length::px(5.0)),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 1.0).abs() < 0.01);
    assert!((y.alignment - 1.0).abs() < 0.01);
    assert!((x.offset.value + 10.0).abs() < 0.01 && x.offset.unit == LengthUnit::Percent);
    assert!((y.offset.value + 5.0).abs() < 0.01 && y.offset.unit == LengthUnit::Px);
  }

  #[test]
  fn parses_background_position_with_offsets_and_axis_defaults() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "background-position".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("right".to_string()),
        PropertyValue::Length(Length::px(10.0)),
        PropertyValue::Keyword("top".to_string()),
        PropertyValue::Percentage(25.0),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 1.0).abs() < 0.01);
    assert!((y.alignment - 0.0).abs() < 0.01);
    assert!((x.offset.value + 10.0).abs() < 0.01 && x.offset.unit == LengthUnit::Px);
    assert!((y.offset.value - 25.0).abs() < 0.01 && y.offset.unit == LengthUnit::Percent);

    // Default the missing axis to center when only one axis is provided.
    let decl = Declaration {
      property: "background-position".to_string(),
      value: PropertyValue::Keyword("top".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 0.5).abs() < 0.01);
    assert!(x.offset.is_zero());
    assert!((y.alignment - 0.0).abs() < 0.01);
  }

  #[test]
  fn parses_text_decoration_longhands() {
    let mut style = ComputedStyle::default();

    let decl = Declaration {
      property: "text-decoration-line".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("underline".to_string()),
        PropertyValue::Keyword("overline".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));
    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::OVERLINE));

    let decl = Declaration {
      property: "text-decoration-style".to_string(),
      value: PropertyValue::Keyword("dashed".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.text_decoration.style, TextDecorationStyle::Dashed);

    let decl = Declaration {
      property: "text-decoration-color".to_string(),
      value: PropertyValue::Color(Color::Rgba(Rgba::BLUE)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.text_decoration.color, Some(Rgba::BLUE));

    let decl = Declaration {
      property: "text-decoration-thickness".to_string(),
      value: PropertyValue::Length(Length::px(3.0)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
        style.text_decoration.thickness,
        TextDecorationThickness::Length(l) if (l.to_px() - 3.0).abs() < 0.01
    ));

    let decl = Declaration {
      property: "text-decoration-thickness".to_string(),
      value: PropertyValue::Keyword("from-font".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.text_decoration.thickness,
      TextDecorationThickness::FromFont
    ));

    let decl = Declaration {
      property: "text-underline-position".to_string(),
      value: PropertyValue::Keyword("under".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.text_underline_position,
      TextUnderlinePosition::Under
    ));

    let decl = Declaration {
      property: "text-underline-position".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("left".to_string()),
        PropertyValue::Keyword("under".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.text_underline_position,
      TextUnderlinePosition::UnderLeft
    ));
  }

  #[test]
  fn parses_text_emphasis_properties() {
    let mut style = ComputedStyle::default();

    let decl = Declaration {
      property: "text-emphasis-style".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("open".to_string()),
        PropertyValue::Keyword("sesame".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.text_emphasis_style,
      TextEmphasisStyle::Mark {
        fill: TextEmphasisFill::Open,
        shape: TextEmphasisShape::Sesame
      }
    ));

    let decl = Declaration {
      property: "text-emphasis-color".to_string(),
      value: PropertyValue::Color(Color::Rgba(Rgba::RED)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.text_emphasis_color, Some(Rgba::RED));

    let decl = Declaration {
      property: "text-emphasis-position".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("under".to_string()),
        PropertyValue::Keyword("right".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.text_emphasis_position,
      TextEmphasisPosition::UnderRight
    ));

    let decl = Declaration {
      property: "text-emphasis".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("circle".to_string()),
        PropertyValue::Color(Color::Rgba(Rgba::BLUE)),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.text_emphasis_style,
      TextEmphasisStyle::Mark {
        fill: TextEmphasisFill::Filled,
        shape: TextEmphasisShape::Circle
      }
    ));
    assert_eq!(style.text_emphasis_color, Some(Rgba::BLUE));
  }

  #[test]
  fn text_size_adjust_parses() {
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-size-adjust".to_string(),
        value: PropertyValue::Keyword("none".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.text_size_adjust, TextSizeAdjust::None));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-size-adjust".to_string(),
        value: PropertyValue::Percentage(125.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(
      matches!(style.text_size_adjust, TextSizeAdjust::Percentage(p) if (p - 125.0).abs() < f32::EPSILON)
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-size-adjust".to_string(),
        value: PropertyValue::Percentage(-10.0),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    // Negative percentages are ignored; previous value is preserved.
    assert!(
      matches!(style.text_size_adjust, TextSizeAdjust::Percentage(p) if (p - 125.0).abs() < f32::EPSILON)
    );
  }

  #[test]
  fn text_size_adjust_inherits_with_keyword() {
    let parent = ComputedStyle {
      text_size_adjust: TextSizeAdjust::Percentage(80.0),
      ..ComputedStyle::default()
    };

    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-size-adjust".to_string(),
        value: PropertyValue::Keyword("inherit".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );

    assert_eq!(style.text_size_adjust, parent.text_size_adjust);
  }

  #[test]
  fn forced_color_adjust_parses_and_handles_globals() {
    let mut style = ComputedStyle::default();
    assert!(matches!(style.forced_color_adjust, ForcedColorAdjust::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".to_string(),
        value: PropertyValue::Keyword("none".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.forced_color_adjust, ForcedColorAdjust::None));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".to_string(),
        value: PropertyValue::Keyword("preserve-parent-color".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.forced_color_adjust,
      ForcedColorAdjust::PreserveParentColor
    ));

    let parent = ComputedStyle {
      forced_color_adjust: ForcedColorAdjust::None,
      ..ComputedStyle::default()
    };

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".to_string(),
        value: PropertyValue::Keyword("inherit".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert_eq!(style.forced_color_adjust, parent.forced_color_adjust);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "forced-color-adjust".to_string(),
        value: PropertyValue::Keyword("unset".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.forced_color_adjust, ForcedColorAdjust::Auto));
  }

  #[test]
  fn text_underline_position_rejects_invalid_combinations() {
    let mut style = ComputedStyle::default();
    style.text_underline_position = TextUnderlinePosition::Under;

    let decl = Declaration {
      property: "text-underline-position".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("auto".to_string()),
        PropertyValue::Keyword("under".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert!(
      matches!(style.text_underline_position, TextUnderlinePosition::Under),
      "invalid keyword combinations should be ignored without changing the computed value"
    );
  }

  #[test]
  fn parses_text_decoration_shorthand_and_resets() {
    let mut style = ComputedStyle::default();
    style.text_decoration.lines = TextDecorationLine::LINE_THROUGH;
    style.text_decoration.style = TextDecorationStyle::Double;
    style.text_decoration.color = Some(Rgba::BLUE);
    style.text_decoration.thickness = TextDecorationThickness::Length(Length::px(2.0));

    let decl = Declaration {
      property: "text-decoration".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("underline".to_string()),
        PropertyValue::Keyword("dotted".to_string()),
        PropertyValue::Color(Color::Rgba(Rgba::RED)),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));
    assert!(!style
      .text_decoration
      .lines
      .contains(TextDecorationLine::LINE_THROUGH));
    assert_eq!(style.text_decoration.style, TextDecorationStyle::Dotted);
    assert_eq!(style.text_decoration.color, Some(Rgba::RED));
    assert!(matches!(
      style.text_decoration.thickness,
      TextDecorationThickness::Auto
    ));

    // currentcolor leaves color unset, shorthand resets missing pieces back to initial
    let decl = Declaration {
      property: "text-decoration".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("line-through".to_string()),
        PropertyValue::Keyword("wavy".to_string()),
        PropertyValue::Keyword("currentcolor".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::LINE_THROUGH));
    assert_eq!(style.text_decoration.style, TextDecorationStyle::Wavy);
    assert_eq!(style.text_decoration.color, None);
    assert!(matches!(
      style.text_decoration.thickness,
      TextDecorationThickness::Auto
    ));

    // thickness in shorthand
    let decl = Declaration {
      property: "text-decoration".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("overline".to_string()),
        PropertyValue::Keyword("double".to_string()),
        PropertyValue::Color(Color::Rgba(Rgba::GREEN)),
        PropertyValue::Length(Length::px(3.2)),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::OVERLINE));
    assert_eq!(style.text_decoration.style, TextDecorationStyle::Double);
    assert_eq!(style.text_decoration.color, Some(Rgba::GREEN));
    assert!(matches!(
        style.text_decoration.thickness,
        TextDecorationThickness::Length(l) if (l.to_px() - 3.2).abs() < 0.01
    ));
  }

  #[test]
  fn text_decoration_keywords_are_case_insensitive() {
    let mut style = ComputedStyle::default();

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-decoration-line".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("UNDERLINE".to_string()),
          PropertyValue::Keyword("OVERLINE".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::UNDERLINE));
    assert!(style
      .text_decoration
      .lines
      .contains(TextDecorationLine::OVERLINE));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-decoration-style".to_string(),
        value: PropertyValue::Keyword("DASHED".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.text_decoration.style, TextDecorationStyle::Dashed);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-decoration-thickness".to_string(),
        value: PropertyValue::Keyword("FROM-FONT".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.text_decoration.thickness,
      TextDecorationThickness::FromFont
    ));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-decoration-skip-ink".to_string(),
        value: PropertyValue::Keyword("ALL".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.text_decoration_skip_ink, TextDecorationSkipInk::All);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-underline-position".to_string(),
        value: PropertyValue::Keyword("UNDER RIGHT".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
      style.text_underline_position,
      TextUnderlinePosition::UnderRight
    ));
  }

  #[test]
  fn parses_list_style_properties() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::Keyword("square".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::Square);

    let decl = Declaration {
      property: "list-style-position".to_string(),
      value: PropertyValue::Keyword("inside".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_position, ListStylePosition::Inside);

    let decl = Declaration {
      property: "list-style-image".to_string(),
      value: PropertyValue::Url("marker.png".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.list_style_image,
      ListStyleImage::Url("marker.png".to_string())
    );

    let decl = Declaration {
      property: "list-style".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("upper-roman".to_string()),
        PropertyValue::Keyword("outside".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::UpperRoman);
    assert_eq!(style.list_style_position, ListStylePosition::Outside);
    assert_eq!(style.list_style_image, ListStyleImage::None);

    let decl = Declaration {
      property: "list-style".to_string(),
      value: PropertyValue::Url("img.png".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.list_style_image,
      ListStyleImage::Url("img.png".to_string())
    );
    assert_eq!(style.list_style_type, ListStyleType::Disc);
    assert_eq!(style.list_style_position, ListStylePosition::Outside);

    let decl = Declaration {
      property: "list-style".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::None);
    assert_eq!(style.list_style_image, ListStyleImage::None);

    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::Keyword("lower-greek".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::LowerGreek);

    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::Keyword("armenian".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::Armenian);

    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::Keyword("lower-armenian".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::LowerArmenian);

    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::Keyword("georgian".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::Georgian);

    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::Keyword("disclosure-open".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::DisclosureOpen);

    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::Keyword("disclosure-closed".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.list_style_type, ListStyleType::DisclosureClosed);

    let decl = Declaration {
      property: "list-style-type".to_string(),
      value: PropertyValue::String("★".to_string()),
      raw_value: "\"★\"".to_string(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.list_style_type, ListStyleType::String(ref s) if s == "★"));
  }

  #[test]
  fn cursor_keyword_parses() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "cursor".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("pointer".to_string()),
          PropertyValue::Keyword(",".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.cursor, CursorKeyword::Pointer);
    assert!(style.cursor_images.is_empty());
  }

  #[test]
  fn cursor_allows_custom_image_and_hotspot_with_fallback() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "cursor".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Url("cursor.cur".to_string()),
          PropertyValue::Number(5.0),
          PropertyValue::Number(7.0),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Keyword("move".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.cursor, CursorKeyword::Move);
    assert_eq!(style.cursor_images.len(), 1);
    assert_eq!(style.cursor_images[0].url, "cursor.cur");
    assert_eq!(style.cursor_images[0].hotspot, Some((5.0, 7.0)));
  }

  #[test]
  fn cursor_accepts_image_set_and_fallback_keyword() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "cursor".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("image-set(url(\"c1.cur\") 1x, url(\"c2.cur\") 2x)".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Keyword("crosshair".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(style.cursor, CursorKeyword::Crosshair);
    assert_eq!(style.cursor_images.len(), 1);
    assert_eq!(style.cursor_images[0].url, "c1.cur");
  }

  #[test]
  fn cursor_image_set_respects_device_pixel_ratio() {
    let mut style = ComputedStyle::default();
    with_image_set_dpr(2.0, || {
      apply_declaration(
        &mut style,
        &Declaration {
          property: "cursor".to_string(),
          value: PropertyValue::Multiple(vec![
            PropertyValue::Keyword(
              "image-set(url(\"low.cur\") 1x, url(\"hi.cur\") 2x)".to_string(),
            ),
            PropertyValue::Keyword(",".to_string()),
            PropertyValue::Keyword("crosshair".to_string()),
          ]),
          raw_value: String::new(),
          important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
      );
    });

    assert_eq!(style.cursor, CursorKeyword::Crosshair);
    assert_eq!(style.cursor_images.len(), 1);
    assert_eq!(style.cursor_images[0].url, "hi.cur");
  }

  #[test]
  fn list_style_image_accepts_image_set() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "list-style-image".to_string(),
        value: PropertyValue::Keyword(
          "image-set(url(\"marker-1x.png\") 1x, url(\"marker-2x.png\") 2x)".to_string(),
        ),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    assert_eq!(
      style.list_style_image,
      ListStyleImage::Url("marker-1x.png".to_string())
    );
  }

  #[test]
  fn parses_quotes_property() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "quotes".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::String("«".to_string()),
        PropertyValue::String("»".to_string()),
        PropertyValue::String("‹".to_string()),
        PropertyValue::String("›".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.quotes,
      vec![
        ("«".to_string(), "»".to_string()),
        ("‹".to_string(), "›".to_string())
      ]
    );

    let decl = Declaration {
      property: "quotes".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style.quotes.is_empty());
  }

  #[test]
  fn parses_letter_and_word_spacing() {
    let mut style = ComputedStyle::default();
    style.font_size = 20.0;
    style.letter_spacing = 3.0;

    let decl = Declaration {
      property: "letter-spacing".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.letter_spacing, 0.0);

    let decl = Declaration {
      property: "letter-spacing".to_string(),
      value: PropertyValue::Length(Length::em(0.25)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!((style.letter_spacing - 5.0).abs() < 0.01);

    let decl = Declaration {
      property: "word-spacing".to_string(),
      value: PropertyValue::Percentage(50.0),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!((style.word_spacing - 10.0).abs() < 0.01);

    let decl = Declaration {
      property: "word-spacing".to_string(),
      value: PropertyValue::Length(Length::em(-0.5)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!((style.word_spacing + 10.0).abs() < 0.01);
  }

  #[test]
  fn word_break_parses_anywhere_and_inherits() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "word-break".to_string(),
        value: PropertyValue::Keyword("anywhere".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.word_break, WordBreak::Anywhere));

    let parent = ComputedStyle {
      word_break: WordBreak::BreakAll,
      ..ComputedStyle::default()
    };
    apply_declaration(
      &mut style,
      &Declaration {
        property: "word-break".to_string(),
        value: PropertyValue::Keyword("inherit".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.word_break, WordBreak::BreakAll));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "word-break".to_string(),
        value: PropertyValue::Keyword("initial".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.word_break, WordBreak::Normal));
  }

  #[test]
  fn text_wrap_parses_and_inherits() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("nowrap".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.text_wrap, TextWrap::NoWrap));

    let parent = ComputedStyle {
      text_wrap: TextWrap::Balance,
      ..ComputedStyle::default()
    };
    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("inherit".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.text_wrap, TextWrap::Balance));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("auto".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.text_wrap, TextWrap::Auto));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "text-wrap".to_string(),
        value: PropertyValue::Keyword("initial".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &parent,
      16.0,
      16.0,
    );
    assert!(matches!(style.text_wrap, TextWrap::Auto));
  }

  #[test]
  fn parses_counter_properties() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "counter-reset".to_string(),
      value: PropertyValue::Keyword("chapter 3 section".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    let reset = style.counters.counter_reset.as_ref().unwrap();
    assert_eq!(reset.items.len(), 2);
    assert_eq!(reset.items[0].name, "chapter");
    assert_eq!(reset.items[0].value, 3);
    assert_eq!(reset.items[1].name, "section");
    assert_eq!(reset.items[1].value, 0);

    let decl = Declaration {
      property: "counter-increment".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("item".to_string()),
        PropertyValue::Number(2.0),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    let increment = style.counters.counter_increment.as_ref().unwrap();
    assert_eq!(increment.items.len(), 1);
    assert_eq!(increment.items[0].name, "item");
    assert_eq!(increment.items[0].value, 2);

    let decl = Declaration {
      property: "counter-set".to_string(),
      value: PropertyValue::Keyword("item 7".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    let set = style.counters.counter_set.as_ref().unwrap();
    assert_eq!(set.items.len(), 1);
    assert_eq!(set.items[0].name, "item");
    assert_eq!(set.items[0].value, 7);

    let decl = Declaration {
      property: "counter-increment".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style
      .counters
      .counter_increment
      .as_ref()
      .map(|c| c.is_empty())
      .unwrap_or(false));
  }

  #[test]
  fn background_shorthand_resets_unspecified_fields() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      repeat: BackgroundRepeat::repeat_x(),
      attachment: BackgroundAttachment::Fixed,
      position: BackgroundPosition::Position {
        x: crate::style::types::BackgroundPositionComponent {
          alignment: 1.0,
          offset: Length::px(5.0),
        },
        y: crate::style::types::BackgroundPositionComponent {
          alignment: 1.0,
          offset: Length::px(5.0),
        },
      },
      size: BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(10.0)),
        BackgroundSizeComponent::Auto,
      ),
      origin: BackgroundBox::ContentBox,
      clip: BackgroundBox::ContentBox,
      ..BackgroundLayer::default()
    }]);

    let decl = Declaration {
      property: "background".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Url("example.png".to_string()),
        PropertyValue::Keyword("no-repeat".to_string()),
        PropertyValue::Keyword("right".to_string()),
        PropertyValue::Keyword("/".to_string()),
        PropertyValue::Keyword("contain".to_string()),
        PropertyValue::Keyword("fixed".to_string()),
        PropertyValue::Keyword("content-box".to_string()),
        PropertyValue::Keyword("padding-box".to_string()),
        PropertyValue::Color(Color::Rgba(Rgba::RED)),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert_eq!(style.background_color, Rgba::RED);
    let layer = &style.background_layers[0];
    assert!(matches!(layer.image, Some(BackgroundImage::Url(ref s)) if s == "example.png"));
    assert_eq!(layer.repeat, BackgroundRepeat::no_repeat());
    let BackgroundPosition::Position { x, y } = layer.position;
    assert!((x.alignment - 1.0).abs() < 0.01);
    assert!(x.offset.is_zero());
    assert!((y.alignment - 0.5).abs() < 0.01);
    assert!(y.offset.is_zero());
    assert_eq!(
      layer.size,
      BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)
    );
    assert_eq!(layer.attachment, BackgroundAttachment::Fixed);
    assert_eq!(layer.origin, BackgroundBox::ContentBox);
    assert_eq!(layer.clip, BackgroundBox::PaddingBox);
  }

  #[test]
  fn background_longhand_lists_expand_layers() {
    let mut style = ComputedStyle::default();
    // Two images
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Url("a.png".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Url("b.png".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.background_layers.len(), 2);
    // Only one size provided; values repeat across layers
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-size".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Length(Length::px(10.0)),
          PropertyValue::Length(Length::px(20.0)),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.background_layers[0].size,
      BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(10.0)),
        BackgroundSizeComponent::Length(Length::px(20.0))
      )
    );
    assert_eq!(
      style.background_layers[1].size,
      BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(10.0)),
        BackgroundSizeComponent::Length(Length::px(20.0))
      )
    );
    // Two repeats; both layers updated
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-repeat".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("no-repeat".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Keyword("space".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.background_layers[0].repeat,
      BackgroundRepeat::no_repeat()
    );
    assert_eq!(
      style.background_layers[1].repeat.x,
      BackgroundRepeatKeyword::Space
    );
  }

  #[test]
  fn background_longhand_extra_values_drop_with_single_image_layer() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Url("one.png".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    // Two positions, but only one image -> second is ignored for layer construction
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-position".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("left".to_string()),
          PropertyValue::Keyword("top".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Keyword("right".to_string()),
          PropertyValue::Keyword("bottom".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.background_layers.len(), 1);
    let BackgroundPosition::Position { x, y } = style.background_layers[0].position;
    assert!((x.alignment - 0.0).abs() < 0.01);
    assert!((y.alignment - 0.0).abs() < 0.01);
  }

  #[test]
  fn background_image_image_set_picks_1x_candidate() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Keyword(
          "image-set(url(\"low.png\") 1x, url(\"retina.png\") 2x)".to_string(),
        ),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    let layer = &style.background_layers[0];
    assert!(matches!(
        layer.image,
        Some(BackgroundImage::Url(ref url)) if url == "low.png"
    ));
  }

  #[test]
  fn background_image_image_set_honors_device_pixel_ratio() {
    let mut style = ComputedStyle::default();
    with_image_set_dpr(2.0, || {
      apply_declaration(
        &mut style,
        &Declaration {
          property: "background-image".to_string(),
          value: PropertyValue::Keyword(
            "image-set(url(\"low.png\") 1x, url(\"retina.png\") 2x)".to_string(),
          ),
          raw_value: String::new(),
          important: false,
        },
        &ComputedStyle::default(),
        16.0,
        16.0,
      );
    });

    assert!(matches!(
        style.background_layers[0].image,
        Some(BackgroundImage::Url(ref url)) if url == "retina.png"
    ));
  }

  #[test]
  fn background_image_image_set_uses_best_available_resolution() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Keyword("image-set(url(\"hi.png\") 192dpi)".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
        style.background_layers[0].image,
        Some(BackgroundImage::Url(ref url)) if url == "hi.png"
    ));

    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Keyword(
          "image-set(url(\"smaller.png\") 0.5x, url(\"bigger.png\") 0.75x)".to_string(),
        ),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(
        style.background_layers[0].image,
        Some(BackgroundImage::Url(ref url)) if url == "bigger.png"
    ));
  }

  #[test]
  fn background_shorthand_accepts_image_set() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword(
            "image-set(url(\"one-x.png\") 1x, url(\"two-x.png\") 2x)".to_string(),
          ),
          PropertyValue::Keyword("no-repeat".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    let layer = &style.background_layers[0];
    assert!(matches!(
        layer.image,
        Some(BackgroundImage::Url(ref url)) if url == "one-x.png"
    ));
    assert_eq!(layer.repeat, BackgroundRepeat::no_repeat());
  }

  #[test]
  fn background_blend_mode_repeats_and_truncates() {
    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Url("a.png".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Url("b.png".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );

    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-blend-mode".to_string(),
        value: PropertyValue::Keyword("screen".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.background_layers[0].blend_mode, MixBlendMode::Screen);
    assert_eq!(style.background_layers[1].blend_mode, MixBlendMode::Screen);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-blend-mode".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("multiply".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Keyword("overlay".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(
      style.background_layers[0].blend_mode,
      MixBlendMode::Multiply
    );
    assert_eq!(style.background_layers[1].blend_mode, MixBlendMode::Overlay);

    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-image".to_string(),
        value: PropertyValue::Url("single.png".to_string()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    apply_declaration(
      &mut style,
      &Declaration {
        property: "background-blend-mode".to_string(),
        value: PropertyValue::Multiple(vec![
          PropertyValue::Keyword("darken".to_string()),
          PropertyValue::Keyword(",".to_string()),
          PropertyValue::Keyword("lighten".to_string()),
        ]),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.background_layers.len(), 1);
    assert_eq!(style.background_layers[0].blend_mode, MixBlendMode::Darken);
  }

  #[test]
  fn background_shorthand_color_only_resets_to_initials() {
    let mut style = ComputedStyle::default();
    style.set_background_layers(vec![BackgroundLayer {
      repeat: BackgroundRepeat::repeat_x(),
      origin: BackgroundBox::ContentBox,
      clip: BackgroundBox::ContentBox,
      size: BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(12.0)),
        BackgroundSizeComponent::Auto,
      ),
      ..BackgroundLayer::default()
    }]);

    let decl = Declaration {
      property: "background".to_string(),
      value: PropertyValue::Color(Color::Rgba(Rgba::RED)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert_eq!(style.background_color, Rgba::RED);
    assert!(style
      .background_layers
      .first()
      .and_then(|l| l.image.as_ref())
      .is_none());
    let layer = &style.background_layers[0];
    assert_eq!(layer.repeat, BackgroundRepeat::repeat());
    let BackgroundPosition::Position { x, y } = layer.position;
    assert!(x.offset.is_zero() && (x.alignment - 0.0).abs() < 0.01);
    assert!(y.offset.is_zero() && (y.alignment - 0.0).abs() < 0.01);
    assert_eq!(
      layer.size,
      BackgroundSize::Explicit(BackgroundSizeComponent::Auto, BackgroundSizeComponent::Auto)
    );
    assert_eq!(layer.attachment, BackgroundAttachment::Scroll);
    assert_eq!(layer.origin, BackgroundBox::PaddingBox);
    assert_eq!(layer.clip, BackgroundBox::BorderBox);
  }

  #[test]
  fn background_shorthand_none_resets_and_clears_image() {
    let mut style = ComputedStyle::default();
    style.background_color = Rgba::RED;
    style.set_background_layers(vec![BackgroundLayer {
      image: Some(BackgroundImage::Url("foo.png".to_string())),
      repeat: BackgroundRepeat::repeat_x(),
      origin: BackgroundBox::ContentBox,
      ..BackgroundLayer::default()
    }]);

    let decl = Declaration {
      property: "background".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert_eq!(style.background_color, Rgba::TRANSPARENT);
    assert_eq!(style.background_layers.len(), 1);
    let layer = &style.background_layers[0];
    assert!(layer.image.is_none());
    assert_eq!(layer.repeat, BackgroundRepeat::repeat());
    assert_eq!(layer.origin, BackgroundBox::PaddingBox);
    assert_eq!(layer.clip, BackgroundBox::BorderBox);
  }

  #[test]
  fn parses_background_repeat_keywords_and_pairs() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "background-repeat".to_string(),
      value: PropertyValue::Keyword("space".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.background_layers[0].repeat.x,
      BackgroundRepeatKeyword::Space
    );
    assert_eq!(
      style.background_layers[0].repeat.y,
      BackgroundRepeatKeyword::Space
    );

    let decl = Declaration {
      property: "background-repeat".to_string(),
      value: PropertyValue::Keyword("repeat-x".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.background_layers[0].repeat.x,
      BackgroundRepeatKeyword::Repeat
    );
    assert_eq!(
      style.background_layers[0].repeat.y,
      BackgroundRepeatKeyword::NoRepeat
    );

    let decl = Declaration {
      property: "background-repeat".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("space".to_string()),
        PropertyValue::Keyword("round".to_string()),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.background_layers[0].repeat.x,
      BackgroundRepeatKeyword::Space
    );
    assert_eq!(
      style.background_layers[0].repeat.y,
      BackgroundRepeatKeyword::Round
    );
  }

  #[test]
  fn parses_background_size_components_and_defaults() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "background-size".to_string(),
      value: PropertyValue::Length(Length::px(25.0)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.background_layers[0].size,
      BackgroundSize::Explicit(
        BackgroundSizeComponent::Length(Length::px(25.0)),
        BackgroundSizeComponent::Auto
      )
    );

    let decl = Declaration {
      property: "background-size".to_string(),
      value: PropertyValue::Multiple(vec![
        PropertyValue::Keyword("auto".to_string()),
        PropertyValue::Percentage(50.0),
      ]),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.background_layers[0].size,
      BackgroundSize::Explicit(
        BackgroundSizeComponent::Auto,
        BackgroundSizeComponent::Length(Length::percent(50.0))
      )
    );

    let decl = Declaration {
      property: "background-size".to_string(),
      value: PropertyValue::Keyword("contain".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.background_layers[0].size,
      BackgroundSize::Keyword(BackgroundSizeKeyword::Contain)
    );
  }

  #[test]
  fn parses_white_space_break_spaces() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "white-space".to_string(),
      value: PropertyValue::Keyword("break-spaces".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.white_space, WhiteSpace::BreakSpaces);
  }

  #[test]
  fn parses_line_break_anywhere() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "line-break".to_string(),
      value: PropertyValue::Keyword("anywhere".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.line_break, LineBreak::Anywhere);
  }

  #[test]
  fn parses_background_origin_and_clip() {
    let mut style = ComputedStyle::default();

    let origin_decl = Declaration {
      property: "background-origin".to_string(),
      value: PropertyValue::Keyword("content-box".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &origin_decl,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.background_layers[0].origin, BackgroundBox::ContentBox);

    let clip_decl = Declaration {
      property: "background-clip".to_string(),
      value: PropertyValue::Keyword("padding-box".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &clip_decl,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.background_layers[0].clip, BackgroundBox::PaddingBox);
  }

  #[test]
  fn parses_filter_list_with_lengths_and_numbers() {
    let filters = parse_filter_list(&PropertyValue::Keyword(
      "blur(4px) brightness(50%)".to_string(),
    ))
    .expect("filters");
    assert_eq!(filters.len(), 2);
    match &filters[0] {
      FilterFunction::Blur(len) => assert!((len.to_px() - 4.0).abs() < 0.01),
      _ => panic!("expected blur filter"),
    }
    match &filters[1] {
      FilterFunction::Brightness(v) => assert!((*v - 0.5).abs() < 0.001),
      _ => panic!("expected brightness filter"),
    }
  }

  #[test]
  fn filter_none_returns_empty_list() {
    let filters = parse_filter_list(&PropertyValue::Keyword("none".to_string())).expect("filters");
    assert!(filters.is_empty());
  }

  #[test]
  fn parses_svg_url_filter() {
    let filters = parse_filter_list(&PropertyValue::Keyword(
      "url(\"filters.svg#blur\")".to_string(),
    ))
    .expect("filters");
    assert_eq!(filters.len(), 1);
    assert!(matches!(filters.first(), Some(FilterFunction::Url(url)) if url == "filters.svg#blur"));
  }

  #[test]
  fn filter_arguments_default_when_omitted() {
    let filters = parse_filter_list(&PropertyValue::Keyword(
            "blur() brightness() contrast() grayscale() sepia() saturate() invert() opacity() hue-rotate()".to_string(),
        ))
        .expect("filters");
    assert_eq!(filters.len(), 9);
    match &filters[0] {
      FilterFunction::Blur(len) => assert!(len.to_px().abs() < 0.001),
      other => panic!("expected blur default, got {:?}", other),
    }
    match &filters[1] {
      FilterFunction::Brightness(v) => assert!((*v - 1.0).abs() < 0.001),
      other => panic!("expected brightness default, got {:?}", other),
    }
    match &filters[2] {
      FilterFunction::Contrast(v) => assert!((*v - 1.0).abs() < 0.001),
      other => panic!("expected contrast default, got {:?}", other),
    }
    match &filters[3] {
      FilterFunction::Grayscale(v) => assert!((*v - 1.0).abs() < 0.001),
      other => panic!("expected grayscale default, got {:?}", other),
    }
    match &filters[4] {
      FilterFunction::Sepia(v) => assert!((*v - 1.0).abs() < 0.001),
      other => panic!("expected sepia default, got {:?}", other),
    }
    match &filters[5] {
      FilterFunction::Saturate(v) => assert!((*v - 1.0).abs() < 0.001),
      other => panic!("expected saturate default, got {:?}", other),
    }
    match &filters[6] {
      FilterFunction::Invert(v) => assert!((*v - 1.0).abs() < 0.001),
      other => panic!("expected invert default, got {:?}", other),
    }
    match &filters[7] {
      FilterFunction::Opacity(v) => assert!((*v - 1.0).abs() < 0.001),
      other => panic!("expected opacity default, got {:?}", other),
    }
    match &filters[8] {
      FilterFunction::HueRotate(v) => assert!((*v).abs() < 0.001),
      other => panic!("expected hue-rotate default, got {:?}", other),
    }
  }

  #[test]
  fn hue_rotate_accepts_calc_angles() {
    let filters = parse_filter_list(&PropertyValue::Keyword(
      "hue-rotate(calc(1turn / 2))".to_string(),
    ))
    .expect("filters");
    match &filters[0] {
      FilterFunction::HueRotate(v) => assert!((*v - 180.0).abs() < 0.01),
      other => panic!("expected hue-rotate filter, got {:?}", other),
    }
  }

  #[test]
  fn parses_drop_shadow_defaulting_to_current_color() {
    let filters = parse_filter_list(&PropertyValue::Keyword(
      "drop-shadow(2px 3px 4px)".to_string(),
    ))
    .expect("filters");
    assert_eq!(filters.len(), 1);
    match &filters[0] {
      FilterFunction::DropShadow(shadow) => {
        assert!(matches!(shadow.color, FilterColor::CurrentColor));
        assert!((shadow.offset_x.to_px() - 2.0).abs() < 0.01);
        assert!((shadow.offset_y.to_px() - 3.0).abs() < 0.01);
      }
      _ => panic!("expected drop-shadow"),
    }
  }

  #[test]
  fn drop_shadow_accepts_spread_length() {
    let filters = parse_filter_list(&PropertyValue::Keyword(
      "drop-shadow(1px 2px 3px 4px)".to_string(),
    ))
    .expect("filters");
    match &filters[0] {
      FilterFunction::DropShadow(shadow) => {
        assert!((shadow.blur_radius.to_px() - 3.0).abs() < 0.01);
        assert!((shadow.spread.to_px() - 4.0).abs() < 0.01);
      }
      _ => panic!("expected drop-shadow with spread"),
    }
  }

  #[test]
  fn parses_plus_lighter_mix_blend_mode() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "mix-blend-mode".to_string(),
      value: PropertyValue::Keyword("plus-lighter".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.mix_blend_mode, MixBlendMode::PlusLighter));
  }

  #[test]
  fn parses_plus_darker_and_color_spaces_mix_blend_mode() {
    let mut style = ComputedStyle::default();
    let darker = Declaration {
      property: "mix-blend-mode".to_string(),
      value: PropertyValue::Keyword("plus-darker".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &darker, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.mix_blend_mode, MixBlendMode::PlusDarker));

    let hsv = Declaration {
      property: "mix-blend-mode".to_string(),
      value: PropertyValue::Keyword("hue-hsv".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &hsv, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.mix_blend_mode, MixBlendMode::HueHsv));

    let oklch = Declaration {
      property: "mix-blend-mode".to_string(),
      value: PropertyValue::Keyword("color-oklch".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &oklch, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.mix_blend_mode, MixBlendMode::ColorOklch));
  }

  #[test]
  fn parses_backdrop_filter_as_filter_list() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "backdrop-filter".to_string(),
      value: PropertyValue::Keyword("blur(5px)".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.backdrop_filter.len(), 1);
    match &style.backdrop_filter[0] {
      FilterFunction::Blur(len) => assert!((len.to_px() - 5.0).abs() < 0.01),
      _ => panic!("expected blur"),
    }
  }

  #[test]
  fn filter_lengths_reject_percentages() {
    assert!(
      parse_filter_list(&PropertyValue::Keyword("blur(10%)".to_string())).is_none(),
      "percentage blur should be invalid"
    );
    assert!(
      parse_filter_list(&PropertyValue::Keyword(
        "drop-shadow(1px 2px 10%)".to_string()
      ))
      .is_none(),
      "percentage drop-shadow blur should be invalid"
    );
  }

  #[test]
  fn negative_blur_lengths_are_invalid() {
    assert!(
      parse_filter_list(&PropertyValue::Keyword("blur(-1px)".to_string())).is_none(),
      "negative blur should be invalid"
    );
    assert!(
      parse_filter_list(&PropertyValue::Keyword(
        "drop-shadow(1px 2px -5px)".to_string()
      ))
      .is_none(),
      "negative drop-shadow blur should be invalid"
    );
    assert!(
      parse_filter_list(&PropertyValue::Keyword("blur(-0.5px)".to_string())).is_none(),
      "sub-pixel negative blur should still be rejected"
    );
  }

  #[test]
  fn parses_tab_size_number_and_length() {
    let mut style = ComputedStyle::default();
    let number_decl = Declaration {
      property: "tab-size".to_string(),
      value: PropertyValue::Number(4.0),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &number_decl,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.tab_size, TabSize::Number(n) if (n - 4.0).abs() < 0.001));

    let length_decl = Declaration {
      property: "tab-size".to_string(),
      value: PropertyValue::Length(Length::px(20.0)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &length_decl,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    match style.tab_size {
      TabSize::Length(len) => assert!((len.to_px() - 20.0).abs() < 0.001),
      TabSize::Number(n) => panic!("expected length tab size, got Number({n})"),
    }
  }

  #[test]
  fn parses_font_shorthand_with_style_weight_size_line_height_and_family() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("italic 700 20px/30px \"Fira Sans\", serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_style, FontStyle::Italic));
    assert!(matches!(style.font_weight, FontWeight::Number(700)));
    assert!(matches!(
      style.font_stretch,
      FontStretch::Normal | FontStretch::Percentage(_)
    ));
    assert!((style.font_size - 20.0).abs() < 0.01);
    match style.line_height {
      LineHeight::Length(len) => assert!((len.to_px() - 30.0).abs() < 0.01),
      _ => panic!("expected length line-height"),
    }
    assert_eq!(
      style.font_family,
      vec!["Fira Sans".to_string(), "serif".to_string()]
    );
  }

  #[test]
  fn oblique_font_angle_accepts_calc() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("oblique calc(60deg - 10deg) 16px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    match style.font_style {
      FontStyle::Oblique(Some(angle)) => assert!((angle - 50.0).abs() < 0.01),
      other => panic!("expected oblique angle, got {:?}", other),
    }
    assert!((style.font_size - 16.0).abs() < 0.01);
  }

  #[test]
  fn parses_font_shorthand_with_relative_size_and_percentage_line_height() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("bold larger/125% serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 20.0, 16.0);
    assert!(matches!(style.font_weight, FontWeight::Bold));
    assert!((style.font_stretch.to_percentage() - 100.0).abs() < 0.01);
    assert!((style.font_size - 24.0).abs() < 0.01);
    assert!(matches!(style.line_height, LineHeight::Percentage(p) if (p - 125.0).abs() < 0.001));
    assert_eq!(style.font_family, vec!["serif".to_string()]);
  }

  #[test]
  fn parses_font_shorthand_with_calc_line_height() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("bold 20px/calc(50% + 25%) serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_weight, FontWeight::Bold));
    assert!((style.font_size - 20.0).abs() < 0.01);
    assert!(matches!(style.line_height, LineHeight::Percentage(p) if (p - 75.0).abs() < 0.001));
    assert_eq!(style.font_family, vec!["serif".to_string()]);
  }

  #[test]
  fn parses_font_shorthand_with_calc_font_size() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("bold calc(10px + 5px)/normal serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_weight, FontWeight::Bold));
    assert!((style.font_size - 15.0).abs() < 0.01);
    assert!(matches!(style.line_height, LineHeight::Normal));
    assert_eq!(style.font_family, vec!["serif".to_string()]);
  }

  #[test]
  fn font_shorthand_with_negative_calc_font_size_is_ignored() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("bold calc(-10px + 2px)/normal serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    // Invalid size leaves defaults
    assert!(matches!(style.font_weight, FontWeight::Normal));
    assert!((style.font_size - 16.0).abs() < 0.01);
    assert!(matches!(style.line_height, LineHeight::Normal));
    assert_eq!(style.font_family, vec!["serif".to_string()]);
  }

  #[test]
  fn font_shorthand_with_negative_line_height_is_ignored() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("bold 20px/-1px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 20.0, 16.0);
    // Invalid shorthand should leave defaults intact.
    assert!(matches!(style.font_weight, FontWeight::Normal));
    assert!((style.font_size - 16.0).abs() < 0.01);
    assert!(matches!(style.line_height, LineHeight::Normal));
    assert_eq!(style.font_family, vec!["serif".to_string()]);
  }

  #[test]
  fn font_family_parses_quoted_names_with_commas() {
    let mut style = ComputedStyle::default();
    let value = parse_property_value("font-family", "\"Font, With, Commas\", Open Sans, serif")
      .expect("font-family parse");
    let decl = Declaration {
      property: "font-family".to_string(),
      value,
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(
      style.font_family,
      vec![
        "Font, With, Commas".to_string(),
        "Open Sans".to_string(),
        "serif".to_string()
      ]
    );
  }

  #[test]
  fn font_family_honors_global_keywords() {
    let mut parent = ComputedStyle::default();
    parent.font_family = vec!["ParentFace".to_string()];
    let mut style = ComputedStyle::default();
    style.font_family = vec!["ChildDefault".to_string()];

    let decl = Declaration {
      property: "font-family".to_string(),
      value: parse_property_value("font-family", "inherit").expect("font-family inherit"),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &parent, 16.0, 16.0);
    assert_eq!(style.font_family, parent.font_family);
  }

  #[test]
  fn font_shorthand_without_family_is_ignored() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("italic 16px".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    // Defaults stay intact because the shorthand is invalid.
    assert!(matches!(style.font_style, FontStyle::Normal));
    assert!((style.font_size - 16.0).abs() < 0.01);
    assert!(matches!(style.font_optical_sizing, FontOpticalSizing::Auto));
    assert!(matches!(
      style.font_language_override,
      crate::style::types::FontLanguageOverride::Normal
    ));
  }

  #[test]
  fn parses_font_variation_settings() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variation-settings".to_string(),
      value: PropertyValue::Keyword("\"wght\" 600, \"wdth\" 80".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.font_variation_settings.len(), 2);
    assert_eq!(style.font_variation_settings[0].tag, *b"wght");
    assert!((style.font_variation_settings[0].value - 600.0).abs() < 0.001);
    assert_eq!(style.font_variation_settings[1].tag, *b"wdth");
    assert!((style.font_variation_settings[1].value - 80.0).abs() < 0.001);
  }

  #[test]
  fn font_variation_settings_normal_clears_values() {
    let mut style = ComputedStyle::default();
    style.font_variation_settings = vec![FontVariationSetting {
      tag: *b"wght",
      value: 500.0,
    }];

    let decl = Declaration {
      property: "font-variation-settings".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style.font_variation_settings.is_empty());
  }

  #[test]
  fn parses_font_optical_sizing() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-optical-sizing".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_optical_sizing, FontOpticalSizing::None));

    let decl2 = Declaration {
      property: "font-optical-sizing".to_string(),
      value: PropertyValue::Keyword("auto".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl2, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_optical_sizing, FontOpticalSizing::Auto));
  }

  #[test]
  fn parses_font_language_override() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-language-override".to_string(),
      value: PropertyValue::Keyword("\"SRB\"".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
        style.font_language_override,
        crate::style::types::FontLanguageOverride::Override(ref tag) if tag == "SRB"
    ));

    let decl2 = Declaration {
      property: "font-language-override".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl2, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_language_override,
      crate::style::types::FontLanguageOverride::Normal
    ));
  }

  #[test]
  fn font_shorthand_does_not_reset_low_level_font_controls() {
    let mut style = ComputedStyle::default();
    let decls = vec![
      Declaration {
        property: "font-feature-settings".to_string(),
        value: PropertyValue::Keyword("\"kern\" off".to_string()),
        raw_value: String::new(),
        important: false,
      },
      Declaration {
        property: "font-variation-settings".to_string(),
        value: PropertyValue::Keyword("\"wght\" 700".to_string()),
        raw_value: String::new(),
        important: false,
      },
      Declaration {
        property: "font-language-override".to_string(),
        value: PropertyValue::Keyword("\"SRB\"".to_string()),
        raw_value: String::new(),
        important: false,
      },
      Declaration {
        property: "font".to_string(),
        value: PropertyValue::Keyword("italic 16px/20px serif".to_string()),
        raw_value: String::new(),
        important: false,
      },
    ];

    for decl in decls {
      apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    }

    assert_eq!(style.font_feature_settings.len(), 1);
    assert_eq!(style.font_variation_settings.len(), 1);
    assert!(matches!(
      style.font_language_override,
      crate::style::types::FontLanguageOverride::Override(_)
    ));
  }

  #[test]
  fn line_height_percentage_is_resolved_to_number() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Percentage(150.0),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.line_height, LineHeight::Percentage(p) if (p - 150.0).abs() < 0.001));
  }

  #[test]
  fn line_height_calc_lengths_are_accepted() {
    let mut style = ComputedStyle::default();
    let len = parse_length("calc(10px + 20px)").expect("calc length");
    let decl = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Length(len),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.line_height, LineHeight::Length(l) if (l.to_px() - 30.0).abs() < 0.001));
  }

  #[test]
  fn line_height_calc_percentages_are_normalized() {
    let mut style = ComputedStyle::default();
    let len = parse_length("calc(50% + 25%)").expect("calc percent");
    let decl = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Length(len),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.line_height, LineHeight::Percentage(p) if (p - 75.0).abs() < 0.001));
  }

  #[test]
  fn line_height_negative_calc_is_ignored() {
    let mut style = ComputedStyle::default();
    style.line_height = LineHeight::Number(1.2);
    let len = parse_length("calc(5px - 10px)").expect("calc length");
    let decl = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Length(len),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.line_height, LineHeight::Number(n) if (n - 1.2).abs() < 0.001));
  }

  #[test]
  fn line_height_percentage_length_is_normalized_to_percentage_variant() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Length(Length::percent(125.0)),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.line_height, LineHeight::Percentage(p) if (p - 125.0).abs() < 0.001));
  }

  #[test]
  fn line_height_negative_values_are_ignored() {
    let mut style = ComputedStyle::default();
    style.line_height = LineHeight::Number(1.2);

    let negative_number = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Number(-1.0),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &negative_number,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.line_height, LineHeight::Number(n) if (n - 1.2).abs() < 0.001));

    let negative_percent = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Percentage(-50.0),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &negative_percent,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.line_height, LineHeight::Number(n) if (n - 1.2).abs() < 0.001));

    let negative_length = Declaration {
      property: "line-height".to_string(),
      value: PropertyValue::Length(Length::px(-10.0)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(
      &mut style,
      &negative_length,
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert!(matches!(style.line_height, LineHeight::Number(n) if (n - 1.2).abs() < 0.001));
  }

  #[test]
  fn font_size_keywords_and_percentages_resolve_against_parent() {
    let mut style = ComputedStyle::default();

    let decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Keyword("large".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!((style.font_size - 19.2).abs() < 0.01);

    let decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Percentage(150.0),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 20.0, 16.0);
    assert!((style.font_size - 30.0).abs() < 0.01);

    let decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Length(Length::em(2.0)),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 10.0, 16.0);
    assert!((style.font_size - 20.0).abs() < 0.01);

    let decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Length(parse_length("calc(10px + 5px)").unwrap()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!((style.font_size - 15.0).abs() < 0.01);

    let decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Length(parse_length("calc(50% + 0%)").unwrap()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 20.0, 16.0);
    assert!((style.font_size - 10.0).abs() < 0.01);

    let decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Length(parse_length("calc(-10px + 2px)").unwrap()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    // Negative calc is ignored; previous value (10.0 from percent) stays.
    assert!((style.font_size - 10.0).abs() < 0.01);
  }

  #[test]
  fn font_size_viewport_units_resolve_with_viewport_dimensions() {
    let mut style = ComputedStyle::default();
    let viewport = Size::new(1000.0, 500.0);

    let decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Length(parse_length("10vw").unwrap()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration_with_viewport(
      &mut style,
      &decl,
      &ComputedStyle::default(),
      16.0,
      16.0,
      viewport,
    );
    assert!((style.font_size - 100.0).abs() < 0.01);

    let vh_decl = Declaration {
      property: "font-size".to_string(),
      value: PropertyValue::Length(parse_length("5vh").unwrap()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration_with_viewport(
      &mut style,
      &vh_decl,
      &ComputedStyle::default(),
      16.0,
      16.0,
      viewport,
    );
    assert!((style.font_size - 25.0).abs() < 0.01);
  }

  #[test]
  fn font_style_oblique_angle_parses_and_ranges() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-style".to_string(),
      value: PropertyValue::Keyword("oblique 20deg".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 20.0).abs() < 0.01));

    let invalid = Declaration {
      property: "font-style".to_string(),
      value: PropertyValue::Keyword("oblique 120deg".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &invalid, &ComputedStyle::default(), 16.0, 16.0);
    // Out-of-range angle invalidates the declaration; previous value remains.
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 20.0).abs() < 0.01));
  }

  #[test]
  fn font_style_oblique_angle_rejects_out_of_range_calc() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-style".to_string(),
      value: PropertyValue::Keyword("oblique 10deg".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 10.0).abs() < 0.01));

    let invalid = Declaration {
      property: "font-style".to_string(),
      value: PropertyValue::Keyword("oblique calc(120deg - 10deg)".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &invalid, &ComputedStyle::default(), 16.0, 16.0);
    // Calculated angle (110deg) is out of range, so the declaration is ignored.
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 10.0).abs() < 0.01));
  }

  #[test]
  fn calc_zero_accepted_for_margin_and_inset() {
    let lengths = extract_margin_values(&PropertyValue::Keyword("calc(0)".into()))
      .expect("calc(0) should parse as zero length");
    assert_eq!(lengths, vec![Some(Length::px(0.0))]);

    let mut style = ComputedStyle::default();
    apply_declaration(
      &mut style,
      &Declaration {
        property: "inset".to_string(),
        value: PropertyValue::Keyword("calc(0)".into()),
        raw_value: String::new(),
        important: false,
      },
      &ComputedStyle::default(),
      16.0,
      16.0,
    );
    assert_eq!(style.top, Some(Length::px(0.0)));
    assert_eq!(style.right, Some(Length::px(0.0)));
    assert_eq!(style.bottom, Some(Length::px(0.0)));
    assert_eq!(style.left, Some(Length::px(0.0)));
  }

  #[test]
  fn font_shorthand_oblique_angle_parses_and_rejects_out_of_range() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("oblique 15deg 16px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 15.0).abs() < 0.01));
    assert!((style.font_size - 16.0).abs() < 0.01);

    let invalid = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("oblique 100deg 20px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &invalid, &ComputedStyle::default(), 16.0, 16.0);
    // Invalid oblique angle makes the declaration invalid; style stays unchanged.
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 15.0).abs() < 0.01));
    assert!((style.font_size - 16.0).abs() < 0.01);
  }

  #[test]
  fn oblique_font_style_rejects_out_of_range_angle() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-style".to_string(),
      value: PropertyValue::Keyword("oblique 120deg".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert_eq!(style.font_style, FontStyle::Normal);
  }

  #[test]
  fn font_shorthand_oblique_angle_rejects_out_of_range_calc() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("oblique 12deg 16px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 12.0).abs() < 0.01));

    let invalid = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("oblique calc(95deg + 10deg) 18px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &invalid, &ComputedStyle::default(), 16.0, 16.0);
    // Out-of-range calc angle invalidates the font shorthand; previous values remain.
    assert!(matches!(style.font_style, FontStyle::Oblique(Some(a)) if (a - 12.0).abs() < 0.01));
    assert!((style.font_size - 16.0).abs() < 0.01);
  }

  #[test]
  fn parses_font_stretch_longhand_keywords_and_percentages() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-stretch".to_string(),
      value: PropertyValue::Keyword("expanded".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_stretch, FontStretch::Expanded));

    let decl = Declaration {
      property: "font-stretch".to_string(),
      value: PropertyValue::Percentage(125.0),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!((style.font_stretch.to_percentage() - 125.0).abs() < 0.01);
  }

  #[test]
  fn font_shorthand_accepts_font_stretch_keyword() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("italic bold condensed 16px/20px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(
      (style.font_stretch.to_percentage() - FontStretch::Condensed.to_percentage()).abs() < 0.01
    );
    assert!(matches!(style.font_weight, FontWeight::Bold));
    assert!(matches!(style.font_style, FontStyle::Italic));
    assert!((style.font_size - 16.0).abs() < 0.01);
    assert!(matches!(style.line_height, LineHeight::Length(_)));
  }

  #[test]
  fn font_shorthand_resets_variant_subproperties() {
    let mut style = ComputedStyle::default();
    style.font_variant_ligatures.common = false;
    style.font_variant_caps = FontVariantCaps::AllSmallCaps;
    style.font_variant_alternates.historical_forms = true;
    style.font_variant_numeric.figure = NumericFigure::Oldstyle;
    style.font_variant_east_asian.width = Some(EastAsianWidth::FullWidth);
    style.font_variant_position = FontVariantPosition::Super;
    style.font_feature_settings.push(FontFeatureSetting {
      tag: *b"TEST",
      value: 1,
    });
    style.font_variant_emoji = FontVariantEmoji::Emoji;

    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("italic bold small-caps 16px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    // font shorthand resets variant subproperties to initial values and maps small-caps to caps.
    assert_eq!(
      style.font_variant_ligatures,
      FontVariantLigatures::default()
    );
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::SmallCaps
    ));
    assert_eq!(
      style.font_variant_alternates,
      FontVariantAlternates::default()
    );
    assert_eq!(style.font_variant_numeric, FontVariantNumeric::default());
    assert_eq!(
      style.font_variant_east_asian,
      FontVariantEastAsian::default()
    );
    assert!(matches!(
      style.font_variant_position,
      FontVariantPosition::Normal
    ));
    assert!(matches!(style.font_variant_emoji, FontVariantEmoji::Normal));
    // font-feature-settings should remain untouched by the shorthand
    assert_eq!(style.font_feature_settings.len(), 1);
  }

  #[test]
  fn parses_font_variant_longhand_and_shorthand() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("small-caps".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_variant, FontVariant::SmallCaps));

    let decl = Declaration {
      property: "font".to_string(),
      value: PropertyValue::Keyword("small-caps 16px serif".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_variant, FontVariant::SmallCaps));
  }

  #[test]
  fn font_variant_invalid_token_preserves_previous() {
    let mut style = ComputedStyle::default();
    style.font_variant_caps = FontVariantCaps::AllSmallCaps;
    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("small-caps bogus".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::AllSmallCaps
    ));
  }

  #[test]
  fn font_variant_conflicting_caps_invalidates_declaration() {
    let mut style = ComputedStyle::default();
    style.font_variant_caps = FontVariantCaps::AllSmallCaps;
    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("small-caps all-petite-caps".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::AllSmallCaps
    ));
  }

  #[test]
  fn font_variant_normal_resets_all_subproperties() {
    let mut style = ComputedStyle::default();
    style.font_variant = FontVariant::SmallCaps;
    style.font_variant_caps = FontVariantCaps::AllSmallCaps;
    style.font_variant_alternates.historical_forms = true;
    style.font_variant_ligatures.common = false;
    style.font_variant_numeric.figure = NumericFigure::Oldstyle;
    style.font_variant_numeric.slashed_zero = true;
    style.font_variant_east_asian.variant = Some(EastAsianVariant::Traditional);
    style.font_variant_position = FontVariantPosition::Super;

    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert!(matches!(style.font_variant, FontVariant::Normal));
    assert!(matches!(style.font_variant_caps, FontVariantCaps::Normal));
    assert_eq!(
      style.font_variant_alternates,
      FontVariantAlternates::default()
    );
    assert_eq!(
      style.font_variant_ligatures,
      FontVariantLigatures::default()
    );
    assert_eq!(style.font_variant_numeric, FontVariantNumeric::default());
    assert_eq!(
      style.font_variant_east_asian,
      FontVariantEastAsian::default()
    );
    assert!(matches!(
      style.font_variant_position,
      FontVariantPosition::Normal
    ));
  }

  #[test]
  fn font_variant_shorthand_sets_components() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword(
        "small-caps oldstyle-nums tabular-nums stacked-fractions ordinal slashed-zero \
                 jis90 proportional-width ruby no-common-ligatures discretionary-ligatures \
                 historical-forms styleset(1,2) swash(3) annotation(note) sub"
          .to_string(),
      ),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert!(matches!(style.font_variant, FontVariant::SmallCaps));
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::SmallCaps
    ));
    assert!(matches!(
      style.font_variant_numeric.figure,
      NumericFigure::Oldstyle
    ));
    assert!(matches!(
      style.font_variant_numeric.spacing,
      NumericSpacing::Tabular
    ));
    assert!(matches!(
      style.font_variant_numeric.fraction,
      NumericFraction::Stacked
    ));
    assert!(style.font_variant_numeric.ordinal);
    assert!(style.font_variant_numeric.slashed_zero);
    assert!(matches!(
      style.font_variant_east_asian.variant,
      Some(EastAsianVariant::Jis90)
    ));
    assert!(matches!(
      style.font_variant_east_asian.width,
      Some(EastAsianWidth::ProportionalWidth)
    ));
    assert!(style.font_variant_east_asian.ruby);
    assert!(!style.font_variant_ligatures.common);
    assert!(style.font_variant_ligatures.discretionary);
    assert!(style.font_variant_ligatures.contextual);
    assert!(style.font_variant_alternates.historical_forms);
    assert_eq!(style.font_variant_alternates.stylesets, vec![1, 2]);
    assert_eq!(style.font_variant_alternates.swash, Some(3));
    assert_eq!(
      style.font_variant_alternates.annotation.as_deref(),
      Some("note")
    );
    assert!(matches!(
      style.font_variant_position,
      FontVariantPosition::Sub
    ));
  }

  #[test]
  fn font_variant_shorthand_allows_whitespace_inside_function_tokens() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("small-caps styleset(1 2 3) swash(4)".to_string()),
      raw_value: String::new(),
      important: false,
    };

    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert_eq!(style.font_variant_alternates.stylesets, vec![1, 2, 3]);
    assert_eq!(style.font_variant_alternates.swash, Some(4));
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::SmallCaps
    ));
  }

  #[test]
  fn font_variant_shorthand_resets_omitted_components_to_initial() {
    let mut style = ComputedStyle::default();
    style.font_variant_ligatures.common = false;
    style.font_variant_numeric.fraction = NumericFraction::Stacked;
    style.font_variant_east_asian.variant = Some(EastAsianVariant::Jis04);
    style.font_variant_alternates.historical_forms = true;
    style.font_variant_position = FontVariantPosition::Super;

    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("small-caps oldstyle-nums".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert!(matches!(style.font_variant, FontVariant::SmallCaps));
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::SmallCaps
    ));
    assert!(matches!(
      style.font_variant_numeric.figure,
      NumericFigure::Oldstyle
    ));
    assert!(matches!(
      style.font_variant_numeric.spacing,
      NumericSpacing::Normal
    ));
    assert!(matches!(
      style.font_variant_numeric.fraction,
      NumericFraction::Normal
    ));
    assert_eq!(
      style.font_variant_ligatures,
      FontVariantLigatures::default()
    );
    assert_eq!(
      style.font_variant_alternates,
      FontVariantAlternates::default()
    );
    assert_eq!(
      style.font_variant_east_asian,
      FontVariantEastAsian::default()
    );
    assert!(matches!(
      style.font_variant_position,
      FontVariantPosition::Normal
    ));
  }

  #[test]
  fn font_variant_shorthand_conflict_invalidates_entire_declaration() {
    let mut style = ComputedStyle::default();
    style.font_variant = FontVariant::SmallCaps;
    style.font_variant_caps = FontVariantCaps::AllSmallCaps;
    style.font_variant_numeric.spacing = NumericSpacing::Proportional;
    style.font_variant_position = FontVariantPosition::Super;

    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("small-caps tabular-nums proportional-nums".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);

    assert!(matches!(style.font_variant, FontVariant::SmallCaps));
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::AllSmallCaps
    ));
    assert!(matches!(
      style.font_variant_numeric.spacing,
      NumericSpacing::Proportional
    ));
    assert!(matches!(
      style.font_variant_position,
      FontVariantPosition::Super
    ));
  }

  #[test]
  fn parses_font_variant_caps_longhand() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant-caps".to_string(),
      value: PropertyValue::Keyword("all-small-caps".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_caps,
      FontVariantCaps::AllSmallCaps
    ));

    let decl = Declaration {
      property: "font-variant".to_string(),
      value: PropertyValue::Keyword("all-petite-caps titling-caps".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(
      matches!(style.font_variant_caps, FontVariantCaps::AllSmallCaps),
      "conflicting caps keywords should invalidate the declaration and keep the prior value"
    );
  }

  #[test]
  fn parses_font_variant_position() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant-position".to_string(),
      value: PropertyValue::Keyword("super".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_position,
      FontVariantPosition::Super
    ));

    let decl = Declaration {
      property: "font-variant-position".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_position,
      FontVariantPosition::Normal
    ));
  }

  #[test]
  fn parses_font_size_adjust() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-size-adjust".to_string(),
      value: PropertyValue::Number(0.7),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_size_adjust, FontSizeAdjust::Number(v) if (v - 0.7).abs() < 1e-6));

    let decl = Declaration {
      property: "font-size-adjust".to_string(),
      value: PropertyValue::Keyword("from-font".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_size_adjust, FontSizeAdjust::FromFont));

    let decl = Declaration {
      property: "font-size-adjust".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_size_adjust, FontSizeAdjust::None));
  }

  #[test]
  fn parses_font_synthesis() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-synthesis".to_string(),
      value: PropertyValue::Keyword("weight style position".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style.font_synthesis.weight);
    assert!(style.font_synthesis.style);
    assert!(!style.font_synthesis.small_caps);
    assert!(style.font_synthesis.position);

    let decl = Declaration {
      property: "font-synthesis".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(!style.font_synthesis.weight);
    assert!(!style.font_synthesis.style);
    assert!(!style.font_synthesis.small_caps);
    assert!(!style.font_synthesis.position);
  }

  #[test]
  fn parses_font_variant_emoji() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant-emoji".to_string(),
      value: PropertyValue::Keyword("emoji".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_emoji,
      crate::style::types::FontVariantEmoji::Emoji
    ));

    let decl = Declaration {
      property: "font-variant-emoji".to_string(),
      value: PropertyValue::Keyword("text".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_emoji,
      crate::style::types::FontVariantEmoji::Text
    ));

    let decl = Declaration {
      property: "font-variant-emoji".to_string(),
      value: PropertyValue::Keyword("unicode".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_emoji,
      crate::style::types::FontVariantEmoji::Unicode
    ));

    let decl = Declaration {
      property: "font-variant-emoji".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_emoji,
      crate::style::types::FontVariantEmoji::Normal
    ));
  }

  #[test]
  fn parses_font_synthesis_longhands() {
    let mut style = ComputedStyle::default();

    let decl = Declaration {
      property: "font-synthesis-weight".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(!style.font_synthesis.weight);

    let decl = Declaration {
      property: "font-synthesis-style".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(!style.font_synthesis.style);

    let decl = Declaration {
      property: "font-synthesis-small-caps".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(!style.font_synthesis.small_caps);

    let decl = Declaration {
      property: "font-synthesis-position".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(!style.font_synthesis.position);

    let reset = Declaration {
      property: "font-synthesis".to_string(),
      value: PropertyValue::Keyword("weight style small-caps position".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &reset, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style.font_synthesis.weight);
    assert!(style.font_synthesis.style);
    assert!(style.font_synthesis.small_caps);
    assert!(style.font_synthesis.position);
  }

  #[test]
  fn parses_font_variant_east_asian() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant-east-asian".to_string(),
      value: PropertyValue::Keyword("jis90 proportional-width ruby".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_east_asian.variant,
      Some(EastAsianVariant::Jis90)
    ));
    assert!(matches!(
      style.font_variant_east_asian.width,
      Some(EastAsianWidth::ProportionalWidth)
    ));
    assert!(style.font_variant_east_asian.ruby);

    let decl = Declaration {
      property: "font-variant-east-asian".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style.font_variant_east_asian.variant.is_none());
    assert!(style.font_variant_east_asian.width.is_none());
    assert!(!style.font_variant_east_asian.ruby);
  }

  #[test]
  fn font_variant_east_asian_invalid_token_is_ignored() {
    let mut style = ComputedStyle::default();
    style.font_variant_east_asian.variant = Some(EastAsianVariant::Jis78);
    let decl = Declaration {
      property: "font-variant-east-asian".to_string(),
      value: PropertyValue::Keyword("bogus".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_east_asian.variant,
      Some(EastAsianVariant::Jis78)
    ));
  }

  #[test]
  fn font_variant_east_asian_conflict_invalidates_declaration() {
    let mut style = ComputedStyle::default();
    style.font_variant_east_asian.width = Some(EastAsianWidth::FullWidth);
    let decl = Declaration {
      property: "font-variant-east-asian".to_string(),
      value: PropertyValue::Keyword("full-width proportional-width".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_east_asian.width,
      Some(EastAsianWidth::FullWidth)
    ));
  }

  #[test]
  fn parses_font_variant_numeric_longhand() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant-numeric".to_string(),
      value: PropertyValue::Keyword(
        "oldstyle-nums tabular-nums stacked-fractions ordinal slashed-zero".to_string(),
      ),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_numeric.figure,
      NumericFigure::Oldstyle
    ));
    assert!(matches!(
      style.font_variant_numeric.spacing,
      NumericSpacing::Tabular
    ));
    assert!(matches!(
      style.font_variant_numeric.fraction,
      NumericFraction::Stacked
    ));
    assert!(style.font_variant_numeric.ordinal);
    assert!(style.font_variant_numeric.slashed_zero);

    let decl = Declaration {
      property: "font-variant-numeric".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_numeric.figure,
      NumericFigure::Normal
    ));
    assert!(matches!(
      style.font_variant_numeric.spacing,
      NumericSpacing::Normal
    ));
    assert!(matches!(
      style.font_variant_numeric.fraction,
      NumericFraction::Normal
    ));
    assert!(!style.font_variant_numeric.ordinal);
    assert!(!style.font_variant_numeric.slashed_zero);
  }

  #[test]
  fn font_variant_alternates_invalid_token_is_ignored() {
    let mut style = ComputedStyle::default();
    style.font_variant_alternates.historical_forms = true;
    let decl = Declaration {
      property: "font-variant-alternates".to_string(),
      value: PropertyValue::Keyword("bogus".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(style.font_variant_alternates.historical_forms);
  }

  #[test]
  fn font_variant_alternates_conflict_invalidates_declaration() {
    let mut style = ComputedStyle::default();
    style.font_variant_alternates.stylistic = Some(1);
    let decl = Declaration {
      property: "font-variant-alternates".to_string(),
      value: PropertyValue::Keyword("stylistic(1) stylistic(2)".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.font_variant_alternates.stylistic, Some(1));
  }

  #[test]
  fn font_variant_alternates_allows_whitespace_inside_functions() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-variant-alternates".to_string(),
      value: PropertyValue::Keyword("styleset(1 2 3) character-variant(4 5) swash(6)".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert_eq!(style.font_variant_alternates.stylesets, vec![1, 2, 3]);
    assert_eq!(style.font_variant_alternates.character_variants, vec![4, 5]);
    assert_eq!(style.font_variant_alternates.swash, Some(6));
  }

  #[test]
  fn font_variant_numeric_invalid_token_is_ignored() {
    let mut style = ComputedStyle::default();
    style.font_variant_numeric.figure = NumericFigure::Lining;
    let decl = Declaration {
      property: "font-variant-numeric".to_string(),
      value: PropertyValue::Keyword("bogus".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_numeric.figure,
      NumericFigure::Lining
    ));
  }

  #[test]
  fn font_variant_numeric_conflict_invalidates_declaration() {
    let mut style = ComputedStyle::default();
    style.font_variant_numeric.spacing = NumericSpacing::Proportional;
    let decl = Declaration {
      property: "font-variant-numeric".to_string(),
      value: PropertyValue::Keyword("tabular-nums proportional-nums".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(
      style.font_variant_numeric.spacing,
      NumericSpacing::Proportional
    ));
  }

  #[test]
  fn font_variant_ligatures_invalid_token_is_ignored() {
    let mut style = ComputedStyle::default();
    style.font_variant_ligatures.common = false;
    let decl = Declaration {
      property: "font-variant-ligatures".to_string(),
      value: PropertyValue::Keyword("foo".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(!style.font_variant_ligatures.common);
  }

  #[test]
  fn font_variant_ligatures_conflict_invalidates_declaration() {
    let mut style = ComputedStyle::default();
    style.font_variant_ligatures.contextual = false;
    let decl = Declaration {
      property: "font-variant-ligatures".to_string(),
      value: PropertyValue::Keyword("contextual no-contextual".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(!style.font_variant_ligatures.contextual);
  }

  #[test]
  fn parses_font_kerning() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "font-kerning".to_string(),
      value: PropertyValue::Keyword("none".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_kerning, FontKerning::None));

    let decl = Declaration {
      property: "font-kerning".to_string(),
      value: PropertyValue::Keyword("normal".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.font_kerning, FontKerning::Normal));
  }

  #[test]
  fn parses_float_and_clear() {
    let mut style = ComputedStyle::default();
    let decl = Declaration {
      property: "float".to_string(),
      value: PropertyValue::Keyword("left".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.float, crate::style::float::Float::Left));

    let decl = Declaration {
      property: "clear".to_string(),
      value: PropertyValue::Keyword("both".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    assert!(matches!(style.clear, crate::style::float::Clear::Both));

    let decl = Declaration {
      property: "float".to_string(),
      value: PropertyValue::Keyword("invalid".to_string()),
      raw_value: String::new(),
      important: false,
    };
    apply_declaration(&mut style, &decl, &ComputedStyle::default(), 16.0, 16.0);
    // Invalid value leaves previous value unchanged
    assert!(matches!(style.float, crate::style::float::Float::Left));
  }
}
#[derive(Default)]
struct BackgroundShorthand {
  color: Option<Rgba>,
  image: Option<BackgroundImage>,
  repeat: Option<BackgroundRepeat>,
  position: Option<BackgroundPosition>,
  size: Option<BackgroundSize>,
  attachment: Option<BackgroundAttachment>,
  origin: Option<BackgroundBox>,
  clip: Option<BackgroundBox>,
}

fn parse_background_shorthand(
  tokens: &[PropertyValue],
  current_color: Rgba,
) -> Option<BackgroundShorthand> {
  if tokens.is_empty() {
    return None;
  }

  let mut shorthand = BackgroundShorthand::default();

  // Split position/size by `/` if present
  let mut slash_idx = None;
  let mut size_end = tokens.len();
  if let Some(idx) = tokens
    .iter()
    .position(|t| matches!(t, PropertyValue::Keyword(k) if k == "/"))
  {
    slash_idx = Some(idx);
    let mut size_tokens: Vec<PropertyValue> = Vec::new();
    let mut cursor = idx + 1;
    while cursor < tokens.len() {
      let t = &tokens[cursor];
      let is_size_token = match t {
        PropertyValue::Length(_) | PropertyValue::Percentage(_) => true,
        PropertyValue::Number(n) if *n == 0.0 => true,
        PropertyValue::Keyword(k) if k == "auto" || k == "cover" || k == "contain" => true,
        _ => false,
      };
      if is_size_token {
        size_tokens.push(t.clone());
        cursor += 1;
      } else {
        break;
      }
    }
    size_end = cursor;
    let pos_tokens = &tokens[..idx];
    if !pos_tokens.is_empty() {
      if let Some(pos) = parse_background_position(&PropertyValue::Multiple(pos_tokens.to_vec())) {
        shorthand.position = Some(pos);
      }
    }
    if !size_tokens.is_empty() {
      if let Some(size) = parse_background_size(&PropertyValue::Multiple(size_tokens.clone())) {
        shorthand.size = Some(size);
      }
    }
  }

  let mut boxes: Vec<BackgroundBox> = Vec::new();
  let mut idx = 0;
  while idx < tokens.len() {
    if slash_idx == Some(idx) {
      idx = size_end;
      continue;
    }

    let token = &tokens[idx];

    // Color
    if shorthand.color.is_none() {
      let parsed_color = match token {
        PropertyValue::Color(c) => Some(c.to_rgba(current_color)),
        PropertyValue::Keyword(kw) if kw.eq_ignore_ascii_case("currentcolor") => {
          Some(current_color)
        }
        PropertyValue::Keyword(kw) => crate::style::color::Color::parse(kw)
          .ok()
          .map(|c| c.to_rgba(current_color)),
        _ => None,
      };
      if let Some(color) = parsed_color {
        shorthand.color = Some(color);
        idx += 1;
        continue;
      }
    }

    // Image
    if shorthand.image.is_none() {
      match token {
        PropertyValue::Url(url) => {
          shorthand.image = Some(BackgroundImage::Url(url.clone()));
          idx += 1;
          continue;
        }
        PropertyValue::LinearGradient { angle, stops } => {
          shorthand.image = Some(BackgroundImage::LinearGradient {
            angle: *angle,
            stops: stops.clone(),
          });
          idx += 1;
          continue;
        }
        PropertyValue::RadialGradient {
          shape,
          size,
          position,
          stops,
        } => {
          shorthand.image = Some(BackgroundImage::RadialGradient {
            shape: *shape,
            size: size.clone(),
            position: BackgroundPosition::Position {
              x: BackgroundPositionComponent {
                alignment: position.x.alignment,
                offset: position.x.offset,
              },
              y: BackgroundPositionComponent {
                alignment: position.y.alignment,
                offset: position.y.offset,
              },
            },
            stops: stops.clone(),
          });
          idx += 1;
          continue;
        }
        PropertyValue::RepeatingLinearGradient { angle, stops } => {
          shorthand.image = Some(BackgroundImage::RepeatingLinearGradient {
            angle: *angle,
            stops: stops.clone(),
          });
          idx += 1;
          continue;
        }
        PropertyValue::RepeatingRadialGradient {
          shape,
          size,
          position,
          stops,
        } => {
          shorthand.image = Some(BackgroundImage::RepeatingRadialGradient {
            shape: *shape,
            size: size.clone(),
            position: BackgroundPosition::Position {
              x: BackgroundPositionComponent {
                alignment: position.x.alignment,
                offset: position.x.offset,
              },
              y: BackgroundPositionComponent {
                alignment: position.y.alignment,
                offset: position.y.offset,
              },
            },
            stops: stops.clone(),
          });
          idx += 1;
          continue;
        }
        PropertyValue::ConicGradient {
          from_angle,
          position,
          stops,
        } => {
          shorthand.image = Some(BackgroundImage::ConicGradient {
            from_angle: *from_angle,
            position: BackgroundPosition::Position {
              x: BackgroundPositionComponent {
                alignment: position.x.alignment,
                offset: position.x.offset,
              },
              y: BackgroundPositionComponent {
                alignment: position.y.alignment,
                offset: position.y.offset,
              },
            },
            stops: stops.clone(),
          });
          idx += 1;
          continue;
        }
        PropertyValue::RepeatingConicGradient {
          from_angle,
          position,
          stops,
        } => {
          shorthand.image = Some(BackgroundImage::RepeatingConicGradient {
            from_angle: *from_angle,
            position: BackgroundPosition::Position {
              x: BackgroundPositionComponent {
                alignment: position.x.alignment,
                offset: position.x.offset,
              },
              y: BackgroundPositionComponent {
                alignment: position.y.alignment,
                offset: position.y.offset,
              },
            },
            stops: stops.clone(),
          });
          idx += 1;
          continue;
        }
        PropertyValue::Keyword(kw) if kw.to_ascii_lowercase().starts_with("image-set(") => {
          if let Some(img) = parse_image_set(kw) {
            shorthand.image = Some(img);
            idx += 1;
            continue;
          }
        }
        PropertyValue::Keyword(kw) if kw == "none" => {
          shorthand.image = None;
          idx += 1;
          continue;
        }
        _ => {}
      }
    }

    // Repeat
    if shorthand.repeat.is_none() {
      if let Some(rep) = parse_background_repeat(token) {
        shorthand.repeat = Some(rep);
        idx += 1;
        continue;
      }
      if idx + 1 < tokens.len() {
        if let PropertyValue::Keyword(_) = token {
          if let PropertyValue::Keyword(_) = tokens[idx + 1] {
            let pair = PropertyValue::Multiple(vec![token.clone(), tokens[idx + 1].clone()]);
            if let Some(rep) = parse_background_repeat(&pair) {
              shorthand.repeat = Some(rep);
              idx += 2;
              continue;
            }
          }
        }
      }
    }

    // Attachment
    if shorthand.attachment.is_none() {
      if let PropertyValue::Keyword(kw) = token {
        shorthand.attachment = match kw.as_str() {
          "scroll" => Some(BackgroundAttachment::Scroll),
          "fixed" => Some(BackgroundAttachment::Fixed),
          "local" => Some(BackgroundAttachment::Local),
          _ => None,
        };
        if shorthand.attachment.is_some() {
          idx += 1;
          continue;
        }
      }
    }

    // Background boxes
    if let Some(b) = parse_background_box(token) {
      boxes.push(b);
      idx += 1;
      continue;
    }

    // Position (if not already parsed via slash)
    if shorthand.position.is_none() {
      if let Some(pos) = parse_background_position(token) {
        shorthand.position = Some(pos);
        idx += 1;
        continue;
      }
    }

    idx += 1;
  }

  if boxes.len() == 1 {
    shorthand.origin = Some(boxes[0]);
    shorthand.clip = Some(boxes[0]);
  } else if boxes.len() >= 2 {
    shorthand.origin = Some(boxes[0]);
    shorthand.clip = Some(boxes[1]);
  }

  Some(shorthand)
}
fn parse_feature_setting<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> Result<FontFeatureSetting, cssparser::ParseError<'i, ()>> {
  parser.skip_whitespace();
  let location = parser.current_source_location();

  let tag_bytes: [u8; 4] = match parser.next()? {
    Token::QuotedString(s) | Token::Ident(s)
      if s.len() == 4 && s.as_bytes().iter().all(|b| b.is_ascii()) =>
    {
      s.as_bytes()
        .try_into()
        .map_err(|_| location.new_custom_error(()))?
    }
    _ => return Err(location.new_custom_error(())),
  };

  parser.skip_whitespace();
  let value = if let Ok(num) = parser.try_parse(|p| p.expect_number()) {
    num.max(0.0) as u32
  } else if let Ok(ident) = parser.try_parse(|p| {
    p.expect_ident()
      .map(|ident| ident.as_ref().to_ascii_lowercase())
  }) {
    match ident.as_str() {
      "on" => 1,
      "off" => 0,
      _ => return Err(location.new_custom_error(())),
    }
  } else {
    1
  };

  Ok(FontFeatureSetting {
    tag: tag_bytes,
    value,
  })
}

fn parse_variation_setting<'i, 't>(
  parser: &mut Parser<'i, 't>,
) -> Result<FontVariationSetting, cssparser::ParseError<'i, ()>> {
  parser.skip_whitespace();
  let location = parser.current_source_location();

  let tag_bytes: [u8; 4] = match parser.next()? {
    Token::QuotedString(s) | Token::Ident(s)
      if s.len() == 4 && s.as_bytes().iter().all(|b| b.is_ascii()) =>
    {
      s.as_bytes()
        .try_into()
        .map_err(|_| location.new_custom_error(()))?
    }
    _ => return Err(location.new_custom_error(())),
  };

  parser.skip_whitespace();
  let value = parser.expect_number()?;

  Ok(FontVariationSetting {
    tag: tag_bytes,
    value,
  })
}

fn inline_axis_positive(wm: WritingMode, dir: Direction) -> bool {
  match wm {
    WritingMode::HorizontalTb => dir != Direction::Rtl,
    WritingMode::VerticalRl
    | WritingMode::VerticalLr
    | WritingMode::SidewaysRl
    | WritingMode::SidewaysLr => true,
  }
}

fn block_axis_positive(wm: WritingMode) -> bool {
  match wm {
    WritingMode::VerticalRl | WritingMode::SidewaysRl => false,
    _ => true,
  }
}

fn inline_physical_sides(
  styles: &ComputedStyle,
) -> (crate::style::PhysicalSide, crate::style::PhysicalSide) {
  let horizontal = inline_axis_is_horizontal(styles.writing_mode);
  let positive = inline_axis_positive(styles.writing_mode, styles.direction);
  if horizontal {
    if positive {
      (
        crate::style::PhysicalSide::Left,
        crate::style::PhysicalSide::Right,
      )
    } else {
      (
        crate::style::PhysicalSide::Right,
        crate::style::PhysicalSide::Left,
      )
    }
  } else if positive {
    (
      crate::style::PhysicalSide::Top,
      crate::style::PhysicalSide::Bottom,
    )
  } else {
    (
      crate::style::PhysicalSide::Bottom,
      crate::style::PhysicalSide::Top,
    )
  }
}

fn block_physical_sides(
  styles: &ComputedStyle,
) -> (crate::style::PhysicalSide, crate::style::PhysicalSide) {
  let horizontal = block_axis_is_horizontal(styles.writing_mode);
  let positive = block_axis_positive(styles.writing_mode);
  if horizontal {
    if positive {
      (
        crate::style::PhysicalSide::Left,
        crate::style::PhysicalSide::Right,
      )
    } else {
      (
        crate::style::PhysicalSide::Right,
        crate::style::PhysicalSide::Left,
      )
    }
  } else if positive {
    (
      crate::style::PhysicalSide::Top,
      crate::style::PhysicalSide::Bottom,
    )
  } else {
    (
      crate::style::PhysicalSide::Bottom,
      crate::style::PhysicalSide::Top,
    )
  }
}

fn sides_for_axis(
  axis: crate::style::LogicalAxis,
  inline_sides: (crate::style::PhysicalSide, crate::style::PhysicalSide),
  block_sides: (crate::style::PhysicalSide, crate::style::PhysicalSide),
) -> (crate::style::PhysicalSide, crate::style::PhysicalSide) {
  match axis {
    crate::style::LogicalAxis::Inline => inline_sides,
    crate::style::LogicalAxis::Block => block_sides,
  }
}

fn corner_from_logical_sides(
  block_side: crate::style::PhysicalSide,
  inline_side: crate::style::PhysicalSide,
) -> Option<PhysicalCorner> {
  let vertical = match (block_side, inline_side) {
    (crate::style::PhysicalSide::Top, _) | (crate::style::PhysicalSide::Bottom, _) => {
      Some(block_side)
    }
    (_, crate::style::PhysicalSide::Top) | (_, crate::style::PhysicalSide::Bottom) => {
      Some(inline_side)
    }
    _ => None,
  }?;
  let horizontal = match (block_side, inline_side) {
    (crate::style::PhysicalSide::Left, _) | (crate::style::PhysicalSide::Right, _) => {
      Some(block_side)
    }
    (_, crate::style::PhysicalSide::Left) | (_, crate::style::PhysicalSide::Right) => {
      Some(inline_side)
    }
    _ => None,
  }?;

  match (vertical, horizontal) {
    (crate::style::PhysicalSide::Top, crate::style::PhysicalSide::Left) => {
      Some(PhysicalCorner::TopLeft)
    }
    (crate::style::PhysicalSide::Top, crate::style::PhysicalSide::Right) => {
      Some(PhysicalCorner::TopRight)
    }
    (crate::style::PhysicalSide::Bottom, crate::style::PhysicalSide::Left) => {
      Some(PhysicalCorner::BottomLeft)
    }
    (crate::style::PhysicalSide::Bottom, crate::style::PhysicalSide::Right) => {
      Some(PhysicalCorner::BottomRight)
    }
    _ => None,
  }
}

pub fn resolve_pending_logical_properties(styles: &mut ComputedStyle) {
  if styles.logical.pending.is_empty() {
    return;
  }

  let inline_sides = inline_physical_sides(styles);
  let block_sides = block_physical_sides(styles);

  let pending = std::mem::take(&mut styles.logical.pending);
  for pending_prop in pending {
    match pending_prop.property {
      crate::style::LogicalProperty::Margin { axis, start, end } => {
        let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
        if let Some(v) = start {
          set_margin_side(styles, start_side, v, pending_prop.order);
        }
        if let Some(v) = end {
          set_margin_side(styles, end_side, v, pending_prop.order);
        }
      }
      crate::style::LogicalProperty::Padding { axis, start, end } => {
        let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
        if let Some(v) = start {
          set_padding_side(styles, start_side, v, pending_prop.order);
        }
        if let Some(v) = end {
          set_padding_side(styles, end_side, v, pending_prop.order);
        }
      }
      crate::style::LogicalProperty::BorderWidth { axis, start, end } => {
        let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
        if let Some(v) = start {
          set_border_width_side(styles, start_side, v, pending_prop.order);
        }
        if let Some(v) = end {
          set_border_width_side(styles, end_side, v, pending_prop.order);
        }
      }
      crate::style::LogicalProperty::BorderStyle { axis, start, end } => {
        let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
        if let Some(v) = start {
          set_border_style_side(styles, start_side, v, pending_prop.order);
        }
        if let Some(v) = end {
          set_border_style_side(styles, end_side, v, pending_prop.order);
        }
      }
      crate::style::LogicalProperty::BorderColor { axis, start, end } => {
        let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
        if let Some(v) = start {
          set_border_color_side(styles, start_side, v, pending_prop.order);
        }
        if let Some(v) = end {
          set_border_color_side(styles, end_side, v, pending_prop.order);
        }
      }
      crate::style::LogicalProperty::InlineSize { value } => {
        if let Some(v) = value {
          set_axis_dimension(
            styles,
            crate::style::LogicalAxis::Inline,
            v,
            pending_prop.order,
          );
        }
      }
      crate::style::LogicalProperty::BlockSize { value } => {
        if let Some(v) = value {
          set_axis_dimension(
            styles,
            crate::style::LogicalAxis::Block,
            v,
            pending_prop.order,
          );
        }
      }
      crate::style::LogicalProperty::MinInlineSize { value } => {
        if let Some(v) = value {
          set_axis_min_dimension(
            styles,
            crate::style::LogicalAxis::Inline,
            v,
            pending_prop.order,
          );
        }
      }
      crate::style::LogicalProperty::MinBlockSize { value } => {
        if let Some(v) = value {
          set_axis_min_dimension(
            styles,
            crate::style::LogicalAxis::Block,
            v,
            pending_prop.order,
          );
        }
      }
      crate::style::LogicalProperty::MaxInlineSize { value } => {
        if let Some(v) = value {
          set_axis_max_dimension(
            styles,
            crate::style::LogicalAxis::Inline,
            v,
            pending_prop.order,
          );
        }
      }
      crate::style::LogicalProperty::MaxBlockSize { value } => {
        if let Some(v) = value {
          set_axis_max_dimension(
            styles,
            crate::style::LogicalAxis::Block,
            v,
            pending_prop.order,
          );
        }
      }
      crate::style::LogicalProperty::Inset { axis, start, end } => {
        let (start_side, end_side) = sides_for_axis(axis, inline_sides, block_sides);
        if let Some(v) = start {
          set_inset_side(styles, start_side, v, pending_prop.order);
        }
        if let Some(v) = end {
          set_inset_side(styles, end_side, v, pending_prop.order);
        }
      }
      crate::style::LogicalProperty::BorderCorner {
        block_start,
        inline_start,
        value,
      } => {
        let block_side = if block_start {
          block_sides.0
        } else {
          block_sides.1
        };
        let inline_side = if inline_start {
          inline_sides.0
        } else {
          inline_sides.1
        };
        if let Some(corner) = corner_from_logical_sides(block_side, inline_side) {
          set_corner_radius(styles, corner, value, pending_prop.order);
        }
      }
    }
  }
}
