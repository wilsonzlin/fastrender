//! Parsing for `<meta name="viewport">` directives.
//!
//! This is a minimal parser that covers the most common directives used by
//! real sites. It intentionally ignores unknown keys and malformed values to
//! avoid derailing layout when authors provide non-standard tokens.

use crate::dom::{DomNode, DomNodeType};

/// A parsed viewport directive.
#[derive(Debug, Clone, PartialEq)]
pub struct MetaViewport {
  /// Requested viewport width (`device-width` or a numeric value).
  pub width: Option<ViewportLength>,
  /// Requested viewport height (`device-height` or a numeric value).
  pub height: Option<ViewportLength>,
  /// Initial zoom scale.
  pub initial_scale: Option<f32>,
  /// Minimum zoom scale.
  pub minimum_scale: Option<f32>,
  /// Maximum zoom scale.
  pub maximum_scale: Option<f32>,
  /// Whether user zooming is allowed.
  pub user_scalable: Option<bool>,
}

impl Default for MetaViewport {
  fn default() -> Self {
    Self {
      width: None,
      height: None,
      initial_scale: None,
      minimum_scale: None,
      maximum_scale: None,
      user_scalable: None,
    }
  }
}

/// Length used by viewport directives.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ViewportLength {
  /// Use the device's viewport size.
  Device,
  /// Use an explicit numeric viewport size.
  Absolute(f32),
}

/// Parse the `content` attribute of a `<meta name="viewport">` tag.
///
/// Returns `None` when no recognized directives are present or all directives
/// are malformed. Unknown keys are ignored.
pub fn parse_meta_viewport_content(content: &str) -> Option<MetaViewport> {
  let mut viewport = MetaViewport::default();
  let mut seen = false;

  for directive in content.split(|c| c == ',' || c == ';') {
    let directive = directive.trim();
    if directive.is_empty() {
      continue;
    }

    let mut parts = directive.splitn(2, '=');
    let key = parts.next().unwrap_or("").trim().to_ascii_lowercase();
    let Some(raw_value) = parts.next() else {
      continue;
    };
    let value = raw_value.trim().trim_matches(|c| c == '"' || c == '\'');

    match key.as_str() {
      "width" => {
        if viewport.width.is_none() {
          if let Some(len) = parse_viewport_length(value) {
            viewport.width = Some(len);
            seen = true;
          }
        }
      }
      "height" => {
        if viewport.height.is_none() {
          if let Some(len) = parse_viewport_length(value) {
            viewport.height = Some(len);
            seen = true;
          }
        }
      }
      "initial-scale" => {
        if viewport.initial_scale.is_none() {
          if let Some(scale) = parse_positive_number(value) {
            viewport.initial_scale = Some(scale);
            seen = true;
          }
        }
      }
      "minimum-scale" => {
        if viewport.minimum_scale.is_none() {
          if let Some(scale) = parse_positive_number(value) {
            viewport.minimum_scale = Some(scale);
            seen = true;
          }
        }
      }
      "maximum-scale" => {
        if viewport.maximum_scale.is_none() {
          if let Some(scale) = parse_positive_number(value) {
            viewport.maximum_scale = Some(scale);
            seen = true;
          }
        }
      }
      "user-scalable" => {
        if viewport.user_scalable.is_none() {
          if let Some(val) = parse_user_scalable(value) {
            viewport.user_scalable = Some(val);
            seen = true;
          }
        }
      }
      _ => {}
    }
  }

  if seen {
    Some(viewport)
  } else {
    None
  }
}

/// Extracts the first valid `<meta name="viewport">` directive in document order.
///
/// Unknown and malformed directives are skipped until a valid one is found.
pub fn extract_viewport(dom: &DomNode) -> Option<MetaViewport> {
  let mut stack = vec![dom];
  while let Some(node) = stack.pop() {
    if let DomNodeType::Element {
      tag_name,
      attributes,
      ..
    } = &node.node_type
    {
      if tag_name.eq_ignore_ascii_case("meta") {
        let mut name_attr: Option<&str> = None;
        let mut content_attr: Option<&str> = None;
        for (k, v) in attributes.iter() {
          if k.eq_ignore_ascii_case("name") {
            name_attr = Some(v);
          } else if k.eq_ignore_ascii_case("content") {
            content_attr = Some(v);
          }
        }

        if name_attr
          .map(|n| n.eq_ignore_ascii_case("viewport"))
          .unwrap_or(false)
        {
          if let Some(content) = content_attr {
            if let Some(parsed) = parse_meta_viewport_content(content) {
              return Some(parsed);
            }
          }
        }
      }

      for child in node.children.iter().rev() {
        stack.push(child);
      }
    } else {
      for child in node.children.iter().rev() {
        stack.push(child);
      }
    }
  }

  None
}

fn parse_viewport_length(value: &str) -> Option<ViewportLength> {
  if value.eq_ignore_ascii_case("device-width") {
    return Some(ViewportLength::Device);
  }
  if value.eq_ignore_ascii_case("device-height") {
    return Some(ViewportLength::Device);
  }

  parse_positive_number(value).map(ViewportLength::Absolute)
}

fn parse_positive_number(value: &str) -> Option<f32> {
  let trimmed = value.trim();
  if trimmed.is_empty() {
    return None;
  }

  if let Ok(num) = trimmed.parse::<f32>() {
    if num.is_finite() && num > 0.0 {
      return Some(num);
    }
  }
  None
}

fn parse_user_scalable(value: &str) -> Option<bool> {
  let normalized = value.trim().to_ascii_lowercase();
  match normalized.as_str() {
    "yes" | "true" | "1" => Some(true),
    "no" | "false" | "0" => Some(false),
    _ => None,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn parses_common_viewport_pairs() {
    let parsed = parse_meta_viewport_content("width=device-width, initial-scale=1.5").unwrap();
    assert_eq!(parsed.width, Some(ViewportLength::Device));
    assert_eq!(parsed.initial_scale, Some(1.5));
  }

  #[test]
  fn parses_numeric_lengths_and_flags() {
    let parsed =
      parse_meta_viewport_content("width=320; height=500; maximum-scale=2; user-scalable=no")
        .unwrap();
    assert_eq!(parsed.width, Some(ViewportLength::Absolute(320.0)));
    assert_eq!(parsed.height, Some(ViewportLength::Absolute(500.0)));
    assert_eq!(parsed.maximum_scale, Some(2.0));
    assert_eq!(parsed.user_scalable, Some(false));
  }

  #[test]
  fn ignores_unknown_and_invalid_pairs() {
    assert!(parse_meta_viewport_content("foo=bar, baz=qux").is_none());
    assert!(parse_meta_viewport_content("width=,initial-scale=zero").is_none());
  }

  #[test]
  fn extracts_first_valid_meta_viewport() {
    let html = "<meta name=viewport content='width=bad'><meta name=viewport content='width=500'>";
    let dom = crate::dom::parse_html(html).unwrap();
    let parsed = extract_viewport(&dom).unwrap();
    assert_eq!(parsed.width, Some(ViewportLength::Absolute(500.0)));
  }
}
