//! Parsing helpers for responsive image HTML attributes (`srcset` / `sizes`).
//!
//! These helpers are shared by the renderer (box generation / replaced elements)
//! and developer tooling (e.g. asset prefetch) so both paths interpret author
//! markup consistently.

use crate::tree::box_tree::{SizesEntry, SizesList, SrcsetCandidate, SrcsetDescriptor};
use cssparser::{Parser, ParserInput, Token};

/// Parse an HTML `srcset` attribute into candidate URLs with descriptors.
///
/// This is a small, allocation-minimal parser intended to match the renderer's
/// internal behavior. Invalid candidate strings are ignored.
pub fn parse_srcset(attr: &str) -> Vec<SrcsetCandidate> {
  parse_srcset_with_limit(attr, usize::MAX)
}

/// Parse an HTML `srcset` attribute into candidate URLs with descriptors,
/// returning at most `max_candidates` valid entries.
///
/// This is primarily used by developer tooling (e.g. regex-based HTML asset
/// discovery) to keep memory bounded when encountering pathological attributes.
pub fn parse_srcset_with_limit(attr: &str, max_candidates: usize) -> Vec<SrcsetCandidate> {
  fn is_data_url(bytes: &[u8], start: usize) -> bool {
    if start + 5 > bytes.len() {
      return false;
    }
    let matches =
      |offset: usize, expected: u8| bytes[start + offset].to_ascii_lowercase() == expected;
    matches(0, b'd')
      && matches(1, b'a')
      && matches(2, b't')
      && matches(3, b'a')
      && bytes[start + 4] == b':'
  }

  fn is_likely_comma_in_cdn_transform_url(bytes: &[u8], url_start: usize, comma_idx: usize) -> bool {
    // Heuristic for malformed-but-common production markup:
    //
    // Many CDNs (including Condé Nast properties like wired.com) embed transform parameters
    // directly in the URL path using unescaped commas, e.g.:
    //   https://img.example/master/w_2560,c_limit/foo.jpg
    //
    // In valid `srcset`, commas are candidate separators, so such URLs should have escaped the
    // comma. But in practice, treating every comma as a separator produces bogus relative URLs
    // like `/c_limit/foo.jpg`.
    //
    // We treat a comma as part of the URL when it appears between two underscore-separated
    // transform params (`w_2560,c_limit`) that are followed by another path segment or transform.

    // Find the transform param before the comma (from the last `/`, `?`, or `#`).
    let mut segment_start = url_start;
    for i in (url_start..comma_idx).rev() {
      match bytes[i] {
        b'/' | b'?' | b'#' => {
          segment_start = i + 1;
          break;
        }
        _ => {}
      }
    }
    if segment_start >= comma_idx {
      return false;
    }
    let before = &bytes[segment_start..comma_idx];
    if !before.contains(&b'_') || before.contains(&b'.') {
      return false;
    }

    // Find the transform param after the comma (up to the next delimiter).
    let mut after_end = comma_idx + 1;
    while after_end < bytes.len() {
      let b = bytes[after_end];
      if b.is_ascii_whitespace() || matches!(b, b'/' | b'?' | b'#' | b',') {
        break;
      }
      after_end += 1;
    }
    if comma_idx + 1 >= after_end {
      return false;
    }
    let after = &bytes[comma_idx + 1..after_end];
    if !after.contains(&b'_') || after.contains(&b'.') {
      return false;
    }

    // Finally, ensure the URL continues after this transform list with another path segment
    // (common patterns include `/foo.jpg`).
    let mut idx = comma_idx + 1;
    while idx < bytes.len() {
      let b = bytes[idx];
      if b.is_ascii_whitespace() {
        break;
      }
      if b == b'/' {
        return true;
      }
      idx += 1;
    }

    false
  }

  fn is_likely_comma_in_query_numeric_list(bytes: &[u8], url_start: usize, comma_idx: usize) -> bool {
    // Some sites (notably WordPress, including nasa.gov fixtures) embed comma-separated numeric
    // values in query parameters, e.g. `?resize=300,163`.
    //
    // Like the CDN transform case above, these commas are invalid per spec but common in
    // production `srcset` values, so we treat them as part of the URL.
    let query_start = bytes[url_start..comma_idx]
      .iter()
      .rposition(|&b| b == b'?')
      .map(|pos| url_start + pos);
    let Some(query_start) = query_start else {
      return false;
    };

    // Find the start of the current query parameter (after the last '&' following '?').
    let mut param_start = query_start + 1;
    for i in (query_start + 1..comma_idx).rev() {
      if bytes[i] == b'&' {
        param_start = i + 1;
        break;
      }
    }

    // Find the '=' within the parameter.
    let eq_pos = bytes[param_start..comma_idx]
      .iter()
      .rposition(|&b| b == b'=')
      .map(|pos| param_start + pos);
    let Some(eq_pos) = eq_pos else {
      return false;
    };
    if eq_pos + 1 >= comma_idx {
      return false;
    }

    fn is_numeric_list_char(b: u8) -> bool {
      b.is_ascii_digit() || matches!(b, b'.' | b'-' | b'+' | b',')
    }

    // Verify numeric-list characters on both sides of the comma.
    let before = &bytes[eq_pos + 1..comma_idx];
    if before.is_empty() || !before.iter().all(|&b| is_numeric_list_char(b)) {
      return false;
    }
    if !before.iter().any(|b| b.is_ascii_digit()) {
      return false;
    }

    let mut after_end = comma_idx + 1;
    while after_end < bytes.len() {
      let b = bytes[after_end];
      if b.is_ascii_whitespace() || matches!(b, b'&' | b'#' | b',') {
        break;
      }
      after_end += 1;
    }
    if comma_idx + 1 >= after_end {
      return false;
    }
    let after = &bytes[comma_idx + 1..after_end];
    if after.is_empty() || !after.iter().all(|&b| is_numeric_list_char(b)) {
      return false;
    }
    if !after.iter().any(|b| b.is_ascii_digit()) {
      return false;
    }

    true
  }

  let bytes = attr.as_bytes();
  let mut out = Vec::new();
  let mut idx = 0;

  while idx < bytes.len() {
    if out.len() >= max_candidates {
      break;
    }
    while idx < bytes.len() && (bytes[idx].is_ascii_whitespace() || bytes[idx] == b',') {
      idx += 1;
    }
    if idx >= bytes.len() {
      break;
    }

    let url_start = idx;
    let data_url = is_data_url(bytes, url_start);
    let mut data_commas_seen = 0usize;

    while idx < bytes.len() {
      let b = bytes[idx];
      if b.is_ascii_whitespace() {
        break;
      }
      if b == b',' {
        if data_url && data_commas_seen == 0 {
          // Data URLs contain a required comma separating metadata and payload.
          // Treat the first comma as part of the URL.
          data_commas_seen = 1;
          idx += 1;
          continue;
        }
        if idx + 1 < bytes.len() && !bytes[idx + 1].is_ascii_whitespace() {
          if is_likely_comma_in_cdn_transform_url(bytes, url_start, idx)
            || is_likely_comma_in_query_numeric_list(bytes, url_start, idx)
          {
            idx += 1;
            continue;
          }
        }
        // Candidate separator (no descriptors).
        break;
      }
      idx += 1;
    }

    let url = attr[url_start..idx].trim();
    if url.is_empty() {
      while idx < bytes.len() && bytes[idx] != b',' {
        idx += 1;
      }
      continue;
    }

    while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
      idx += 1;
    }

    let desc_start = idx;
    while idx < bytes.len() && bytes[idx] != b',' {
      idx += 1;
    }
    let desc_str = attr[desc_start..idx].trim();

    let mut descriptor: Option<SrcsetDescriptor> = None;
    let mut valid = true;
    for desc in desc_str.split_whitespace() {
      if descriptor.is_some() {
        valid = false;
        break;
      }
      let d = desc.trim();
      if let Some(raw) = d.strip_suffix('x') {
        if let Ok(val) = raw.parse::<f32>() {
          descriptor = Some(SrcsetDescriptor::Density(val));
        }
      } else if let Some(raw) = d.strip_suffix("dppx") {
        if let Ok(val) = raw.parse::<f32>() {
          descriptor = Some(SrcsetDescriptor::Density(val));
        }
      } else if let Some(raw) = d.strip_suffix('w') {
        if let Ok(val) = raw.parse::<u32>() {
          descriptor = Some(SrcsetDescriptor::Width(val));
        }
      }
    }
    if valid {
      out.push(SrcsetCandidate {
        url: url.to_string(),
        descriptor: descriptor.unwrap_or(SrcsetDescriptor::Density(1.0)),
      });
    }

    if idx < bytes.len() && bytes[idx] == b',' {
      idx += 1;
    }
  }

  out
}

/// Parse an HTML `sizes` attribute into a `SizesList`.
///
/// Returns `None` if no valid size entries are found.
pub fn parse_sizes(attr: &str) -> Option<SizesList> {
  use crate::style::media::MediaQuery;

  let mut entries = Vec::new();
  for item in attr.split(',') {
    let trimmed = item.trim();
    if trimmed.is_empty() {
      continue;
    }
    let mut parts = trimmed.rsplitn(2, char::is_whitespace);
    let length_part = parts.next().map(str::trim);
    let media_part = parts.next().map(str::trim);
    let length = match length_part.and_then(parse_sizes_length) {
      Some(l) => l,
      None => continue,
    };

    let media = match media_part {
      Some(cond) if !cond.is_empty() => MediaQuery::parse_list(cond).ok(),
      _ => None,
    };

    entries.push(SizesEntry { media, length });
  }

  if entries.is_empty() {
    None
  } else {
    Some(SizesList { entries })
  }
}

fn parse_sizes_length(value: &str) -> Option<crate::style::values::Length> {
  use crate::css::properties::parse_calc_function_length;
  use crate::css::properties::parse_clamp_function_length;
  use crate::css::properties::parse_min_max_function_length;
  use crate::css::properties::MathFn;
  use crate::style::values::Length;
  use crate::style::values::LengthUnit;

  let mut input = ParserInput::new(value);
  let mut parser = Parser::new(&mut input);

  let parsed = match parser.next() {
    Ok(Token::Dimension {
      value, ref unit, ..
    }) => {
      let unit = unit.as_ref();
      if unit.eq_ignore_ascii_case("px") {
        Some(Length::px(*value))
      } else if unit.eq_ignore_ascii_case("em") {
        Some(Length::em(*value))
      } else if unit.eq_ignore_ascii_case("rem") {
        Some(Length::rem(*value))
      } else if unit.eq_ignore_ascii_case("ex") {
        Some(Length::ex(*value))
      } else if unit.eq_ignore_ascii_case("ch") {
        Some(Length::ch(*value))
      } else if unit.eq_ignore_ascii_case("pt") {
        Some(Length::pt(*value))
      } else if unit.eq_ignore_ascii_case("pc") {
        Some(Length::pc(*value))
      } else if unit.eq_ignore_ascii_case("in") {
        Some(Length::inches(*value))
      } else if unit.eq_ignore_ascii_case("cm") {
        Some(Length::cm(*value))
      } else if unit.eq_ignore_ascii_case("mm") {
        Some(Length::mm(*value))
      } else if unit.eq_ignore_ascii_case("q") {
        Some(Length::q(*value))
      } else if unit.eq_ignore_ascii_case("vw") {
        Some(Length::new(*value, LengthUnit::Vw))
      } else if unit.eq_ignore_ascii_case("vh") {
        Some(Length::new(*value, LengthUnit::Vh))
      } else if unit.eq_ignore_ascii_case("vmin") {
        Some(Length::new(*value, LengthUnit::Vmin))
      } else if unit.eq_ignore_ascii_case("vmax") {
        Some(Length::new(*value, LengthUnit::Vmax))
      } else {
        None
      }
    }
    Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("calc") => {
      parse_calc_function_length(&mut parser).ok()
    }
    Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("min") => {
      parse_min_max_function_length(&mut parser, MathFn::Min).ok()
    }
    Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("max") => {
      parse_min_max_function_length(&mut parser, MathFn::Max).ok()
    }
    Ok(Token::Function(ref name)) if name.eq_ignore_ascii_case("clamp") => {
      parse_clamp_function_length(&mut parser).ok()
    }
    Ok(Token::Percentage { unit_value, .. }) => Some(Length::percent(*unit_value * 100.0)),
    Ok(Token::Number { value, .. }) if *value == 0.0 => Some(Length::px(0.0)),
    Err(_) => None,
    _ => None,
  }?;

  parser.skip_whitespace();
  if parser.is_exhausted() {
    Some(parsed)
  } else {
    None
  }
}

#[cfg(test)]
mod tests {
  use super::{parse_sizes, parse_srcset};
  use crate::style::values::{Length, LengthUnit};
  use crate::tree::box_tree::SrcsetDescriptor;

  #[test]
  fn parse_srcset_parses_density_descriptors() {
    let parsed = parse_srcset("a.png 1x, b.png 2x, c.png 1.5x");
    assert_eq!(parsed.len(), 3);
    assert_eq!(parsed[0].url, "a.png");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Density(d) if d == 2.0));
    assert!(
      matches!(parsed[2].descriptor, SrcsetDescriptor::Density(d) if (d - 1.5).abs() < f32::EPSILON)
    );
  }

  #[test]
  fn parse_srcset_parses_width_descriptors() {
    let parsed = parse_srcset("a.png 320w, b.png 640w");
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].url, "a.png");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Width(320)));
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Width(640)));
  }

  #[test]
  fn parse_srcset_ignores_invalid_descriptor_tokens() {
    // Unknown descriptor tokens should be ignored, producing the default 1x descriptor.
    let parsed = parse_srcset("a.png foo, b.png 2x bar, c.png 2x");
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].url, "a.png");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
    assert_eq!(parsed[1].url, "c.png");
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Density(d) if d == 2.0));
  }

  #[test]
  fn parse_srcset_allows_commas_inside_urls() {
    let parsed = parse_srcset(
      "https://img.example/master/w_2560,c_limit/foo.jpg 1x,https://img.example/bar.jpg 2x",
    );
    assert_eq!(parsed.len(), 2);
    assert_eq!(
      parsed[0].url,
      "https://img.example/master/w_2560,c_limit/foo.jpg"
    );
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
    assert_eq!(parsed[1].url, "https://img.example/bar.jpg");
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Density(d) if d == 2.0));
  }

  #[test]
  fn parse_srcset_parses_urls_without_descriptors() {
    let parsed = parse_srcset("a.png, b.png");
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].url, "a.png");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
    assert_eq!(parsed[1].url, "b.png");
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
  }

  #[test]
  fn parse_srcset_parses_urls_without_descriptors_without_whitespace() {
    let parsed = parse_srcset("a.png,b.png");
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].url, "a.png");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
    assert_eq!(parsed[1].url, "b.png");
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
  }

  #[test]
  fn parse_srcset_parses_data_urls() {
    let parsed = parse_srcset("data:image/png;base64,abcd 1x, b.png 2x");
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].url, "data:image/png;base64,abcd");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
    assert_eq!(parsed[1].url, "b.png");
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Density(d) if d == 2.0));
  }

  #[test]
  fn parse_srcset_allows_commas_in_query_params() {
    let parsed = parse_srcset(
      "https://img.example/foo.jpg?resize=300,163 300w, https://img.example/bar.jpg 600w",
    );
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].url, "https://img.example/foo.jpg?resize=300,163");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Width(300)));
    assert_eq!(parsed[1].url, "https://img.example/bar.jpg");
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Width(600)));
  }

  #[test]
  fn parse_srcset_allows_multiple_commas_in_query_numeric_lists() {
    let parsed = parse_srcset(
      "https://img.example/foo.jpg?rect=0,0,100,100 1x, https://img.example/bar.jpg 2x",
    );
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].url, "https://img.example/foo.jpg?rect=0,0,100,100");
    assert!(matches!(parsed[0].descriptor, SrcsetDescriptor::Density(d) if d == 1.0));
    assert_eq!(parsed[1].url, "https://img.example/bar.jpg");
    assert!(matches!(parsed[1].descriptor, SrcsetDescriptor::Density(d) if d == 2.0));
  }

  #[test]
  fn parse_sizes_parses_lengths_and_media_conditions() {
    let parsed = parse_sizes("(max-width: 600px) 50vw, 100vw").expect("sizes parsed");
    assert_eq!(parsed.entries.len(), 2);
    assert!(parsed.entries[0].media.is_some());
    assert_eq!(parsed.entries[0].length, Length::new(50.0, LengthUnit::Vw));
    assert!(parsed.entries[1].media.is_none());
    assert_eq!(parsed.entries[1].length, Length::new(100.0, LengthUnit::Vw));
  }

  #[test]
  fn parse_sizes_skips_invalid_lengths() {
    let parsed = parse_sizes("bad, 100vw").expect("sizes parsed");
    assert_eq!(parsed.entries.len(), 1);
    assert_eq!(parsed.entries[0].length, Length::new(100.0, LengthUnit::Vw));
  }
}
