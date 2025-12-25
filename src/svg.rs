/// Utility helpers for working with SVG metadata.
pub(crate) fn parse_svg_length_px(value: &str) -> Option<f32> {
  let trimmed = value.trim();
  if trimmed.is_empty() || trimmed.ends_with('%') {
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

  let unit = trimmed[end..].trim_start();
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

  if px.is_finite() {
    Some(px)
  } else {
    None
  }
}
