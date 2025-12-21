use encoding_rs::Encoding;
use encoding_rs::WINDOWS_1252;

/// Decode raw HTML bytes into a string using HTML encoding sniffing rules.
///
/// Follows the HTML Living Standard approach:
/// - Respect a BOM when present
/// - Honor an HTTP `Content-Type` charset parameter when available
/// - Scan for `<meta charset>` or equivalent http-equiv declarations
/// - Fall back to Windows-1252
pub fn decode_html_bytes(bytes: &[u8], content_type: Option<&str>) -> String {
  if bytes.is_empty() {
    return String::new();
  }

  if let Some((enc, bom_len)) = Encoding::for_bom(bytes) {
    return enc
      .decode_without_bom_handling(&bytes[bom_len..])
      .0
      .into_owned();
  }

  if let Some(label) = content_type.and_then(charset_from_content_type) {
    if let Some(enc) = Encoding::for_label(label.as_bytes()) {
      return enc.decode_with_bom_removal(bytes).0.into_owned();
    }
  }

  if let Some(enc) = sniff_html_meta_charset(bytes) {
    return enc.decode_with_bom_removal(bytes).0.into_owned();
  }

  // HTML default encoding is Windows-1252 per HTML Living Standard.
  WINDOWS_1252.decode_with_bom_removal(bytes).0.into_owned()
}

fn charset_from_content_type(content_type: &str) -> Option<String> {
  for param in content_type.split(';').skip(1) {
    let mut parts = param.splitn(2, '=');
    let name = parts.next()?.trim();
    if !name.eq_ignore_ascii_case("charset") {
      continue;
    }
    let value = parts.next()?.trim().trim_matches('"').trim_matches('\'');
    if !value.is_empty() {
      return Some(value.to_string());
    }
  }
  None
}

/// Pre-scan the first bytes of an HTML document to find a `<meta charset>` declaration.
/// Follows the HTML encoding sniffing algorithm in spirit (ASCII scan, Windows-1252 default).
fn sniff_html_meta_charset(bytes: &[u8]) -> Option<&'static Encoding> {
  let limit = bytes.len().min(4096);
  let slice = &bytes[..limit];
  let lower: Vec<u8> = slice.iter().map(|b| b.to_ascii_lowercase()).collect();
  let mut i = 0;
  while i < lower.len() {
    if lower[i] == b'<' {
      if lower[i..].starts_with(b"<!--") {
        if let Some(end) = find_bytes(&lower[i + 4..], b"-->") {
          i += 4 + end + 3;
          continue;
        } else {
          break;
        }
      }
      if lower[i..].starts_with(b"<meta") {
        if let Some(tag_end) = find_tag_end(&lower, i + 5) {
          let attrs = &lower[i + 5..tag_end];
          if let Some(enc) = parse_meta_charset(attrs) {
            return Some(enc);
          }
          i = tag_end + 1;
          continue;
        } else {
          break;
        }
      }
    }
    i += 1;
  }
  None
}

fn find_bytes(haystack: &[u8], needle: &[u8]) -> Option<usize> {
  haystack.windows(needle.len()).position(|w| w == needle)
}

fn find_tag_end(bytes: &[u8], mut idx: usize) -> Option<usize> {
  while idx < bytes.len() {
    if bytes[idx] == b'>' {
      return Some(idx);
    }
    idx += 1;
  }
  None
}

fn parse_meta_charset(attrs_lower: &[u8]) -> Option<&'static Encoding> {
  // Look for charset attribute.
  if let Some(pos) = find_bytes(attrs_lower, b"charset") {
    let after = &attrs_lower[pos + "charset".len()..];
    let (raw_val, _) = parse_attr_value(after);
    if let Some(label) = raw_val {
      if let Some(enc) = Encoding::for_label(label.as_bytes()) {
        return Some(enc);
      }
    }
  }

  // Look for http-equiv/content pair specifying charset
  if let Some(http_equiv_pos) = find_bytes(attrs_lower, b"http-equiv") {
    let (http_equiv, _) = parse_attr_value(&attrs_lower[http_equiv_pos + "http-equiv".len()..]);
    if http_equiv.as_deref() != Some("content-type") {
      return None;
    }
    if let Some(content_pos) = find_bytes(attrs_lower, b"content") {
      let (content, _) = parse_attr_value(&attrs_lower[content_pos + "content".len()..]);
      if let Some(content) = content {
        if let Some(idx) = content.to_ascii_lowercase().find("charset=") {
          let label = content[idx + "charset=".len()..].trim_matches(['\'', '"', ' ', ';']);
          if let Some(enc) = Encoding::for_label(label.as_bytes()) {
            return Some(enc);
          }
        }
      }
    }
  }
  None
}

/// Parses an attribute value after the attribute name.
/// Returns (value, bytes_consumed_from_lowercase_slice_after_name)
fn parse_attr_value(slice_lower: &[u8]) -> (Option<String>, usize) {
  let mut i = 0;
  while i < slice_lower.len() && (slice_lower[i].is_ascii_whitespace() || slice_lower[i] == b'=') {
    i += 1;
  }
  if i >= slice_lower.len() {
    return (None, i);
  }
  let (value, consumed) = if slice_lower[i] == b'"' || slice_lower[i] == b'\'' {
    let quote = slice_lower[i];
    i += 1;
    let start = i;
    while i < slice_lower.len() && slice_lower[i] != quote {
      i += 1;
    }
    let end = i.min(slice_lower.len());
    let value_bytes = &slice_lower[start..end];
    (
      Some(String::from_utf8_lossy(value_bytes).into_owned()),
      end + 1,
    )
  } else {
    let start = i;
    while i < slice_lower.len() && !slice_lower[i].is_ascii_whitespace() && slice_lower[i] != b'>' {
      i += 1;
    }
    let end = i;
    let value_bytes = &slice_lower[start..end];
    (Some(String::from_utf8_lossy(value_bytes).into_owned()), end)
  };
  (value, consumed)
}

#[cfg(test)]
mod tests {
  use super::decode_html_bytes;

  #[test]
  fn decode_html_uses_content_type_charset() {
    let encoded = encoding_rs::SHIFT_JIS.encode("abcデ").0;
    let decoded = decode_html_bytes(&encoded, Some("text/html; charset=shift_jis"));
    assert!(
      decoded.contains('デ'),
      "decoded text should include kana: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_uses_meta_charset() {
    let mut encoded = encoding_rs::WINDOWS_1252
      .encode("<html><head><meta charset=\"windows-1252\"></head><body>\u{00a3}</body></html>")
      .0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains('£'),
      "decoded text should contain pound sign when meta declares charset: {}",
      decoded
    );

    encoded = encoding_rs::SHIFT_JIS
      .encode("<html><head><meta charset='shift_jis'></head><body>デ</body></html>")
      .0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains('デ'),
      "decoded text should contain kana when meta declares charset: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_defaults_to_windows_1252() {
    let bytes = vec![0xa3]; // U+00A3 in Windows-1252
    let decoded = decode_html_bytes(&bytes, None);
    assert_eq!(decoded, "£");
  }
}
