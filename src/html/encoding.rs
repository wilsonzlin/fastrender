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

fn charset_from_content_type(content_type: &str) -> Option<&str> {
  for param in content_type.split(';').skip(1) {
    let mut parts = param.splitn(2, '=');
    let name = parts.next()?.trim();
    if !name.eq_ignore_ascii_case("charset") {
      continue;
    }
    let value = parts.next()?.trim();
    let value = value.trim_matches('"').trim_matches('\'');
    if !value.is_empty() {
      return Some(value);
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
      if lower[i..].starts_with(b"<meta") && is_meta_tag_boundary(lower.get(i + 5).copied()) {
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

fn is_meta_tag_boundary(b: Option<u8>) -> bool {
  matches!(b, None | Some(b'>') | Some(b'/')) || b.is_some_and(|b| b.is_ascii_whitespace())
}

fn find_bytes(haystack: &[u8], needle: &[u8]) -> Option<usize> {
  haystack.windows(needle.len()).position(|w| w == needle)
}

fn find_tag_end(bytes: &[u8], mut idx: usize) -> Option<usize> {
  let mut quote: Option<u8> = None;
  while idx < bytes.len() {
    match quote {
      Some(q) => {
        if bytes[idx] == q {
          quote = None;
        }
      }
      None => match bytes[idx] {
        b'>' => return Some(idx),
        b'"' | b'\'' => quote = Some(bytes[idx]),
        _ => {}
      },
    }
    idx += 1;
  }
  None
}

fn parse_meta_charset(attrs_lower: &[u8]) -> Option<&'static Encoding> {
  let mut charset: Option<&[u8]> = None;
  let mut http_equiv: Option<&[u8]> = None;
  let mut content: Option<&[u8]> = None;

  // Parse attributes by name (not substring search) to avoid matching "charset"/"content" inside
  // other attribute values like `http-equiv="Content-Type"`.
  let mut i = 0;
  while i < attrs_lower.len() {
    // Skip whitespace between attributes.
    while i < attrs_lower.len() && attrs_lower[i].is_ascii_whitespace() {
      i += 1;
    }
    if i >= attrs_lower.len() {
      break;
    }

    // Ignore a self-closing `/>` marker.
    if attrs_lower[i] == b'/' {
      i += 1;
      continue;
    }

    // Parse attribute name.
    let name_start = i;
    while i < attrs_lower.len() {
      let b = attrs_lower[i];
      if b.is_ascii_whitespace() || b == b'=' || b == b'/' {
        break;
      }
      i += 1;
    }
    let name_end = i;
    if name_end == name_start {
      i += 1;
      continue;
    }
    let name = &attrs_lower[name_start..name_end];

    // Skip whitespace after name.
    while i < attrs_lower.len() && attrs_lower[i].is_ascii_whitespace() {
      i += 1;
    }

    // Parse optional attribute value.
    let mut value: Option<&[u8]> = None;
    if i < attrs_lower.len() && attrs_lower[i] == b'=' {
      i += 1;
      while i < attrs_lower.len() && attrs_lower[i].is_ascii_whitespace() {
        i += 1;
      }
      if i < attrs_lower.len() {
        match attrs_lower[i] {
          b'"' | b'\'' => {
            let quote = attrs_lower[i];
            i += 1;
            let start = i;
            while i < attrs_lower.len() && attrs_lower[i] != quote {
              i += 1;
            }
            value = Some(&attrs_lower[start..i]);
            if i < attrs_lower.len() && attrs_lower[i] == quote {
              i += 1;
            }
          }
          _ => {
            let start = i;
            while i < attrs_lower.len() {
              let b = attrs_lower[i];
              if b.is_ascii_whitespace() {
                break;
              }
              // In HTML, attribute values may legally contain `/` (e.g. `text/html`), so we must not
              // treat `/` as a terminator. However, a `<meta .../>` tag can appear without
              // whitespace between the last attribute value and the self-closing marker. When the
              // trailing `/` is the last byte before `>`, ignore it so we still parse the value
              // correctly.
              if b == b'/' && i + 1 == attrs_lower.len() {
                break;
              }
              i += 1;
            }
            value = Some(&attrs_lower[start..i]);
          }
        }
      }
    }

    match name {
      b"charset" => charset = value,
      b"http-equiv" => http_equiv = value,
      b"content" => content = value,
      _ => {}
    }
  }

  if let Some(label) = charset {
    let label = trim_ascii_quotes(trim_ascii_whitespace(label));
    if !label.is_empty() {
      if let Some(enc) = Encoding::for_label(label) {
        return Some(normalize_meta_encoding(enc));
      }
    }
  }

  let http_equiv = http_equiv.map(trim_ascii_whitespace);
  if http_equiv == Some(b"content-type") {
    if let Some(content) = content {
      let content = trim_ascii_whitespace(content);
      if let Some(label) = charset_from_content_type_bytes(content) {
        if let Some(enc) = Encoding::for_label(label) {
          return Some(normalize_meta_encoding(enc));
        }
      }
    }
  }
  None
}

fn normalize_meta_encoding(enc: &'static Encoding) -> &'static Encoding {
  // Per HTML Living Standard, meta-declared UTF-16 encodings are treated as UTF-8. UTF-16 HTML
  // documents must be detected via BOM sniffing instead.
  if std::ptr::eq(enc, encoding_rs::UTF_16LE) || std::ptr::eq(enc, encoding_rs::UTF_16BE) {
    encoding_rs::UTF_8
  } else {
    enc
  }
}

fn charset_from_content_type_bytes(content_type: &[u8]) -> Option<&[u8]> {
  for param in content_type.split(|&b| b == b';').skip(1) {
    let param = trim_ascii_whitespace(param);
    if param.is_empty() {
      continue;
    }

    let mut parts = param.splitn(2, |&b| b == b'=');
    let name = trim_ascii_whitespace(parts.next()?);
    if name != b"charset" {
      continue;
    }

    let value = parts.next().unwrap_or_default();
    let value = trim_ascii_quotes(trim_ascii_whitespace(value));
    if !value.is_empty() {
      return Some(value);
    }
  }
  None
}

fn trim_ascii_whitespace(bytes: &[u8]) -> &[u8] {
  let mut start = 0;
  let mut end = bytes.len();
  while start < end && bytes[start].is_ascii_whitespace() {
    start += 1;
  }
  while end > start && bytes[end - 1].is_ascii_whitespace() {
    end -= 1;
  }
  &bytes[start..end]
}

fn trim_ascii_quotes(mut bytes: &[u8]) -> &[u8] {
  loop {
    bytes = trim_ascii_whitespace(bytes);
    if bytes.len() >= 2 {
      let first = bytes[0];
      let last = bytes[bytes.len() - 1];
      if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
        bytes = &bytes[1..bytes.len() - 1];
        continue;
      }
    }

    let mut changed = false;
    if !bytes.is_empty() && matches!(bytes[0], b'"' | b'\'') {
      bytes = &bytes[1..];
      changed = true;
    }
    if !bytes.is_empty() && matches!(bytes[bytes.len() - 1], b'"' | b'\'') {
      bytes = &bytes[..bytes.len() - 1];
      changed = true;
    }
    if !changed {
      break;
    }
  }
  bytes
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
  fn decode_html_uses_meta_http_equiv_content_type_charset() {
    let encoded = encoding_rs::GBK.encode(
      "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=gbk\"></head><body>中文</body></html>",
    ).0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains("中文"),
      "decoded text should contain Han characters when meta declares charset via http-equiv: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_meta_sniff_does_not_match_attribute_values() {
    // The `data` attribute value contains the substring `content="..."`, but it is not the `content`
    // attribute. Only actual attribute names should be considered by the sniffing algorithm.
    let encoded = encoding_rs::WINDOWS_1252
      .encode("<html><head><meta data='content=\"text/html; charset=shift_jis\"' http-equiv=\"Content-Type\"></head><body>\u{00a3}</body></html>")
      .0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains('£'),
      "decoded text should not treat attribute values as meta declarations: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_uses_meta_http_equiv_content_type_charset_unquoted() {
    // Common legacy form with unquoted attribute values.
    let encoded = encoding_rs::GBK.encode(
      "<html><head><meta http-equiv=Content-Type content=text/html;charset=gbk></head><body>中文</body></html>",
    ).0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains("中文"),
      "decoded text should contain Han characters when meta declares charset via unquoted http-equiv/content: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_meta_utf16_is_treated_as_utf8() {
    // HTML encoding sniffing treats meta-declared UTF-16 as UTF-8 (BOM controls actual UTF-16).
    let encoded = encoding_rs::UTF_8
      .encode("<html><head><meta charset=\"utf-16\"></head><body>£</body></html>")
      .0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains('£'),
      "decoded text should contain pound sign when meta declares utf-16 (treated as utf-8): {}",
      decoded
    );
    assert!(
      !decoded.contains('Â'),
      "decoded text should not look like Windows-1252 mojibake: {}",
      decoded
    );
  }

  #[test]
  fn decode_html_meta_http_equiv_utf16_is_treated_as_utf8() {
    // Same rule applies for the legacy http-equiv Content-Type form.
    let encoded = encoding_rs::UTF_8.encode(
      "<html><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-16\"></head><body>£</body></html>",
    ).0;
    let decoded = decode_html_bytes(&encoded, None);
    assert!(
      decoded.contains('£'),
      "decoded text should contain pound sign when http-equiv declares utf-16 (treated as utf-8): {}",
      decoded
    );
    assert!(
      !decoded.contains('\u{FFFD}'),
      "decoded text should not contain replacement characters when decoding utf-8 content: {}",
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
