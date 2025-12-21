//! CSS encoding detection and decoding helpers.
//!
//! Implements the encoding sniffing steps from the CSS Syntax spec:
//! - BOM wins when present.
//! - Otherwise, an initial `@charset "<label>";` declaration chooses the encoding.
//! - Otherwise, a `charset` parameter on the Content-Type header is used.
//! - Otherwise, default to UTF-8.

use encoding_rs::Encoding;
use encoding_rs::UTF_8;

/// Decode raw stylesheet bytes into a UTF-8 `String`, following CSS encoding rules.
///
/// - BOM, if present, determines the encoding and is stripped.
/// - Otherwise, the first `@charset` rule wins.
/// - Otherwise, a charset from the HTTP `Content-Type` header is honored.
/// - Otherwise, UTF-8 is used.
pub fn decode_css_bytes(bytes: &[u8], content_type: Option<&str>) -> String {
  if bytes.is_empty() {
    return String::new();
  }

  // BOM check first.
  if let Some((enc, bom_len)) = Encoding::for_bom(bytes) {
    let (text, _) = enc.decode_without_bom_handling(&bytes[bom_len..]);
    return text.into_owned();
  }

  // @charset detection.
  if let Some(enc) = sniff_charset_at_rule(bytes) {
    let (text, _) = enc.decode_with_bom_removal(bytes);
    return text.into_owned();
  }

  // Content-Type: charset=...
  if let Some(ct) = content_type {
    if let Some(label) = charset_from_content_type(ct) {
      if let Some(enc) = Encoding::for_label(label.as_bytes()) {
        let (text, _) = enc.decode_with_bom_removal(bytes);
        return text.into_owned();
      }
    }
  }

  // Default to UTF-8.
  let (text, _) = UTF_8.decode_with_bom_removal(bytes);
  text.into_owned()
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

fn sniff_charset_at_rule(bytes: &[u8]) -> Option<&'static Encoding> {
  let mut i = 0;
  while i < bytes.len() {
    match bytes[i] {
      b'\n' | b'\r' | b'\t' | b'\x0C' | b' ' => {
        i += 1;
      }
      b'/' if i + 1 < bytes.len() && bytes[i + 1] == b'*' => {
        i = skip_comment(bytes, i + 2)?;
      }
      b'@' => {
        // case-insensitive match for "charset"
        if bytes[i + 1..].len() < 7 {
          return None;
        }
        let rest = &bytes[i + 1..];
        if !starts_with_ignore_ascii_case(rest, b"charset") {
          return None;
        }
        let mut j = i + 1 + 7;
        while j < bytes.len() && is_css_whitespace(bytes[j]) {
          j += 1;
        }
        if j >= bytes.len() || (bytes[j] != b'"' && bytes[j] != b'\'') {
          return None;
        }
        let quote = bytes[j];
        j += 1;
        let start = j;
        while j < bytes.len() && bytes[j] != quote {
          j += 1;
        }
        if j >= bytes.len() {
          return None;
        }
        let label = &bytes[start..j];
        return Encoding::for_label(label);
      }
      _ => return None,
    }
  }
  None
}

fn is_css_whitespace(b: u8) -> bool {
  matches!(b, b'\n' | b'\r' | b'\t' | b'\x0C' | b' ')
}

fn skip_comment(bytes: &[u8], mut i: usize) -> Option<usize> {
  while i + 1 < bytes.len() {
    if bytes[i] == b'*' && bytes[i + 1] == b'/' {
      return Some(i + 2);
    }
    i += 1;
  }
  None
}

fn starts_with_ignore_ascii_case(haystack: &[u8], needle: &[u8]) -> bool {
  if haystack.len() < needle.len() {
    return false;
  }
  haystack
    .iter()
    .zip(needle.iter())
    .all(|(a, b)| a.eq_ignore_ascii_case(b))
}

#[cfg(test)]
mod tests {
  use super::*;
  use encoding_rs::SHIFT_JIS;
  use encoding_rs::WINDOWS_1252;

  #[test]
  fn decodes_with_bom() {
    let data = [0xef, 0xbb, 0xbf, b'b', b'o', b'd', b'y'];
    let text = decode_css_bytes(&data, None);
    assert_eq!(text, "body");
  }

  #[test]
  fn decodes_with_charset_rule() {
    let encoded = WINDOWS_1252
      .encode("@charset \"windows-1252\"; body { content: \"\u{00a3}\"; }")
      .0;
    let text = decode_css_bytes(&encoded, None);
    assert!(
      text.contains("£"),
      "decoded text should contain pound sign: {}",
      text
    );
  }

  #[test]
  fn decodes_with_content_type_charset() {
    let encoded = SHIFT_JIS.encode("body { color: red; }").0;
    let text = decode_css_bytes(&encoded, Some("text/css; charset=shift_jis"));
    assert!(text.contains("color: red"));
  }

  #[test]
  fn defaults_to_utf8() {
    let text = decode_css_bytes(b"body { color: blue; }", None);
    assert!(text.contains("blue"));
  }
}
