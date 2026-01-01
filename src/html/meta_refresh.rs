//! Meta refresh parsing utilities.
//!
//! Provides a lightweight extractor for `<meta http-equiv="refresh">` URLs so
//! callers can follow scriptless redirects commonly used as `<noscript>` fallbacks.

/// Parses the first `<meta http-equiv="refresh">` URL in the provided HTML.
///
/// Returns `Some(url)` when a refresh URL is found, otherwise `None`.
pub fn extract_meta_refresh_url(html: &str) -> Option<String> {
  let lower = html.to_ascii_lowercase();
  let mut idx = 0usize;
  while let Some(pos) = lower[idx..].find("<meta") {
    let start = idx + pos;
    let end = html[start..]
      .find('>')
      .map(|e| start + e + 1)
      .unwrap_or_else(|| html.len());
    let tag = &html[start..end];
    let attrs = parse_attributes(tag);
    let mut http_equiv: Option<String> = None;
    let mut content: Option<String> = None;
    for (name, value) in attrs {
      let normalized = normalize_attr_value(&value);
      if name.eq_ignore_ascii_case("http-equiv") {
        http_equiv = Some(normalized);
      } else if name.eq_ignore_ascii_case("content") {
        content = Some(normalized);
      }
    }

    if http_equiv
      .as_ref()
      .map(|v| v.eq_ignore_ascii_case("refresh"))
      .unwrap_or(false)
    {
      if let Some(content) = content {
        if let Some(url) = parse_refresh_content(&content) {
          return Some(url);
        }
      }
    }

    idx = end;
  }

  None
}

/// Extracts a literal URL from simple JavaScript redirects such as
/// `window.location.href = "https://example.com"` or `location.replace('/next')`.
pub fn extract_js_location_redirect(html: &str) -> Option<String> {
  const MAX_REDIRECT_LEN: usize = 2048;

  let decoded = decode_refresh_entities(html);
  let lower = decoded.to_ascii_lowercase();
  #[derive(Clone, Copy)]
  enum PatternKind {
    Call,
    Assign,
  }

  // Restrict to explicit navigations to avoid false positives from innocuous property reads like
  // `location.pathname`. We also support direct assignment to the Location object (e.g. `location =
  // "/next"`).
  let patterns: [(&str, PatternKind); 24] = [
    ("window.location.replace", PatternKind::Call),
    ("document.location.replace", PatternKind::Call),
    ("top.location.replace", PatternKind::Call),
    ("self.location.replace", PatternKind::Call),
    ("parent.location.replace", PatternKind::Call),
    ("location.replace", PatternKind::Call),
    ("window.location.assign", PatternKind::Call),
    ("document.location.assign", PatternKind::Call),
    ("top.location.assign", PatternKind::Call),
    ("self.location.assign", PatternKind::Call),
    ("parent.location.assign", PatternKind::Call),
    ("location.assign", PatternKind::Call),
    ("window.location.href", PatternKind::Assign),
    ("document.location.href", PatternKind::Assign),
    ("top.location.href", PatternKind::Assign),
    ("self.location.href", PatternKind::Assign),
    ("parent.location.href", PatternKind::Assign),
    ("location.href", PatternKind::Assign),
    ("window.location", PatternKind::Assign),
    ("document.location", PatternKind::Assign),
    ("top.location", PatternKind::Assign),
    ("self.location", PatternKind::Assign),
    ("parent.location", PatternKind::Assign),
    ("location", PatternKind::Assign),
    // Intentionally omit generic `.location` matches to avoid picking up unrelated object
    // properties (e.g. `foo.location = ...`).
  ];

  for (pat, kind) in patterns.iter() {
    let mut search_start = 0usize;
    while let Some(found) = lower[search_start..].find(pat) {
      let idx = search_start + found;
      search_start = idx + pat.len();

      // Require the match to start on a non-identifier boundary to avoid picking up attributes
      // like data-location="...".
      if idx > 0 {
        let prev = lower.as_bytes()[idx - 1];
        if prev.is_ascii_alphanumeric() || prev == b'_' || prev == b'-' {
          continue;
        }
      }

      let after = idx + pat.len();
      if after < lower.len() {
        let next = lower.as_bytes()[after];
        if next.is_ascii_alphanumeric() || next == b'_' {
          continue;
        }
      }

      // If we are matching an unqualified `location*` token, ensure it isn't a property access
      // (e.g. `foo.location = ...`).
      if pat.starts_with("location") && idx > 0 && lower.as_bytes()[idx - 1] == b'.' {
        continue;
      }

      // Avoid misclassifying `var/let/const location = ...` as a navigation.
      if *pat == "location" {
        let mut j = idx;
        while j > 0 && lower.as_bytes()[j - 1].is_ascii_whitespace() {
          j -= 1;
        }
        let mut k = j;
        while k > 0
          && (lower.as_bytes()[k - 1].is_ascii_alphanumeric() || lower.as_bytes()[k - 1] == b'_')
        {
          k -= 1;
        }
        if let Some(word) = lower.get(k..j) {
          if matches!(word, "var" | "let" | "const") {
            continue;
          }
        }
      }

      let mut i = idx + pat.len();
      while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
      }

      match kind {
        PatternKind::Assign => {
          if i >= lower.len() || lower.as_bytes()[i] != b'=' {
            continue;
          }
          i += 1;
          while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
            i += 1;
          }
        }
        PatternKind::Call => {
          if i >= lower.len() || lower.as_bytes()[i] != b'(' {
            continue;
          }
          i += 1;
          while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
            i += 1;
          }
        }
      }

      // Allow redundant grouping parentheses like `location = ('/next')` or
      // `location.replace(('/next'))`.
      while i < lower.len() && lower.as_bytes()[i] == b'(' {
        i += 1;
        while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
          i += 1;
        }
      }

      if let Some(url) = extract_js_string_literal(&decoded, &lower, i, MAX_REDIRECT_LEN) {
        return Some(url);
      }

      if let Some(url) = extract_wrapped_js_string_literal(&decoded, &lower, i, MAX_REDIRECT_LEN) {
        return Some(url);
      }

      {
        let start = i;
        while i < lower.len() {
          let b = lower.as_bytes()[i];
          if b.is_ascii_whitespace() || b == b';' {
            break;
          }
          i += 1;
        }
        if i > start {
          let candidate = decoded[start..i].trim();
          if !candidate.is_empty()
            && candidate.len() <= MAX_REDIRECT_LEN
            && (candidate.starts_with("http")
              || candidate.starts_with("//")
              || candidate.starts_with('/')
              || candidate.starts_with("www."))
          {
            return Some(unescape_js_literal(candidate));
          }
        }
      }
    }
  }

  // Fallback: look for a variable assignment that captures a URL literal
  let url_decls = ["var url", "let url", "const url"];
  for decl in url_decls.iter() {
    let mut search_start = 0usize;
    while let Some(found) = lower[search_start..].find(decl) {
      let idx = search_start + found;
      search_start = idx + decl.len();

      if idx > 0 {
        let prev = lower.as_bytes()[idx - 1];
        if prev.is_ascii_alphanumeric() || prev == b'_' {
          continue;
        }
      }

      let after = idx + decl.len();
      if after < lower.len() {
        let next = lower.as_bytes()[after];
        if next.is_ascii_alphanumeric() || next == b'_' {
          continue;
        }
      }

      let mut i = after;
      while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
      }
      if lower.as_bytes().get(i) != Some(&b'=') {
        continue;
      }
      i += 1;
      while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
      }
      while i < lower.len() && lower.as_bytes()[i] == b'(' {
        i += 1;
        while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
          i += 1;
        }
      }

      if let Some(mut url) = extract_js_string_literal(&decoded, &lower, i, MAX_REDIRECT_LEN)
        .or_else(|| extract_wrapped_js_string_literal(&decoded, &lower, i, MAX_REDIRECT_LEN))
      {
        if url.starts_with("//") {
          url = format!("https:{}", url);
        }
        return Some(url);
      }
    }
  }

  None
}

fn extract_js_string_literal(
  decoded: &str,
  lower: &str,
  start_idx: usize,
  max_len: usize,
) -> Option<String> {
  let bytes = lower.as_bytes();
  let quote = *bytes.get(start_idx)?;
  if quote != b'"' && quote != b'\'' && quote != b'`' {
    return None;
  }

  let mut i = start_idx + 1;
  let start = i;
  let mut has_interpolation = false;
  while i < bytes.len() {
    let b = bytes[i];

    if quote == b'`' && b == b'$' && bytes.get(i + 1) == Some(&b'{') {
      has_interpolation = true;
    }

    if b == b'\\' {
      i += 1;
      if i < bytes.len() {
        i += 1;
      }
      continue;
    }

    if b == quote {
      break;
    }
    i += 1;
  }

  if quote == b'`' && has_interpolation {
    return None;
  }

  let end = i.min(decoded.len());
  let candidate = decoded[start..end].trim();
  if candidate.is_empty() || candidate.len() > max_len {
    return None;
  }

  Some(unescape_js_literal(candidate))
}

fn extract_wrapped_js_string_literal(
  decoded: &str,
  lower: &str,
  start_idx: usize,
  max_len: usize,
) -> Option<String> {
  // Some redirects wrap a static string in common URL-decoding helpers.
  let wrappers = ["decodeuricomponent", "decodeuri", "unescape"];
  let rest = lower.get(start_idx..)?;
  for wrapper in wrappers.iter() {
    if rest.starts_with(wrapper) {
      let mut i = start_idx + wrapper.len();
      while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
      }
      if lower.as_bytes().get(i) != Some(&b'(') {
        continue;
      }
      i += 1;
      while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
        i += 1;
      }
      while i < lower.len() && lower.as_bytes()[i] == b'(' {
        i += 1;
        while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
          i += 1;
        }
      }
      if let Some(url) = extract_js_string_literal(decoded, lower, i, max_len) {
        return Some(url);
      }
    }
  }
  None
}

fn unescape_js_literal(s: &str) -> String {
  let mut out = String::with_capacity(s.len());
  let mut s = s.replace("\\\\/", "/");
  s = s.replace("\\/", "/");
  let mut chars = s.chars();
  while let Some(ch) = chars.next() {
    if ch == '\\' {
      if let Some(next) = chars.next() {
        match next {
          '\\' | '"' | '\'' => out.push(next),
          '/' => out.push('/'),
          'x' => {
            let hi = chars.next();
            let lo = chars.next();
            if let (Some(hi), Some(lo)) = (hi, lo) {
              if let (Some(hi_v), Some(lo_v)) = (hi.to_digit(16), lo.to_digit(16)) {
                if let Some(c) = char::from_u32(hi_v * 16 + lo_v) {
                  out.push(c);
                  continue;
                }
              }
            }
            out.push_str("\\x");
            if let Some(h) = hi {
              out.push(h);
            }
            if let Some(l) = lo {
              out.push(l);
            }
          }
          'u' => {
            let mut code = String::new();
            for _ in 0..4 {
              if let Some(d) = chars.next() {
                code.push(d);
              }
            }
            if code.len() == 4 {
              if let Ok(val) = u16::from_str_radix(&code, 16) {
                if let Some(c) = char::from_u32(val as u32) {
                  out.push(c);
                  continue;
                }
              }
            }
            out.push_str("\\u");
            out.push_str(&code);
          }
          _ => out.push(next),
        }
      }
    } else if ch == '%' {
      let a = chars.next();
      let b = chars.next();
      if let (Some(a), Some(b)) = (a, b) {
        if let (Some(hi), Some(lo)) = (a.to_digit(16), b.to_digit(16)) {
          if let Some(c) = char::from_u32(hi * 16 + lo) {
            out.push(c);
            continue;
          }
        }
        out.push('%');
        out.push(a);
        out.push(b);
      } else {
        out.push('%');
        if let Some(a) = a {
          out.push(a);
        }
        if let Some(b) = b {
          out.push(b);
        }
      }
    } else {
      out.push(ch);
    }
  }
  let mut out = out.replace("\\/", "/");
  out = out.replace("\\\\", "\\");
  out
}

fn normalize_attr_value(value: &str) -> String {
  let unescaped = value.replace("\\\"", "\"").replace("\\'", "'");
  let trimmed_slashes = unescaped.trim_end_matches('\\');
  trimmed_slashes
    .trim_matches(|c| c == '"' || c == '\'')
    .trim()
    .to_string()
}

fn parse_refresh_content(content: &str) -> Option<String> {
  let decoded = decode_refresh_entities(content);
  let bytes = decoded.as_bytes();
  let lower: Vec<u8> = bytes.iter().map(|b| b.to_ascii_lowercase()).collect();

  let mut i = 0usize;
  while i + 2 < lower.len() {
    if lower[i] == b'u' && lower[i + 1] == b'r' && lower[i + 2] == b'l' {
      let prev_is_delim = i == 0
        || bytes[i - 1].is_ascii_whitespace()
        || bytes[i - 1] == b';'
        || bytes[i - 1] == b',';
      if prev_is_delim {
        let mut j = i + 3;
        while j < lower.len() && bytes[j].is_ascii_whitespace() {
          j += 1;
        }
        if j < lower.len() && bytes[j] == b'=' {
          j += 1;
          while j < lower.len() && bytes[j].is_ascii_whitespace() {
            j += 1;
          }

          let value = slice_until_unquoted_semicolon(&decoded, j);
          let cleaned = value.trim().trim_matches(['"', '\'']);
          if !cleaned.is_empty() {
            return Some(cleaned.to_string());
          }
        }
      }
    }

    // Skip over quoted segments so we don't match "url" inside a quoted URL value.
    if bytes[i] == b'"' || bytes[i] == b'\'' {
      let quote = bytes[i] as char;
      i += 1;
      while i < lower.len() {
        if bytes[i] as char == quote {
          break;
        }
        i += 1;
      }
    }

    i += 1;
  }

  None
}

fn decode_refresh_entities(content: &str) -> String {
  content
    .replace("&quot;", "\"")
    .replace("&QUOT;", "\"")
    .replace("&#34;", "\"")
    .replace("&amp;", "&")
    .replace("&AMP;", "&")
    .replace("&#39;", "'")
    .replace("&#x27;", "'")
    .replace("&#X27;", "'")
    .replace("&apos;", "'")
    .replace("&APOS;", "'")
}

fn slice_until_unquoted_semicolon(s: &str, start: usize) -> &str {
  let mut in_quote: Option<char> = None;
  for (idx, ch) in s[start..].char_indices() {
    match in_quote {
      Some(q) if ch == q => in_quote = None,
      None => match ch {
        '"' | '\'' => in_quote = Some(ch),
        ';' => return &s[start..start + idx],
        _ => {}
      },
      _ => {}
    }
  }

  &s[start..]
}

fn parse_attributes(tag: &str) -> Vec<(String, String)> {
  let mut attrs = Vec::new();
  let mut i = 0usize;
  let bytes = tag.as_bytes();

  // Skip leading "<meta" or any leading whitespace
  while i < bytes.len() {
    let b = bytes[i];
    if b == b'<' {
      while i < bytes.len() && bytes[i] != b'>' && !bytes[i].is_ascii_whitespace() {
        i += 1;
      }
      break;
    }
    if !b.is_ascii_whitespace() {
      break;
    }
    i += 1;
  }

  while i < bytes.len() {
    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
      i += 1;
    }
    if i >= bytes.len() || bytes[i] == b'>' {
      break;
    }

    let name_start = i;
    while i < bytes.len() && !bytes[i].is_ascii_whitespace() && bytes[i] != b'=' && bytes[i] != b'>'
    {
      i += 1;
    }
    let name = tag[name_start..i].trim();

    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
      i += 1;
    }

    let mut value = String::new();
    if i < bytes.len() && bytes[i] == b'=' {
      i += 1;
      while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
      }
      if i + 1 < bytes.len() && bytes[i] == b'\\' && (bytes[i + 1] == b'"' || bytes[i + 1] == b'\'')
      {
        let quote = bytes[i + 1];
        i += 2;
        let start = i;
        while i < bytes.len() && bytes[i] != quote {
          i += 1;
        }
        value = tag[start..i.min(bytes.len())].to_string();
        if i < bytes.len() {
          i += 1;
        }
      } else if i < bytes.len() && (bytes[i] == b'"' || bytes[i] == b'\'') {
        let quote = bytes[i];
        i += 1;
        let start = i;
        while i < bytes.len() && bytes[i] != quote {
          i += 1;
        }
        value = tag[start..i.min(bytes.len())].to_string();
        if i < bytes.len() {
          i += 1;
        }
      } else {
        let start = i;
        while i < bytes.len() && !bytes[i].is_ascii_whitespace() && bytes[i] != b'>' {
          i += 1;
        }
        value = tag[start..i].to_string();
      }
    }

    attrs.push((name.to_string(), value));
  }

  attrs
}

#[cfg(test)]
mod tests {
  use super::extract_js_location_redirect;
  use super::extract_meta_refresh_url;

  #[test]
  fn extracts_meta_refresh_url() {
    let html =
      r"<html><head><meta http-equiv='refresh' content='0; url=/fallback.html'></head></html>";
    assert_eq!(
      extract_meta_refresh_url(html),
      Some("/fallback.html".to_string())
    );
  }

  #[test]
  fn extracts_quoted_and_entity_decoded_url() {
    let html = r#"<meta http-equiv="refresh" content="0;URL='https://example.com/?a=1&amp;b=2'">"#;
    assert_eq!(
      extract_meta_refresh_url(html),
      Some("https://example.com/?a=1&b=2".to_string())
    );
  }

  #[test]
  fn parses_quoted_meta_refresh_url() {
    let html = r#"
            <html><head>
            <noscript>
                <meta http-equiv=\"refresh\" content=\"0; url=&quot;https://html.duckduckgo.com/html&quot;\">
            </noscript>
            </head><body></body></html>
        "#;
    assert_eq!(
      extract_meta_refresh_url(html),
      Some("https://html.duckduckgo.com/html".to_string())
    );
  }

  #[test]
  fn extracts_meta_refresh_url_with_semicolon_in_value() {
    let html =
      r#"<meta http-equiv="REFRESH" content="0; URL='https://example.com/path;param=1?q=2'">"#;
    assert_eq!(
      extract_meta_refresh_url(html),
      Some("https://example.com/path;param=1?q=2".to_string())
    );
  }

  #[test]
  fn decodes_entities_in_refresh_url() {
    let html = r#"<meta http-equiv="refresh" content="0; url=&apos;/html/?q=1&amp;r=2&apos;">"#;
    assert_eq!(
      extract_meta_refresh_url(html),
      Some("/html/?q=1&r=2".to_string())
    );
  }

  #[test]
  fn handles_refresh_without_delay() {
    let html = r#"<meta http-equiv="refresh" content="url=/noscript/landing">"#;
    assert_eq!(
      extract_meta_refresh_url(html),
      Some("/noscript/landing".to_string())
    );
  }

  #[test]
  fn ignores_non_refresh_meta() {
    let html = "<meta charset=\"utf-8\"><meta name='viewport' content='width=device-width'>";
    assert_eq!(extract_meta_refresh_url(html), None);
  }

  #[test]
  fn parses_common_meta_refresh_content_formats() {
    let html = r#"<meta http-equiv="refresh" content="0; url=/next">"#;
    assert_eq!(extract_meta_refresh_url(html), Some("/next".to_string()));

    let html = r#"<meta http-equiv="REFRESH" content="0; URL=/caps">"#;
    assert_eq!(extract_meta_refresh_url(html), Some("/caps".to_string()));

    let html = r#"<meta http-equiv="refresh" content="0; url='https://example.com'">"#;
    assert_eq!(
      extract_meta_refresh_url(html),
      Some("https://example.com".to_string())
    );

    let html = r#"<meta http-equiv="refresh" content=" 0 ;  URL =  '/spaced'  ">"#;
    assert_eq!(extract_meta_refresh_url(html), Some("/spaced".to_string()));
  }

  #[test]
  fn extracts_js_location_href() {
    let html = "<script>window.location.href = 'https://example.com/next';</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/next".to_string())
    );
  }

  #[test]
  fn extracts_js_location_href_with_entities() {
    let html = "<script>window.location.href=&quot;/entity/path&quot;;</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/entity/path".to_string())
    );
  }

  #[test]
  fn extracts_js_location_href_with_escaped_slashes() {
    let html = r#"<script>location.href = "https:\\/\\/example.com\\/next";</script>"#;
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/next".to_string())
    );

    let html = "<script>window.location.assign('https://example.com/assign');</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/assign".to_string())
    );

    let html = "<script>document.location.assign('/plain');</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/plain".to_string())
    );
  }

  #[test]
  fn unescapes_js_literal_sequences() {
    assert_eq!(
      super::unescape_js_literal("https:\\/\\/example.com\\/next"),
      "https://example.com/next"
    );
    assert_eq!(super::unescape_js_literal("/path\\x2fwith"), "/path/with");
    assert_eq!(
      super::unescape_js_literal("https:\\/\\/example.com\\/unicode\\u002fpath"),
      "https://example.com/unicode/path"
    );
    assert_eq!(
      super::unescape_js_literal("/encoded%2Fpath%20with"),
      "/encoded/path with"
    );
  }

  #[test]
  fn extracts_js_location_replace() {
    let html = "<script>location.replace(\"/foo\");</script>";
    assert_eq!(extract_js_location_redirect(html), Some("/foo".to_string()));
  }

  #[test]
  fn extracts_url_from_var_assignment() {
    let html = "<script>var url = \"//example.com/next\"; window.location.replace(url);</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/next".to_string())
    );
  }

  #[test]
  fn extracts_url_from_let_const_assignment() {
    let html = "<script>let url = '/from-let'; location.replace(url);</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/from-let".to_string())
    );

    let html = "<script>const url = '/from-const'; window.location.assign(url);</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/from-const".to_string())
    );
  }

  #[test]
  fn extracts_js_location_assignments() {
    let html = "<script>location = '/next';</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/next".to_string())
    );

    let html = "<script>window.location='/win';</script>";
    assert_eq!(extract_js_location_redirect(html), Some("/win".to_string()));

    let html = "<script>document.location = \"https:\\/\\/example.com\\/doc\";</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/doc".to_string())
    );

    let html = "<script>top.location = \"https:\\/\\/example.com\\/top?x=1\\u0026y=2\";</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/top?x=1&y=2".to_string())
    );

    let html = "<script>self.location = '/encoded%2Fpath%20with';</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/encoded/path with".to_string())
    );
  }

  #[test]
  fn extracts_js_location_qualified_calls_and_href_assignments() {
    let html = "<script>top.location.href = '/top-href';</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/top-href".to_string())
    );

    let html = "<script>parent.location.replace('/parent-replace');</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/parent-replace".to_string())
    );

    let html = "<script>self.location.assign('https://example.com/self-assign');</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/self-assign".to_string())
    );
  }

  #[test]
  fn extracts_js_location_backtick_literal() {
    let html = "<script>window.location = `/backtick`;</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/backtick".to_string())
    );
  }

  #[test]
  fn ignores_js_template_literal_interpolation() {
    let html = "<script>location = `/next?x=${y}`;</script>";
    assert_eq!(extract_js_location_redirect(html), None);
  }

  #[test]
  fn handles_escaped_quotes_in_js_string_literals() {
    let html = r#"<script>location.href = "https://example.com/with\"quote";</script>"#;
    assert_eq!(
      extract_js_location_redirect(html),
      Some("https://example.com/with\"quote".to_string())
    );
  }

  #[test]
  fn extracts_parenthesized_js_location_literals() {
    let html = "<script>location = ('/paren');</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/paren".to_string())
    );

    let html = "<script>window.location.href=(\"/paren-href\");</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/paren-href".to_string())
    );

    let html = "<script>location.replace((\"/paren-call\"));</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/paren-call".to_string())
    );
  }

  #[test]
  fn extracts_js_location_wrapped_decode_uri_literals() {
    let html = "<script>location.href = decodeURIComponent(\"%2Fwrapped%2Fnext\");</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/wrapped/next".to_string())
    );

    let html = "<script>location.replace(decodeURI('%2Fwrapped-uri'));</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/wrapped-uri".to_string())
    );

    let html = "<script>location = (decodeURIComponent(('%2Fdouble-paren')));</script>";
    assert_eq!(
      extract_js_location_redirect(html),
      Some("/double-paren".to_string())
    );
  }

  #[test]
  fn ignores_js_location_wrapped_non_literals() {
    let html = "<script>location.href = decodeURIComponent(path);</script>";
    assert_eq!(extract_js_location_redirect(html), None);
  }

  #[test]
  fn ignores_location_pathname_reads() {
    let html = "<script>var p = location.pathname; console.log(p);</script>";
    assert_eq!(extract_js_location_redirect(html), None);
  }

  #[test]
  fn ignores_location_variable_declarations() {
    let html = "<script>var location = '/shadowed';</script>";
    assert_eq!(extract_js_location_redirect(html), None);

    let html = "<script>const location = '/shadowed';</script>";
    assert_eq!(extract_js_location_redirect(html), None);
  }

  #[test]
  fn ignores_non_window_location_properties() {
    let html = "<script>foo.location = '/not-a-redirect';</script>";
    assert_eq!(extract_js_location_redirect(html), None);

    let html = "<script>foo.location.href = '/not-a-redirect';</script>";
    assert_eq!(extract_js_location_redirect(html), None);

    let html = "<script>foo.location.replace('/not-a-redirect');</script>";
    assert_eq!(extract_js_location_redirect(html), None);
  }

  #[test]
  fn ignores_data_location_attributes() {
    // data-location attribute should not be mistaken for a JS redirect target.
    let html = r#"<head data-location="{\"minlon\":1}"></head>"#;
    assert_eq!(extract_js_location_redirect(html), None);
  }
}
