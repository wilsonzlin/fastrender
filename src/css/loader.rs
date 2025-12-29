//! Helpers for loading and inlining external stylesheets.
//!
//! These utilities resolve stylesheet URLs against a base, rewrite relative
//! `url(...)` references to absolute URLs, inline `@import` rules, and inject
//! fetched CSS into an HTML document. They are shared by the developer
//! tooling binaries so cached pages can be rendered with their real styles.

use crate::css::parser::{rel_list_contains_stylesheet, tokenize_rel_list};
use crate::debug::runtime;
use crate::error::{Error, RenderError, RenderStage, Result};
use crate::render_control::{check_active, check_active_periodic, RenderDeadline};
use cssparser::{Parser, ParserInput, Token};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::path::Path;
use url::Url;

/// Resolve a possibly-relative `href` against a base URL.
///
/// Supports protocol-relative URLs (`//example.com`), `data:` URLs (returned
/// as-is), absolute URLs, and filesystem bases (`file://`) that may reference
/// directory paths. JavaScript-escaped hrefs (e.g. `https:\/\/example.com`) are
/// unescaped before resolution.
pub fn resolve_href(base: &str, href: &str) -> Option<String> {
  let href = unescape_js_escapes(href);
  let href = href.trim();
  if href.is_empty() {
    return None;
  }

  // CSS/HTML authors sometimes escape path characters with backslashes. The WHATWG URL parser
  // treats `\` as a path separator for special schemes, but for our resource fetching and URL
  // rewriting we want a stable percent-encoded representation instead.
  let href = if href.contains('\\') {
    Cow::Owned(href.replace('\\', "%5C"))
  } else {
    Cow::Borrowed(href)
  };

  // Ignore fragment-only hrefs (e.g., "#section") since they don't resolve to fetchable stylesheets.
  if href.starts_with('#') {
    return None;
  }

  if href.starts_with("data:") {
    return Some(href.to_string());
  }

  let href_lower = href.to_ascii_lowercase();
  if href_lower.starts_with("javascript:")
    || href_lower.starts_with("vbscript:")
    || href_lower.starts_with("mailto:")
  {
    return None;
  }

  if href.starts_with('#') {
    return None;
  }

  if let Ok(abs) = Url::parse(href.as_ref()) {
    return Some(abs.to_string());
  }

  let mut base_candidate = base.to_string();
  if base_candidate.starts_with("file://") {
    let path = &base_candidate["file://".len()..];
    if Path::new(path).is_dir() && !base_candidate.ends_with('/') {
      base_candidate.push('/');
    }
  }

  Url::parse(&base_candidate)
    .or_else(|_| {
      Url::from_file_path(&base_candidate).map_err(|()| url::ParseError::RelativeUrlWithoutBase)
    })
    .ok()?
    .join(href.as_ref())
    .ok()
    .map(|u| u.to_string())
}

/// Resolve an href against an optional base, returning absolute URLs when possible.
///
/// When no base is provided, absolute URLs (including `data:`) are returned as-is while
/// relative URLs are ignored.
pub fn resolve_href_with_base(base: Option<&str>, href: &str) -> Option<String> {
  match base {
    Some(base) => resolve_href(base, href),
    None => resolve_href("", href),
  }
}

/// Best-effort unescaping for JavaScript-escaped URL strings embedded in HTML/JS.
///
/// Handles `\uXXXX`/`\UXXXX` Unicode escapes (common for `\u0026` encoded ampersands)
/// and simple backslash escaping of quotes or slashes. If the input contains no
/// backslashes, it returns a borrowed slice to avoid allocations; otherwise it
/// builds a new string with the escapes resolved.
///
/// # Examples
///
/// ```rust,ignore
/// use fastrender::css::loader::unescape_js_escapes;
///
/// assert_eq!(unescape_js_escapes("https:\\u002f\\u002fexample.com"), "https://example.com");
/// assert_eq!(unescape_js_escapes(r#"https:\/\/example.com\/path"#), "https://example.com/path");
/// ```
fn unescape_js_escapes(input: &str) -> Cow<'_, str> {
  if !input.contains('\\') {
    return Cow::Borrowed(input);
  }

  let mut out = String::with_capacity(input.len());
  let bytes = input.as_bytes();
  let mut i = 0;
  while i < bytes.len() {
    if bytes[i] == b'\\' {
      if i + 1 < bytes.len()
        && (bytes[i + 1] == b'"' || bytes[i + 1] == b'\'' || bytes[i + 1] == b'/')
      {
        out.push(bytes[i + 1] as char);
        i += 2;
        continue;
      }

      if i + 5 < bytes.len() && (bytes[i + 1] == b'u' || bytes[i + 1] == b'U') {
        if let Ok(code) = u16::from_str_radix(&input[i + 2..i + 6], 16) {
          if let Some(ch) = char::from_u32(code as u32) {
            out.push(ch);
            i += 6;
            continue;
          }
        }
      }
    }

    out.push(bytes[i] as char);
    i += 1;
  }

  Cow::Owned(out)
}

fn normalize_embedded_css_candidate(candidate: &str) -> Option<String> {
  let mut cleaned = candidate
    .trim_matches(|c: char| matches!(c, '"' | '\'' | '(' | ')'))
    .trim()
    .to_string();

  if cleaned.is_empty() {
    return None;
  }

  // Strip common sourceURL markers that get inlined with CSS text (e.g.,
  // "sourceURL=https://example.com/style.css").
  if cleaned.to_ascii_lowercase().starts_with("sourceurl=") {
    if let Some((_, rest)) = cleaned.split_once('=') {
      cleaned = rest.to_string();
    }
  }

  if let Some(pos) = cleaned.to_ascii_lowercase().rfind(".css") {
    let trailing = &cleaned[pos + 4..];
    if trailing.chars().all(|c| c == '/') {
      cleaned.truncate(pos + 4);
    }
  }

  cleaned = decode_html_entities(&cleaned);
  cleaned = unescape_js_escapes(&cleaned).into_owned();
  cleaned = normalize_scheme_slashes(&cleaned);
  if cleaned.contains('\\') {
    cleaned = cleaned.replace('\\', "");
  }

  if cleaned.is_empty() {
    None
  } else {
    Some(cleaned)
  }
}

/// Rewrite `url(...)` references in a CSS string to be absolute using the stylesheet's base URL.
///
/// This walks cssparser tokens so only real `url` tokens are rewritten (including nested
/// `url()` calls inside other functions/blocks). Strings and comments are preserved verbatim.
pub fn absolutize_css_urls(css: &str, base_url: &str) -> std::result::Result<String, RenderError> {
  fn escape_url_for_css(url: &str) -> String {
    let mut escaped = String::with_capacity(url.len());
    for ch in url.chars() {
      match ch {
        '"' => escaped.push_str("\\\""),
        '\\' => escaped.push_str("\\\\"),
        '\n' => escaped.push_str("\\0a "),
        '\r' => escaped.push_str("\\0d "),
        '\t' => escaped.push_str("\\09 "),
        _ => escaped.push(ch),
      }
    }
    escaped
  }

  fn rewrite_urls_in_parser<'i, 't>(
    parser: &mut Parser<'i, 't>,
    base_url: &str,
    capacity_hint: usize,
    deadline_counter: &mut usize,
  ) -> std::result::Result<String, RenderError> {
    let mut out = if capacity_hint > 0 {
      String::with_capacity(capacity_hint)
    } else {
      String::new()
    };
    let mut last_emitted = parser.position();

    check_active(RenderStage::Css)?;
    while !parser.is_exhausted() {
      check_active_periodic(deadline_counter, 256, RenderStage::Css)?;
      let token_start = parser.position();
      let token = match parser.next_including_whitespace_and_comments() {
        Ok(t) => t,
        Err(_) => break,
      };

      match token {
        Token::UnquotedUrl(url_value) => {
          let url_value = url_value.as_ref().to_string();
          let token_text = parser.slice_from(token_start);
          let chunk = parser.slice_from(last_emitted);
          let prefix_len = chunk.len().saturating_sub(token_text.len());
          out.push_str(&chunk[..prefix_len]);

          if let Some(resolved) = resolve_href(base_url, &url_value) {
            let escaped = escape_url_for_css(&resolved);
            out.push_str(&format!("url(\"{}\")", escaped));
          } else {
            out.push_str(token_text);
          }

          last_emitted = parser.position();
        }
        Token::Function(ref name) if name.eq_ignore_ascii_case("url") => {
          let parse_result = parser.parse_nested_block(|nested| {
            let start = nested.position();
            let mut arg: Option<String> = None;

            while !nested.is_exhausted() {
              match nested.next_including_whitespace_and_comments() {
                Ok(Token::WhiteSpace(_)) | Ok(Token::Comment(_)) => {}
                Ok(Token::QuotedString(s)) | Ok(Token::UnquotedUrl(s)) => {
                  arg = Some(s.as_ref().to_string());
                }
                Ok(Token::Ident(s)) => {
                  arg = Some(s.as_ref().to_string());
                }
                Ok(Token::BadUrl(_)) => {
                  arg = None;
                }
                Ok(_) => {}
                Err(_) => break,
              }
            }

            let original_inner = nested.slice_from(start);
            Ok::<_, cssparser::ParseError<'i, ()>>((arg, original_inner.len()))
          });

          let block_text = parser.slice_from(token_start);
          let chunk = parser.slice_from(last_emitted);
          let prefix_len = chunk.len().saturating_sub(block_text.len());
          out.push_str(&chunk[..prefix_len]);

          if let Ok((arg, _)) = parse_result {
            if let Some(url_arg) = arg {
              if let Some(resolved) = resolve_href(base_url, &url_arg) {
                let escaped = escape_url_for_css(&resolved);
                out.push_str(&format!("url(\"{}\")", escaped));
                last_emitted = parser.position();
                continue;
              }
            }
          }

          out.push_str(block_text);
          last_emitted = parser.position();
        }
        Token::Function(_)
        | Token::ParenthesisBlock
        | Token::SquareBracketBlock
        | Token::CurlyBracketBlock => {
          let mut nested_error: Option<RenderError> = None;
          let parse_result = parser.parse_nested_block(|nested| {
            let start = nested.position();
            let rewritten = match rewrite_urls_in_parser(nested, base_url, 0, deadline_counter) {
              Ok(r) => r,
              Err(err) => {
                nested_error = Some(err);
                return Err(nested.new_custom_error(()));
              }
            };
            let original = nested.slice_from(start);
            let changed = rewritten != original;
            Ok::<_, cssparser::ParseError<'i, ()>>((rewritten, original.len(), changed))
          });

          let block_text = parser.slice_from(token_start);
          let chunk = parser.slice_from(last_emitted);
          let prefix_len = chunk.len().saturating_sub(block_text.len());
          out.push_str(&chunk[..prefix_len]);

          if let Some(err) = nested_error {
            return Err(err);
          }
          if let Ok((inner_rewritten, inner_len, changed)) = parse_result {
            const CLOSING_LEN: usize = 1;
            if !changed {
              out.push_str(block_text);
              last_emitted = parser.position();
              continue;
            }
            if block_text.len() >= inner_len + CLOSING_LEN {
              let open_len = block_text.len() - inner_len - CLOSING_LEN;
              if open_len <= block_text.len() {
                let (open_part, _) = block_text.split_at(open_len);
                let close_part = &block_text[block_text.len() - CLOSING_LEN..];
                out.push_str(open_part);
                out.push_str(&inner_rewritten);
                out.push_str(close_part);
                last_emitted = parser.position();
                continue;
              }
            }
          }

          out.push_str(block_text);
          last_emitted = parser.position();
        }
        _ => {}
      }
    }

    out.push_str(parser.slice_from(last_emitted));
    Ok(out)
  }

  let mut input = ParserInput::new(css);
  let mut parser = Parser::new(&mut input);
  let mut deadline_counter = 0usize;
  rewrite_urls_in_parser(&mut parser, base_url, css.len(), &mut deadline_counter)
}

fn parse_import_target(rule: &str) -> Option<(String, String)> {
  let after_at = rule
    .get(..7)
    .filter(|prefix| prefix.eq_ignore_ascii_case("@import"))?;
  let after_at = rule[after_at.len()..].trim_start();
  let (target, rest) = if let Some(inner) = after_at.strip_prefix("url(") {
    let close = inner.find(')')?;
    let url_part = &inner[..close].trim();
    let url_str = url_part.trim_matches(|c| c == '"' || c == '\'').to_string();
    let media = inner[close + 1..]
      .trim()
      .trim_end_matches(';')
      .trim()
      .to_string();
    (url_str, media)
  } else if let Some(quote) = after_at.chars().next().filter(|c| *c == '"' || *c == '\'') {
    let rest = &after_at[1..];
    let close_idx = rest.find(quote)?;
    let url_str = rest[..close_idx].to_string();
    let media = rest[close_idx + 1..]
      .trim()
      .trim_end_matches(';')
      .trim()
      .to_string();
    (url_str, media)
  } else {
    return None;
  };
  Some((target, rest))
}

const MAX_INLINE_IMPORTS: usize = 64;

/// Tracks recursion state and cached inlined content for `@import` processing.
#[derive(Default)]
pub struct InlineImportState {
  stack: Vec<String>,
  seen: HashSet<String>,
  cache: HashMap<String, String>,
}

impl InlineImportState {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn register_stylesheet(&mut self, url: impl Into<String>) {
    self.seen.insert(url.into());
  }
}

/// Inline `@import` rules by fetching their targets recursively.
///
/// All fetched stylesheets have their `url(...)` references rewritten against the
/// stylesheet URL before inlining, so relative asset references continue to work
/// once the CSS is embedded in the document.
pub fn inline_imports<F>(
  css: &str,
  base_url: &str,
  fetch: &mut F,
  state: &mut InlineImportState,
  deadline: Option<&RenderDeadline>,
) -> std::result::Result<String, RenderError>
where
  F: FnMut(&str) -> Result<String>,
{
  inline_imports_with_diagnostics(
    css,
    base_url,
    fetch,
    state,
    &mut |_url, _reason| {},
    deadline,
  )
}

/// Inline `@import` rules with diagnostics about cycles and cutoffs.
///
/// This variant mirrors [`inline_imports`] but surfaces skipped imports to the caller.
pub fn inline_imports_with_diagnostics<F, D>(
  css: &str,
  base_url: &str,
  fetch: &mut F,
  state: &mut InlineImportState,
  diagnostics: &mut D,
  deadline: Option<&RenderDeadline>,
) -> std::result::Result<String, RenderError>
where
  F: FnMut(&str) -> Result<String>,
  D: FnMut(&str, &str),
{
  state.stack.push(base_url.to_string());
  let result = inline_imports_inner(css, base_url, fetch, state, diagnostics, deadline);
  state.stack.pop();
  result
}

fn inline_imports_inner<F, D>(
  css: &str,
  base_url: &str,
  fetch: &mut F,
  state: &mut InlineImportState,
  diagnostics: &mut D,
  deadline: Option<&RenderDeadline>,
) -> std::result::Result<String, RenderError>
where
  F: FnMut(&str) -> Result<String>,
  D: FnMut(&str, &str),
{
  #[derive(PartialEq)]
  enum State {
    Normal,
    Single,
    Double,
    Comment,
  }

  let mut out = String::with_capacity(css.len());
  let mut parser_state = State::Normal;
  let bytes = css.as_bytes();
  let mut i = 0usize;
  let mut last_emit = 0usize;
  let mut active_deadline_counter = 0usize;
  let mut explicit_deadline_counter = 0usize;

  check_active(RenderStage::Css)?;
  if let Some(limit) = deadline {
    limit.check(RenderStage::Css)?;
  }

  while i < bytes.len() {
    check_active_periodic(&mut active_deadline_counter, 512, RenderStage::Css)?;
    if let Some(limit) = deadline {
      limit.check_periodic(&mut explicit_deadline_counter, 512, RenderStage::Css)?;
    }
    match parser_state {
      State::Normal => {
        if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
          parser_state = State::Comment;
          i += 2;
          continue;
        }
        if bytes[i] == b'\'' {
          parser_state = State::Single;
          i += 1;
          continue;
        }
        if bytes[i] == b'"' {
          parser_state = State::Double;
          i += 1;
          continue;
        }

        if bytes[i] == b'@' {
          let remainder = &css[i..];
          if remainder.len() >= 7 && remainder[1..].to_lowercase().starts_with("import") {
            let mut j = i;
            let mut inner_state = State::Normal;
            while j < bytes.len() {
              match inner_state {
                State::Normal => {
                  if bytes[j] == b';' {
                    j += 1;
                    break;
                  }
                  if bytes[j] == b'\'' {
                    inner_state = State::Single;
                  } else if bytes[j] == b'"' {
                    inner_state = State::Double;
                  } else if bytes[j] == b'/' && j + 1 < bytes.len() && bytes[j + 1] == b'*' {
                    inner_state = State::Comment;
                    j += 1;
                  }
                }
                State::Single => {
                  if bytes[j] == b'\\' {
                    j += 1;
                  } else if bytes[j] == b'\'' {
                    inner_state = State::Normal;
                  }
                }
                State::Double => {
                  if bytes[j] == b'\\' {
                    j += 1;
                  } else if bytes[j] == b'"' {
                    inner_state = State::Normal;
                  }
                }
                State::Comment => {
                  if bytes[j] == b'*' && j + 1 < bytes.len() && bytes[j + 1] == b'/' {
                    inner_state = State::Normal;
                    j += 1;
                  }
                }
              }
              j += 1;
            }

            let rule = css[i..j].trim();
            if let Some((target, media)) = parse_import_target(rule) {
              if let Some(resolved) = resolve_href(base_url, &target) {
                if state.seen.len() >= MAX_INLINE_IMPORTS {
                  diagnostics(&resolved, "import limit reached");
                  out.push_str(&css[last_emit..]);
                  return Ok(out);
                }
                out.push_str(&css[last_emit..i]);
                if state.stack.contains(&resolved) {
                  diagnostics(&resolved, "skipping cyclic @import");
                } else {
                  let mut inlined: Option<String> = None;
                  if let Some(cached) = state.cache.get(&resolved) {
                    inlined = Some(cached.clone());
                  } else {
                    state.seen.insert(resolved.clone());
                    if let Some(limit) = deadline {
                      limit.check(RenderStage::Css)?;
                    }
                    match fetch(&resolved) {
                      Ok(fetched) => {
                        if let Some(limit) = deadline {
                          limit.check(RenderStage::Css)?;
                        }
                        let rewritten = absolutize_css_urls(&fetched, &resolved)?;
                        let nested = inline_imports_with_diagnostics(
                          &rewritten,
                          &resolved,
                          fetch,
                          state,
                          diagnostics,
                          deadline,
                        )?;
                        state.cache.insert(resolved.clone(), nested.clone());
                        inlined = Some(nested);
                      }
                      Err(Error::Render(RenderError::Timeout { stage, elapsed })) => {
                        return Err(RenderError::Timeout { stage, elapsed });
                      }
                      Err(_) => {}
                    }
                  }

                  if let Some(inlined) = inlined {
                    if media.is_empty() || media.eq_ignore_ascii_case("all") {
                      out.push_str(&inlined);
                    } else {
                      let _ = write!(out, "@media {} {{\n{}\n}}\n", media, inlined);
                    }
                  }
                }
                last_emit = j;
                i = j;
                continue;
              }
            }
          }
        }

        i += 1;
      }
      State::Single => {
        if bytes[i] == b'\\' {
          i += 2;
          continue;
        }
        if bytes[i] == b'\'' {
          parser_state = State::Normal;
        }
        i += 1;
      }
      State::Double => {
        if bytes[i] == b'\\' {
          i += 2;
          continue;
        }
        if bytes[i] == b'"' {
          parser_state = State::Normal;
        }
        i += 1;
      }
      State::Comment => {
        if bytes[i] == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
          parser_state = State::Normal;
          i += 2;
        } else {
          i += 1;
        }
      }
    }
  }

  out.push_str(&css[last_emit..]);
  Ok(out)
}

fn extract_attr_value(tag_source: &str, attr: &str) -> Option<String> {
  let target = attr.to_ascii_lowercase();
  let mut chars = tag_source.chars().peekable();

  // Skip the tag name itself.
  if let Some('<') = chars.peek().copied() {
    chars.next();
  }
  while let Some(&c) = chars.peek() {
    if c.is_whitespace() || c == '>' {
      break;
    }
    chars.next();
  }

  loop {
    // Skip whitespace between attributes.
    while let Some(&c) = chars.peek() {
      if c.is_whitespace() {
        chars.next();
      } else {
        break;
      }
    }

    match chars.peek().copied() {
      None | Some('>') => break,
      Some('/') => {
        chars.next();
        continue;
      }
      _ => {}
    }

    let mut name = String::new();
    while let Some(&c) = chars.peek() {
      if c.is_whitespace() || c == '=' || c == '>' {
        break;
      }
      name.push(c);
      chars.next();
    }

    if name.is_empty() {
      if chars.next().is_none() {
        break;
      }
      continue;
    }

    let name_lower = name.to_ascii_lowercase();

    while let Some(&c) = chars.peek() {
      if c.is_whitespace() {
        chars.next();
      } else {
        break;
      }
    }

    let value = if let Some('=') = chars.peek().copied() {
      chars.next();

      while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
          chars.next();
        } else {
          break;
        }
      }

      if let Some(next) = chars.peek().copied() {
        if next == '"' || next == '\'' {
          let quote = next;
          chars.next();
          let mut val = String::new();
          while let Some(ch) = chars.next() {
            if ch == quote {
              break;
            }
            val.push(ch);
          }
          val
        } else {
          let mut val = String::new();
          while let Some(&ch) = chars.peek() {
            if ch.is_whitespace() || ch == '>' {
              break;
            }
            val.push(ch);
            chars.next();
          }
          val
        }
      } else {
        String::new()
      }
    } else {
      // Boolean attribute: treat as present with an empty value.
      String::new()
    };

    if name_lower == target {
      return Some(decode_html_entities(&value));
    }
  }

  None
}

fn parse_tag_attributes(tag_source: &str) -> HashMap<String, String> {
  let mut attrs = HashMap::new();
  let bytes = tag_source.as_bytes();
  let mut i = tag_source.find('<').map(|idx| idx + 1).unwrap_or(0);

  while i < bytes.len() && !bytes[i].is_ascii_whitespace() && bytes[i] != b'>' {
    i += 1;
  }

  while i < bytes.len() {
    while i < bytes.len() {
      let b = bytes[i];
      if b.is_ascii_whitespace() || b == b'/' {
        i += 1;
      } else {
        break;
      }
    }

    if i >= bytes.len() || bytes[i] == b'>' {
      break;
    }

    let name_start = i;
    while i < bytes.len() {
      let b = bytes[i];
      if b.is_ascii_whitespace() || b == b'=' || b == b'>' {
        break;
      }
      i += 1;
    }

    if name_start == i {
      i += 1;
      continue;
    }

    let name = tag_source[name_start..i].to_ascii_lowercase();

    while i < bytes.len() && bytes[i].is_ascii_whitespace() {
      i += 1;
    }

    let mut value = String::new();
    if i < bytes.len() && bytes[i] == b'=' {
      i += 1;
      while i < bytes.len() && bytes[i].is_ascii_whitespace() {
        i += 1;
      }

      if i >= bytes.len() {
        attrs.insert(name, value);
        break;
      }

      let quote = bytes[i];
      if quote == b'"' || quote == b'\'' {
        i += 1;
        let val_start = i;
        while i < bytes.len() && bytes[i] != quote {
          i += 1;
        }
        value.push_str(&tag_source[val_start..i]);
        if i < bytes.len() {
          i += 1;
        }
      } else {
        let val_start = i;
        while i < bytes.len() {
          let b = bytes[i];
          if b.is_ascii_whitespace() || b == b'>' {
            break;
          }
          i += 1;
        }
        value.push_str(&tag_source[val_start..i]);
      }
    }

    attrs.insert(name, decode_html_entities(value.trim()));
  }

  attrs
}

fn decode_html_entities(input: &str) -> String {
  let mut out = String::with_capacity(input.len());
  let mut chars = input.chars().peekable();
  while let Some(c) = chars.next() {
    if c != '&' {
      out.push(c);
      continue;
    }

    let mut entity = String::new();
    while let Some(&next) = chars.peek() {
      entity.push(next);
      chars.next();
      if next == ';' {
        break;
      }
    }

    if entity.is_empty() {
      out.push('&');
      continue;
    }

    let mut ent = entity.as_str();
    if let Some(stripped) = ent.strip_prefix('/') {
      ent = stripped;
    }

    let decoded = match ent {
      "amp;" => Some('&'),
      "quot;" => Some('"'),
      "apos;" => Some('\''),
      "lt;" => Some('<'),
      "gt;" => Some('>'),
      _ => {
        if let Some(num) = ent.strip_prefix('#') {
          let trimmed = num.trim_end_matches(';');
          if let Some(hex) = trimmed.strip_prefix(['x', 'X']) {
            u32::from_str_radix(hex, 16).ok().and_then(char::from_u32)
          } else {
            trimmed.parse::<u32>().ok().and_then(char::from_u32)
          }
        } else {
          None
        }
      }
    };

    if let Some(ch) = decoded {
      out.push(ch);
    } else {
      out.push('&');
      out.push_str(&entity);
    }
  }
  normalize_scheme_slashes(&out)
}

fn normalize_scheme_slashes(s: &str) -> String {
  if s.starts_with("//") {
    // Preserve scheme-relative URLs as-is so they can be resolved against the base
    // scheme rather than collapsing the host into a path segment.
    return s.to_string();
  }

  let mut path_end = s.len();
  if let Some(pos) = s.find('?') {
    path_end = path_end.min(pos);
  }
  if let Some(pos) = s.find('#') {
    path_end = path_end.min(pos);
  }

  let (path, suffix) = s.split_at(path_end);

  let normalized = if let Some(pos) = path.find("://") {
    let (scheme, rest) = path.split_at(pos + 3);
    let mut trimmed = rest.trim_start_matches('/').to_string();
    while trimmed.contains("//") {
      trimmed = trimmed.replace("//", "/");
    }
    format!("{}{}", scheme, trimmed)
  } else {
    let mut out = path.to_string();
    while out.contains("//") {
      out = out.replace("//", "/");
    }
    out
  };

  format!("{}{}", normalized, suffix)
}

/// Extract `<link rel="stylesheet">` URLs from an HTML document.
pub fn extract_css_links(
  html: &str,
  base_url: &str,
  media_type: crate::style::media::MediaType,
) -> std::result::Result<Vec<String>, RenderError> {
  let mut css_urls = Vec::new();
  let toggles = runtime::runtime_toggles();
  let debug = toggles.truthy("FASTR_LOG_CSS_LINKS");
  let preload_stylesheets_enabled =
    toggles.truthy_with_default("FASTR_FETCH_PRELOAD_STYLESHEETS", true);
  let modulepreload_stylesheets_enabled =
    toggles.truthy_with_default("FASTR_FETCH_MODULEPRELOAD_STYLESHEETS", false);
  let alternate_stylesheets_enabled =
    toggles.truthy_with_default("FASTR_FETCH_ALTERNATE_STYLESHEETS", true);

  let lower = html.to_lowercase();
  let mut pos = 0;
  let mut deadline_counter = 0usize;

  while let Some(link_start) = lower[pos..].find("<link") {
    check_active_periodic(&mut deadline_counter, 1, RenderStage::Css)?;
    let abs_start = pos + link_start;
    if let Some(link_end) = lower[abs_start..].find('>') {
      let link_tag = &html[abs_start..=abs_start + link_end];
      let attrs = parse_tag_attributes(link_tag);
      let rel_tokens = attrs
        .get("rel")
        .map(|rel| tokenize_rel_list(rel))
        .unwrap_or_default();
      let rel_has_stylesheet = rel_list_contains_stylesheet(&rel_tokens);
      let rel_has_alternate = rel_tokens
        .iter()
        .any(|t| t.eq_ignore_ascii_case("alternate"));
      let rel_has_preload = rel_tokens.iter().any(|t| t.eq_ignore_ascii_case("preload"));
      let rel_has_modulepreload = rel_tokens
        .iter()
        .any(|t| t.eq_ignore_ascii_case("modulepreload"));

      let as_style = attrs
        .get("as")
        .map(|v| v.trim().eq_ignore_ascii_case("style"))
        .unwrap_or(false);

      let mut is_stylesheet_link =
        rel_has_stylesheet && (alternate_stylesheets_enabled || !rel_has_alternate);

      if !is_stylesheet_link && preload_stylesheets_enabled && rel_has_preload && as_style {
        is_stylesheet_link = true;
      }

      if !is_stylesheet_link
        && modulepreload_stylesheets_enabled
        && rel_has_modulepreload
        && as_style
      {
        is_stylesheet_link = true;
      }

      let link_tag_lower = link_tag.to_lowercase();

      if !is_stylesheet_link && rel_tokens.is_empty() && link_tag_lower.contains("stylesheet") {
        is_stylesheet_link = true;
      }

      if is_stylesheet_link {
        if debug {
          eprintln!("[css] found <link>: {}", link_tag);
        }
        let mut allowed = true;

        if let Some(media) = attrs.get("media") {
          allowed = media_attr_allows_target(media, media_type);
          if debug {
            eprintln!(
              "[css] media attr: {} (target={:?}, allow={})",
              media, media_type, allowed
            );
          }
        } else if link_tag_lower.contains("media") {
          let has_screen = link_tag_lower.contains("screen") || link_tag_lower.contains("all");
          let has_print = link_tag_lower.contains("print");
          let has_speech = link_tag_lower.contains("speech");
          allowed = media_flags_allow_target(has_screen, has_print, has_speech, media_type);
          if debug {
            eprintln!(
              "[css] media substring in tag (no attr parsed), print={}, screen={}, speech={}, allow={}",
              has_print, has_screen, has_speech, allowed
            );
          }
        }
        if let Some(href) = attrs.get("href") {
          let href = normalize_scheme_slashes(href);
          if let Some(full_url) = resolve_href(base_url, &href) {
            if allowed {
              css_urls.push(full_url);
            }
          }
        }
      }

      pos = abs_start + link_end + 1;
    } else {
      break;
    }
  }

  Ok(dedupe_links_preserving_order(css_urls))
}

fn media_attr_allows_target(value: &str, target: crate::style::media::MediaType) -> bool {
  let value = value.trim();
  if value.is_empty() {
    return true;
  }

  let mut allowed = false;
  for token in value.split(',') {
    let t = token.trim().to_ascii_lowercase();
    allowed |= match t.as_str() {
      "all" => return true,
      "screen" => {
        matches!(
          target,
          crate::style::media::MediaType::Screen | crate::style::media::MediaType::All
        )
      }
      "print" => {
        matches!(
          target,
          crate::style::media::MediaType::Print | crate::style::media::MediaType::All
        )
      }
      "speech" => {
        matches!(
          target,
          crate::style::media::MediaType::Speech | crate::style::media::MediaType::All
        )
      }
      _ => false,
    };
  }

  allowed
}

fn media_flags_allow_target(
  has_screen: bool,
  has_print: bool,
  has_speech: bool,
  target: crate::style::media::MediaType,
) -> bool {
  match target {
    crate::style::media::MediaType::Screen => {
      has_screen || (!has_print && !has_speech && !has_screen)
    }
    crate::style::media::MediaType::Print => {
      has_print || (!has_screen && !has_speech && !has_print)
    }
    crate::style::media::MediaType::Speech => {
      has_speech || (!has_screen && !has_print && !has_speech)
    }
    crate::style::media::MediaType::All => true,
  }
}

/// Heuristic extraction of CSS URLs that appear inside inline scripts or attributes.
///
/// Some sites load their primary stylesheets dynamically and never emit a
/// `<link rel="stylesheet">` element in the static HTML. To render those pages
/// without executing JavaScript, scan the raw HTML for any substring that looks
/// like a CSS URL (ends with `.css`, possibly with a query string) and try to
/// resolve and fetch it as a stylesheet.
#[allow(clippy::cognitive_complexity)]
pub fn extract_embedded_css_urls(
  html: &str,
  base_url: &str,
) -> std::result::Result<Vec<String>, RenderError> {
  let mut urls = Vec::new();
  let mut seen = HashSet::new();
  let bytes = html.as_bytes();
  let mut idx = 0;
  let mut deadline_counter = 0usize;

  while let Some(pos) = memchr::memmem::find(&bytes[idx..], b".css") {
    check_active_periodic(&mut deadline_counter, 1, RenderStage::Css)?;
    let abs_pos = idx + pos;

    let mut start = abs_pos;
    while start > 0 {
      let c = bytes[start - 1] as char;
      if matches!(c, '"' | '\'' | '(' | '<') || c.is_whitespace() {
        break;
      }
      start -= 1;
    }

    let mut end = abs_pos + 4;
    while end < bytes.len() {
      let c = bytes[end] as char;
      if matches!(c, '"' | '\'' | ')' | '>' | '{' | '}') || c.is_whitespace() {
        break;
      }
      end += 1;
    }

    // If this candidate appears inside a <link> tag that is print-only, skip it.
    if abs_pos > 0 {
      let tag_start = bytes[..abs_pos].iter().rposition(|&b| b == b'<');
      let tag_end = bytes[abs_pos..].iter().position(|&b| b == b'>');
      if let (Some(ts), Some(te_rel)) = (tag_start, tag_end) {
        let te = abs_pos + te_rel;
        if te > ts {
          let tag = &html[ts..=te];
          let tag_lower = tag.to_ascii_lowercase();
          if tag_lower.contains("<link") && tag_lower.contains("media") {
            let has_screen = tag_lower.contains("screen") || tag_lower.contains("all");
            let has_print = tag_lower.contains("print");
            if has_print && !has_screen {
              idx = end;
              continue;
            }
          }
        }
      }
    }

    // Skip identifiers like `window.css = ...` where the token is an assignment target
    // rather than a URL. If the next non-whitespace character after the match is '=',
    // treat it as a property access and ignore it.
    let mut lookahead = end;
    while lookahead < bytes.len() && (bytes[lookahead] as char).is_whitespace() {
      lookahead += 1;
    }
    if lookahead < bytes.len() && bytes[lookahead] == b'=' {
      idx = end;
      continue;
    }

    if end > start {
      let candidate = &html[start..end];
      if candidate.len() < 512 {
        let raw_lower = candidate.to_ascii_lowercase();

        // Detect sourceURL-style sourcemap markers (/*# or //#) immediately preceding the token.
        let mut marker_back = start;
        while marker_back > 0 && (bytes[marker_back - 1] as char).is_whitespace() {
          marker_back -= 1;
        }
        let sourcemap_marker = if marker_back > 0 && bytes[marker_back - 1] == b'#' {
          (marker_back >= 3 && bytes[marker_back - 2] == b'*' && bytes[marker_back - 3] == b'/')
            || (marker_back >= 2 && bytes[marker_back - 2] == b'/')
        } else {
          false
        };

        if sourcemap_marker && raw_lower.contains("sourceurl=") {
          idx = end;
          continue;
        }

        if let Some(cleaned) = normalize_embedded_css_candidate(candidate) {
          if cleaned.contains('{') || cleaned.contains('}') {
            idx = end;
            continue;
          }
          if let Some(first) = cleaned.chars().next() {
            if !(first.is_ascii_alphanumeric() || matches!(first, '/' | '.' | '#')) {
              idx = end;
              continue;
            }
          }

          let cleaned_lower = cleaned.to_ascii_lowercase();
          if cleaned_lower.contains("sourceurl=") {
            idx = end;
            continue;
          }
          let css_pos = cleaned_lower.find(".css");
          if let Some(pos) = css_pos {
            let after = cleaned_lower.as_bytes().get(pos + 4).copied();
            if let Some(ch) = after {
              let ch = ch as char;
              if ch != '?' && ch != '#' && ch != '/' && ch != '%' && ch != '"' && ch != '\'' {
                idx = end;
                continue;
              }
            }
          } else {
            idx = end;
            continue;
          }
          if !cleaned_lower.contains("style.csstext") && !cleaned.trim_end().ends_with(':') {
            if let Some(resolved) = resolve_href(base_url, &cleaned) {
              if seen.insert(resolved.clone()) {
                urls.push(resolved);
              }
            }
          }
        }
      }
    }

    idx = end;
  }

  let lower = html.to_lowercase();
  let mut pos = 0;
  while let Some(hit) = lower[pos..].find("cssurl") {
    check_active_periodic(&mut deadline_counter, 1, RenderStage::Css)?;
    let abs = pos + hit;
    let slice = &html[abs..];
    if let Some(colon) = slice.find(':') {
      let after_colon = &slice[colon + 1..];
      if let Some(q_start_rel) = after_colon.find(['"', '\'']) {
        let quote = after_colon.chars().nth(q_start_rel).unwrap();
        let after_quote = &after_colon[q_start_rel + 1..];
        if let Some(q_end_rel) = after_quote.find(quote) {
          let candidate = &after_quote[..q_end_rel];
          if !candidate.to_ascii_lowercase().contains("style.csstext")
            && !candidate.trim_end().ends_with(':')
          {
            if let Some(cleaned) = normalize_embedded_css_candidate(candidate) {
              let lower = cleaned.to_ascii_lowercase();
              if !lower.contains("style.csstext") && !cleaned.trim_end().ends_with(':') {
                if let Some(resolved) = resolve_href(base_url, &cleaned) {
                  if seen.insert(resolved.clone()) {
                    urls.push(resolved);
                  }
                }
              }
            }
          }
        }
      }
    }
    pos = abs + 6;
  }

  Ok(urls)
}

/// Deduplicate a list while preserving the order of first occurrence.
pub fn dedupe_links_preserving_order(mut links: Vec<String>) -> Vec<String> {
  let mut seen: HashSet<String> = HashSet::with_capacity(links.len());
  links.retain(|link| seen.insert(link.clone()));
  links
}

/// Inject a `<style>` block containing `css` into the HTML document.
pub fn inject_css_into_html(html: &str, css: &str) -> String {
  let style_tag = format!("<style>{css}</style>");

  if let Some(head_end) = find_tag_case_insensitive(html, "head", true) {
    let mut result = String::with_capacity(html.len() + style_tag.len());
    result.push_str(&html[..head_end]);
    result.push_str(&style_tag);
    result.push_str(&html[head_end..]);
    result
  } else if let Some(body_start) = find_tag_case_insensitive(html, "body", false) {
    let mut result = String::with_capacity(html.len() + style_tag.len());
    result.push_str(&html[..body_start]);
    result.push_str(&style_tag);
    result.push_str(&html[body_start..]);
    result
  } else {
    format!("{style_tag}{html}")
  }
}

fn find_tag_case_insensitive(html: &str, tag: &str, closing: bool) -> Option<usize> {
  debug_assert!(tag.as_bytes().iter().all(|b| !b.is_ascii_uppercase()));

  let bytes = html.as_bytes();
  let tag_bytes = tag.as_bytes();
  let mut search_from = 0;

  while search_from < bytes.len() {
    let rel = bytes[search_from..].iter().position(|b| *b == b'<')?;
    let start = search_from + rel;
    let mut pos = start + 1;

    while pos < bytes.len() && bytes[pos].is_ascii_whitespace() {
      pos += 1;
    }

    if closing {
      if pos >= bytes.len() || bytes[pos] != b'/' {
        search_from = start + 1;
        continue;
      }
      pos += 1;
      while pos < bytes.len() && bytes[pos].is_ascii_whitespace() {
        pos += 1;
      }
    }

    if pos + tag_bytes.len() > bytes.len() {
      break;
    }

    let mut matched = true;
    for (idx, expected) in tag_bytes.iter().enumerate() {
      if bytes[pos + idx].to_ascii_lowercase() != *expected {
        matched = false;
        break;
      }
    }

    if matched {
      let after = pos + tag_bytes.len();
      if after >= bytes.len() {
        return Some(start);
      }

      let next = bytes[after];
      if next == b'>' || next == b'/' || next.is_ascii_whitespace() {
        return Some(start);
      }
    }

    search_from = start + 1;
  }

  None
}

/// Infer a reasonable base URL for the document.
///
/// Prefers an explicit `<base href>` when present, otherwise uses the document
/// URL itself. For `file://` inputs without a `<base>` hint, falls back to
/// `<link rel="canonical">`/`<meta property="og:url">` if present and, as a last
/// resort, an `https://{filename}/` origin so relative resources resolve against
/// the original site instead of the local filesystem.
pub fn infer_base_url<'a>(html: &'a str, input_url: &'a str) -> Cow<'a, str> {
  // Canonicalize file:// inputs so relative cached paths become absolute.
  let mut input = Cow::Borrowed(input_url);
  let mut is_file_input = false;
  if input_url.starts_with("file://") && !input_url.starts_with("file:///") {
    // file://relative/path.html
    let rel = &input_url["file://".len()..];
    if let Ok(canon) = std::fs::canonicalize(rel) {
      input = Cow::Owned(format!("file://{}", canon.display()));
    }
  } else if let Ok(url) = Url::parse(input_url) {
    if url.scheme() == "file" {
      if let Ok(path) = url.to_file_path() {
        if let Ok(canon) = path.canonicalize() {
          input = Cow::Owned(format!("file://{}", canon.display()));
        }
      }
    }
  }

  if let Ok(url) = Url::parse(&input) {
    is_file_input = url.scheme() == "file";
  }

  enum BaseUrlHintFilter {
    Any,
    RelCanonical,
    PropertyOgUrl,
  }

  let lower = html.to_lowercase();
  for (needle, attr, filter, allow_for_http_inputs) in [
    ("<base", "href", BaseUrlHintFilter::Any, true),
    ("<link", "href", BaseUrlHintFilter::RelCanonical, false),
    ("<meta", "content", BaseUrlHintFilter::PropertyOgUrl, false),
  ] {
    if !allow_for_http_inputs && !is_file_input {
      continue;
    }
    let mut pos = 0;
    while let Some(idx) = lower[pos..].find(needle) {
      let abs = pos + idx;
      if let Some(end) = lower[abs..].find('>') {
        let tag_slice = &html[abs..=abs + end];
        let attrs = parse_tag_attributes(tag_slice);

        let matches_filter = match filter {
          BaseUrlHintFilter::Any => true,
          BaseUrlHintFilter::RelCanonical => attrs.get("rel").map_or(false, |rel| {
            rel
              .split_whitespace()
              .any(|token| token.eq_ignore_ascii_case("canonical"))
          }),
          BaseUrlHintFilter::PropertyOgUrl => attrs
            .get("property")
            .map_or(false, |prop| prop.eq_ignore_ascii_case("og:url")),
        };

        if matches_filter {
          if let Some(val) = attrs.get(attr) {
            if let Some(resolved) = resolve_href(&input, val) {
              if resolved.starts_with("http://") || resolved.starts_with("https://") {
                return Cow::Owned(resolved);
              }
            }
          }
        }
        pos = abs + end + 1;
      } else {
        break;
      }
    }
  }

  if let Ok(url) = Url::parse(&input) {
    if url.scheme() == "file" {
      if let Some(seg) = url.path_segments().and_then(|mut s| s.next_back()) {
        if let Some(host) = seg.strip_suffix(".html") {
          // Heuristic: cached pages typically use a `{host}.html` filename. Avoid applying this
          // to local files like `page.html` by requiring the host portion to look like a domain.
          if host.contains('.') {
            let guess = format!("https://{host}/");
            if Url::parse(&guess).is_ok() {
              return Cow::Owned(guess);
            }
          }
        }
      }
    }
  }

  input
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::debug::runtime::{self, RuntimeToggles};
  use crate::style::media::MediaType;
  use std::collections::HashMap;
  use std::sync::Arc;
  use tempfile;

  #[test]
  fn resolves_relative_http_links() {
    let base = "https://example.com/a/b/page.html";
    let href = "../styles/site.css";
    let resolved = resolve_href(base, href).expect("resolved");
    assert_eq!(resolved, "https://example.com/a/styles/site.css");
  }

  #[test]
  fn resolves_protocol_relative_links() {
    let base = "https://example.com/index.html";
    let href = "//cdn.example.com/main.css";
    let resolved = resolve_href(base, href).expect("resolved");
    assert_eq!(resolved, "https://cdn.example.com/main.css");
  }

  #[test]
  fn absolutizes_css_urls_rewrites_urls() {
    let css = "body { background: url(\"images/bg.png\"); }";
    let out = absolutize_css_urls(css, "https://example.com/styles/main.css").unwrap();
    assert!(out.contains("https://example.com/styles/images/bg.png"));
  }

  #[test]
  fn inline_imports_flattens_nested_imports() {
    let mut state = InlineImportState::new();
    let css = "@import \"nested.css\";\nbody { color: black; }";
    let mut fetched = |url: &str| -> Result<String> {
      if url.ends_with("nested.css") {
        Ok("p { margin: 0; }".to_string())
      } else {
        Ok(String::new())
      }
    };
    let out = inline_imports(
      css,
      "https://example.com/main.css",
      &mut fetched,
      &mut state,
      None,
    )
    .unwrap();
    if !out.contains("p { margin: 0; }") {
      eprintln!("inline_imports output: {out}");
    }
    assert!(out.contains("p { margin: 0; }"));
    assert!(out.contains("body { color: black; }"));
  }

  #[test]
  fn injects_before_uppercase_head_close() {
    let html = "<html><HEAD><title>Test</title></HEAD   ><body></body></html>";
    let css = "body { color: red; }";
    let injected = inject_css_into_html(html, css);
    assert_eq!(
      injected,
      "<html><HEAD><title>Test</title><style>body { color: red; }</style></HEAD   ><body></body></html>"
    );
  }

  #[test]
  fn injects_before_body_with_attributes() {
    let html = "<html><BODY class=\"main\" data-flag=\"1\">Content</BODY></html>";
    let css = "body { background: blue; }";
    let injected = inject_css_into_html(html, css);
    assert_eq!(
      injected,
      "<html><style>body { background: blue; }</style><BODY class=\"main\" data-flag=\"1\">Content</BODY></html>"
    );
  }

  #[test]
  fn inject_css_only_adds_style_block() {
    let html = "<!doctype html>\n<body>\n  <p>unchanged</p>\n</body>";
    let css = "p { margin: 0; }";
    let injected = inject_css_into_html(html, css);
    let style_tag = format!("<style>{css}</style>");
    assert_eq!(injected.matches(&style_tag).count(), 1);
    assert_eq!(injected.replace(&style_tag, ""), html);
  }

  #[test]
  fn inline_imports_handles_uppercase_at_keyword() {
    let mut state = InlineImportState::new();
    let css = "@IMPORT url(\"nested.css\");\nbody { color: black; }";
    let mut fetched = |url: &str| -> Result<String> {
      assert_eq!(url, "https://example.com/nested.css");
      Ok("p { color: blue; }".to_string())
    };
    let out = inline_imports(
      css,
      "https://example.com/main.css",
      &mut fetched,
      &mut state,
      None,
    )
    .unwrap();
    assert!(out.contains("p { color: blue; }"));
    assert!(out.contains("body { color: black; }"));
  }

  #[test]
  fn inline_imports_preserves_media_wrappers_for_duplicates() {
    let mut state = InlineImportState::new();
    let css = "@import url(\"shared.css\") screen;\n@import url(\"shared.css\") print;";
    let mut fetched = |_url: &str| -> Result<String> { Ok("p { color: green; }".to_string()) };
    let out = inline_imports(
      css,
      "https://example.com/main.css",
      &mut fetched,
      &mut state,
      None,
    )
    .unwrap();
    assert!(out.contains("@media screen"));
    assert!(out.contains("@media print"));
    assert_eq!(out.matches("p { color: green; }").count(), 2);
  }

  #[test]
  fn inline_imports_reports_cycles() {
    let mut state = InlineImportState::new();
    let mut fetched = |url: &str| -> Result<String> {
      if url.ends_with("a.css") {
        Ok("@import \"b.css\";\nbody { color: red; }".to_string())
      } else {
        Ok("@import \"a.css\";".to_string())
      }
    };
    let mut diags: Vec<(String, String)> = Vec::new();
    let mut record = |url: &str, reason: &str| {
      diags.push((url.to_string(), reason.to_string()));
    };
    let out = inline_imports_with_diagnostics(
      "@import \"a.css\";",
      "https://example.com/root.css",
      &mut fetched,
      &mut state,
      &mut record,
      None,
    )
    .unwrap();
    assert!(out.contains("body { color: red; }"));
    assert!(diags.iter().any(|(_, reason)| reason.contains("cyclic")));
  }

  #[test]
  fn extracts_stylesheet_hrefs_with_resolution() {
    let html = r#"
            <link rel="stylesheet" href="../styles/a.css">
            <link rel="alternate stylesheet" href="b.css">
            <link rel="icon" href="favicon.ico">
        "#;
    let urls = extract_css_links(
      html,
      "https://example.com/app/index.html",
      MediaType::Screen,
    )
    .unwrap();
    assert_eq!(urls.len(), 2);
    assert!(urls.contains(&"https://example.com/styles/a.css".to_string()));
    assert!(urls.contains(&"https://example.com/app/b.css".to_string()));
  }

  #[test]
  fn extracts_unquoted_stylesheet_hrefs() {
    let html = r#"
            <link rel=stylesheet href=/styles/a.css media=screen>
            <link rel=stylesheet href=/styles/print.css media=print>
            <link rel=stylesheet href=/styles/b.css media=all>
        "#;
    let urls =
      extract_css_links(html, "https://example.com/app/page.html", MediaType::Screen).unwrap();
    assert_eq!(
      urls,
      vec![
        "https://example.com/styles/a.css".to_string(),
        "https://example.com/styles/b.css".to_string(),
      ]
    );
  }

  #[test]
  fn ignores_non_stylesheet_links_even_with_stylesheet_in_hrefs() {
    let html = r#"
            <link rel=icon href="/foo/stylesheet-logo.png">
            <link rel=icon data-kind="stylesheet icon">
        "#;
    let urls =
      extract_css_links(html, "https://example.com/app/page.html", MediaType::Screen).unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn extracts_alternate_stylesheet_hrefs() {
    let html = r#"
            <link rel="alternate stylesheet" href="/styles/alt.css">
        "#;
    let urls =
      extract_css_links(html, "https://example.com/app/page.html", MediaType::Screen).unwrap();
    assert_eq!(urls, vec!["https://example.com/styles/alt.css".to_string()]);
  }

  #[test]
  fn alternate_stylesheets_can_be_disabled() {
    let html = r#"
            <link rel="alternate stylesheet" href="/styles/alt.css">
        "#;
    let toggles = RuntimeToggles::from_map(HashMap::from([(
      "FASTR_FETCH_ALTERNATE_STYLESHEETS".to_string(),
      "0".to_string(),
    )]));
    let urls = runtime::with_runtime_toggles(Arc::new(toggles), || {
      extract_css_links(html, "https://example.com/app/page.html", MediaType::Screen).unwrap()
    });
    assert!(urls.is_empty());
  }

  #[test]
  fn unescapes_js_escaped_stylesheet_hrefs() {
    let html = r#"
            <link rel="stylesheet" href="https://cdn.example.com/app.css?foo=bar\u0026baz=qux">
        "#;
    let urls = extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap();
    assert_eq!(
      urls,
      vec!["https://cdn.example.com/app.css?foo=bar&baz=qux".to_string()]
    );
  }

  #[test]
  fn detects_embedded_css_urls() {
    let html = r#"
            <script>var cssUrl="assets/site.css?v=1";</script>
            <style>@import url("/shared/base.css");</style>
        "#;
    let urls = extract_embedded_css_urls(html, "https://example.com/app/").unwrap();
    assert!(urls.contains(&"https://example.com/app/assets/site.css?v=1".to_string()));
    assert!(urls.contains(&"https://example.com/shared/base.css".to_string()));
  }

  #[test]
  fn normalizes_escaped_embedded_css_urls() {
    let html = r#"
            <link rel="stylesheet" href="https://cdn.example.com/styles/main.css">
            <script>
                var url = "https://cdn.example.com/styles/main.css\\\"/\u003c";
            </script>
        "#;
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert_eq!(
      urls,
      vec!["https://cdn.example.com/styles/main.css".to_string()]
    );
  }

  #[test]
  fn decodes_html_entities_in_stylesheet_hrefs() {
    let html = r#"
            <link rel="stylesheet" href="https://&#47;&#47;cdn.example.com&#47;main.css">
            <link rel="stylesheet" href="https://&/#47;&#47;cdn.example.com&#47;other.css">
            <link rel="stylesheet" href="https:////cdn.example.com////more.css">
        "#;
    let urls = extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap();
    assert_eq!(
      urls,
      vec![
        "https://cdn.example.com/main.css".to_string(),
        "https://cdn.example.com/other.css".to_string(),
        "https://cdn.example.com/more.css".to_string(),
      ]
    );
  }

  #[test]
  fn resolves_scheme_relative_urls() {
    let html = r#"
            <link rel="stylesheet" href="//cdn.example.com/app.css">
        "#;
    let urls = extract_css_links(html, "https://example.com/page", MediaType::Screen).unwrap();
    assert_eq!(urls, vec!["https://cdn.example.com/app.css".to_string()]);

    let resolved = resolve_href("https://example.com/page", "//cdn.example.com/app.css");
    assert_eq!(
      resolved,
      Some("https://cdn.example.com/app.css".to_string())
    );
  }

  #[test]
  fn skips_print_only_stylesheets() {
    let html = r#"
            <link rel="stylesheet" media="print" href="https://cdn.example.com/print.css">
            <link rel="stylesheet" media="print, screen" href="https://cdn.example.com/both.css">
            <link rel="stylesheet" media="screen" href="https://cdn.example.com/screen.css">
        "#;
    let urls = extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap();
    assert_eq!(
      urls,
      vec![
        "https://cdn.example.com/both.css".to_string(),
        "https://cdn.example.com/screen.css".to_string(),
      ]
    );
  }

  #[test]
  fn unescapes_json_style_embedded_urls() {
    let html = r#"
            <script>
                window.css = "https:\\/\\/cdn.example.com\\/app.css\\"";
            </script>
        "#;
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert_eq!(urls, vec!["https://cdn.example.com/app.css".to_string()]);
  }

  #[test]
  fn ignores_sourceurl_comments_in_embedded_css_scan() {
    let html = r"
            <style>
            /*# sourceURL=https://example.com/wp-includes/blocks/button/style.min.css */
            body { color: black; }
            </style>
        ";
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn unescapes_js_escaped_embedded_css_urls() {
    let html = r#"
            <script>
                const css = "https://cdn.example.com/app.css?foo=bar\\u0026baz=qux";
            </script>
        "#;
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert_eq!(
      urls,
      vec!["https://cdn.example.com/app.css?foo=bar&baz=qux".to_string()]
    );
  }

  #[test]
  fn embedded_scan_skips_print_only_link_tags() {
    let html = r#"
            <link rel="stylesheet" media="print" href="https://cdn.example.com/print.css">
        "#;
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn decodes_html_entities_in_embedded_css_urls() {
    let html = r#"
            <script>
                const css = "https://&/#47;&#47;cdn.example.com&#47;main.css";
                const other = "https:////cdn.example.com////more.css";
            </script>
        "#;
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert_eq!(
      urls,
      vec![
        "https://cdn.example.com/main.css".to_string(),
        "https://cdn.example.com/more.css".to_string()
      ]
    );
  }

  #[test]
  fn strips_sourceurl_prefix_in_embedded_css_urls() {
    let html = r"
            <script>
                /* sourceURL=https://example.com/assets/style.css */
            </script>
        ";
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert_eq!(
      urls,
      vec!["https://example.com/assets/style.css".to_string()]
    );
  }

  #[test]
  fn includes_preload_style_links() {
    let html = r#"
            <link rel=preload as=style href=/a.css>
        "#;
    let toggles = RuntimeToggles::from_map(HashMap::from([(
      "FASTR_FETCH_PRELOAD_STYLESHEETS".to_string(),
      "1".to_string(),
    )]));
    let urls = runtime::with_runtime_toggles(Arc::new(toggles), || {
      extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap()
    });
    assert_eq!(urls, vec!["https://example.com/a.css".to_string()]);
  }

  #[test]
  fn ignores_non_style_preloads() {
    let html = r#"
            <link rel=preload as=font href=/a.css>
        "#;
    let urls = extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn preload_style_links_can_be_disabled() {
    let html = r#"
            <link rel=preload as=style href=/a.css>
        "#;
    let toggles = RuntimeToggles::from_map(HashMap::from([(
      "FASTR_FETCH_PRELOAD_STYLESHEETS".to_string(),
      "0".to_string(),
    )]));
    let urls = runtime::with_runtime_toggles(Arc::new(toggles), || {
      extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap()
    });
    assert!(urls.is_empty());
  }

  #[test]
  fn modulepreload_style_links_are_opt_in() {
    let html = r#"
            <link rel=modulepreload as=style href=/a.css>
        "#;
    let disabled = RuntimeToggles::from_map(HashMap::from([(
      "FASTR_FETCH_MODULEPRELOAD_STYLESHEETS".to_string(),
      "0".to_string(),
    )]));
    let default_urls = runtime::with_runtime_toggles(Arc::new(disabled), || {
      extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap()
    });
    assert!(default_urls.is_empty());

    let enabled = RuntimeToggles::from_map(HashMap::from([(
      "FASTR_FETCH_MODULEPRELOAD_STYLESHEETS".to_string(),
      "1".to_string(),
    )]));
    let enabled_urls = runtime::with_runtime_toggles(Arc::new(enabled), || {
      extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap()
    });
    assert_eq!(enabled_urls, vec!["https://example.com/a.css".to_string()]);
  }

  #[test]
  fn dedupes_stylesheet_links_preserving_order() {
    let html = r#"
            <link rel="stylesheet" href="/a.css">
            <link rel="stylesheet" href="/b.css">
            <link rel="stylesheet" href="/a.css">
        "#;
    let urls = extract_css_links(
      html,
      "https://example.com/app/index.html",
      MediaType::Screen,
    )
    .unwrap();
    assert_eq!(
      urls,
      vec![
        "https://example.com/a.css".to_string(),
        "https://example.com/b.css".to_string(),
      ]
    );
  }

  #[test]
  fn dedupes_preload_and_stylesheet_preserving_order() {
    let html = r#"
            <link rel=preload as=style href="/a.css">
            <link rel="stylesheet" href="/a.css">
            <link rel="stylesheet" href="/b.css">
        "#;
    let urls = extract_css_links(
      html,
      "https://example.com/app/index.html",
      MediaType::Screen,
    )
    .unwrap();
    assert_eq!(
      urls,
      vec![
        "https://example.com/a.css".to_string(),
        "https://example.com/b.css".to_string(),
      ]
    );
  }

  #[test]
  fn ignores_embedded_css_class_tokens() {
    let html = r"
            <style>
                .css-v2kfba{height:100%;width:100%;}
            </style>
            <script>
                const cls = '.css-15ru6p1{font-size:inherit;font-weight:normal;}'
            </script>
        ";
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn ignores_percent_encoded_css_class_tokens() {
    let html = r#"
            <script>
                const bogus = ">%3E.css-v2kfba%7Bheight:100%;width:100%;%7D%3C/style";
            </script>
        "#;
    let urls = extract_embedded_css_urls(html, "https://example.com/").unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn does_not_fall_back_to_print_styles_when_no_screen_stylesheets() {
    let html = r#"
            <link rel="stylesheet" media="print" href="/print.css">
        "#;
    let urls = extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn does_not_fall_back_to_unquoted_print_stylesheets() {
    let html = r#"
            <link rel=stylesheet media=print href=/print.css>
        "#;
    let urls = extract_css_links(html, "https://example.com/", MediaType::Screen).unwrap();
    assert!(urls.is_empty());
  }

  #[test]
  fn infers_base_from_cached_file() {
    let html = "<html><head></head></html>";
    let base = infer_base_url(html, "file:///tmp/fetches/html/news.ycombinator.com.html");
    assert_eq!(base, "https://news.ycombinator.com/");
  }

  #[test]
  fn canonicalizes_relative_file_url_before_inference() {
    let html = "<html><head></head></html>";
    let tmp = tempfile::tempdir().unwrap();
    let prev_cwd = std::env::current_dir().unwrap();
    std::env::set_current_dir(tmp.path()).unwrap();

    let rel = "fetches/html/news.ycombinator.com.html";
    let rel_path = std::path::Path::new(rel);
    if let Some(parent) = rel_path.parent() {
      std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(rel_path, html).unwrap();

    let abs = rel_path.canonicalize().unwrap();
    let rel_url = format!("file://{}", rel);
    let inferred = infer_base_url(html, &rel_url);
    // When the file exists locally, we still expect the HTTPS origin guess.
    assert_eq!(inferred, "https://news.ycombinator.com/");
    // And the canonicalized file URL was at least parseable (implicit by no panic).
    assert!(abs.exists());

    std::env::set_current_dir(prev_cwd).unwrap();
  }

  #[test]
  fn prefers_document_url_over_canonical_for_http_inputs() {
    let html = r#"
            <link rel=canonical href="https://example.com/">
        "#;
    let base = infer_base_url(html, "https://example.com/path/page.html");
    assert_eq!(base, "https://example.com/path/page.html");

    let resolved = resolve_href(&base, "../styles/app.css").expect("resolved");
    assert_eq!(resolved, "https://example.com/styles/app.css");
  }

  #[test]
  fn uses_canonical_hint_for_file_inputs() {
    let html = r#"
            <link rel="canonical" href="https://example.net/app/">
        "#;
    let base = infer_base_url(html, "file:///tmp/cache/example.net.html");
    assert_eq!(base, "https://example.net/app/");
  }

  #[test]
  fn uses_single_quoted_canonical_for_file_inputs() {
    let html = r#"
            <link rel='canonical' href='https://example.net/single/'>
        "#;
    let base = infer_base_url(html, "file:///tmp/cache/example.net.single.html");
    assert_eq!(base, "https://example.net/single/");
  }

  #[test]
  fn uses_unquoted_canonical_for_file_inputs() {
    let html = r#"
            <link rel=canonical href="https://example.net/unquoted/">
        "#;
    let base = infer_base_url(html, "file:///tmp/cache/example.net.unquoted.html");
    assert_eq!(base, "https://example.net/unquoted/");
  }

  #[test]
  fn uses_single_quoted_og_url_for_file_inputs() {
    let html = r#"
            <meta property='og:url' content='https://example.org/from-og/'>
        "#;
    let base = infer_base_url(html, "file:///tmp/cache/example.org.from-og.html");
    assert_eq!(base, "https://example.org/from-og/");
  }

  #[test]
  fn uses_unquoted_og_url_for_file_inputs() {
    let html = r#"
            <meta property=og:url content="https://example.org/unquoted-og/">
        "#;
    let base = infer_base_url(html, "file:///tmp/cache/example.org.unquoted.html");
    assert_eq!(base, "https://example.org/unquoted-og/");
  }

  #[test]
  fn unescape_js_handles_slashes_and_quotes() {
    let input = r#"https:\/\/example.com\/path\"quoted\'"#;
    let unescaped = unescape_js_escapes(input);
    assert_eq!(unescaped, "https://example.com/path\"quoted\'");
  }

  #[test]
  fn unescape_js_handles_unicode_escapes() {
    let input = r"foo\u0026bar\U0041baz"; // & and 'A'
    let unescaped = unescape_js_escapes(input);
    assert_eq!(unescaped, "foo&barAbaz");
  }

  #[test]
  fn unescape_js_borrows_when_unescaped() {
    use std::borrow::Cow;

    let input = "https://example.com/path";
    let out = unescape_js_escapes(input);
    match out {
      Cow::Borrowed(s) => assert_eq!(s, input),
      Cow::Owned(_) => panic!("expected borrowed output for unescaped input"),
    }
  }

  #[test]
  fn resolve_href_unescapes_js_escapes() {
    let base = "https://example.com/";
    let href = r"https:\/\/cdn.example.com\/styles\/main.css";
    let resolved = resolve_href(base, href).expect("resolved href");
    assert_eq!(resolved, "https://cdn.example.com/styles/main.css");
  }

  #[test]
  fn resolve_href_preserves_data_urls() {
    let base = "https://example.com/";
    let href = "data:text/css,body%7Bcolor%3Ared%7D";
    let resolved = resolve_href(base, href).expect("resolved href");
    assert_eq!(resolved, href);
  }

  #[test]
  fn resolve_href_with_file_base_directory() {
    let dir = tempfile::tempdir().unwrap();
    let base = format!("file://{}", dir.path().display());
    let resolved = resolve_href(&base, "styles/app.css").expect("resolved file href");
    assert_eq!(
      resolved,
      format!("file://{}/styles/app.css", dir.path().display())
    );
  }

  #[test]
  fn resolve_href_with_file_base_file_parent() {
    let dir = tempfile::tempdir().unwrap();
    let base = format!("file://{}/html/page.html", dir.path().display());
    std::fs::create_dir_all(dir.path().join("html")).unwrap();
    let resolved = resolve_href(&base, "../styles/app.css").expect("resolved file href");
    assert_eq!(
      resolved,
      format!("file://{}/styles/app.css", dir.path().display())
    );
  }

  #[test]
  fn resolve_href_rejects_non_parseable_base() {
    // Base that cannot be parsed as URL or file path should yield None
    let base = "not-a-url";
    assert_eq!(resolve_href(base, "styles/app.css"), None);
  }

  #[test]
  fn resolve_href_rejects_script_and_mailto_schemes() {
    let base = "https://example.com/";
    assert_eq!(resolve_href(base, "javascript:alert(1)"), None);
    assert_eq!(resolve_href(base, "mailto:test@example.com"), None);
    assert_eq!(resolve_href(base, "vbscript:msgbox('hi')"), None);

    // Schemes are matched case-insensitively.
    assert_eq!(resolve_href(base, "JaVaScRiPt:alert(1)"), None);
    assert_eq!(resolve_href(base, "MAILTO:UPPER@EXAMPLE.COM"), None);
    assert_eq!(resolve_href(base, "VbScRiPt:msgbox('hi')"), None);
  }

  #[test]
  fn resolve_href_ignores_fragment_only_hrefs() {
    let base = "https://example.com/";
    assert_eq!(resolve_href(base, "#section"), None);
    assert_eq!(resolve_href(base, "#"), None);
  }

  #[test]
  fn resolve_href_trims_whitespace() {
    let base = "https://example.com/";
    let resolved = resolve_href(base, "   ./foo.css \n").expect("resolved href");
    assert_eq!(resolved, "https://example.com/foo.css");
  }

  #[test]
  fn resolve_href_rejects_whitespace_only() {
    let base = "https://example.com/";
    assert_eq!(resolve_href(base, "   \t\n"), None);
  }

  #[test]
  fn decode_html_entities_decodes_known_and_preserves_unknown() {
    let input = "&amp;&lt;&gt;&quot;&apos;&#65;&#x41;&copy;";
    let decoded = decode_html_entities(input);
    assert_eq!(decoded, "&<>\"'AA&copy;");
  }

  #[test]
  fn normalize_scheme_slashes_collapses_extra_slashes() {
    let input = "https:////example.com//foo//bar";
    let normalized = normalize_scheme_slashes(input);
    assert_eq!(normalized, "https://example.com/foo/bar");
  }

  #[test]
  fn normalize_scheme_slashes_preserves_scheme_relative() {
    let input = "//cdn.example.com//assets//img.png";
    let normalized = normalize_scheme_slashes(input);
    assert_eq!(normalized, input);
  }

  #[test]
  fn normalize_scheme_slashes_preserves_embedded_scheme_in_query() {
    let input = "https://example.com/?u=https://cdn.com/x//y";
    let normalized = normalize_scheme_slashes(input);
    assert_eq!(normalized, input);
  }

  #[test]
  fn normalize_scheme_slashes_only_touches_path_before_query_and_fragment() {
    let input =
      "https:////cdn.example.com////more.css?redirect=https://cdn.example.com//next#hash=https://foo.bar//baz";
    let normalized = normalize_scheme_slashes(input);
    let expected =
      "https://cdn.example.com/more.css?redirect=https://cdn.example.com//next#hash=https://foo.bar//baz";
    assert_eq!(normalized, expected);
  }

  #[test]
  fn resolve_href_returns_none_for_empty_href() {
    let base = "https://example.com/";
    assert_eq!(resolve_href(base, ""), None);
  }

  #[test]
  fn unescape_js_preserves_invalid_sequences() {
    let input = r"bad\u00zzescape and \q";
    let unescaped = unescape_js_escapes(input);
    assert_eq!(unescaped, input);
  }
}
