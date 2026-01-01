//! Best-effort asset discovery helpers shared by CLI tooling binaries.
//!
//! These helpers intentionally do not attempt full HTML/CSS parsing; they aim to be
//! deterministic and "good enough" for offline bundling and cache warming workflows.

use fastrender::css::loader::resolve_href;
use regex::Regex;

/// Extract inline CSS-like text from an HTML string.
///
/// Captures:
/// - `<style> ... </style>` blocks
/// - `style="..."` attributes
/// - `style='...'` attributes
pub fn extract_inline_css_chunks(html: &str) -> Vec<String> {
  fn capture_group(regex: &Regex, input: &str) -> Vec<String> {
    regex
      .captures_iter(input)
      .filter_map(|caps| caps.get(1).map(|m| m.as_str().to_string()))
      .collect()
  }

  // Best-effort: this is not a full HTML parser, but is sufficient for capturing deterministic
  // offline bundles from the common authoring patterns.
  let style_tag = Regex::new("(?is)<style[^>]*>(.*?)</style>").expect("style tag regex");
  let style_attr_double =
    Regex::new("(?is)\\bstyle\\s*=\\s*\"([^\"]*)\"").expect("style attr double regex");
  let style_attr_single =
    Regex::new("(?is)\\bstyle\\s*=\\s*'([^']*)'").expect("style attr single regex");

  let mut out = Vec::new();
  out.extend(capture_group(&style_tag, html));
  out.extend(capture_group(&style_attr_double, html));
  out.extend(capture_group(&style_attr_single, html));
  out
}

/// Discover image-like URLs referenced by HTML tags.
///
/// Looks for:
/// - `<img src=...>`
/// - `<img srcset=...>`
/// - `<source srcset=...>`
///
/// URLs are resolved against `base_url` using [`resolve_href`].
pub fn discover_html_image_urls(html: &str, base_url: &str) -> Vec<String> {
  let img_src = Regex::new("(?is)<img[^>]*\\bsrc\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)'|([^\\s>]+))")
    .expect("img src regex");
  let img_srcset = Regex::new("(?is)<img[^>]*\\bsrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')")
    .expect("img srcset regex");
  let source_srcset = Regex::new("(?is)<source[^>]*\\bsrcset\\s*=\\s*(?:\"([^\"]*)\"|'([^']*)')")
    .expect("source srcset regex");

  let mut urls = Vec::new();
  for caps in img_src.captures_iter(html) {
    let raw = caps
      .get(1)
      .or_else(|| caps.get(2))
      .or_else(|| caps.get(3))
      .map(|m| m.as_str())
      .unwrap_or("");
    if let Some(resolved) = resolve_href(base_url, raw) {
      urls.push(resolved);
    }
  }

  const MAX_SRCSET_CANDIDATES: usize = 16;
  for caps in img_srcset.captures_iter(html) {
    let raw_srcset = caps
      .get(1)
      .or_else(|| caps.get(2))
      .map(|m| m.as_str())
      .unwrap_or("");
    for candidate in parse_srcset_urls(raw_srcset, MAX_SRCSET_CANDIDATES) {
      if let Some(resolved) = resolve_href(base_url, &candidate) {
        urls.push(resolved);
      }
    }
  }

  for caps in source_srcset.captures_iter(html) {
    let raw_srcset = caps
      .get(1)
      .or_else(|| caps.get(2))
      .map(|m| m.as_str())
      .unwrap_or("");
    for candidate in parse_srcset_urls(raw_srcset, MAX_SRCSET_CANDIDATES) {
      if let Some(resolved) = resolve_href(base_url, &candidate) {
        urls.push(resolved);
      }
    }
  }

  urls
}

/// Parse a `srcset` attribute into a list of URL strings (without descriptors).
///
/// This is intentionally permissive: we split on commas and then take the first
/// whitespace-delimited token of each candidate.
pub fn parse_srcset_urls(srcset: &str, max_candidates: usize) -> Vec<String> {
  let mut out = Vec::new();
  for candidate in srcset.split(',') {
    if out.len() >= max_candidates {
      break;
    }
    let trimmed = candidate.trim();
    if trimmed.is_empty() {
      continue;
    }
    let url_part = trimmed.split_whitespace().next().unwrap_or("").trim();
    if url_part.is_empty() {
      continue;
    }
    out.push(url_part.to_string());
  }
  out
}

/// Discover URLs referenced inside a CSS string.
///
/// This walks `cssparser` tokens (including nested blocks) and collects:
/// - `url(...)` / `url` tokens
/// - `@import` targets
///
/// URLs are resolved against `base_url` using [`resolve_href`].
pub fn discover_css_urls(css: &str, base_url: &str) -> Vec<String> {
  use cssparser::{Parser, ParserInput, Token};

  fn record(out: &mut Vec<String>, base_url: &str, raw: &str) {
    if let Some(resolved) = resolve_href(base_url, raw) {
      out.push(resolved);
    }
  }

  fn scan<'i, 't>(parser: &mut Parser<'i, 't>, base_url: &str, out: &mut Vec<String>) {
    while !parser.is_exhausted() {
      let token = match parser.next_including_whitespace_and_comments() {
        Ok(t) => t,
        Err(_) => break,
      };

      match token {
        Token::UnquotedUrl(url) => record(out, base_url, url.as_ref()),
        Token::Function(name) if name.eq_ignore_ascii_case("url") => {
          let parse_result = parser.parse_nested_block(|nested| {
            let mut arg: Option<String> = None;
            while !nested.is_exhausted() {
              match nested.next_including_whitespace_and_comments() {
                Ok(Token::WhiteSpace(_)) | Ok(Token::Comment(_)) => {}
                Ok(Token::QuotedString(s)) | Ok(Token::UnquotedUrl(s)) => {
                  arg = Some(s.as_ref().to_string());
                  break;
                }
                Ok(Token::Ident(s)) => {
                  arg = Some(s.as_ref().to_string());
                  break;
                }
                Ok(Token::BadUrl(_)) | Err(_) => break,
                Ok(_) => {}
              }
            }
            Ok::<_, cssparser::ParseError<'i, ()>>(arg)
          });

          if let Ok(Some(arg)) = parse_result {
            record(out, base_url, &arg);
          }
        }
        Token::AtKeyword(name) if name.eq_ignore_ascii_case("import") => {
          let mut target: Option<String> = None;
          while !parser.is_exhausted() {
            let next = match parser.next_including_whitespace_and_comments() {
              Ok(t) => t,
              Err(_) => break,
            };
            match next {
              Token::WhiteSpace(_) | Token::Comment(_) => continue,
              Token::QuotedString(s) | Token::UnquotedUrl(s) => {
                target = Some(s.as_ref().to_string());
                break;
              }
              Token::Function(fname) if fname.eq_ignore_ascii_case("url") => {
                let parse_result = parser.parse_nested_block(|nested| {
                  let mut arg: Option<String> = None;
                  while !nested.is_exhausted() {
                    match nested.next_including_whitespace_and_comments() {
                      Ok(Token::WhiteSpace(_)) | Ok(Token::Comment(_)) => {}
                      Ok(Token::QuotedString(s)) | Ok(Token::UnquotedUrl(s)) => {
                        arg = Some(s.as_ref().to_string());
                        break;
                      }
                      Ok(Token::Ident(s)) => {
                        arg = Some(s.as_ref().to_string());
                        break;
                      }
                      Ok(Token::BadUrl(_)) | Err(_) => break,
                      Ok(_) => {}
                    }
                  }
                  Ok::<_, cssparser::ParseError<'i, ()>>(arg)
                });
                target = parse_result.ok().flatten();
                break;
              }
              Token::Ident(s) => {
                target = Some(s.as_ref().to_string());
                break;
              }
              Token::Semicolon => break,
              _ => break,
            }
          }
          if let Some(target) = target {
            record(out, base_url, &target);
          }
        }
        Token::Function(_)
        | Token::ParenthesisBlock
        | Token::SquareBracketBlock
        | Token::CurlyBracketBlock => {
          let _ = parser.parse_nested_block(|nested| {
            scan(nested, base_url, out);
            Ok::<_, cssparser::ParseError<'i, ()>>(())
          });
        }
        _ => {}
      }
    }
  }

  let mut out = Vec::new();
  let mut input = ParserInput::new(css);
  let mut parser = Parser::new(&mut input);
  scan(&mut parser, base_url, &mut out);
  out
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn extracts_style_tag_and_attributes() {
    let html = r#"
      <html>
        <head>
          <style>body { background: url('/bg.png'); }</style>
        </head>
        <body style="background-image: url(/inline.png)">
          <div style='color: red; background: url("nested.png")'></div>
        </body>
      </html>
    "#;

    let chunks = extract_inline_css_chunks(html);
    assert_eq!(chunks.len(), 3);
    assert!(chunks[0].contains("body { background"));
    assert!(chunks[1].contains("background-image"));
    assert!(chunks[2].contains("nested.png"));
  }

  #[test]
  fn parses_srcset_urls_with_limit() {
    let srcset = "a.png 1x, b.png 2x, c.png 3x";
    assert_eq!(parse_srcset_urls(srcset, 2), vec!["a.png", "b.png"]);
  }

  #[test]
  fn discovers_html_image_urls_and_resolves_against_base() {
    let base = "https://example.com/a/b/page.html";
    let html = r#"
      <img src="img.png">
      <img srcset="a.png 1x, /b.png 2x">
      <picture>
        <source srcset="c.png 1x, d.png 2x">
      </picture>
    "#;

    let mut urls = discover_html_image_urls(html, base);
    urls.sort();
    assert_eq!(
      urls,
      vec![
        "https://example.com/a/b/a.png",
        "https://example.com/a/b/c.png",
        "https://example.com/a/b/d.png",
        "https://example.com/a/b/img.png",
        "https://example.com/b.png",
      ]
    );
  }

  #[test]
  fn discovers_css_urls_including_nested_blocks() {
    let base = "https://example.com/styles/main.css";
    let css = r#"
      @import "other.css";
      body { background-image: url("bg.png"); }
      @media screen {
        .icon { background: url(/icons/icon.svg#hash); }
      }
    "#;

    let mut urls = discover_css_urls(css, base);
    urls.sort();
    assert_eq!(
      urls,
      vec![
        "https://example.com/icons/icon.svg#hash",
        "https://example.com/styles/bg.png",
        "https://example.com/styles/other.css",
      ]
    );
  }
}
