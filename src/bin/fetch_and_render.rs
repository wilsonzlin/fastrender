#![allow(clippy::io_other_error)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::len_zero)]

use fastrender::{Error, FastRender, Result};
use fastrender::css::encoding::decode_css_bytes;
use encoding_rs::{Encoding, UTF_8};
use std::env;
use std::collections::HashSet;
use std::path::Path;
use url::Url;

fn fetch_bytes(url: &str) -> Result<(Vec<u8>, Option<String>)> {
    // Handle file:// URLs
    if url.starts_with("file://") {
        let path = url.strip_prefix("file://").unwrap();
        let bytes = std::fs::read(path).map_err(Error::Io)?;
        return Ok((bytes, None));
    }

    // Configure agent with timeout for ureq 3.x
    let config = ureq::Agent::config_builder()
        .timeout_global(Some(std::time::Duration::from_secs(30)))
        .build();
    let agent: ureq::Agent = config.into();

    let mut response = agent
        .get(url)
        .call()
        .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

    let content_type = response
        .headers()
        .get("content-type")
        .and_then(|h| h.to_str().ok())
        .map(|s| s.to_string());

    let bytes = response
        .body_mut()
        .read_to_vec()
        .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

    Ok((bytes, content_type))
}

fn decode_html_bytes(bytes: &[u8], content_type: Option<&str>) -> String {
    if bytes.is_empty() {
        return String::new();
    }

    if let Some((enc, bom_len)) = Encoding::for_bom(bytes) {
        return enc.decode_without_bom_handling(&bytes[bom_len..]).0.into_owned();
    }

    if let Some(label) = content_type.and_then(charset_from_content_type) {
        if let Some(enc) = Encoding::for_label(label.as_bytes()) {
            return enc.decode_with_bom_removal(bytes).0.into_owned();
        }
    }

    UTF_8.decode_with_bom_removal(bytes).0.into_owned()
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

fn resolve_href(base: &str, href: &str) -> Option<String> {
    if href.is_empty() {
        return None;
    }

    if href.starts_with("data:") {
        return Some(href.to_string());
    }

    if let Ok(abs) = Url::parse(href) {
        return Some(abs.to_string());
    }

    let mut base_candidate = base.to_string();
    if base_candidate.starts_with("file://") {
        let path = &base_candidate["file://".len()..];
        if Path::new(path).is_dir() && !base_candidate.ends_with('/') {
            base_candidate.push('/');
        }
    }

    let base_url = Url::parse(&base_candidate)
        .or_else(|_| Url::from_file_path(&base_candidate).map_err(|_| url::ParseError::RelativeUrlWithoutBase))
        .ok()?;

    base_url.join(href).ok().map(|u| u.to_string())
}

/// Rewrite url(...) references in a CSS string to be absolute using the stylesheet's base URL.
fn absolutize_css_urls(css: &str, base_url: &str) -> String {
    #[derive(PartialEq)]
    enum State {
        Normal,
        SingleString,
        DoubleString,
        Comment,
    }

    let mut out = String::with_capacity(css.len());
    let mut state = State::Normal;
    let bytes = css.as_bytes();
    let mut i = 0usize;
    let mut last_emit = 0usize;

    while i < bytes.len() {
        match state {
            State::Normal => {
                // Start of comment
                if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    i += 2;
                    state = State::Comment;
                    continue;
                }
                // Strings
                if bytes[i] == b'\'' {
                    i += 1;
                    state = State::SingleString;
                    continue;
                }
                if bytes[i] == b'"' {
                    i += 1;
                    state = State::DoubleString;
                    continue;
                }

                // Check for url(
                if (bytes[i] == b'u' || bytes[i] == b'U')
                    && i + 3 < bytes.len()
                    && bytes[i + 1..].len() >= 3
                    && bytes[i + 1].eq_ignore_ascii_case(&b'r')
                    && bytes[i + 2].eq_ignore_ascii_case(&b'l')
                {
                    let mut j = i + 3;
                    while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                        j += 1;
                    }
                    if j < bytes.len() && bytes[j] == b'(' {
                        j += 1;
                        // Skip leading whitespace inside url(
                        while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                            j += 1;
                        }
                        let content_start = j;
                        let mut in_single = false;
                        let mut in_double = false;
                        while j < bytes.len() {
                            let b = bytes[j];
                            if !in_single && !in_double && b == b')' {
                                break;
                            }
                            match b {
                                b'\\' => {
                                    // Skip escaped next char
                                    j += 1;
                                }
                                b'\'' if !in_double => in_single = !in_single,
                                b'"' if !in_single => in_double = !in_double,
                                _ => {}
                            }
                            j += 1;
                        }
                        if j < bytes.len() && bytes[j] == b')' {
                            let content_end = j;
                            // Emit preceding slice
                            out.push_str(&css[last_emit..i]);

                            let raw = css[content_start..content_end].trim();
                            let unquoted = raw
                                .trim_matches('"')
                                .trim_matches('\'')
                                .to_string();
                            if let Some(resolved) = resolve_href(base_url, &unquoted) {
                                out.push_str(&format!("url(\"{}\")", resolved));
                            } else {
                                // Fallback: leave original text
                                out.push_str(&css[i..=content_end]);
                            }
                            i = content_end + 1;
                            last_emit = i;
                            continue;
                        }
                    }
                }
                i += 1;
            }
            State::SingleString => {
                if bytes[i] == b'\\' {
                    i += 2;
                    continue;
                }
                if bytes[i] == b'\'' {
                    state = State::Normal;
                }
                i += 1;
            }
            State::DoubleString => {
                if bytes[i] == b'\\' {
                    i += 2;
                    continue;
                }
                if bytes[i] == b'"' {
                    state = State::Normal;
                }
                i += 1;
            }
            State::Comment => {
                if bytes[i] == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    i += 2;
                    state = State::Normal;
                } else {
                    i += 1;
                }
            }
        }
    }

    out.push_str(&css[last_emit..]);
    out
}

fn parse_import_target(rule: &str) -> Option<(String, String)> {
    let after_at = rule.strip_prefix("@import")?.trim_start();
    let (target, rest) = if let Some(inner) = after_at.strip_prefix("url(") {
        let close = inner.find(')')?;
        let url_part = &inner[..close].trim();
        let url_str = url_part.trim_matches(|c| c == '"' || c == '\'').to_string();
        let media = inner[close + 1..].trim().trim_end_matches(';').trim().to_string();
        (url_str, media)
    } else if after_at.starts_with('"') || after_at.starts_with('\'') {
        let quote = after_at.as_bytes()[0] as char;
        let mut iter = after_at[1..].char_indices();
        let mut end = None;
        while let Some((idx, ch)) = iter.next() {
            if ch == quote {
                end = Some(idx + 1);
                break;
            }
        }
        let end = end?;
        let url_str = after_at[1..end - 1].to_string();
        let media = after_at[end..].trim().trim_end_matches(';').trim().to_string();
        (url_str, media)
    } else {
        return None;
    };
    Some((target, rest))
}

fn inline_imports(
    css: &str,
    base_url: &str,
    fetch: &dyn Fn(&str) -> Result<String>,
    seen: &mut HashSet<String>,
) -> String {
    #[derive(PartialEq)]
    enum State {
        Normal,
        Single,
        Double,
        Comment,
    }

    let mut out = String::with_capacity(css.len());
    let mut state = State::Normal;
    let bytes = css.as_bytes();
    let mut i = 0usize;
    let mut last_emit = 0usize;

    while i < bytes.len() {
        match state {
            State::Normal => {
                if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    state = State::Comment;
                    i += 2;
                    continue;
                }
                if bytes[i] == b'\'' {
                    state = State::Single;
                    i += 1;
                    continue;
                }
                if bytes[i] == b'"' {
                    state = State::Double;
                    i += 1;
                    continue;
                }

                if bytes[i] == b'@' {
                    let remainder = &css[i..];
                    if remainder.len() >= 7
                        && remainder[1..].to_lowercase().starts_with("import")
                    {
                        // Find end of import statement (;)
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
                                out.push_str(&css[last_emit..i]);
                                if seen.insert(resolved.clone()) {
                                    if let Ok(fetched) = fetch(&resolved) {
                                        let rewritten = absolutize_css_urls(&fetched, &resolved);
                                        let inlined = inline_imports(&rewritten, &resolved, fetch, seen);
                                        if media.is_empty() || media.eq_ignore_ascii_case("all") {
                                            out.push_str(&inlined);
                                        } else {
                                            out.push_str(&format!("@media {} {{\n{}\n}}\n", media, inlined));
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
                    state = State::Normal;
                }
                i += 1;
            }
            State::Double => {
                if bytes[i] == b'\\' {
                    i += 2;
                    continue;
                }
                if bytes[i] == b'"' {
                    state = State::Normal;
                }
                i += 1;
            }
            State::Comment => {
                if bytes[i] == b'*' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    state = State::Normal;
                    i += 2;
                } else {
                    i += 1;
                }
            }
        }
    }

    out.push_str(&css[last_emit..]);
    out
}

fn extract_css_links(html: &str, base_url: &str) -> Vec<String> {
    let mut css_urls = Vec::new();

    // Simple regex-like extraction of <link> tags with CSS
    let lower = html.to_lowercase();
    let mut pos = 0;

    while let Some(link_start) = lower[pos..].find("<link") {
        let abs_start = pos + link_start;
        if let Some(link_end) = lower[abs_start..].find('>') {
            let link_tag = &html[abs_start..abs_start + link_end + 1];
            let link_tag_lower = link_tag.to_lowercase();

            // Check if it's a stylesheet
            if link_tag_lower.contains("stylesheet") {
                // Extract href
                if let Some(href_start) = link_tag_lower.find("href") {
                    let href_section = &link_tag[href_start..];
                    if let Some(quote_start) = href_section.find('"').or_else(|| href_section.find('\'')) {
                        let quote_char = href_section.chars().nth(quote_start).unwrap();
                        let after_quote = &href_section[quote_start + 1..];
                        if let Some(quote_end) = after_quote.find(quote_char) {
                            let href = &after_quote[..quote_end];
                            if let Some(full_url) = resolve_href(base_url, href) {
                                css_urls.push(full_url);
                            }
                        }
                    }
                }
            }

            pos = abs_start + link_end + 1;
        } else {
            break;
        }
    }

    css_urls
}

fn inject_css_into_html(html: &str, css: &str) -> String {
    // Find </head> or <body> and inject <style> tag before it
    let style_tag = format!("<style>{}</style>", css);

    if let Some(head_end) = html.find("</head>") {
        let mut result = String::with_capacity(html.len() + style_tag.len());
        result.push_str(&html[..head_end]);
        result.push_str(&style_tag);
        result.push_str(&html[head_end..]);
        result
    } else if let Some(body_start) = html.find("<body") {
        let mut result = String::with_capacity(html.len() + style_tag.len());
        result.push_str(&html[..body_start]);
        result.push_str(&style_tag);
        result.push_str(&html[body_start..]);
        result
    } else {
        // No head or body, just prepend
        format!("{}{}", style_tag, html)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn resolves_file_scheme_links() {
        let dir = tempfile::tempdir().expect("temp dir");
        let base = format!("file://{}", dir.path().join("page.html").display());
        let href = "css/app.css";
        let resolved = resolve_href(&base, href).expect("resolved");
        assert!(resolved.ends_with("/css/app.css"), "resolved={}", resolved);
    }

    #[test]
    fn absolutizes_css_urls_against_stylesheet() {
        let css = r#"
            body { background: url("../img/bg.png") no-repeat; }
            @font-face { src: url('fonts/font.woff2'); }
        "#;
        let base = "https://example.com/assets/css/main.css";
        let out = absolutize_css_urls(css, base);
        assert!(out.contains("url(\"https://example.com/assets/img/bg.png\")"));
        assert!(out.contains("url(\"https://example.com/assets/css/fonts/font.woff2\")"));
    }

    #[test]
    fn inlines_imports_with_media_wrapping() {
        fn fake_fetch(url: &str) -> Result<String> {
            match url {
                "https://example.com/css/imported.css" => Ok("h1 { color: red; }".to_string()),
                _ => Err(Error::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "not found",
                ))),
            }
        }
        let css = r#"@import url("https://example.com/css/imported.css") screen;"#;
        let mut seen = HashSet::new();
        let inlined = inline_imports(css, "https://example.com/css/main.css", &fake_fetch, &mut seen);
        assert!(inlined.contains("@media screen"), "expected media wrapper: {}", inlined);
        assert!(inlined.contains("color: red"));
    }

    #[test]
    fn extracts_stylesheet_hrefs_with_resolution() {
        let html = r#"
            <html><head>
                <link rel="stylesheet" href="../styles/a.css">
                <link rel="icon" href="/favicon.ico">
            </head><body></body></html>
        "#;
        let urls = extract_css_links(html, "https://example.com/site/page.html");
        assert_eq!(urls, vec!["https://example.com/styles/a.css".to_string()]);
    }

    #[test]
    fn decode_html_uses_content_type_charset() {
        let encoded = encoding_rs::SHIFT_JIS.encode("abcデ").0;
        let decoded = decode_html_bytes(&encoded, Some("text/html; charset=shift_jis"));
        assert!(decoded.contains('デ'), "decoded text should include kana: {}", decoded);
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <url> [output.png] [width] [height] [scroll_y]", args[0]);
        eprintln!("Example: {} https://www.example.com output.png 1200 800 0", args[0]);
        eprintln!("  width: viewport width (default: 1200)");
        eprintln!("  height: viewport height (default: 800)");
        eprintln!("  scroll_y: vertical scroll offset (default: 0)");
        std::process::exit(1);
    }

    let url = &args[1];
    let output = if args.len() >= 3 {
        args[2].clone()
    } else {
        "fetched_output.png".to_string()
    };

    let width = if args.len() >= 4 {
        args[3].parse::<u32>().unwrap_or(1200)
    } else {
        1200
    };

    let height = if args.len() >= 5 {
        args[4].parse::<u32>().unwrap_or(800)
    } else {
        800
    };

    let scroll_y = if args.len() >= 6 {
        args[5].parse::<u32>().unwrap_or(0)
    } else {
        0
    };

    println!("Fetching HTML from: {}", url);
    let (html_bytes, html_content_type) = fetch_bytes(url)?;
    let html = decode_html_bytes(&html_bytes, html_content_type.as_deref());

    println!("Extracting CSS links...");
    let css_links = extract_css_links(&html, url);

    println!("Found {} CSS link(s)", css_links.len());

    let mut combined_css = String::new();
    let mut seen_imports = HashSet::new();
    for css_url in css_links {
        println!("Fetching CSS from: {}", css_url);
        seen_imports.insert(css_url.clone());
        match fetch_bytes(&css_url) {
            Ok((bytes, content_type)) => {
                let css_text = decode_css_bytes(&bytes, content_type.as_deref());
                let rewritten = absolutize_css_urls(&css_text, &css_url);
                let inlined = inline_imports(&rewritten, &css_url, &|u| {
                    fetch_bytes(u).map(|(b, ct)| decode_css_bytes(&b, ct.as_deref()))
                }, &mut seen_imports);
                combined_css.push_str(&inlined);
                combined_css.push('\n');
            }
            Err(e) => {
                eprintln!("Warning: Failed to fetch CSS from {}: {}", css_url, e);
            }
        }
    }

    println!("Injecting CSS into HTML...");
    let html_with_css = if !combined_css.is_empty() {
        inject_css_into_html(&html, &combined_css)
    } else {
        html
    };

    println!("Rendering to image ({}x{} viewport)...", width, height);
    let mut renderer = FastRender::new()?;
    renderer.set_base_url(url.clone());
    // Note: scroll_y is currently not supported, will render from top
    if scroll_y != 0 {
        eprintln!("Warning: scroll_y parameter is not yet supported, rendering from top");
    }
    let png_data = renderer.render_to_png(&html_with_css, width, height)?;

    println!("Saving to {}...", output);
    std::fs::write(&output, png_data)?;

    println!("✓ Successfully rendered {} to {}", url, output);
    println!("  Image size: {} bytes", std::fs::metadata(&output)?.len());

    Ok(())
}
