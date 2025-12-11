#![allow(clippy::io_other_error)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::len_zero)]

use fastrender::{Error, FastRender, Result};
use std::env;
use std::path::Path;
use url::Url;

fn fetch_url(url: &str) -> Result<String> {
    // Handle file:// URLs
    if url.starts_with("file://") {
        let path = url.strip_prefix("file://").unwrap();
        return std::fs::read_to_string(path).map_err(|e| Error::Io(e));
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

    let body = response
        .body_mut()
        .read_to_string()
        .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

    Ok(body)
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
    let html = fetch_url(url)?;

    println!("Extracting CSS links...");
    let css_links = extract_css_links(&html, url);

    println!("Found {} CSS link(s)", css_links.len());

    let mut combined_css = String::new();
    for css_url in css_links {
        println!("Fetching CSS from: {}", css_url);
        match fetch_url(&css_url) {
            Ok(css) => {
                let rewritten = absolutize_css_urls(&css, &css_url);
                combined_css.push_str(&rewritten);
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
