//! Fetch a single page and render it to an image.
//!
//! Usage: fetch_and_render [--timeout SECONDS] [--dpr FLOAT] <url> [output.png] [width] [height] [scroll_x] [scroll_y]
//!
//! Examples:
//!   fetch_and_render --timeout 120 --dpr 2.0 https://www.example.com output.png 1200 800 0 0
//!
//! Options:
//!   --timeout SECONDS   Per-page timeout (default: 0 = no timeout)
//!   --dpr FLOAT         Device pixel ratio for media queries/srcset (default: 1.0)

#![allow(clippy::io_other_error)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::len_zero)]

use encoding_rs::{Encoding, WINDOWS_1252};
use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::{
    absolutize_css_urls, extract_css_links, extract_embedded_css_urls, infer_base_url, inject_css_into_html,
    inline_imports,
};
use fastrender::{Error, FastRender, Result};
use std::collections::HashSet;
use std::env;
use std::sync::mpsc::{channel, RecvTimeoutError};
use std::thread;
use std::time::Duration;

const STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

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
        (Some(String::from_utf8_lossy(value_bytes).into_owned()), end + 1)
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

/// Best-effort extraction of CSS URLs that appear inside inline scripts or attributes.
///
/// Some sites load their primary stylesheets dynamically and never emit a
/// `<link rel="stylesheet">` element in the static HTML. To render those pages
/// without executing JavaScript, scan the raw HTML for any substring that looks
/// like a CSS URL (ends with `.css`, possibly with a query string) and try to
/// resolve and fetch it as a stylesheet.
#[cfg(test)]
mod tests {
    use super::*;
    use fastrender::css::loader::resolve_href;

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
        let bytes = vec![0xA3]; // U+00A3 in Windows-1252
        let decoded = decode_html_bytes(&bytes, None);
        assert_eq!(decoded, "£");
    }
}

fn usage(program: &str) {
    eprintln!("Usage: {program} [--timeout SECONDS] [--dpr FLOAT] <url> [output.png] [width] [height] [scroll_x] [scroll_y]");
    eprintln!("Example: {program} --timeout 120 --dpr 2.0 https://www.example.com output.png 1200 800 0 0");
    eprintln!("  width: viewport width (default: 1200)");
    eprintln!("  height: viewport height (default: 800)");
    eprintln!("  dpr: device pixel ratio for media queries/srcset (default: 1.0)");
    eprintln!("  scroll_x: horizontal scroll offset (default: 0)");
    eprintln!("  scroll_y: vertical scroll offset (default: 0)");
}

fn render_once(
    url: &str,
    output: &str,
    width: u32,
    height: u32,
    scroll_x: u32,
    scroll_y: u32,
    dpr: f32,
) -> Result<()> {
    let _ = scroll_x; // horizontal scroll not yet supported by the renderer API
    println!("Fetching HTML from: {}", url);
    let (html_bytes, html_content_type) = fetch_bytes(url)?;
    let html = decode_html_bytes(&html_bytes, html_content_type.as_deref());
    let resource_base = infer_base_url(&html, url).into_owned();

    println!("Extracting CSS links...");
    let mut css_links = extract_css_links(&html, &resource_base);

    // Fallback: also scan inline content for CSS URLs that are not linked.
    let mut seen = HashSet::new();
    for link in &css_links {
        seen.insert(link.clone());
    }
    for extra in extract_embedded_css_urls(&html, &resource_base) {
        if seen.insert(extra.clone()) {
            css_links.push(extra);
        }
    }

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
                let inlined = inline_imports(
                    &rewritten,
                    &css_url,
                    &|u| fetch_bytes(u).map(|(b, ct)| decode_css_bytes(&b, ct.as_deref())),
                    &mut seen_imports,
                );
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

    println!(
        "Rendering to image ({}x{} viewport, scroll_x={}, scroll_y={})...",
        width, height, scroll_x, scroll_y
    );
    let mut renderer = FastRender::builder().device_pixel_ratio(dpr).build()?;
    renderer.set_base_url(resource_base.clone());
    let png_data = renderer.render_to_png_with_scroll(
        &html_with_css,
        width,
        height,
        scroll_x as f32,
        scroll_y as f32,
    )?;

    println!("Saving to {}...", output);
    std::fs::write(&output, png_data)?;

    println!("✓ Successfully rendered {} to {}", url, output);
    println!("  Image size: {} bytes", std::fs::metadata(&output)?.len());
    Ok(())
}

fn main() -> Result<()> {
    let program = env::args().next().unwrap_or_else(|| "fetch_and_render".to_string());
    let mut args = env::args().skip(1);
    let mut timeout_secs: Option<u64> = None;
    let mut dpr: f32 = 1.0;
    let mut positional: Vec<String> = Vec::new();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                usage(&program);
                return Ok(());
            }
            "--timeout" => {
                if let Some(val) = args.next() {
                    timeout_secs = val.parse().ok();
                }
            }
            "--dpr" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse::<f32>() {
                        if parsed.is_finite() && parsed > 0.0 {
                            dpr = parsed;
                        }
                    }
                }
            }
            _ => positional.push(arg),
        }
    }

    if positional.is_empty() {
        usage(&program);
        std::process::exit(1);
    }

    let url = positional.get(0).cloned().unwrap();
    let output = positional
        .get(1)
        .cloned()
        .unwrap_or_else(|| "fetched_output.png".to_string());
    let width = positional.get(2).and_then(|v| v.parse::<u32>().ok()).unwrap_or(1200);
    let height = positional.get(3).and_then(|v| v.parse::<u32>().ok()).unwrap_or(800);
    let scroll_x = positional.get(4).and_then(|v| v.parse::<u32>().ok()).unwrap_or(0);
    let scroll_y = positional.get(5).and_then(|v| v.parse::<u32>().ok()).unwrap_or(0);

    let (tx, rx) = channel();
    let url_clone = url.clone();
    let output_clone = output.clone();
    thread::Builder::new()
        .name("fetch_and_render-worker".to_string())
        .stack_size(STACK_SIZE)
        .spawn(move || {
            let _ = tx.send(render_once(
                &url_clone,
                &output_clone,
                width,
                height,
                scroll_x,
                scroll_y,
                dpr,
            ));
        })
        .expect("spawn render worker");

    if let Some(secs) = timeout_secs {
        match rx.recv_timeout(Duration::from_secs(secs)) {
            Ok(res) => res,
            Err(RecvTimeoutError::Timeout) => {
                eprintln!("Render timed out after {secs} seconds");
                std::process::exit(1);
            }
            Err(RecvTimeoutError::Disconnected) => {
                eprintln!("Render worker exited unexpectedly");
                std::process::exit(1);
            }
        }
    } else {
        match rx.recv() {
            Ok(res) => res,
            Err(_) => {
                eprintln!("Render worker exited unexpectedly");
                std::process::exit(1);
            }
        }
    }
}
