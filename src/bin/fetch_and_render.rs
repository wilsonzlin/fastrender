//! Fetch a single page and render it to an image.
//!
//! Usage: fetch_and_render [--timeout SECONDS] [--dpr FLOAT] [--prefers-reduced-transparency <value>] [--prefers-reduced-motion <value>] [--prefers-reduced-data <value>] [--full-page] [--user-agent UA] <url> [output.png] [width] [height] [scroll_x] [scroll_y]
//!
//! Examples:
//!   fetch_and_render --timeout 120 --dpr 2.0 https://www.example.com output.png 1200 800 0 0
//!
//! Options:
//!   --timeout SECONDS   Per-page timeout (default: 0 = no timeout)
//!   --dpr FLOAT         Device pixel ratio for media queries/srcset (default: 1.0)
//!   --prefers-reduced-transparency reduce|no-preference|true|false
//!                        User media preference for reduced transparency (overrides env)
//!   --prefers-reduced-motion reduce|no-preference|true|false
//!                        User media preference for reduced motion (overrides env)
//!   --prefers-reduced-data reduce|no-preference|true|false
//!                        User media preference for reduced data (overrides env)
//!   --full-page         Expand the render target to the full content size (respects FASTR_FULL_PAGE env)
//!   --user-agent UA     Override the User-Agent header (default: Chrome-like)

#![allow(clippy::io_other_error)]
#![allow(clippy::redundant_closure)]
#![allow(clippy::len_zero)]

use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::{
    absolutize_css_urls, extract_css_links, extract_embedded_css_urls, infer_base_url, inject_css_into_html,
    inline_imports,
};
use fastrender::html::encoding::decode_html_bytes;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::{Error, FastRender, Result};
use std::collections::HashSet;
use std::env;
use std::sync::mpsc::{channel, RecvTimeoutError};
use std::thread;
use std::time::Duration;
use url::Url;

const STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

fn fetch_bytes(
    url: &str,
    timeout: Option<Duration>,
    user_agent: &str,
) -> Result<(Vec<u8>, Option<String>, Option<String>)> {
    // Handle file:// URLs
    if url.starts_with("file://") {
        let path = url.strip_prefix("file://").unwrap();
        let bytes = std::fs::read(path).map_err(Error::Io)?;

        let mut meta_path = std::path::PathBuf::from(path);
        if let Some(ext) = meta_path.extension().and_then(|e| e.to_str()) {
            meta_path.set_extension(format!("{ext}.meta"));
        } else {
            meta_path.set_extension("meta");
        }
        let meta = std::fs::read_to_string(&meta_path).ok();
        let (content_type, source_url) = meta
            .as_deref()
            .map(fastrender::resource::parse_cached_html_meta)
            .unwrap_or((None, None));

        return Ok((bytes, content_type, source_url));
    }

    // Configure agent with timeout for ureq 3.x
    let config = ureq::Agent::config_builder().timeout_global(timeout).build();
    let agent: ureq::Agent = config.into();

    let mut current = url.to_string();
    for _ in 0..10 {
        let mut response = agent
            .get(&current)
            .header("User-Agent", user_agent)
            .call()
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

        let status = response.status();
        if (300..400).contains(&status.as_u16()) {
            if let Some(loc) = response.headers().get("location").and_then(|h| h.to_str().ok()) {
                if let Ok(base) = Url::parse(&current) {
                    if let Ok(next) = base.join(loc) {
                        current = next.to_string();
                        continue;
                    }
                }
                current = loc.to_string();
                continue;
            }
        }

        let content_type = response
            .headers()
            .get("content-type")
            .and_then(|h| h.to_str().ok())
            .map(|s| s.to_string());

        let bytes = response
            .body_mut()
            .read_to_vec()
            .map_err(|e| Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())))?;

        return Ok((bytes, content_type, None));
    }

    Err(Error::Io(std::io::Error::new(
        std::io::ErrorKind::Other,
        "Too many redirects",
    )))
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

    #[test]
    fn fetch_bytes_reads_local_meta_content_type() {
        let dir = tempfile::tempdir().expect("temp dir");
        let html_path = dir.path().join("page.html");
        let meta_path = dir.path().join("page.html.meta");

        // Write Shift-JIS encoded body and matching meta
        let encoded = encoding_rs::SHIFT_JIS.encode("<html><body>デ</body></html>").0.to_vec();
        std::fs::write(&html_path, &encoded).unwrap();
        std::fs::write(&meta_path, "text/html; charset=shift_jis").unwrap();

        let url = format!("file://{}", html_path.display());
        let (bytes, ct, base_url) = fetch_bytes(&url, None, DEFAULT_USER_AGENT).expect("fetch bytes");
        assert_eq!(ct.as_deref(), Some("text/html; charset=shift_jis"));
        assert!(base_url.is_none(), "legacy meta should not include a url");
        let decoded = decode_html_bytes(&bytes, ct.as_deref());
        assert!(
            decoded.contains('デ'),
            "decoded html should respect meta charset: {}",
            decoded
        );
    }

    #[test]
    fn parse_prefers_reduced_transparency_values() {
        assert_eq!(parse_prefers_reduced_transparency("reduce"), Some(true));
        assert_eq!(parse_prefers_reduced_transparency("no-preference"), Some(false));
        assert_eq!(parse_prefers_reduced_transparency("yes"), Some(true));
        assert_eq!(parse_prefers_reduced_transparency("off"), Some(false));
        assert_eq!(parse_prefers_reduced_transparency("maybe"), None);
    }

    #[test]
    fn parse_prefers_reduced_data_values() {
        assert_eq!(parse_prefers_reduced_data("reduce"), Some(true));
        assert_eq!(parse_prefers_reduced_data("no-preference"), Some(false));
        assert_eq!(parse_prefers_reduced_data("yes"), Some(true));
        assert_eq!(parse_prefers_reduced_data("off"), Some(false));
        assert_eq!(parse_prefers_reduced_data("maybe"), None);
    }

    #[test]
    fn parse_prefers_reduced_motion_values() {
        assert_eq!(parse_prefers_reduced_motion("reduce"), Some(true));
        assert_eq!(parse_prefers_reduced_motion("no-preference"), Some(false));
        assert_eq!(parse_prefers_reduced_motion("yes"), Some(true));
        assert_eq!(parse_prefers_reduced_motion("off"), Some(false));
        assert_eq!(parse_prefers_reduced_motion("maybe"), None);
    }
}

fn usage(program: &str) {
    eprintln!(
        "Usage: {program} [--timeout SECONDS] [--dpr FLOAT] [--prefers-reduced-transparency <value>] [--prefers-reduced-motion <value>] [--prefers-reduced-data <value>] [--full-page] [--user-agent UA] <url> [output.png] [width] [height] [scroll_x] [scroll_y]"
    );
    eprintln!("Example: {program} --timeout 120 --dpr 2.0 https://www.example.com output.png 1200 800 0 0");
    eprintln!("  width: viewport width (default: 1200)");
    eprintln!("  height: viewport height (default: 800)");
    eprintln!("  dpr: device pixel ratio for media queries/srcset (default: 1.0)");
    eprintln!("  prefers-reduced-transparency: reduce|no-preference|true|false (overrides env)");
    eprintln!("  prefers-reduced-motion: reduce|no-preference|true|false (overrides env)");
    eprintln!("  prefers-reduced-data: reduce|no-preference|true|false (overrides env)");
    eprintln!("  full-page: expand render target to full content size (or set FASTR_FULL_PAGE)");
    eprintln!("  user-agent: override the User-Agent header (default: Chrome-like)");
    eprintln!("  scroll_x: horizontal scroll offset (default: 0)");
    eprintln!("  scroll_y: vertical scroll offset (default: 0)");
}

fn parse_prefers_reduced_transparency(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn parse_prefers_reduced_data(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn parse_prefers_reduced_motion(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer" | "reduce-motion"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn render_once(
    url: &str,
    output: &str,
    width: u32,
    height: u32,
    scroll_x: u32,
    scroll_y: u32,
    dpr: f32,
    timeout_secs: Option<u64>,
    user_agent: &str,
) -> Result<()> {
    println!("Fetching HTML from: {}", url);
    let timeout = timeout_secs.map(Duration::from_secs);
    let (html_bytes, html_content_type, source_url) = fetch_bytes(url, timeout, user_agent)?;
    let html = decode_html_bytes(&html_bytes, html_content_type.as_deref());
    let base_hint = source_url.as_deref().unwrap_or(url);
    let resource_base = infer_base_url(&html, base_hint).into_owned();

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
        match fetch_bytes(&css_url, timeout, user_agent) {
            Ok((bytes, content_type, _)) => {
                let css_text = decode_css_bytes(&bytes, content_type.as_deref());
                let rewritten = absolutize_css_urls(&css_text, &css_url);
                let inlined = inline_imports(
                    &rewritten,
                    &css_url,
                    &|u| {
                        fetch_bytes(u, timeout, user_agent)
                            .map(|(b, ct, _)| decode_css_bytes(&b, ct.as_deref()))
                    },
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
    let png_data =
        renderer.render_to_png_with_scroll(&html_with_css, width, height, scroll_x as f32, scroll_y as f32)?;

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
    let mut prefers_reduced_transparency: Option<bool> = None;
    let mut prefers_reduced_motion: Option<bool> = None;
    let mut prefers_reduced_data: Option<bool> = None;
    let mut positional: Vec<String> = Vec::new();
    let mut full_page = false;
    let mut user_agent = DEFAULT_USER_AGENT.to_string();
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
            "--prefers-reduced-transparency" => {
                if let Some(val) = args.next() {
                    prefers_reduced_transparency = parse_prefers_reduced_transparency(&val);
                }
            }
            "--prefers-reduced-motion" => {
                if let Some(val) = args.next() {
                    prefers_reduced_motion = parse_prefers_reduced_motion(&val);
                }
            }
            "--prefers-reduced-data" => {
                if let Some(val) = args.next() {
                    prefers_reduced_data = parse_prefers_reduced_data(&val);
                }
            }
            "--full-page" => {
                full_page = true;
            }
            "--user-agent" => {
                if let Some(val) = args.next() {
                    if !val.trim().is_empty() {
                        user_agent = val;
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

    if let Some(reduce) = prefers_reduced_transparency {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_TRANSPARENCY",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(reduce) = prefers_reduced_motion {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_MOTION",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(reduce) = prefers_reduced_data {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_DATA",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if full_page {
        std::env::set_var("FASTR_FULL_PAGE", "1");
    }

    eprintln!(
        "User-Agent: {}\nViewport: {}x{} @{}x, scroll ({}, {})\nOutput: {}",
        user_agent, width, height, dpr, scroll_x, scroll_y, output
    );

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
                timeout_secs,
                &user_agent,
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
