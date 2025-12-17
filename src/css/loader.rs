//! Helpers for loading and inlining external stylesheets.
//!
//! These utilities resolve stylesheet URLs against a base, rewrite relative
//! `url(...)` references to absolute URLs, inline `@import` rules, and inject
//! fetched CSS into an HTML document. They are shared by the developer
//! tooling binaries so cached pages can be rendered with their real styles.

use crate::error::Result;
use std::borrow::Cow;
use std::collections::HashSet;
use std::path::Path;
use url::Url;

/// Resolve a possibly-relative `href` against a base URL.
///
/// Supports protocol-relative URLs (`//example.com`), `data:` URLs (returned
/// as-is), absolute URLs, and filesystem bases (`file://`) that may reference
/// directory paths.
pub fn resolve_href(base: &str, href: &str) -> Option<String> {
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

    Url::parse(&base_candidate)
        .or_else(|_| Url::from_file_path(&base_candidate).map_err(|_| url::ParseError::RelativeUrlWithoutBase))
        .ok()?
        .join(href)
        .ok()
        .map(|u| u.to_string())
}

fn normalize_embedded_css_candidate(candidate: &str) -> Option<String> {
    let mut cleaned = candidate
        .trim_matches(|c: char| matches!(c, '"' | '\'' | '(' | ')'))
        .trim()
        .to_string();

    if cleaned.is_empty() {
        return None;
    }

    if cleaned.contains('\\') {
        cleaned = cleaned.replace("\\/", "/").replace('\\', "");
    }

    if let Some(pos) = cleaned.to_ascii_lowercase().rfind(".css") {
        let trailing = &cleaned[pos + 4..];
        if trailing.chars().all(|c| c == '/') {
            cleaned.truncate(pos + 4);
        }
    }

    if cleaned.is_empty() {
        None
    } else {
        Some(cleaned)
    }
}

/// Rewrite `url(...)` references in a CSS string to be absolute using the stylesheet's base URL.
pub fn absolutize_css_urls(css: &str, base_url: &str) -> String {
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
                if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'*' {
                    i += 2;
                    state = State::Comment;
                    continue;
                }
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
                            out.push_str(&css[last_emit..i]);

                            let raw = css[content_start..content_end].trim();
                            let unquoted = raw.trim_matches('"').trim_matches('\'').to_string();
                            if let Some(resolved) = resolve_href(base_url, &unquoted) {
                                out.push_str(&format!("url(\"{}\")", resolved));
                            } else {
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
    } else if let Some(quote) = after_at.chars().next().filter(|c| *c == '"' || *c == '\'') {
        let rest = &after_at[1..];
        let close_idx = rest.find(quote)?;
        let url_str = rest[..close_idx].to_string();
        let media = rest[close_idx + 1..].trim().trim_end_matches(';').trim().to_string();
        (url_str, media)
    } else {
        return None;
    };
    Some((target, rest))
}

/// Inline `@import` rules by fetching their targets recursively.
///
/// All fetched stylesheets have their `url(...)` references rewritten against the
/// stylesheet URL before inlining, so relative asset references continue to work
/// once the CSS is embedded in the document.
pub fn inline_imports(
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

fn extract_attr_value(tag_source: &str, attr: &str) -> Option<String> {
    let tag_lower = tag_source.to_lowercase();
    if let Some(attr_pos) = tag_lower.find(attr) {
        let attr_slice = &tag_source[attr_pos..];
        if let Some(qpos) = attr_slice.find('"').or_else(|| attr_slice.find('\'')) {
            let quote = attr_slice.chars().nth(qpos).unwrap();
            let after = &attr_slice[qpos + 1..];
            if let Some(end) = after.find(quote) {
                return Some(after[..end].to_string());
            }
        }
    }
    None
}

/// Extract `<link rel="stylesheet">` URLs from an HTML document.
pub fn extract_css_links(html: &str, base_url: &str) -> Vec<String> {
    let mut css_urls = Vec::new();

    let lower = html.to_lowercase();
    let mut pos = 0;

    while let Some(link_start) = lower[pos..].find("<link") {
        let abs_start = pos + link_start;
        if let Some(link_end) = lower[abs_start..].find('>') {
            let link_tag = &html[abs_start..abs_start + link_end + 1];
            let link_tag_lower = link_tag.to_lowercase();

            if link_tag_lower.contains("stylesheet") {
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

    dedupe_links_preserving_order(css_urls)
}

/// Heuristic extraction of CSS URLs that appear inside inline scripts or attributes.
///
/// Some sites load their primary stylesheets dynamically and never emit a
/// `<link rel="stylesheet">` element in the static HTML. To render those pages
/// without executing JavaScript, scan the raw HTML for any substring that looks
/// like a CSS URL (ends with `.css`, possibly with a query string) and try to
/// resolve and fetch it as a stylesheet.
pub fn extract_embedded_css_urls(html: &str, base_url: &str) -> Vec<String> {
    let mut urls = Vec::new();
    let mut seen = HashSet::new();
    let bytes = html.as_bytes();
    let mut idx = 0;

    while let Some(pos) = memchr::memmem::find(&bytes[idx..], b".css") {
        let abs_pos = idx + pos;

        let mut start = abs_pos;
        while start > 0 {
            let c = bytes[start - 1] as char;
            if c == '"' || c == '\'' || c == '(' || c.is_whitespace() || c == '<' {
                break;
            }
            start -= 1;
        }

        let mut end = abs_pos + 4;
        while end < bytes.len() {
            let c = bytes[end] as char;
            if c == '"' || c == '\'' || c == ')' || c.is_whitespace() || c == '>' || c == '{' || c == '}' {
                break;
            }
            end += 1;
        }

        if end > start {
            let candidate = &html[start..end];
            if candidate.len() < 512 {
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
        let abs = pos + hit;
        let slice = &html[abs..];
        if let Some(colon) = slice.find(':') {
            let after_colon = &slice[colon + 1..];
            if let Some(q_start_rel) = after_colon.find(|c: char| c == '"' || c == '\'') {
                let quote = after_colon.chars().nth(q_start_rel).unwrap();
                let after_quote = &after_colon[q_start_rel + 1..];
                if let Some(q_end_rel) = after_quote.find(quote) {
                    let candidate = &after_quote[..q_end_rel];
                    if !candidate.to_ascii_lowercase().contains("style.csstext") && !candidate.trim_end().ends_with(':')
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

    urls
}

/// Deduplicate a list while preserving the order of first occurrence.
pub fn dedupe_links_preserving_order(mut links: Vec<String>) -> Vec<String> {
    let mut seen: HashSet<String> = HashSet::with_capacity(links.len());
    links.retain(|link| seen.insert(link.clone()));
    links
}

/// Inject a `<style>` block containing `css` into the HTML document.
pub fn inject_css_into_html(html: &str, css: &str) -> String {
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
        format!("{style_tag}{html}")
    }
}

/// Infer a reasonable base URL for the document.
///
/// Prefers `<base href>`, `<link rel="canonical">`, or `<meta property="og:url">`
/// when present. When rendering cached `file://.../*.html` inputs without those
/// hints, falls back to an `https://{filename}/` origin so relative resources
/// resolve against the original site instead of the local filesystem.
pub fn infer_base_url<'a>(html: &'a str, input_url: &'a str) -> Cow<'a, str> {
    // Canonicalize file:// inputs so relative cached paths become absolute.
    let mut input = Cow::Borrowed(input_url);
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

    let lower = html.to_lowercase();
    for (needle, attr, filter) in [
        ("<base", "href", None),
        ("<link", "href", Some("rel=\"canonical\"")),
        ("<meta", "content", Some("property=\"og:url\"")),
    ] {
        let mut pos = 0;
        while let Some(idx) = lower[pos..].find(needle) {
            let abs = pos + idx;
            if let Some(end) = lower[abs..].find('>') {
                let tag_slice = &html[abs..abs + end + 1];
                let tag_lower = &lower[abs..abs + end + 1];
                if let Some(f) = filter {
                    if !tag_lower.contains(f) {
                        pos = abs + end + 1;
                        continue;
                    }
                }
                if let Some(val) = extract_attr_value(tag_slice, attr) {
                    if let Some(resolved) = resolve_href(&input, &val) {
                        if resolved.starts_with("http://") || resolved.starts_with("https://") {
                            return Cow::Owned(resolved);
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
            if let Some(seg) = url.path_segments().and_then(|s| s.last()) {
                if let Some(host) = seg.strip_suffix(".html") {
                    let guess = format!("https://{host}/");
                    if Url::parse(&guess).is_ok() {
                        return Cow::Owned(guess);
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
        let out = absolutize_css_urls(css, "https://example.com/styles/main.css");
        assert!(out.contains("https://example.com/styles/images/bg.png"));
    }

    #[test]
    fn inline_imports_flattens_nested_imports() {
        let mut seen = HashSet::new();
        let css = "@import \"nested.css\";\nbody { color: black; }";
        let fetched = |url: &str| -> Result<String> {
            if url.ends_with("nested.css") {
                Ok("p { margin: 0; }".to_string())
            } else {
                Ok(String::new())
            }
        };
        let out = inline_imports(css, "https://example.com/main.css", &fetched, &mut seen);
        if !out.contains("p { margin: 0; }") {
            eprintln!("inline_imports output: {out}");
        }
        assert!(out.contains("p { margin: 0; }"));
        assert!(out.contains("body { color: black; }"));
    }

    #[test]
    fn extracts_stylesheet_hrefs_with_resolution() {
        let html = r#"
            <link rel="stylesheet" href="../styles/a.css">
            <link rel="alternate stylesheet" href="b.css">
            <link rel="icon" href="favicon.ico">
        "#;
        let urls = extract_css_links(html, "https://example.com/app/index.html");
        assert_eq!(urls.len(), 2);
        assert!(urls.contains(&"https://example.com/styles/a.css".to_string()));
        assert!(urls.contains(&"https://example.com/app/b.css".to_string()));
    }

    #[test]
    fn detects_embedded_css_urls() {
        let html = r#"
            <script>var cssUrl="assets/site.css?v=1";</script>
            <style>@import url("/shared/base.css");</style>
        "#;
        let urls = extract_embedded_css_urls(html, "https://example.com/app/");
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
        let urls = extract_embedded_css_urls(html, "https://example.com/");
        assert_eq!(urls, vec!["https://cdn.example.com/styles/main.css".to_string()]);
    }

    #[test]
    fn unescapes_json_style_embedded_urls() {
        let html = r#"
            <script>
                window.css = "https:\\/\\/cdn.example.com\\/app.css\\"";
            </script>
        "#;
        let urls = extract_embedded_css_urls(html, "https://example.com/");
        assert_eq!(urls, vec!["https://cdn.example.com/app.css".to_string()]);
    }

    #[test]
    fn dedupes_stylesheet_links_preserving_order() {
        let html = r#"
            <link rel="stylesheet" href="/a.css">
            <link rel="stylesheet" href="/b.css">
            <link rel="stylesheet" href="/a.css">
        "#;
        let urls = extract_css_links(html, "https://example.com/app/index.html");
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
        let html = r#"
            <style>
                .css-v2kfba{height:100%;width:100%;}
            </style>
            <script>
                const cls = '.css-15ru6p1{font-size:inherit;font-weight:normal;}'
            </script>
        "#;
        let urls = extract_embedded_css_urls(html, "https://example.com/");
        assert!(urls.is_empty());
    }

    #[test]
    fn ignores_percent_encoded_css_class_tokens() {
        let html = r#"
            <script>
                const bogus = ">%3E.css-v2kfba%7Bheight:100%;width:100%;%7D%3C/style";
            </script>
        "#;
        let urls = extract_embedded_css_urls(html, "https://example.com/");
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
}
