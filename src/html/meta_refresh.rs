//! Meta refresh parsing utilities.
//!
//! Provides a lightweight extractor for `<meta http-equiv="refresh">` URLs so
//! callers can follow non-JS redirects commonly used as `<noscript>` fallbacks.

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
            if name.eq_ignore_ascii_case("http-equiv") {
                http_equiv = Some(value);
            } else if name.eq_ignore_ascii_case("content") {
                content = Some(value);
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
    let lower = html.to_ascii_lowercase();
    let patterns = [
        "location.replace",
        "window.location.replace",
        "document.location.replace",
        "location.href",
        "window.location.href",
        "document.location.href",
        "window.location",
        "document.location",
        "location",
    ];

    for pat in patterns.iter() {
        if let Some(idx) = lower.find(pat) {
            let mut i = idx + pat.len();
            while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
                i += 1;
            }
            if i < lower.len() && lower.as_bytes()[i] == b'=' {
                i += 1;
                while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
                    i += 1;
                }
            }
            if i < lower.len() && lower.as_bytes()[i] == b'(' {
                i += 1;
                while i < lower.len() && lower.as_bytes()[i].is_ascii_whitespace() {
                    i += 1;
                }
            }
            if i < lower.len() && (lower.as_bytes()[i] == b'"' || lower.as_bytes()[i] == b'\'') {
                let quote = lower.as_bytes()[i];
                i += 1;
                let start = i;
                while i < lower.len() && lower.as_bytes()[i] != quote {
                    i += 1;
                }
                let end = i.min(html.len());
                let candidate = html[start..end].trim();
                if !candidate.is_empty() {
                    return Some(candidate.to_string());
                }
            }
        }
    }

    None
}

fn parse_refresh_content(content: &str) -> Option<String> {
    let decoded = content
        .replace("&quot;", "\"")
        .replace("&amp;", "&")
        .replace("&#39;", "'");
    for part in decoded.split(';') {
        let trimmed = part.trim();
        if let Some(rest) = trimmed.strip_prefix(|c: char| c.eq_ignore_ascii_case(&'u')) {
            // unlikely branch when content starts directly with "url=..."
            if let Some(url) = rest.strip_prefix(|c: char| c.eq_ignore_ascii_case(&'r')) {
                let val =
                    url.trim_start_matches(|c: char| c.eq_ignore_ascii_case(&'l') || c == '=' || c.is_whitespace());
                let cleaned = val.trim_matches(['"', '\'']);
                if !cleaned.is_empty() {
                    return Some(cleaned.to_string());
                }
            }
        }

        if let Some(pos) = trimmed.find('=') {
            let (name, value) = trimmed.split_at(pos);
            if name.trim().eq_ignore_ascii_case("url") {
                let cleaned = value[1..].trim().trim_matches(['"', '\'']);
                if !cleaned.is_empty() {
                    return Some(cleaned.to_string());
                }
            }
        }
    }
    None
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
        while i < bytes.len() && !bytes[i].is_ascii_whitespace() && bytes[i] != b'=' && bytes[i] != b'>' {
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
            if i < bytes.len() && (bytes[i] == b'"' || bytes[i] == b'\'') {
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
    use super::{extract_js_location_redirect, extract_meta_refresh_url};

    #[test]
    fn extracts_meta_refresh_url() {
        let html = r#"<html><head><meta http-equiv='refresh' content='0; url=/fallback.html'></head></html>"#;
        assert_eq!(extract_meta_refresh_url(html), Some("/fallback.html".to_string()));
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
    fn ignores_non_refresh_meta() {
        let html = "<meta charset=\"utf-8\"><meta name='viewport' content='width=device-width'>";
        assert_eq!(extract_meta_refresh_url(html), None);
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
    fn extracts_js_location_href() {
        let html = "<script>window.location.href = 'https://example.com/next';</script>";
        assert_eq!(
            extract_js_location_redirect(html),
            Some("https://example.com/next".to_string())
        );
    }

    #[test]
    fn extracts_js_location_replace() {
        let html = "<script>location.replace(\"/foo\");</script>";
        assert_eq!(extract_js_location_redirect(html), Some("/foo".to_string()));
    }
}
