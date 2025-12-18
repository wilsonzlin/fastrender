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

    // Fallback: look for a variable assignment that captures a URL literal
    if let Some(mut idx) = lower.find("var url") {
        if let Some(eq) = lower[idx..].find('=') {
            idx += eq + 1;
            while idx < lower.len() && lower.as_bytes()[idx].is_ascii_whitespace() {
                idx += 1;
            }
            if idx < lower.len() && (lower.as_bytes()[idx] == b'"' || lower.as_bytes()[idx] == b'\'') {
                let quote = lower.as_bytes()[idx];
                idx += 1;
                let start = idx;
                while idx < lower.len() && lower.as_bytes()[idx] != quote {
                    idx += 1;
                }
                let end = idx.min(html.len());
                let mut candidate = html[start..end].trim().to_string();
                if candidate.starts_with("//") {
                    candidate = format!("https:{}", candidate);
                }
                if !candidate.is_empty() {
                    return Some(candidate);
                }
            }
        }
    }

    None
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
            let prev_is_delim =
                i == 0 || bytes[i - 1].is_ascii_whitespace() || bytes[i - 1] == b';' || bytes[i - 1] == b',';
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
            if i + 1 < bytes.len() && bytes[i] == b'\\' && (bytes[i + 1] == b'"' || bytes[i + 1] == b'\'') {
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
        let html = r#"<meta http-equiv="REFRESH" content="0; URL='https://example.com/path;param=1?q=2'">"#;
        assert_eq!(
            extract_meta_refresh_url(html),
            Some("https://example.com/path;param=1?q=2".to_string())
        );
    }

    #[test]
    fn decodes_entities_in_refresh_url() {
        let html = r#"<meta http-equiv="refresh" content="0; url=&apos;/html/?q=1&amp;r=2&apos;">"#;
        assert_eq!(extract_meta_refresh_url(html), Some("/html/?q=1&r=2".to_string()));
    }

    #[test]
    fn handles_refresh_without_delay() {
        let html = r#"<meta http-equiv="refresh" content="url=/noscript/landing">"#;
        assert_eq!(extract_meta_refresh_url(html), Some("/noscript/landing".to_string()));
    }

    #[test]
    fn ignores_non_refresh_meta() {
        let html = "<meta charset=\"utf-8\"><meta name='viewport' content='width=device-width'>";
        assert_eq!(extract_meta_refresh_url(html), None);
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

    #[test]
    fn extracts_url_from_var_assignment() {
        let html = "<script>var url = \"//example.com/next\"; window.location.replace(url);</script>";
        assert_eq!(
            extract_js_location_redirect(html),
            Some("https://example.com/next".to_string())
        );
    }
}
