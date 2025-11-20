use fastrender::{Error, Renderer, Result};
use std::env;

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

    let mut response = agent.get(url).call().map_err(|e| {
        Error::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            e.to_string(),
        ))
    })?;

    let body = response.body_mut().read_to_string().map_err(|e| {
        Error::Io(std::io::Error::new(
            std::io::ErrorKind::Other,
            e.to_string(),
        ))
    })?;

    Ok(body)
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
                    if let Some(quote_start) =
                        href_section.find('"').or_else(|| href_section.find('\''))
                    {
                        let quote_char = href_section.chars().nth(quote_start).unwrap();
                        let after_quote = &href_section[quote_start + 1..];
                        if let Some(quote_end) = after_quote.find(quote_char) {
                            let href = &after_quote[..quote_end];

                            // Resolve relative URLs
                            let full_url = if href.starts_with("http://")
                                || href.starts_with("https://")
                            {
                                href.to_string()
                            } else if href.starts_with("//") {
                                format!("https:{}", href)
                            } else if href.starts_with('/') {
                                // Absolute path
                                let base_parts: Vec<&str> = base_url.splitn(4, '/').collect();
                                if base_parts.len() >= 3 {
                                    format!("{}//{}{}", base_parts[0], base_parts[2], href)
                                } else {
                                    href.to_string()
                                }
                            } else {
                                // Relative path - need to append to base URL's directory
                                // Parse the base URL to handle it correctly
                                if base_url.contains("://") {
                                    // Find the path part after the domain
                                    let protocol_end = base_url.find("://").unwrap() + 3;
                                    let after_protocol = &base_url[protocol_end..];

                                    // Find where the path starts (after the domain)
                                    let path_start =
                                        after_protocol.find('/').map(|p| protocol_end + p);

                                    let base_without_file = if let Some(path_pos) = path_start {
                                        // There's a path - find the last / to get directory
                                        if let Some(last_slash) = base_url[path_pos..].rfind('/') {
                                            &base_url[..=path_pos + last_slash]
                                        } else {
                                            // No slash in path, use the path start
                                            &base_url[..=path_pos]
                                        }
                                    } else {
                                        // No path, just domain - add trailing slash
                                        base_url
                                    };

                                    format!("{}/{}", base_without_file.trim_end_matches('/'), href)
                                } else {
                                    // Fallback for non-URL base paths
                                    format!("{}/{}", base_url.trim_end_matches('/'), href)
                                }
                            };

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

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!(
            "Usage: {} <url> [output.png] [width] [height] [scroll_y]",
            args[0]
        );
        eprintln!(
            "Example: {} https://www.example.com output.png 1200 800 0",
            args[0]
        );
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
                combined_css.push_str(&css);
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
        "Rendering to image ({}x{} viewport, scroll_y={})...",
        width, height, scroll_y
    );
    let renderer = Renderer::new();
    // Always use the base URL for image resolution, whether scrolling or not
    let png_data = renderer.render_to_png_with_scroll_and_base_url(
        &html_with_css,
        width,
        height,
        scroll_y,
        url.to_string(),
    )?;

    println!("Saving to {}...", output);
    std::fs::write(&output, png_data)?;

    println!("✓ Successfully rendered {} to {}", url, output);
    println!("  Image size: {} bytes", std::fs::metadata(&output)?.len());

    Ok(())
}
