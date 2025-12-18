//! Fetch and cache HTML pages for testing
//!
//! Usage: fetch_pages [--refresh] [--jobs N] [--timeout SECONDS] [--pages a,b,c] [--user-agent UA] [--timings]
//!
//! Fetches all target pages in parallel and caches to fetches/html/

use std::collections::HashSet;

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use fastrender::html::encoding::decode_html_bytes;
use fastrender::html::meta_refresh::{extract_js_location_redirect, extract_meta_refresh_url};
use fastrender::resource::{HttpFetcher, ResourceFetcher, DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT};
use rayon::ThreadPoolBuilder;
use url::Url;

struct Config {
    refresh: bool,
    jobs: usize,
    timeout: Duration,
    page_filter: Option<HashSet<String>>, // normalized via url_to_filename
    user_agent: String,
    accept_language: String,
    timings: bool,
}

const CACHE_DIR: &str = "fetches/html";

// Target pages for testing
const PAGES: &[&str] = &[
    // Tier 1: Simple
    "https://example.com",
    "https://example.org",
    "https://example.net",
    // Tier 2: Text-heavy
    "https://news.ycombinator.com",
    "https://lobste.rs",
    "https://lite.cnn.com",
    "https://text.npr.org",
    "https://iana.org",
    // Tier 3: Modern
    "https://google.com",
    "https://duckduckgo.com",
    "https://wikipedia.org",
    "https://www.w3.org",
    // Tier 4: Complex
    "https://github.com",
    "https://gitlab.com",
    "https://stackoverflow.com",
    "https://reddit.com",
    "https://twitter.com",
    "https://weibo.cn",
    "https://alibaba.com",
    "https://arxiv.org",
    "https://amazon.com",
    "https://youtube.com",
    "https://facebook.com",
    "https://linkedin.com",
    "https://microsoft.com",
    "https://apple.com",
    "https://developer.apple.com",
    "https://openai.com",
    "https://icloud.com",
    "https://nytimes.com",
    "http://neverssl.com",
    "https://htmldog.com",
    "https://ietf.org",
    "https://bbc.com",
    "https://nhk.or.jp",
    "https://si.edu",
    "https://cdc.gov",
    "https://britannica.com",
    "https://zillow.com",
    "https://apache.org",
    "https://python.org",
    "https://cnn.com",
    "https://fast.com",
    "https://theguardian.com",
    "https://nyu.edu",
    "https://openbsd.org",
    "https://debian.org",
    "https://gentoo.org",
    "https://archlinux.org",
    "https://manjaro.org",
    "https://sina.com.cn",
    "https://espn.com",
    "https://imdb.com",
    "https://craigslist.org",
    "https://pinterest.com",
    "https://medium.com",
    "https://quora.com",
    "https://twitch.tv",
    "https://dropbox.com",
    "https://gitlab.io",
    "https://stripe.com",
    "https://shopify.com",
    "https://weebly.com",
    "https://etsy.com",
    "https://ebay.com",
    "https://walmart.com",
    "https://usatoday.com",
    "https://newsweek.com",
    "https://vox.com",
    "https://fortune.com",
    "https://msnbc.com",
    "https://msn.com",
    "https://huffpost.com",
    "https://airbnb.com",
    "https://booking.com",
    "https://yelp.com",
    "https://spotify.com",
    "https://reuters.com",
    "https://netflix.com",
    "https://wsj.com",
    "https://bloomberg.com",
    "https://time.com",
    "https://forbes.com",
    "https://foxnews.com",
    "https://techcrunch.com",
    "https://theverge.com",
    "https://wired.com",
    "https://arstechnica.com",
    "https://engadget.com",
    "https://nbcnews.com",
    "https://figma.com",
    "https://ft.com",
    "https://phoronix.com",
    "https://nationalgeographic.com",
    "https://cnet.com",
    "https://developer.mozilla.org",
    "https://howtogeek.com",
    "https://macrumors.com",
    "https://washingtonpost.com",
    "https://w3.org",
    "https://abcnews.go.com",
    "https://washington.edu",
    "https://berkeley.edu",
    "https://xkcd.com",
    "https://fandom.com",
    "https://ikea.com",
    "https://elpais.com",
    "https://ndtv.com",
    "https://yahoo.com",
    "https://nasa.gov",
    "https://stackexchange.com",
    "https://rust-lang.org",
    "https://blog.rust-lang.org",
    "https://rfc-editor.org",
    "https://tesco.com",
    "https://bing.com",
    "https://discord.com",
    "https://weather.com",
    "https://bbc.co.uk",
    "https://npmjs.com",
    "https://latimes.com",
    "https://cloudflare.com",
    "https://aliexpress.com",
    "https://apnews.com",
    "https://aljazeera.com",
    "https://tripadvisor.com",
    "https://vogue.com",
    "https://theatlantic.com",
    "https://economist.com",
    "https://newyorker.com",
    "https://hbr.org",
    "https://sqlite.org",
    "https://nginx.org",
    "https://go.dev",
    "https://docs.rs",
    "https://doc.rust-lang.org",
    "https://www.openstreetmap.org",
    "https://docs.python.org",
    "https://kotlinlang.org",
];

fn url_to_filename(url: &str) -> String {
    url.trim_start_matches("https://")
        .trim_start_matches("http://")
        .replace('/', "_")
        .chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '.' || c == '_' || c == '-' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

fn normalize_page_name(raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        None
    } else {
        // Accept full URLs and common "www." variants when filtering.
        let no_scheme = trimmed.trim_start_matches("https://").trim_start_matches("http://");
        let without_www = no_scheme.strip_prefix("www.").unwrap_or(no_scheme);
        Some(url_to_filename(without_www))
    }
}

fn parse_args() -> Config {
    let mut refresh = false;
    let mut jobs = num_cpus::get();
    let mut timeout_secs: u64 = 30;
    let mut filter: HashSet<String> = HashSet::new();
    let mut has_filter = false;
    let mut user_agent = DEFAULT_USER_AGENT.to_string();
    let mut accept_language = DEFAULT_ACCEPT_LANGUAGE.to_string();
    let mut timings = false;

    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                println!("Usage: fetch_pages [--refresh] [--jobs N] [--timeout SECONDS] [--pages a,b,c]");
                println!("\nOptions:");
                println!("  --refresh           Re-fetch all pages even if cached");
                println!("  --jobs N            Number of parallel fetches (default: num_cpus)");
                println!("  --timeout SECONDS   Per-request timeout (default: 30)");
                println!("  --pages a,b,c       Fetch only the listed pages (full URLs or stems ok)");
                println!("  --user-agent UA     Override the User-Agent header (default: Chrome-like)");
                println!("  --accept-language   Override the Accept-Language header (default: en-US,en;q=0.9)");
                println!("  --timings           Print per-page fetch durations");
                std::process::exit(0);
            }
            "--refresh" => refresh = true,
            "--jobs" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse() {
                        if parsed > 0 {
                            jobs = parsed;
                        }
                    }
                }
            }
            "--timeout" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse() {
                        timeout_secs = parsed;
                    }
                }
            }
            "--pages" => {
                if let Some(val) = args.next() {
                    has_filter = true;
                    for name in val.split(',') {
                        if let Some(normalized) = normalize_page_name(name) {
                            filter.insert(normalized);
                        }
                    }
                }
            }
            "--user-agent" => {
                if let Some(val) = args.next() {
                    if !val.trim().is_empty() {
                        user_agent = val;
                    }
                }
            }
            "--accept-language" => {
                if let Some(val) = args.next() {
                    if !val.trim().is_empty() {
                        accept_language = val;
                    }
                }
            }
            "--timings" => timings = true,
            _ => {}
        }
    }

    Config {
        refresh,
        jobs,
        timeout: Duration::from_secs(timeout_secs),
        page_filter: if has_filter { Some(filter) } else { None },
        user_agent,
        accept_language,
        timings,
    }
}

fn selected_pages(filter: Option<&HashSet<String>>) -> Vec<&'static str> {
    PAGES
        .iter()
        .copied()
        .filter(|url| match filter {
            Some(names) => {
                let fname = url_to_filename(url);
                let no_www = fname.strip_prefix("www.");
                names.contains(&fname) || no_www.map(|n| names.contains(n)).unwrap_or(false)
            }
            None => true,
        })
        .collect()
}

fn write_cached_html(
    cache_path: &Path,
    bytes: &[u8],
    content_type: Option<&str>,
    source_url: Option<&str>,
) -> std::io::Result<()> {
    if let Some(parent) = cache_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    std::fs::write(cache_path, bytes)?;

    let meta_path = cache_path.with_extension("html.meta");
    let mut meta = String::new();
    if let Some(ct) = content_type {
        if !ct.is_empty() {
            meta.push_str(&format!("content-type: {}\n", ct));
        }
    }
    if let Some(url) = source_url {
        if !url.trim().is_empty() {
            meta.push_str(&format!("url: {}\n", url.trim()));
        }
    }

    if meta.is_empty() {
        let _ = std::fs::remove_file(meta_path);
    } else {
        let _ = std::fs::write(meta_path, meta);
    }

    Ok(())
}

fn fetch_page(
    url: &str,
    timeout: Duration,
    user_agent: &str,
    accept_language: &str,
) -> Result<(Vec<u8>, Option<String>, String), String> {
    let fetcher = HttpFetcher::default()
        .with_timeout(timeout)
        .with_user_agent(user_agent.to_string())
        .with_accept_language(accept_language.to_string());

    let mut current_url = url.to_string();

    let fetch = |target: &str| -> Result<(Vec<u8>, Option<String>, String), String> {
        let res = fetcher.fetch(target).map_err(|e| e.to_string())?;
        if res.bytes.is_empty() {
            return Err("empty response body".to_string());
        }
        Ok((res.bytes, res.content_type, target.to_string()))
    };

    let mut res = fetch(url)?;

    // Follow a single meta refresh (common for noscript fallbacks)
    let mut html = decode_html_bytes(&res.0, res.1.as_deref());
    if let Some(refresh) = extract_meta_refresh_url(&html) {
        if let Ok(base) = Url::parse(&current_url) {
            if let Ok(next) = base.join(&refresh) {
                match fetch(next.as_str()) {
                    Ok(next_res) => {
                        res = next_res;
                        current_url = next.to_string();
                        html = decode_html_bytes(&res.0, res.1.as_deref());
                    }
                    Err(e) => eprintln!("Warning: meta refresh fetch failed: {} (keeping original)", e),
                }
            }
        }
    }

    // Follow a simple JS location redirect once (common in script-only handoffs)
    if let Some(js_redirect) = extract_js_location_redirect(&html) {
        if let Ok(base) = Url::parse(&current_url) {
            if let Ok(next) = base.join(&js_redirect) {
                match fetch(next.as_str()) {
                    Ok(next_res) => {
                        res = next_res;
                        current_url = next.to_string();
                    }
                    Err(e) => eprintln!("Warning: js redirect fetch failed: {} (keeping original)", e),
                }
            }
        }
    }

    Ok((res.0, res.1, current_url))
}

fn main() {
    let config = parse_args();

    fs::create_dir_all(CACHE_DIR).expect("create cache dir");

    let selected = selected_pages(config.page_filter.as_ref());
    if selected.is_empty() {
        if config.page_filter.is_some() {
            println!("No pages matched the provided filter");
        } else {
            println!("No pages to fetch");
        }
        return;
    }

    if let Some(filter) = &config.page_filter {
        let matched: HashSet<_> = selected
            .iter()
            .flat_map(|u| {
                let fname = url_to_filename(u);
                let mut names = vec![fname.clone()];
                if let Some(no_www) = fname.strip_prefix("www.") {
                    names.push(no_www.to_string());
                }
                names
            })
            .collect();
        let missing: Vec<_> = filter
            .iter()
            .filter(|name| !matched.contains(name.as_str()))
            .cloned()
            .collect();
        if !missing.is_empty() {
            println!("Warning: unknown pages in filter: {}", missing.join(", "));
        }
    }

    println!(
        "Fetching {} pages ({} parallel, {}s timeout)...",
        selected.len(),
        config.jobs,
        config.timeout.as_secs()
    );
    println!("User-Agent: {}", config.user_agent);
    println!("Accept-Language: {}", config.accept_language);
    if config.refresh {
        println!("--refresh: re-fetching all");
    }
    println!();

    let success = Arc::new(AtomicUsize::new(0));
    let failed = Arc::new(AtomicUsize::new(0));
    let skipped = Arc::new(AtomicUsize::new(0));
    let failed_urls: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));

    let pool = ThreadPoolBuilder::new()
        .num_threads(config.jobs)
        .build()
        .expect("create thread pool");

    pool.scope(|s| {
        for &url in &selected {
            let success = Arc::clone(&success);
            let failed = Arc::clone(&failed);
            let skipped = Arc::clone(&skipped);
            let failed_urls = Arc::clone(&failed_urls);
            let refresh = config.refresh;
            let timeout = config.timeout;
            let user_agent = config.user_agent.clone();
            let accept_language = config.accept_language.clone();
            let timings = config.timings;

            s.spawn(move |_| {
                let filename = url_to_filename(url);
                let cache_path = PathBuf::from(CACHE_DIR).join(format!("{}.html", filename));

                // Skip if cached and not refreshing
                if !refresh && cache_path.exists() {
                    skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }

                let start = if timings { Some(std::time::Instant::now()) } else { None };
                match fetch_page(url, timeout, &user_agent, &accept_language) {
                    Ok((bytes, content_type, source_url)) => {
                        if write_cached_html(&cache_path, &bytes, content_type.as_deref(), Some(&source_url)).is_ok() {
                            if let Some(start) = start {
                                println!("✓ {} ({}b, {}ms)", url, bytes.len(), start.elapsed().as_millis());
                            } else {
                                println!("✓ {} ({}b)", url, bytes.len());
                            }
                            success.fetch_add(1, Ordering::Relaxed);
                            return;
                        }
                        if let Some(start) = start {
                            println!("✗ {} (write failed, {}ms)", url, start.elapsed().as_millis());
                        } else {
                            println!("✗ {} (write failed)", url);
                        }
                        failed.fetch_add(1, Ordering::Relaxed);
                        let _ = failed_urls.lock().map(|mut v| v.push(url.to_string()));
                    }
                    Err(e) => {
                        if let Some(start) = start {
                            println!("✗ {} ({}, {}ms)", url, e, start.elapsed().as_millis());
                        } else {
                            println!("✗ {} ({})", url, e);
                        }
                        failed.fetch_add(1, Ordering::Relaxed);
                        let _ = failed_urls.lock().map(|mut v| v.push(url.to_string()));
                    }
                }
            });
        }
    });

    println!();
    println!(
        "Done: {} fetched, {} failed, {} cached",
        success.load(Ordering::Relaxed),
        failed.load(Ordering::Relaxed),
        skipped.load(Ordering::Relaxed)
    );
    if let Ok(urls) = failed_urls.lock() {
        if !urls.is_empty() {
            println!("Failed URLs: {}", urls.join(", "));
        }
    }
    println!("Cache: {}/", CACHE_DIR);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn url_to_filename_replaces_scheme_and_slashes() {
        assert_eq!(url_to_filename("https://example.com/foo/bar"), "example.com_foo_bar");
        assert_eq!(url_to_filename("http://example.com"), "example.com");
    }

    #[test]
    fn selected_pages_respects_filter() {
        let mut filter = HashSet::new();
        filter.insert(url_to_filename("https://cnn.com"));
        filter.insert(url_to_filename("https://example.com"));

        let selected = selected_pages(Some(&filter));
        assert!(selected.contains(&"https://cnn.com"));
        assert!(selected.contains(&"https://example.com"));
        assert!(!selected.contains(&"https://reddit.com"));
    }

    #[test]
    fn selected_pages_none_returns_all() {
        let all = selected_pages(None);
        assert_eq!(all.len(), PAGES.len());
    }

    #[test]
    fn pages_are_unique() {
        let mut seen = HashSet::new();
        let mut dupes = Vec::new();
        for &url in PAGES {
            if !seen.insert(url) {
                dupes.push(url);
            }
        }
        assert!(dupes.is_empty(), "Duplicate entries in PAGES: {:?}", dupes);
    }

    #[test]
    fn filter_accepts_full_urls_and_www() {
        let mut filter = HashSet::new();
        filter.insert(normalize_page_name("https://www.w3.org").unwrap());
        let selected = selected_pages(Some(&filter));
        assert!(selected.contains(&"https://w3.org"));
    }

    #[test]
    fn write_cached_html_persists_meta_when_present() {
        let dir = tempfile::tempdir().expect("temp dir");
        let cache_path = dir.path().join("page.html");

        write_cached_html(
            &cache_path,
            b"hello",
            Some("text/html; charset=utf-8"),
            Some("https://example.com/page"),
        )
        .expect("write ok");

        let html = std::fs::read_to_string(&cache_path).expect("html read");
        assert_eq!(html, "hello");

        let meta_path = cache_path.with_extension("html.meta");
        let meta = std::fs::read_to_string(meta_path).expect("meta read");
        assert!(meta.contains("content-type: text/html; charset=utf-8"));
        assert!(meta.contains("url: https://example.com/page"));
    }

    #[test]
    fn write_cached_html_removes_meta_when_absent() {
        let dir = tempfile::tempdir().expect("temp dir");
        let cache_path = dir.path().join("page.html");
        let meta_path = cache_path.with_extension("html.meta");

        // Pre-populate a meta file to ensure it gets removed.
        std::fs::create_dir_all(dir.path()).unwrap();
        std::fs::write(&cache_path, "stale").unwrap();
        std::fs::write(&meta_path, "old").unwrap();

        write_cached_html(&cache_path, b"hello", None, None).expect("write ok");

        let html = std::fs::read_to_string(&cache_path).expect("html read");
        assert_eq!(html, "hello");
        assert!(!meta_path.exists(), "meta should be removed when content type absent");
    }

    #[test]
    fn fetch_page_rejects_empty_body() {
        use std::io::Write;
        use std::net::TcpListener;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind test server");
        let addr = listener.local_addr().unwrap();

        let handle = std::thread::spawn(move || {
            if let Some(stream) = listener.incoming().next() {
                let mut stream = stream.unwrap();
                let headers = b"HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: 0\r\n\r\n";
                let _ = stream.write_all(headers);
            }
        });

        let url = format!("http://{}", addr);
        let result = fetch_page(
            &url,
            Duration::from_secs(5),
            DEFAULT_USER_AGENT,
            DEFAULT_ACCEPT_LANGUAGE,
        );
        assert!(result.is_err(), "empty bodies should be treated as failures");
        handle.join().unwrap();
    }

    #[test]
    fn fetch_page_sets_accept_language_header() {
        use std::io::{Read, Write};
        use std::net::TcpListener;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind test server");
        let addr = listener.local_addr().unwrap();

        let handle = std::thread::spawn(move || {
            if let Some(stream) = listener.incoming().next() {
                let mut stream = stream.unwrap();
                let mut buf = [0u8; 1024];
                let _ = stream.read(&mut buf);
                let req = String::from_utf8_lossy(&buf).to_lowercase();
                assert!(req.contains("accept-language: es-mx,es;q=0.8"));

                let body = b"ok";
                let headers = format!(
                    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(headers.as_bytes());
                let _ = stream.write_all(body);
            }
        });

        let url = format!("http://{}/", addr);
        let (bytes, _ct, final_url) =
            fetch_page(&url, Duration::from_secs(5), DEFAULT_USER_AGENT, "es-MX,es;q=0.8").expect("fetch succeeds");
        handle.join().unwrap();

        assert_eq!(bytes, b"ok");
        assert_eq!(final_url, url);
    }

    #[test]
    fn fetch_page_follows_meta_refresh() {
        use std::io::{Read, Write};
        use std::net::TcpListener;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind test server");
        let addr = listener.local_addr().unwrap();

        let handle = std::thread::spawn(move || {
            let mut iter = listener.incoming();

            // First response: meta refresh to /next
            if let Some(stream) = iter.next() {
                let mut stream = stream.unwrap();
                let _ = stream.read(&mut [0u8; 1024]);
                let body = b"<meta http-equiv=\"refresh\" content=\"0;url=/next\">";
                let headers = format!(
                    "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(headers.as_bytes());
                let _ = stream.write_all(body);
            }

            // Second response: final content
            if let Some(stream) = iter.next() {
                let mut stream = stream.unwrap();
                let mut buf = [0u8; 1024];
                let _ = stream.read(&mut buf);
                let req = String::from_utf8_lossy(&buf);
                assert!(req.starts_with("GET /next"), "unexpected redirect path: {req}");

                let body = b"refreshed";
                let headers = format!(
                    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(headers.as_bytes());
                let _ = stream.write_all(body);
            }
        });

        let url = format!("http://{}/", addr);
        let (bytes, content_type, final_url) = fetch_page(
            &url,
            Duration::from_secs(5),
            DEFAULT_USER_AGENT,
            DEFAULT_ACCEPT_LANGUAGE,
        )
        .expect("fetch succeeds");
        handle.join().unwrap();

        assert_eq!(bytes, b"refreshed");
        assert_eq!(content_type.as_deref(), Some("text/plain"));
        assert_eq!(final_url, format!("http://{}/next", addr));
    }

    #[test]
    fn fetch_page_follows_js_redirect() {
        use std::io::{Read, Write};
        use std::net::TcpListener;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind test server");
        let addr = listener.local_addr().unwrap();

        let handle = std::thread::spawn(move || {
            let mut iter = listener.incoming();

            // First response: JS redirect to /js
            if let Some(stream) = iter.next() {
                let mut stream = stream.unwrap();
                let _ = stream.read(&mut [0u8; 1024]);
                let body = b"<script>window.location.href='/js'</script>";
                let headers = format!(
                    "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(headers.as_bytes());
                let _ = stream.write_all(body);
            }

            // Second response: final content
            if let Some(stream) = iter.next() {
                let mut stream = stream.unwrap();
                let mut buf = [0u8; 1024];
                let _ = stream.read(&mut buf);
                let req = String::from_utf8_lossy(&buf);
                assert!(req.starts_with("GET /js"), "unexpected redirect path: {req}");

                let body = b"redirected";
                let headers = format!(
                    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(headers.as_bytes());
                let _ = stream.write_all(body);
            }
        });

        let url = format!("http://{}/", addr);
        let (bytes, content_type, final_url) = fetch_page(
            &url,
            Duration::from_secs(5),
            DEFAULT_USER_AGENT,
            DEFAULT_ACCEPT_LANGUAGE,
        )
        .expect("fetch succeeds");
        handle.join().unwrap();

        assert_eq!(bytes, b"redirected");
        assert_eq!(content_type.as_deref(), Some("text/plain"));
        assert_eq!(final_url, format!("http://{}/js", addr));
    }

    #[test]
    fn fetch_page_keeps_original_when_js_redirect_fails() {
        use std::io::{Read, Write};
        use std::net::TcpListener;

        let listener = TcpListener::bind("127.0.0.1:0").expect("bind test server");
        let addr = listener.local_addr().unwrap();

        let handle = std::thread::spawn(move || {
            let mut iter = listener.incoming();

            // First response: JS redirect to /missing
            if let Some(stream) = iter.next() {
                let mut stream = stream.unwrap();
                let _ = stream.read(&mut [0u8; 1024]);
                let body = b"<script>window.location.href='/missing'</script>";
                let headers = format!(
                    "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(headers.as_bytes());
                let _ = stream.write_all(body);
            }

            // Second response: 404 for /missing
            if let Some(stream) = iter.next() {
                let mut stream = stream.unwrap();
                let mut buf = [0u8; 1024];
                let _ = stream.read(&mut buf);
                let req = String::from_utf8_lossy(&buf);
                assert!(req.starts_with("GET /missing"), "unexpected path: {req}");

                let body = b"not found";
                let headers = format!(
                    "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: {}\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(headers.as_bytes());
                let _ = stream.write_all(body);
            }
        });

        let url = format!("http://{}/", addr);
        let (bytes, content_type, final_url) = fetch_page(
            &url,
            Duration::from_secs(5),
            DEFAULT_USER_AGENT,
            DEFAULT_ACCEPT_LANGUAGE,
        )
        .expect("fetch succeeds");
        handle.join().unwrap();

        assert_eq!(bytes, b"<script>window.location.href='/missing'</script>");
        assert_eq!(content_type.as_deref(), Some("text/html"));
        assert_eq!(final_url, url);
    }
}
