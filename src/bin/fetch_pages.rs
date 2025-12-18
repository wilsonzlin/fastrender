//! Fetch and cache HTML pages for testing
//!
//! Usage: fetch_pages [--refresh] [--jobs N] [--timeout SECONDS] [--pages a,b,c] [--user-agent UA]
//!
//! Fetches all target pages in parallel and caches to fetches/html/

use std::collections::HashSet;

use std::fs;
use std::path::PathBuf;
use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use fastrender::resource::{HttpFetcher, ResourceFetcher, DEFAULT_USER_AGENT};
use rayon::ThreadPoolBuilder;

struct Config {
    refresh: bool,
    jobs: usize,
    timeout: Duration,
    page_filter: Option<HashSet<String>>, // normalized via url_to_filename
    user_agent: String,
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
    "https://lite.cnn.com",
    "https://text.npr.org",
    "https://iana.org",
    // Tier 3: Modern
    "https://google.com",
    "https://duckduckgo.com",
    "https://wikipedia.org",
    // Tier 4: Complex
    "https://github.com",
    "https://gitlab.com",
    "https://stackoverflow.com",
    "https://reddit.com",
    "https://twitter.com",
    "https://weibo.cn",
    "https://alibaba.com",
    "https://amazon.com",
    "https://youtube.com",
    "https://facebook.com",
    "https://linkedin.com",
    "https://microsoft.com",
    "https://apple.com",
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
    "https://techcrunch.com",
    "https://theverge.com",
    "https://wired.com",
    "https://arstechnica.com",
    "https://ft.com",
    "https://cnet.com",
    "https://mozilla.org",
    "https://developer.mozilla.org",
    "https://howtogeek.com",
    "https://macrumors.com",
    "https://washingtonpost.com",
    "https://abcnews.go.com",
    "https://washington.edu",
    "https://berkeley.edu",
    "https://fandom.com",
    "https://ikea.com",
    "https://elpais.com",
    "https://ndtv.com",
    "https://stackexchange.com",
    "https://rust-lang.org",
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
    "https://sqlite.org",
    "https://nginx.org",
    "https://go.dev",
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
        Some(url_to_filename(trimmed))
    }
}

fn parse_args() -> Config {
    let mut refresh = false;
    let mut jobs = num_cpus::get();
    let mut timeout_secs: u64 = 30;
    let mut filter: HashSet<String> = HashSet::new();
    let mut has_filter = false;
    let mut user_agent = DEFAULT_USER_AGENT.to_string();

    let mut args = std::env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                println!("Usage: fetch_pages [--refresh] [--jobs N] [--timeout SECONDS] [--pages a,b,c]");
                println!("\nOptions:");
                println!("  --refresh           Re-fetch all pages even if cached");
                println!("  --jobs N            Number of parallel fetches (default: num_cpus)");
                println!("  --timeout SECONDS   Per-request timeout (default: 30)");
                println!("  --pages a,b,c       Fetch only the listed pages (use url_to_filename stems)");
                println!("  --user-agent UA     Override the User-Agent header (default: Chrome-like)");
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
            _ => {}
        }
    }

    Config {
        refresh,
        jobs,
        timeout: Duration::from_secs(timeout_secs),
        page_filter: if has_filter { Some(filter) } else { None },
        user_agent,
    }
}

fn selected_pages(filter: Option<&HashSet<String>>) -> Vec<&'static str> {
    PAGES
        .iter()
        .copied()
        .filter(|url| match filter {
            Some(names) => names.contains(&url_to_filename(url)),
            None => true,
        })
        .collect()
}

fn write_cached_html(cache_path: &Path, bytes: &[u8], content_type: Option<&str>) -> std::io::Result<()> {
    if let Some(parent) = cache_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    std::fs::write(cache_path, bytes)?;

    let meta_path = cache_path.with_extension("html.meta");
    match content_type {
        Some(ct) if !ct.is_empty() => {
            let _ = std::fs::write(meta_path, ct);
        }
        _ => {
            let _ = std::fs::remove_file(meta_path);
        }
    }

    Ok(())
}

fn fetch_page(url: &str, timeout: Duration, user_agent: &str) -> Result<(Vec<u8>, Option<String>), String> {
    let fetcher = HttpFetcher::default()
        .with_timeout(timeout)
        .with_user_agent(user_agent.to_string());
    fetcher
        .fetch(url)
        .map(|res| (res.bytes, res.content_type))
        .map_err(|e| e.to_string())
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
        let matched: HashSet<_> = selected.iter().map(|u| url_to_filename(u)).collect();
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
    if config.refresh {
        println!("--refresh: re-fetching all");
    }
    println!();

    let success = Arc::new(AtomicUsize::new(0));
    let failed = Arc::new(AtomicUsize::new(0));
    let skipped = Arc::new(AtomicUsize::new(0));

    let pool = ThreadPoolBuilder::new()
        .num_threads(config.jobs)
        .build()
        .expect("create thread pool");

    pool.scope(|s| {
        for &url in &selected {
            let success = Arc::clone(&success);
            let failed = Arc::clone(&failed);
            let skipped = Arc::clone(&skipped);
            let refresh = config.refresh;
            let timeout = config.timeout;
            let user_agent = config.user_agent.clone();

            s.spawn(move |_| {
                let filename = url_to_filename(url);
                let cache_path = PathBuf::from(CACHE_DIR).join(format!("{}.html", filename));

                // Skip if cached and not refreshing
                if !refresh && cache_path.exists() {
                    skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }

                match fetch_page(url, timeout, &user_agent) {
                    Ok((bytes, content_type)) => {
                        if write_cached_html(&cache_path, &bytes, content_type.as_deref()).is_ok() {
                            println!("✓ {} ({}b)", url, bytes.len());
                            success.fetch_add(1, Ordering::Relaxed);
                            return;
                        }
                        println!("✗ {} (write failed)", url);
                        failed.fetch_add(1, Ordering::Relaxed);
                    }
                    Err(e) => {
                        println!("✗ {} ({})", url, e);
                        failed.fetch_add(1, Ordering::Relaxed);
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
    println!("Cache: {}/", CACHE_DIR);
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn write_cached_html_persists_meta_when_present() {
        let dir = tempfile::tempdir().expect("temp dir");
        let cache_path = dir.path().join("page.html");

        write_cached_html(&cache_path, b"hello", Some("text/html; charset=utf-8")).expect("write ok");

        let html = std::fs::read_to_string(&cache_path).expect("html read");
        assert_eq!(html, "hello");

        let meta_path = cache_path.with_extension("html.meta");
        let meta = std::fs::read_to_string(meta_path).expect("meta read");
        assert_eq!(meta, "text/html; charset=utf-8");
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

        write_cached_html(&cache_path, b"hello", None).expect("write ok");

        let html = std::fs::read_to_string(&cache_path).expect("html read");
        assert_eq!(html, "hello");
        assert!(!meta_path.exists(), "meta should be removed when content type absent");
    }
}
