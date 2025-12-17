//! Fetch and cache HTML pages for testing
//!
//! Usage: fetch_pages [--refresh] [--jobs N] [--timeout SECONDS] [--pages a,b,c]
//!
//! Fetches all target pages in parallel and caches to fetches/html/

use std::collections::HashSet;

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use rayon::ThreadPoolBuilder;

struct Config {
    refresh: bool,
    jobs: usize,
    timeout: Duration,
    page_filter: Option<HashSet<String>>, // normalized via url_to_filename
}

const CACHE_DIR: &str = "fetches/html";

// Target pages for testing
const PAGES: &[&str] = &[
    // Tier 1: Simple
    "https://example.com",
    "https://example.org",
    // Tier 2: Text-heavy
    "https://news.ycombinator.com",
    "https://lite.cnn.com",
    "https://text.npr.org",
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
    "https://amazon.com",
    "https://youtube.com",
    "https://facebook.com",
    "https://linkedin.com",
    "https://microsoft.com",
    "https://apple.com",
    "https://icloud.com",
    "https://nytimes.com",
    "https://bbc.com",
    "https://nhk.or.jp",
    "https://cnn.com",
    "https://theguardian.com",
    "https://espn.com",
    "https://imdb.com",
    "https://craigslist.org",
    "https://pinterest.com",
    "https://medium.com",
    "https://quora.com",
    "https://twitch.tv",
    "https://dropbox.com",
    "https://stripe.com",
    "https://shopify.com",
    "https://weebly.com",
    "https://etsy.com",
    "https://ebay.com",
    "https://walmart.com",
    "https://airbnb.com",
    "https://booking.com",
    "https://yelp.com",
    "https://spotify.com",
    "https://reuters.com",
    "https://netflix.com",
    "https://wsj.com",
    "https://bloomberg.com",
    "https://forbes.com",
    "https://techcrunch.com",
    "https://theverge.com",
    "https://wired.com",
    "https://arstechnica.com",
    "https://cnet.com",
    "https://mozilla.org",
    "https://howtogeek.com",
    "https://macrumors.com",
    "https://washingtonpost.com",
    "https://fandom.com",
    "https://ikea.com",
    "https://elpais.com",
    "https://ndtv.com",
    "https://stackexchange.com",
    "https://rust-lang.org",
    "https://tesco.com",
    "https://bing.com",
    "https://discord.com",
    "https://weather.com",
    "https://bbc.co.uk",
    "https://cloudflare.com",
    "https://aliexpress.com",
    "https://apnews.com",
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
            _ => {}
        }
    }

    Config {
        refresh,
        jobs,
        timeout: Duration::from_secs(timeout_secs),
        page_filter: if has_filter { Some(filter) } else { None },
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

fn fetch_page(url: &str, timeout: Duration) -> Result<Vec<u8>, String> {
    let config = ureq::Agent::config_builder().timeout_global(Some(timeout)).build();
    let agent: ureq::Agent = config.into();

    let mut response = agent
        .get(url)
        .header("User-Agent", "Mozilla/5.0 (compatible; fastrender/0.1)")
        .call()
        .map_err(|e| e.to_string())?;

    response.body_mut().read_to_vec().map_err(|e| e.to_string())
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

            s.spawn(move |_| {
                let filename = url_to_filename(url);
                let cache_path = PathBuf::from(CACHE_DIR).join(format!("{}.html", filename));

                // Skip if cached and not refreshing
                if !refresh && cache_path.exists() {
                    skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }

                match fetch_page(url, timeout) {
                    Ok(bytes) => {
                        if let Ok(mut f) = fs::File::create(&cache_path) {
                            if f.write_all(&bytes).is_ok() {
                                println!("✓ {} ({}b)", url, bytes.len());
                                success.fetch_add(1, Ordering::Relaxed);
                                return;
                            }
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
}
