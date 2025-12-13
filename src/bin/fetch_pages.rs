//! Fetch and cache HTML pages for testing
//!
//! Usage: fetch_pages [--refresh]
//!
//! Fetches all target pages in parallel and caches to fetches/html/

use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

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
    "https://stackoverflow.com",
    "https://reddit.com",
    "https://twitter.com",
    "https://amazon.com",
    "https://youtube.com",
    "https://facebook.com",
    "https://linkedin.com",
    "https://microsoft.com",
    "https://apple.com",
    "https://nytimes.com",
    "https://bbc.com",
    "https://cnn.com",
    "https://espn.com",
    "https://imdb.com",
    "https://craigslist.org",
    "https://pinterest.com",
    "https://medium.com",
    "https://quora.com",
    "https://twitch.tv",
    "https://dropbox.com",
    "https://shopify.com",
    "https://etsy.com",
    "https://ebay.com",
    "https://walmart.com",
    "https://airbnb.com",
    "https://booking.com",
    "https://yelp.com",
    "https://spotify.com",
    "https://reuters.com",
    "https://wsj.com",
    "https://bloomberg.com",
    "https://forbes.com",
    "https://techcrunch.com",
    "https://theverge.com",
    "https://wired.com",
    "https://arstechnica.com",
    "https://cnet.com",
    "https://howtogeek.com",
    "https://macrumors.com",
];

fn url_to_filename(url: &str) -> String {
    url.trim_start_matches("https://")
        .trim_start_matches("http://")
        .replace('/', "_")
        .chars()
        .map(|c| if c.is_alphanumeric() || c == '.' || c == '_' || c == '-' { c } else { '_' })
        .collect()
}

fn fetch_page(url: &str) -> Result<Vec<u8>, String> {
    let config = ureq::Agent::config_builder()
        .timeout_global(Some(Duration::from_secs(30)))
        .build();
    let agent: ureq::Agent = config.into();

    let mut response = agent
        .get(url)
        .header("User-Agent", "Mozilla/5.0 (compatible; fastrender/0.1)")
        .call()
        .map_err(|e| e.to_string())?;

    response
        .body_mut()
        .read_to_vec()
        .map_err(|e| e.to_string())
}

fn main() {
    let refresh = std::env::args().any(|a| a == "--refresh");
    
    fs::create_dir_all(CACHE_DIR).expect("create cache dir");
    
    println!("Fetching {} pages (parallel)...", PAGES.len());
    if refresh {
        println!("--refresh: re-fetching all");
    }
    println!();

    let success = Arc::new(AtomicUsize::new(0));
    let failed = Arc::new(AtomicUsize::new(0));
    let skipped = Arc::new(AtomicUsize::new(0));

    std::thread::scope(|s| {
        for url in PAGES {
            let success = Arc::clone(&success);
            let failed = Arc::clone(&failed);
            let skipped = Arc::clone(&skipped);
            
            s.spawn(move || {
                let filename = url_to_filename(url);
                let cache_path = PathBuf::from(CACHE_DIR).join(format!("{}.html", filename));
                
                // Skip if cached and not refreshing
                if !refresh && cache_path.exists() {
                    skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }
                
                match fetch_page(url) {
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
