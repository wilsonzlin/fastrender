//! Render all cached pages in parallel
//!
//! Usage: render_pages [--jobs N]
//!
//! Renders all HTML files in fetches/html/ to fetches/renders/
//! Logs per-page to fetches/renders/{name}.log
//! Summary to fetches/renders/_summary.log

mod caching_fetcher;

use caching_fetcher::CachingFetcher;
use fastrender::resource::HttpFetcher;
use fastrender::FastRender;
use std::fs;
use std::panic::AssertUnwindSafe;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Instant;

const HTML_DIR: &str = "fetches/html";
const ASSET_DIR: &str = "fetches/assets";
const RENDER_DIR: &str = "fetches/renders";

struct PageResult {
    name: String,
    status: Status,
    time_ms: u128,
    size: Option<usize>,
}

enum Status {
    Ok,
    Crash(String),
    Error(String),
}

fn main() {
    let jobs: usize = std::env::args()
        .skip_while(|a| a != "--jobs")
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(num_cpus::get);

    // Create directories
    fs::create_dir_all(RENDER_DIR).expect("create render dir");
    fs::create_dir_all(ASSET_DIR).expect("create asset dir");

    // Find all cached HTML files
    let entries: Vec<_> = match fs::read_dir(HTML_DIR) {
        Ok(dir) => dir
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map(|x| x == "html").unwrap_or(false))
            .collect(),
        Err(_) => {
            println!("No cached pages found in {}.", HTML_DIR);
            println!("Run fetch_pages first.");
            return;
        }
    };

    if entries.is_empty() {
        println!("No cached pages in {}. Run fetch_pages first.", HTML_DIR);
        return;
    }

    // Create shared caching fetcher
    let fetcher = Arc::new(CachingFetcher::new(HttpFetcher::new(), ASSET_DIR));

    println!("Rendering {} pages ({} parallel)...", entries.len(), jobs);
    println!();

    let start = Instant::now();
    let results: Mutex<Vec<PageResult>> = Mutex::new(Vec::new());

    // Use a thread pool with limited concurrency
    let pool = rayon::ThreadPoolBuilder::new()
        .num_threads(jobs)
        .build()
        .expect("create thread pool");

    pool.scope(|s| {
        for entry in &entries {
            let results = &results;
            let path = entry.path();
            let fetcher = Arc::clone(&fetcher);

            s.spawn(move |_| {
                let name = path.file_stem().unwrap().to_string_lossy().to_string();
                let output_path = PathBuf::from(RENDER_DIR).join(format!("{}.png", name));
                let log_path = PathBuf::from(RENDER_DIR).join(format!("{}.log", name));

                let page_start = Instant::now();
                let mut log = String::new();

                log.push_str(&format!("=== {} ===\n", name));
                log.push_str(&format!("Source: {}\n", path.display()));
                log.push_str(&format!("Output: {}\n\n", output_path.display()));

                // Read HTML first (before catch_unwind)
                let html = match fs::read_to_string(&path) {
                    Ok(h) => h,
                    Err(e) => {
                        log.push_str(&format!("Read error: {}\n", e));
                        let _ = fs::write(&log_path, &log);
                        results.lock().unwrap().push(PageResult {
                            name,
                            status: Status::Error(format!("read: {}", e)),
                            time_ms: 0,
                            size: None,
                        });
                        return;
                    }
                };
                log.push_str(&format!("HTML size: {} bytes\n", html.len()));

                // Catch panics during render
                let result = std::panic::catch_unwind(AssertUnwindSafe(|| {
                    let mut renderer = FastRender::builder()
                        .fetcher(fetcher as Arc<dyn fastrender::ResourceFetcher>)
                        .base_url(format!("file://{}", path.display()))
                        .build()
                        .expect("create renderer");
                    renderer.render_to_png(&html, 1200, 800)
                }));

                let elapsed = page_start.elapsed();
                let time_ms = elapsed.as_millis();

                let (status, size) = match result {
                    Ok(Ok(png_data)) => {
                        let size = png_data.len();
                        log.push_str(&format!("PNG size: {} bytes\n", size));
                        log.push_str(&format!("Time: {}ms\n", time_ms));
                        log.push_str("Status: OK\n");

                        if let Err(e) = fs::write(&output_path, &png_data) {
                            log.push_str(&format!("Write error: {}\n", e));
                            (Status::Error(format!("write: {}", e)), None)
                        } else {
                            println!("✓ {} ({}b, {}ms)", name, size, time_ms);
                            (Status::Ok, Some(size))
                        }
                    }
                    Ok(Err(e)) => {
                        let msg = e.to_string();
                        log.push_str(&format!("Time: {}ms\n", time_ms));
                        log.push_str("Status: ERROR\n");
                        log.push_str(&format!("Error: {}\n", msg));
                        println!("✗ {} ERROR: {} ({}ms)", name, msg, time_ms);
                        (Status::Error(msg), None)
                    }
                    Err(panic) => {
                        let msg = panic
                            .downcast_ref::<&str>()
                            .map(|s| s.to_string())
                            .or_else(|| panic.downcast_ref::<String>().cloned())
                            .unwrap_or_else(|| "unknown panic".to_string());

                        log.push_str(&format!("Time: {}ms\n", time_ms));
                        log.push_str("Status: CRASH\n");
                        log.push_str(&format!("Panic: {}\n", msg));

                        let short: String = msg.chars().take(50).collect();
                        println!("✗ {} CRASH: {} ({}ms)", name, short, time_ms);
                        (Status::Crash(msg), None)
                    }
                };

                // Write per-page log
                let _ = fs::write(&log_path, &log);

                results.lock().unwrap().push(PageResult {
                    name,
                    status,
                    time_ms,
                    size,
                });
            });
        }
    });

    let total_elapsed = start.elapsed();
    let mut results = results.into_inner().unwrap();
    results.sort_by(|a, b| a.name.cmp(&b.name));

    // Count results
    let pass = results
        .iter()
        .filter(|r| matches!(r.status, Status::Ok))
        .count();
    let crash = results
        .iter()
        .filter(|r| matches!(r.status, Status::Crash(_)))
        .count();
    let error = results
        .iter()
        .filter(|r| matches!(r.status, Status::Error(_)))
        .count();

    // Write summary log
    let summary_path = PathBuf::from(RENDER_DIR).join("_summary.log");
    let mut summary = String::new();
    summary.push_str("=== Render Summary ===\n");
    summary.push_str(&format!(
        "Date: {}\n",
        chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
    ));
    summary.push_str(&format!("Total time: {:.1}s\n", total_elapsed.as_secs_f64()));
    summary.push_str(&format!(
        "Pages: {} total, {} pass, {} crash, {} error\n\n",
        results.len(),
        pass,
        crash,
        error
    ));

    summary.push_str(&format!(
        "{:<40} {:>8} {:>10} {}\n",
        "PAGE", "TIME", "SIZE", "STATUS"
    ));
    summary.push_str(&format!("{}\n", "-".repeat(75)));

    for r in &results {
        let status_str = match &r.status {
            Status::Ok => "OK".to_string(),
            Status::Crash(msg) => format!("CRASH: {}", msg.chars().take(30).collect::<String>()),
            Status::Error(msg) => format!("ERROR: {}", msg.chars().take(30).collect::<String>()),
        };
        let size_str = r.size.map(|s| format!("{}b", s)).unwrap_or("-".to_string());
        summary.push_str(&format!(
            "{:<40} {:>6}ms {:>10} {}\n",
            r.name, r.time_ms, size_str, status_str
        ));
    }

    summary.push_str(&format!("\n{}\n", "-".repeat(75)));
    summary.push_str(&format!("Total: {:.1}s\n", total_elapsed.as_secs_f64()));

    let _ = fs::write(&summary_path, &summary);

    // Print summary
    println!();
    println!(
        "Done in {:.1}s: ✓{} pass, ✗{} crash, ✗{} error",
        total_elapsed.as_secs_f64(),
        pass,
        crash,
        error
    );
    println!();
    println!("Renders: {}/", RENDER_DIR);
    println!("Summary: {}", summary_path.display());
    println!("Logs:    {}/<page>.log", RENDER_DIR);
    println!();
    println!("Inspect: open {}/*.png", RENDER_DIR);

    if crash > 0 || error > 0 {
        std::process::exit(1);
    }
}
