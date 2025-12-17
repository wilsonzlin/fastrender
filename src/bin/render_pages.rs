//! Render all cached pages in parallel
//!
//! Usage: render_pages [--jobs N]
//!
//! Renders all HTML files in fetches/html/ to fetches/renders/
//! Logs per-page to fetches/renders/{name}.log
//! Summary to fetches/renders/_summary.log

mod caching_fetcher;

use caching_fetcher::CachingFetcher;
use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::{
    absolutize_css_urls, extract_css_links, extract_embedded_css_urls, infer_base_url, inject_css_into_html,
    inline_imports,
};
use fastrender::resource::HttpFetcher;
use fastrender::resource::ResourceFetcher;
use fastrender::FastRender;
use std::collections::HashSet;
use std::fs;
use std::panic::AssertUnwindSafe;
use std::path::PathBuf;
use std::sync::mpsc::{channel, RecvTimeoutError};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};

const HTML_DIR: &str = "fetches/html";
const ASSET_DIR: &str = "fetches/assets";
const RENDER_DIR: &str = "fetches/renders";
const RENDER_STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

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
    let mut args = std::env::args().skip(1);
    let mut jobs = num_cpus::get();
    let mut page_filter: Option<HashSet<String>> = None;
    let mut timeout_secs: Option<u64> = None;
    let mut viewport: Option<(u32, u32)> = None;
    let mut device_pixel_ratio: f32 = 1.0;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--jobs" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse() {
                        jobs = parsed;
                    }
                }
            }
            "--timeout" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse() {
                        timeout_secs = Some(parsed);
                    }
                }
            }
            "--viewport" => {
                if let Some(val) = args.next() {
                    let mut parts = val.split('x');
                    if let (Some(w), Some(h)) = (parts.next(), parts.next()) {
                        if let (Ok(w), Ok(h)) = (w.parse::<u32>(), h.parse::<u32>()) {
                            viewport = Some((w, h));
                        }
                    }
                }
            }
            "--dpr" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse::<f32>() {
                        if parsed.is_finite() && parsed > 0.0 {
                            device_pixel_ratio = parsed;
                        }
                    }
                }
            }
            "--pages" => {
                if let Some(val) = args.next() {
                    let mut filter = page_filter.take().unwrap_or_default();
                    for name in val.split(',') {
                        let trimmed = name.trim();
                        if !trimmed.is_empty() {
                            filter.insert(trimmed.to_string());
                        }
                    }
                    page_filter = Some(filter);
                }
            }
            _ => {}
        }
    }

    // Create directories
    fs::create_dir_all(RENDER_DIR).expect("create render dir");
    fs::create_dir_all(ASSET_DIR).expect("create asset dir");

    // Find all cached HTML files
    let entries: Vec<_> = match fs::read_dir(HTML_DIR) {
        Ok(dir) => dir
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().map(|x| x == "html").unwrap_or(false))
            .filter(|e| {
                if let Some(filter) = &page_filter {
                    if let Some(stem_os) = e.path().file_stem() {
                        if let Some(stem) = stem_os.to_str() {
                            return filter.contains(stem);
                        }
                    }
                    false
                } else {
                    true
                }
            })
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
                let (viewport_w, viewport_h) = viewport.unwrap_or((1200, 800));

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
                log.push_str(&format!("Viewport: {}x{}\n", viewport_w, viewport_h));

                let input_url = format!("file://{}", path.display());
                let resource_base = infer_base_url(&html, &input_url).into_owned();

                let mut css_links = extract_css_links(&html, &resource_base);
                let mut seen_links: HashSet<String> = css_links.iter().cloned().collect();
                for extra in extract_embedded_css_urls(&html, &resource_base) {
                    if seen_links.insert(extra.clone()) {
                        css_links.push(extra);
                    }
                }

                let mut combined_css = String::new();
                let mut seen_imports = HashSet::new();
                for css_url in css_links {
                    seen_imports.insert(css_url.clone());
                    match fetcher.fetch(&css_url) {
                        Ok(res) => {
                            let css_text = decode_css_bytes(&res.bytes, res.content_type.as_deref());
                            let rewritten = absolutize_css_urls(&css_text, &css_url);
                            let inlined = inline_imports(
                                &rewritten,
                                &css_url,
                                &|u| {
                                    fetcher
                                        .fetch(u)
                                        .map(|res| decode_css_bytes(&res.bytes, res.content_type.as_deref()))
                                },
                                &mut seen_imports,
                            );
                            combined_css.push_str(&inlined);
                            combined_css.push('\n');
                        }
                        Err(err) => {
                            log.push_str(&format!("CSS fetch error {}: {}\n", css_url, err));
                        }
                    }
                }
                if !combined_css.is_empty() {
                    log.push_str(&format!(
                        "Inlined CSS: {} bytes from {} link(s)\n",
                        combined_css.len(),
                        seen_links.len()
                    ));
                }

                let html_for_render = if combined_css.is_empty() {
                    html
                } else {
                    inject_css_into_html(&html, &combined_css)
                };

                let html_for_render = html_for_render.clone();
                let resource_base = resource_base.clone();
                let worker_name = name.clone();
                let panic_to_string = |panic: Box<dyn std::any::Any + Send + 'static>| -> String {
                    panic
                        .downcast_ref::<&str>()
                        .map(|s| s.to_string())
                        .or_else(|| panic.downcast_ref::<String>().cloned())
                        .unwrap_or_else(|| "unknown panic".to_string())
                };
                let run_render = move || -> Result<Vec<u8>, Status> {
                    let render_work = move || -> Result<Vec<u8>, fastrender::Error> {
                        let mut renderer = FastRender::builder()
                            .fetcher(fetcher as Arc<dyn ResourceFetcher>)
                            .base_url(resource_base.clone())
                            .device_pixel_ratio(device_pixel_ratio)
                            .build()
                            .expect("create renderer");
                        renderer.render_to_png(&html_for_render, viewport_w, viewport_h)
                    };

                    let (tx, rx) = channel();
                    thread::Builder::new()
                        .name(format!("render-pages-worker-{worker_name}"))
                        .stack_size(RENDER_STACK_SIZE)
                        .spawn(move || {
                            let result = std::panic::catch_unwind(AssertUnwindSafe(render_work));
                            let _ = tx.send(result);
                        })
                        .expect("spawn render worker");

                    if let Some(secs) = timeout_secs {
                        match rx.recv_timeout(Duration::from_secs(secs)) {
                            Ok(Ok(png)) => png.map_err(|e| Status::Error(e.to_string())),
                            Ok(Err(panic)) => Err(Status::Crash(panic_to_string(panic))),
                            Err(RecvTimeoutError::Timeout) => {
                                Err(Status::Crash(format!("render timed out after {}s", secs)))
                            }
                            Err(RecvTimeoutError::Disconnected) => {
                                Err(Status::Crash("render worker disconnected".to_string()))
                            }
                        }
                    } else {
                        match rx.recv() {
                            Ok(Ok(png)) => png.map_err(|e| Status::Error(e.to_string())),
                            Ok(Err(panic)) => Err(Status::Crash(panic_to_string(panic))),
                            Err(_) => Err(Status::Crash("render worker disconnected".to_string())),
                        }
                    }
                };

                let result = run_render();

                let elapsed = page_start.elapsed();
                let time_ms = elapsed.as_millis();

                let (status, size) = match result {
                    Ok(png_data) => {
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
                    Err(status) => match status {
                        Status::Error(msg) => {
                            log.push_str(&format!("Time: {}ms\n", time_ms));
                            log.push_str("Status: ERROR\n");
                            log.push_str(&format!("Error: {}\n", msg));
                            println!("✗ {} ERROR: {} ({}ms)", name, msg, time_ms);
                            (Status::Error(msg), None)
                        }
                        Status::Crash(msg) => {
                            log.push_str(&format!("Time: {}ms\n", time_ms));
                            log.push_str("Status: CRASH\n");
                            log.push_str(&format!("Panic: {}\n", msg));

                            let short: String = msg.chars().take(50).collect();
                            println!("✗ {} CRASH: {} ({}ms)", name, short, time_ms);
                            (Status::Crash(msg), None)
                        }
                        Status::Ok => (Status::Error("unexpected ok status".to_string()), None),
                    },
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
    let pass = results.iter().filter(|r| matches!(r.status, Status::Ok)).count();
    let crash = results.iter().filter(|r| matches!(r.status, Status::Crash(_))).count();
    let error = results.iter().filter(|r| matches!(r.status, Status::Error(_))).count();

    // Write summary log
    let summary_path = PathBuf::from(RENDER_DIR).join("_summary.log");
    let mut summary = String::new();
    summary.push_str("=== Render Summary ===\n");
    summary.push_str(&format!("Date: {}\n", chrono::Local::now().format("%Y-%m-%d %H:%M:%S")));
    summary.push_str(&format!("Total time: {:.1}s\n", total_elapsed.as_secs_f64()));
    summary.push_str(&format!(
        "Pages: {} total, {} pass, {} crash, {} error\n\n",
        results.len(),
        pass,
        crash,
        error
    ));

    summary.push_str(&format!("{:<40} {:>8} {:>10} {}\n", "PAGE", "TIME", "SIZE", "STATUS"));
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
