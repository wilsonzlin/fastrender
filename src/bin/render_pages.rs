//! Render all cached pages in parallel
//!
//! Usage: render_pages [--jobs N] [--timeout SECONDS] [--viewport WxH] [--pages a,b,c] [--dpr FLOAT] [--scroll-x PX] [--scroll-y PX] [--prefers-reduced-transparency <value>] [--prefers-reduced-motion <value>] [--prefers-reduced-data <value>] [--prefers-contrast <value>] [--prefers-color-scheme <value>] [--user-agent UA] [--accept-language LANG] [--timings]
//!
//! Renders all HTML files in fetches/html/ to fetches/renders/
//! Logs per-page to fetches/renders/{name}.log
//! Summary to fetches/renders/_summary.log

mod caching_fetcher;

use caching_fetcher::CachingFetcher;
use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::resolve_href;
use fastrender::css::loader::{
    absolutize_css_urls, extract_css_links, extract_embedded_css_urls, infer_base_url, inject_css_into_html,
    inline_imports,
};
use fastrender::html::encoding::decode_html_bytes;
use fastrender::html::meta_refresh::{extract_js_location_redirect, extract_meta_refresh_url};
use fastrender::resource::{
    parse_cached_html_meta, HttpFetcher, ResourceFetcher, DEFAULT_ACCEPT_LANGUAGE, DEFAULT_USER_AGENT,
};
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

fn usage() {
    println!("Usage: render_pages [--jobs N] [--timeout SECONDS] [--viewport WxH] [--pages a,b,c] [--dpr FLOAT] [--scroll-x PX] [--scroll-y PX] [--prefers-reduced-transparency <value>] [--prefers-reduced-motion <value>] [--prefers-reduced-data <value>] [--prefers-contrast <value>] [--prefers-color-scheme <value>] [--user-agent UA] [--accept-language LANG] [--css-limit N] [--timings]");
    println!("  --jobs N          Number of parallel renders (default: num_cpus)");
    println!("  --timeout SECONDS Per-page timeout (optional)");
    println!("  --viewport WxH    Override viewport size for all pages (e.g., 1366x768; default 1200x800)");
    println!("  --pages a,b,c     Render only the listed cached pages (use cache stems like cnn.com)");
    println!("  --dpr FLOAT       Device pixel ratio for media queries/srcset (default: 1.0)");
    println!("  --prefers-reduced-transparency reduce|no-preference|true|false (overrides env)");
    println!("  --prefers-reduced-motion       reduce|no-preference|true|false (overrides env)");
    println!("  --prefers-reduced-data        reduce|no-preference|true|false (overrides env)");
    println!("  --prefers-contrast            more|high|less|low|custom|forced|no-preference (overrides env)");
    println!("  --prefers-color-scheme        light|dark|no-preference (overrides env)");
    println!("  --scroll-x PX     Horizontal scroll offset applied to rendering (default: 0)");
    println!("  --scroll-y PX     Vertical scroll offset applied to rendering (default: 0)");
    println!("  --user-agent UA   Override the User-Agent header (default: Chrome-like)");
    println!("  --accept-language LANG  Override the Accept-Language header (default: en-US,en;q=0.9)");
    println!("  --css-limit N     Maximum number of external stylesheets to fetch (default: unlimited)");
    println!("  --timings         Enable FASTR_RENDER_TIMINGS to print per-stage timings");
}

fn parse_prefers_reduced_transparency(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn parse_prefers_reduced_motion(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

fn parse_prefers_contrast(val: &str) -> Option<String> {
    let v = val.trim().to_ascii_lowercase();
    match v.as_str() {
        "more" | "high" | "less" | "low" | "custom" | "forced" | "no-preference" => Some(v),
        _ => None,
    }
}

fn parse_prefers_color_scheme(val: &str) -> Option<String> {
    let v = val.trim().to_ascii_lowercase();
    match v.as_str() {
        "light" | "dark" | "no-preference" => Some(v),
        _ => None,
    }
}

fn parse_prefers_reduced_data(val: &str) -> Option<bool> {
    let v = val.trim().to_ascii_lowercase();
    if matches!(
        v.as_str(),
        "1" | "true" | "yes" | "on" | "reduce" | "reduced" | "prefer"
    ) {
        return Some(true);
    }
    if matches!(v.as_str(), "0" | "false" | "no" | "off" | "none" | "no-preference") {
        return Some(false);
    }
    None
}

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
    let mut prefers_reduced_transparency: Option<bool> = None;
    let mut prefers_reduced_motion: Option<bool> = None;
    let mut prefers_reduced_data: Option<bool> = None;
    let mut prefers_contrast: Option<String> = None;
    let mut prefers_color_scheme: Option<String> = None;
    let mut scroll_x: f32 = 0.0;
    let mut scroll_y: f32 = 0.0;
    let mut user_agent = DEFAULT_USER_AGENT.to_string();
    let mut accept_language = DEFAULT_ACCEPT_LANGUAGE.to_string();
    let mut enable_timings = false;
    let mut css_limit: Option<usize> = None;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--help" | "-h" => {
                usage();
                return;
            }
            "--jobs" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse() {
                        jobs = parsed;
                    }
                }
            }
            "--timeout" => {
                if let Some(val) = args.next() {
                    match val.parse::<u64>() {
                        Ok(0) => timeout_secs = None,
                        Ok(secs) => timeout_secs = Some(secs),
                        Err(_) => {}
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
            "--prefers-reduced-transparency" => {
                if let Some(val) = args.next() {
                    prefers_reduced_transparency = parse_prefers_reduced_transparency(&val);
                }
            }
            "--prefers-reduced-motion" => {
                if let Some(val) = args.next() {
                    prefers_reduced_motion = parse_prefers_reduced_motion(&val);
                }
            }
            "--prefers-reduced-data" => {
                if let Some(val) = args.next() {
                    prefers_reduced_data = parse_prefers_reduced_data(&val);
                }
            }
            "--prefers-contrast" => {
                if let Some(val) = args.next() {
                    prefers_contrast = parse_prefers_contrast(&val);
                }
            }
            "--prefers-color-scheme" => {
                if let Some(val) = args.next() {
                    prefers_color_scheme = parse_prefers_color_scheme(&val);
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
            "--scroll-y" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse::<f32>() {
                        if parsed.is_finite() {
                            scroll_y = parsed;
                        }
                    }
                }
            }
            "--scroll-x" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse::<f32>() {
                        if parsed.is_finite() {
                            scroll_x = parsed;
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
            "--css-limit" => {
                if let Some(val) = args.next() {
                    if let Ok(parsed) = val.parse::<usize>() {
                        css_limit = Some(parsed);
                    }
                }
            }
            "--timings" => {
                enable_timings = true;
            }
            _ => {}
        }
    }

    if enable_timings {
        std::env::set_var("FASTR_RENDER_TIMINGS", "1");
    }

    if let Some(reduce) = prefers_reduced_transparency {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_TRANSPARENCY",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(reduce) = prefers_reduced_motion {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_MOTION",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(reduce) = prefers_reduced_data {
        std::env::set_var(
            "FASTR_PREFERS_REDUCED_DATA",
            if reduce { "reduce" } else { "no-preference" },
        );
    }

    if let Some(contrast) = prefers_contrast {
        std::env::set_var("FASTR_PREFERS_CONTRAST", contrast);
    }

    if let Some(color_scheme) = prefers_color_scheme {
        std::env::set_var("FASTR_PREFERS_COLOR_SCHEME", color_scheme);
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
    let fetcher = Arc::new(CachingFetcher::new(
        HttpFetcher::new()
            .with_user_agent(user_agent.clone())
            .with_accept_language(accept_language.clone()),
        ASSET_DIR,
    ));

    println!("Rendering {} pages ({} parallel)...", entries.len(), jobs);
    println!("User-Agent: {}", user_agent);
    println!("Accept-Language: {}", accept_language);
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
            let user_agent = user_agent.clone();

            s.spawn(move |_| {
                let name = path.file_stem().unwrap().to_string_lossy().to_string();
                let output_path = PathBuf::from(RENDER_DIR).join(format!("{}.png", name));
                let log_path = PathBuf::from(RENDER_DIR).join(format!("{}.log", name));
                let (viewport_w, viewport_h) = viewport.unwrap_or((1200, 800));

                let page_start = Instant::now();
                let mut log = String::new();

                log.push_str(&format!("=== {} ===\n", name));
                log.push_str(&format!("Source: {}\n", path.display()));
                log.push_str(&format!("Output: {}\n", output_path.display()));
                log.push_str(&format!("User-Agent: {}\n\n", user_agent));

                // Read HTML first (before catch_unwind)
                let html_bytes = match fs::read(&path) {
                    Ok(bytes) => bytes,
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

                let mut meta_path = path.clone();
                meta_path.set_extension("html.meta");
                let (content_type, source_url) = fs::read_to_string(meta_path)
                    .ok()
                    .as_deref()
                    .map(parse_cached_html_meta)
                    .unwrap_or((None, None));

                let mut html = decode_html_bytes(&html_bytes, content_type.as_deref());
                log.push_str(&format!("HTML bytes: {}\n", html_bytes.len()));
                if let Some(ct) = &content_type {
                    log.push_str(&format!("Content-Type: {}\n", ct));
                }
                log.push_str(&format!("Viewport: {}x{}\n", viewport_w, viewport_h));
                log.push_str(&format!("Scroll-X: {}px\n", scroll_x));
                log.push_str(&format!("Scroll-Y: {}px\n", scroll_y));

                let mut input_url = source_url.unwrap_or_else(|| format!("file://{}", path.display()));
                let mut resource_base = infer_base_url(&html, &input_url).into_owned();
                log.push_str(&format!("Resource base: {}\n", resource_base));

                if let Some(refresh) = extract_meta_refresh_url(&html) {
                    if let Some(target) = resolve_href(&resource_base, &refresh) {
                        log.push_str(&format!("Meta refresh to: {}\n", target));
                        match fetcher.fetch(&target) {
                            Ok(res) => {
                                html = decode_html_bytes(&res.bytes, res.content_type.as_deref());
                                input_url = target.clone();
                                resource_base = infer_base_url(&html, &input_url).into_owned();
                                log.push_str(&format!("Followed meta refresh; new base: {}\n", resource_base));
                            }
                            Err(e) => {
                                log.push_str(&format!("Failed to follow meta refresh: {}\n", e));
                            }
                        }
                    }
                }

                if html.to_ascii_lowercase().contains("<noscript") {
                    if let Some(js_redirect) = extract_js_location_redirect(&html) {
                        if let Some(target) = resolve_href(&resource_base, &js_redirect) {
                            log.push_str(&format!("JS redirect to: {}\n", target));
                            match fetcher.fetch(&target) {
                                Ok(res) => {
                                    html = decode_html_bytes(&res.bytes, res.content_type.as_deref());
                                    input_url = target.clone();
                                    resource_base = infer_base_url(&html, &input_url).into_owned();
                                    log.push_str(&format!("Followed JS redirect; new base: {}\n", resource_base));
                                }
                                Err(e) => {
                                    log.push_str(&format!("Failed to follow JS redirect: {}\n", e));
                                }
                            }
                        }
                    }
                }

                let mut css_links = extract_css_links(&html, &resource_base);
                if let Some(limit) = css_limit {
                    if css_links.len() > limit {
                        css_links.truncate(limit);
                    }
                }
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
                        renderer.render_to_png_with_scroll(&html_for_render, viewport_w, viewport_h, scroll_x, scroll_y)
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
