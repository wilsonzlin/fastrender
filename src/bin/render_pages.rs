//! Render all cached pages in parallel
//!
//! Renders all HTML files in fetches/html/ to fetches/renders/
//! Logs per-page to fetches/renders/{name}.log
//! Summary to fetches/renders/_summary.log

mod caching_fetcher;
mod common;

use caching_fetcher::DiskCachingFetcher;
use clap::Parser;
use common::args::{parse_shard, parse_viewport, MediaPreferenceArgs};
use common::media_prefs::MediaPreferences;
use fastrender::css::encoding::decode_css_bytes;
use fastrender::css::loader::absolutize_css_urls;
use fastrender::css::loader::extract_css_links;
use fastrender::css::loader::extract_embedded_css_urls;
use fastrender::css::loader::infer_base_url;
use fastrender::css::loader::inject_css_into_html;
use fastrender::css::loader::inline_imports;
use fastrender::css::loader::resolve_href;
use fastrender::html::encoding::decode_html_bytes;
use fastrender::html::meta_refresh::extract_js_location_redirect;
use fastrender::html::meta_refresh::extract_meta_refresh_url;
use fastrender::resource::normalize_page_name;
use fastrender::resource::normalize_user_agent_for_log;
use fastrender::resource::parse_cached_html_meta;
use fastrender::resource::HttpFetcher;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use fastrender::style::media::MediaType;
use fastrender::FastRender;
use std::collections::HashSet;
use std::fmt::Write;
use std::fs;
use std::panic::AssertUnwindSafe;
use std::path::PathBuf;
use std::sync::mpsc::channel;
use std::sync::mpsc::RecvTimeoutError;
use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time::Duration;
use std::time::Instant;

const HTML_DIR: &str = "fetches/html";
const ASSET_DIR: &str = "fetches/assets";
const RENDER_DIR: &str = "fetches/renders";
const RENDER_STACK_SIZE: usize = 64 * 1024 * 1024; // 64MB to avoid stack overflows on large pages

/// Render all cached pages in parallel
#[derive(Parser, Debug)]
#[command(name = "render_pages", version, about)]
struct Args {
  /// Number of parallel renders
  #[arg(long, short, default_value_t = num_cpus::get())]
  jobs: usize,

  /// Per-page timeout in seconds
  #[arg(long)]
  timeout: Option<u64>,

  /// Viewport size as WxH (e.g., 1200x800)
  #[arg(long, value_parser = parse_viewport, default_value = "1200x800")]
  viewport: (u32, u32),

  /// Device pixel ratio for media queries/srcset
  #[arg(long, default_value = "1.0")]
  dpr: f32,

  #[command(flatten)]
  media_prefs: MediaPreferenceArgs,

  /// Render only listed pages (comma-separated)
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Process only a deterministic shard of the cached pages (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Horizontal scroll offset in CSS px
  #[arg(long, default_value = "0.0")]
  scroll_x: f32,

  /// Vertical scroll offset in CSS px
  #[arg(long, default_value = "0.0")]
  scroll_y: f32,

  /// Override the User-Agent header
  #[arg(long, default_value = DEFAULT_USER_AGENT)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
  accept_language: String,

  /// Maximum number of external stylesheets to fetch
  #[arg(long)]
  css_limit: Option<usize>,

  /// Enable per-stage timing logs
  #[arg(long)]
  timings: bool,

  /// Positional page filters (cache stems)
  #[arg(trailing_var_arg = true)]
  filter_pages: Vec<String>,
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
  if std::env::var("FASTR_LAYOUT_PROFILE")
    .map(|v| v != "0")
    .unwrap_or(false)
  {
    eprintln!("FASTR_LAYOUT_PROFILE enabled: layout timings will be logged");
  }

  let args = Args::parse();
  let media_prefs = MediaPreferences::from(&args.media_prefs);

  // Build page filter from --pages and positional args
  let page_filter: Option<HashSet<String>> = {
    let mut all_pages: Vec<String> = args.pages.unwrap_or_default();
    all_pages.extend(args.filter_pages.clone());
    if all_pages.is_empty() {
      None
    } else {
      Some(
        all_pages
          .iter()
          .filter_map(|name| normalize_page_name(name))
          .collect(),
      )
    }
  };

  if args.timings {
    std::env::set_var("FASTR_RENDER_TIMINGS", "1");
  }

  media_prefs.apply_env();

  // Create directories
  fs::create_dir_all(RENDER_DIR).expect("create render dir");
  fs::create_dir_all(ASSET_DIR).expect("create asset dir");

  // Find all cached HTML files
  let mut entries: Vec<PathBuf> = match fs::read_dir(HTML_DIR) {
    Ok(dir) => dir
      .filter_map(|e| e.ok().map(|e| e.path()))
      .filter(|path| path.extension().map(|x| x == "html").unwrap_or(false))
      .filter(|path| {
        if let Some(ref filter) = page_filter {
          if let Some(stem_os) = path.file_stem() {
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
      eprintln!("No cached pages found in {}.", HTML_DIR);
      eprintln!("Run fetch_pages first.");
      std::process::exit(1);
    }
  };

  entries.sort();

  if let Some((index, total)) = args.shard {
    entries = entries
      .into_iter()
      .enumerate()
      .filter(|(idx, _)| idx % total == index)
      .map(|(_, path)| path)
      .collect();
  }

  if entries.is_empty() {
    eprintln!("No cached pages in {}. Run fetch_pages first.", HTML_DIR);
    std::process::exit(1);
  }

  // Create shared disk caching fetcher (CLI-only helper; library cache is in-memory)
  let fetcher = Arc::new(DiskCachingFetcher::new(
    HttpFetcher::new()
      .with_user_agent(args.user_agent.clone())
      .with_accept_language(args.accept_language.clone()),
    ASSET_DIR,
  ));

  let (viewport_w, viewport_h) = args.viewport;

  println!(
    "Rendering {} pages ({} parallel)...",
    entries.len(),
    args.jobs
  );
  println!(
    "User-Agent: {}",
    normalize_user_agent_for_log(&args.user_agent)
  );
  println!("Accept-Language: {}", args.accept_language);
  if let Some((index, total)) = args.shard {
    println!("Shard: {}/{}", index, total);
  }
  println!();

  let start = Instant::now();
  let results: Mutex<Vec<PageResult>> = Mutex::new(Vec::new());

  // Use a thread pool with limited concurrency
  let pool = rayon::ThreadPoolBuilder::new()
    .num_threads(args.jobs)
    .build()
    .expect("create thread pool");

  let timeout_secs = args.timeout;
  let device_pixel_ratio = args.dpr;
  let scroll_x = args.scroll_x;
  let scroll_y = args.scroll_y;
  let css_limit = args.css_limit;

  pool.scope(|s| {
    for path in &entries {
      let results = &results;
      let path = path.clone();
      let fetcher = Arc::clone(&fetcher);
      let user_agent = args.user_agent.clone();

      s.spawn(move |_| {
        let name = path.file_stem().unwrap().to_string_lossy().to_string();
        let output_path = PathBuf::from(RENDER_DIR).join(format!("{}.png", name));
        let log_path = PathBuf::from(RENDER_DIR).join(format!("{}.log", name));

        let page_start = Instant::now();
        let mut log = String::new();

        let _ = writeln!(log, "=== {} ===", name);
        let _ = writeln!(log, "Source: {}", path.display());
        let _ = writeln!(log, "Output: {}", output_path.display());
        let _ = writeln!(
          log,
          "User-Agent: {}\n",
          normalize_user_agent_for_log(&user_agent)
        );

        // Read HTML first (before catch_unwind)
        let html_bytes = match fs::read(&path) {
          Ok(bytes) => bytes,
          Err(e) => {
            let _ = writeln!(log, "Read error: {}", e);
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
        let _ = writeln!(log, "HTML bytes: {}", html_bytes.len());
        if let Some(ct) = &content_type {
          let _ = writeln!(log, "Content-Type: {}", ct);
        }
        let _ = writeln!(log, "Viewport: {}x{}", viewport_w, viewport_h);
        let _ = writeln!(log, "Scroll-X: {}px", scroll_x);
        let _ = writeln!(log, "Scroll-Y: {}px", scroll_y);

        let mut input_url = source_url.unwrap_or_else(|| format!("file://{}", path.display()));
        let mut resource_base = infer_base_url(&html, &input_url).into_owned();
        let _ = writeln!(log, "Resource base: {}", resource_base);

        if let Some(refresh) = extract_meta_refresh_url(&html) {
          if let Some(target) = resolve_href(&resource_base, &refresh) {
            let _ = writeln!(log, "Meta refresh to: {}", target);
            match fetcher.fetch(&target) {
              Ok(res) => {
                html = decode_html_bytes(&res.bytes, res.content_type.as_deref());
                input_url = target.clone();
                resource_base = infer_base_url(&html, &input_url).into_owned();
                let _ = writeln!(log, "Followed meta refresh; new base: {}", resource_base);
              }
              Err(e) => {
                let _ = writeln!(log, "Failed to follow meta refresh: {}", e);
              }
            }
          }
        }

        if html.to_ascii_lowercase().contains("<noscript") {
          if let Some(js_redirect) = extract_js_location_redirect(&html) {
            if let Some(target) = resolve_href(&resource_base, &js_redirect) {
              let _ = writeln!(log, "JS redirect to: {}", target);
              match fetcher.fetch(&target) {
                Ok(res) => {
                  html = decode_html_bytes(&res.bytes, res.content_type.as_deref());
                  input_url = target.clone();
                  resource_base = infer_base_url(&html, &input_url).into_owned();
                  let _ = writeln!(log, "Followed JS redirect; new base: {}", resource_base);
                }
                Err(e) => {
                  let _ = writeln!(log, "Failed to follow JS redirect: {}", e);
                }
              }
            }
          }
        }

        let mut css_links = extract_css_links(&html, &resource_base, MediaType::Screen);
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
              let mut import_fetch = |u| {
                fetcher
                  .fetch(u)
                  .map(|res| decode_css_bytes(&res.bytes, res.content_type.as_deref()))
              };
              let inlined = inline_imports(
                &rewritten,
                &css_url,
                &mut import_fetch,
                &mut seen_imports,
              );
              combined_css.push_str(&inlined);
              combined_css.push('\n');
            }
            Err(err) => {
              let _ = writeln!(log, "CSS fetch error {}: {}", css_url, err);
            }
          }
        }
        if !combined_css.is_empty() {
          let _ = writeln!(
            log,
            "Inlined CSS: {} bytes from {} link(s)",
            combined_css.len(),
            seen_links.len()
          );
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
              .apply_meta_viewport(true)
              .build()
              .expect("create renderer");
            renderer.render_to_png_with_scroll(
              &html_for_render,
              viewport_w,
              viewport_h,
              scroll_x,
              scroll_y,
            )
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
            let _ = writeln!(log, "PNG size: {} bytes", size);
            let _ = writeln!(log, "Time: {}ms", time_ms);
            log.push_str("Status: OK\n");

            if let Err(e) = fs::write(&output_path, &png_data) {
              let _ = writeln!(log, "Write error: {}", e);
              (Status::Error(format!("write: {}", e)), None)
            } else {
              println!("✓ {} ({}b, {}ms)", name, size, time_ms);
              (Status::Ok, Some(size))
            }
          }
          Err(status) => match status {
            Status::Error(msg) => {
              let _ = writeln!(log, "Time: {}ms", time_ms);
              log.push_str("Status: ERROR\n");
              let _ = writeln!(log, "Error: {}", msg);
              println!("✗ {} ERROR: {} ({}ms)", name, msg, time_ms);
              (Status::Error(msg), None)
            }
            Status::Crash(msg) => {
              let _ = writeln!(log, "Time: {}ms", time_ms);
              log.push_str("Status: CRASH\n");
              let _ = writeln!(log, "Panic: {}", msg);

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
  let _ = writeln!(
    summary,
    "Date: {}",
    chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
  );
  let _ = writeln!(summary, "Total time: {:.1}s", total_elapsed.as_secs_f64());
  let _ = writeln!(
    summary,
    "Pages: {} total, {} pass, {} crash, {} error\n",
    results.len(),
    pass,
    crash,
    error
  );

  let _ = writeln!(
    summary,
    "{:<40} {:>8} {:>10} STATUS",
    "PAGE", "TIME", "SIZE"
  );
  let _ = writeln!(summary, "{}", "-".repeat(75));

  for r in &results {
    let status_str = match &r.status {
      Status::Ok => "OK".to_string(),
      Status::Crash(msg) => format!("CRASH: {}", msg.chars().take(30).collect::<String>()),
      Status::Error(msg) => format!("ERROR: {}", msg.chars().take(30).collect::<String>()),
    };
    let size_str = r
      .size
      .map(|s| format!("{}b", s))
      .unwrap_or_else(|| "-".to_string());
    let _ = writeln!(
      summary,
      "{:<40} {:>6}ms {:>10} {}",
      r.name, r.time_ms, size_str, status_str
    );
  }

  let _ = writeln!(summary, "\n{}", "-".repeat(75));
  let _ = writeln!(summary, "Total: {:.1}s", total_elapsed.as_secs_f64());

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

#[cfg(test)]
mod tests {
  use super::normalize_page_name;

  #[test]
  fn normalize_page_name_strips_scheme_and_www() {
    assert_eq!(
      normalize_page_name("https://example.com/foo").as_deref(),
      Some("example.com_foo")
    );
    assert_eq!(
      normalize_page_name("http://www.example.com").as_deref(),
      Some("example.com")
    );
  }

  #[test]
  fn normalize_page_name_ignores_empty() {
    assert!(normalize_page_name("").is_none());
    assert!(normalize_page_name("   ").is_none());
  }

  #[test]
  fn normalize_page_name_strips_trailing_punctuation_and_whitespace() {
    assert_eq!(
      normalize_page_name(" https://Example.com./path/ ").as_deref(),
      Some("example.com_path")
    );
  }
}
