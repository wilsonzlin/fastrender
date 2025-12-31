//! Fetch and cache HTML pages for testing
//!
//! Fetches all target pages in parallel and caches to fetches/html/

mod common;

use clap::Parser;
use common::args::{parse_shard, TimeoutArgs};
use common::render_pipeline::build_http_fetcher;
use fastrender::html::encoding::decode_html_bytes;
use fastrender::html::meta_refresh::extract_js_location_redirect;
use fastrender::html::meta_refresh::extract_meta_refresh_url;
use fastrender::pageset::{
  cache_html_path, pageset_entries_with_collisions, PagesetEntry, PagesetFilter, CACHE_HTML_DIR,
};
use fastrender::resource::FetchedResource;
use fastrender::resource::ResourceFetcher;
use fastrender::resource::DEFAULT_ACCEPT_LANGUAGE;
use fastrender::resource::DEFAULT_USER_AGENT;
use rayon::ThreadPoolBuilder;
use std::fmt::Write;
use std::fs;
use std::path::Path;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::sync::Mutex;
use url::Url;

/// Fetch and cache HTML pages for testing
#[derive(Parser, Debug)]
#[command(name = "fetch_pages", version, about)]
struct Args {
  /// Re-fetch all pages even if cached
  #[arg(long)]
  refresh: bool,

  /// Number of parallel fetches
  #[arg(long, short, default_value_t = num_cpus::get())]
  jobs: usize,

  #[command(flatten)]
  timeout: TimeoutArgs,

  /// Fetch only listed pages (comma-separated URLs or stems)
  #[arg(long, value_delimiter = ',')]
  pages: Option<Vec<String>>,

  /// Allow duplicate stems by appending a hash suffix
  #[arg(long)]
  allow_collisions: bool,

  /// Process only a deterministic shard of the page set (index/total, 0-based)
  #[arg(long, value_parser = parse_shard)]
  shard: Option<(usize, usize)>,

  /// Override the User-Agent header
  #[arg(long, default_value = DEFAULT_USER_AGENT)]
  user_agent: String,

  /// Override the Accept-Language header
  #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
  accept_language: String,

  /// Print per-page fetch durations
  #[arg(long)]
  timings: bool,
}

fn build_pageset_entries(
  allow_collisions: bool,
) -> Result<(Vec<PagesetEntry>, Vec<(String, Vec<String>)>), String> {
  let (entries, collisions) = pageset_entries_with_collisions();

  if !collisions.is_empty() && !allow_collisions {
    let mut msg = String::from("Pageset stem collisions detected:\n");
    for (stem, urls) in &collisions {
      msg.push_str(&format!("  {stem}: {}\n", urls.join(", ")));
    }
    msg.push_str("Pass --allow-collisions to append a deterministic hash suffix.");
    return Err(msg);
  }

  Ok((entries, collisions))
}

fn selected_pages(
  entries: &[PagesetEntry],
  filter: Option<&PagesetFilter>,
  shard: Option<(usize, usize)>,
) -> Vec<PagesetEntry> {
  let filtered: Vec<PagesetEntry> = entries
    .iter()
    .cloned()
    .filter(|entry| match filter {
      Some(names) => names.matches_entry(entry),
      None => true,
    })
    .collect();

  if let Some((index, total)) = shard {
    filtered
      .into_iter()
      .enumerate()
      .filter(|(idx, _)| idx % total == index)
      .map(|(_, entry)| entry)
      .collect()
  } else {
    filtered
  }
}

fn write_cached_html(
  cache_path: &Path,
  bytes: &[u8],
  content_type: Option<&str>,
  source_url: Option<&str>,
  status: Option<u16>,
) -> std::io::Result<()> {
  if let Some(parent) = cache_path.parent() {
    std::fs::create_dir_all(parent)?;
  }

  std::fs::write(cache_path, bytes)?;

  let meta_path = cache_path.with_extension("html.meta");
  let mut meta = String::new();
  if let Some(ct) = content_type {
    if !ct.is_empty() {
      let _ = writeln!(meta, "content-type: {}", ct);
    }
  }
  if let Some(status) = status {
    let _ = writeln!(meta, "status: {}", status);
  }
  if let Some(url) = source_url {
    if !url.trim().is_empty() {
      let _ = writeln!(meta, "url: {}", url.trim());
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
  timeout_secs: Option<u64>,
  user_agent: &str,
  accept_language: &str,
) -> Result<FetchedResource, String> {
  let fetcher = build_http_fetcher(user_agent, accept_language, timeout_secs);

  let fetch = |target: &str| -> Result<(FetchedResource, String), String> {
    let mut res = fetcher.fetch(target).map_err(|e| e.to_string())?;
    if res.bytes.is_empty() {
      let status = res
        .status
        .map(|code| code.to_string())
        .unwrap_or_else(|| "<unknown>".to_string());
      let final_url = res.final_url.clone().unwrap_or_else(|| target.to_string());
      return Err(format!("empty response body (status {status}, final url {final_url})"));
    }
    let canonical_url = res.final_url.clone().unwrap_or_else(|| target.to_string());
    res.final_url.get_or_insert_with(|| canonical_url.clone());
    Ok((res, canonical_url))
  };

  let (mut res, mut current_url) = fetch(url)?;

  // Follow a single meta refresh (common for noscript fallbacks)
  let mut html = decode_html_bytes(&res.bytes, res.content_type.as_deref());
  if let Some(refresh) = extract_meta_refresh_url(&html) {
    if let Ok(base) = Url::parse(&current_url) {
      if let Ok(next) = base.join(&refresh) {
        match fetch(next.as_str()) {
          Ok((next_res, next_url)) => {
            res = next_res;
            current_url = next_url;
            html = decode_html_bytes(&res.bytes, res.content_type.as_deref());
          }
          Err(e) => eprintln!(
            "Warning: meta refresh fetch failed: {} (keeping original)",
            e
          ),
        }
      }
    }
  }

  // Follow a simple JS location redirect once (common in script-only handoffs)
  if let Some(js_redirect) = extract_js_location_redirect(&html) {
    if let Ok(base) = Url::parse(&current_url) {
      if let Ok(next) = base.join(&js_redirect) {
        match fetch(next.as_str()) {
          Ok((next_res, next_url)) => {
            res = next_res;
            current_url = next_url;
          }
          Err(e) => eprintln!(
            "Warning: js redirect fetch failed: {} (keeping original)",
            e
          ),
        }
      }
    }
  }

  res.final_url.get_or_insert(current_url);
  Ok(res)
}

fn main() {
  let args = Args::parse();

  let (pageset_entries, collisions) = match build_pageset_entries(args.allow_collisions) {
    Ok(result) => result,
    Err(msg) => {
      eprintln!("{msg}");
      std::process::exit(1);
    }
  };

  // Build page filter from --pages
  let page_filter = args
    .pages
    .as_ref()
    .and_then(|pages| PagesetFilter::from_inputs(pages));

  fs::create_dir_all(CACHE_HTML_DIR).expect("create cache dir");

  let selected = selected_pages(&pageset_entries, page_filter.as_ref(), args.shard);
  if selected.is_empty() {
    if page_filter.is_some() {
      println!("No pages matched the provided filter");
    } else {
      println!("No pages to fetch");
    }
    std::process::exit(1);
  }

  if let Some(filter) = &page_filter {
    let missing = filter.unmatched(&selected);
    if !missing.is_empty() {
      println!("Warning: unknown pages in filter: {}", missing.join(", "));
    }
  }

  if !collisions.is_empty() && args.allow_collisions {
    println!("Allowing {} stem collision(s):", collisions.len());
    for (stem, urls) in &collisions {
      println!("  {stem}: {}", urls.join(", "));
    }
  }

  let timeout_secs = args.timeout.seconds(Some(60));
  let timeout_label = timeout_secs
    .map(|secs| format!("{secs}s"))
    .unwrap_or_else(|| "no timeout".to_string());

  println!(
    "Fetching {} pages ({} parallel, timeout={timeout_label})...",
    selected.len(),
    args.jobs,
  );
  println!("User-Agent: {}", args.user_agent);
  println!("Accept-Language: {}", args.accept_language);
  if let Some((index, total)) = args.shard {
    println!("Shard: {}/{}", index, total);
  }
  if args.refresh {
    println!("--refresh: re-fetching all");
  }
  println!();

  let success = Arc::new(AtomicUsize::new(0));
  let failed = Arc::new(AtomicUsize::new(0));
  let skipped = Arc::new(AtomicUsize::new(0));
  let failed_urls: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));

  let pool = ThreadPoolBuilder::new()
    .num_threads(args.jobs)
    .build()
    .expect("create thread pool");

  pool.scope(|s| {
    for entry in &selected {
      let entry = entry.clone();
      let success = Arc::clone(&success);
      let failed = Arc::clone(&failed);
      let skipped = Arc::clone(&skipped);
      let failed_urls = Arc::clone(&failed_urls);
      let refresh = args.refresh;
      let user_agent = args.user_agent.clone();
      let accept_language = args.accept_language.clone();
      let timings = args.timings;

      s.spawn(move |_| {
        let cache_path = cache_html_path(&entry.cache_stem);

        // Skip if cached and not refreshing
        if !refresh && cache_path.exists() {
          skipped.fetch_add(1, Ordering::Relaxed);
          return;
        }

        let start = if timings {
          Some(std::time::Instant::now())
        } else {
          None
        };
        match fetch_page(&entry.url, timeout_secs, &user_agent, &accept_language) {
          Ok(res) => {
            let canonical_url = res.final_url.as_deref().unwrap_or(&entry.url);
            if write_cached_html(
              &cache_path,
              &res.bytes,
              res.content_type.as_deref(),
              Some(canonical_url),
              res.status,
            )
            .is_ok()
            {
              if let Some(start) = start {
                println!(
                  "✓ {} ({}b, {}ms)",
                  entry.url,
                  res.bytes.len(),
                  start.elapsed().as_millis()
                );
              } else {
                println!("✓ {} ({}b)", entry.url, res.bytes.len());
              }
              success.fetch_add(1, Ordering::Relaxed);
              return;
            }
            if let Some(start) = start {
              println!(
                "✗ {} (write failed, {}ms)",
                entry.url,
                start.elapsed().as_millis()
              );
            } else {
              println!("✗ {} (write failed)", entry.url);
            }
            failed.fetch_add(1, Ordering::Relaxed);
            let _ = failed_urls
              .lock()
              .map(|mut v| v.push(entry.url.to_string()));
          }
          Err(e) => {
            if let Some(start) = start {
              println!("✗ {} ({}, {}ms)", entry.url, e, start.elapsed().as_millis());
            } else {
              println!("✗ {} ({})", entry.url, e);
            }
            failed.fetch_add(1, Ordering::Relaxed);
            let _ = failed_urls
              .lock()
              .map(|mut v| v.push(entry.url.to_string()));
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
  println!("Cache: {CACHE_HTML_DIR}/");
}

#[cfg(test)]
mod tests {
  use super::*;
  use fastrender::pageset::{pageset_stem, PAGESET_URLS};
  use std::collections::HashSet;
  use std::time::Duration;

  fn try_bind_localhost(context: &str) -> Option<std::net::TcpListener> {
    match std::net::TcpListener::bind("127.0.0.1:0") {
      Ok(listener) => Some(listener),
      Err(err) if err.kind() == std::io::ErrorKind::PermissionDenied => {
        eprintln!("skipping {context}: cannot bind localhost in this environment: {err}");
        None
      }
      Err(err) => panic!("bind {context}: {err}"),
    }
  }

  #[test]
  fn cache_stems_match_normalization() {
    let (entries, collisions) = build_pageset_entries(false).expect("pageset ok");
    assert!(collisions.is_empty());
    for entry in entries {
      assert_eq!(entry.stem, pageset_stem(&entry.url).unwrap());
      assert_eq!(entry.cache_stem, entry.stem);
    }
  }

  fn pageset_entries() -> Vec<PagesetEntry> {
    build_pageset_entries(false).expect("pageset ok").0
  }

  #[test]
  fn selected_pages_accepts_trailing_slash_filter() {
    let filter =
      PagesetFilter::from_inputs(&vec!["https://example.com/".to_string()]).expect("filter");

    let selected = selected_pages(&pageset_entries(), Some(&filter), None);
    assert!(selected
      .iter()
      .any(|entry| entry.url == "https://example.com"));
  }

  #[test]
  fn selected_pages_respects_filter() {
    let filter = PagesetFilter::from_inputs(&vec![
      "https://cnn.com".to_string(),
      "https://example.com".to_string(),
    ])
    .expect("filter");

    let selected = selected_pages(&pageset_entries(), Some(&filter), None);
    assert!(selected.iter().any(|entry| entry.url == "https://cnn.com"));
    assert!(selected
      .iter()
      .any(|entry| entry.url == "https://example.com"));
    assert!(!selected
      .iter()
      .any(|entry| entry.url == "https://reddit.com"));
  }

  #[test]
  fn selected_pages_combines_multiple_filters_case_insensitive() {
    let filter = PagesetFilter::from_inputs(&vec![
      "HTTPS://DEVELOPER.MOZILLA.ORG/en-US/docs/Web/CSS/text-orientation".to_string(),
      "https://RUST-LANG.ORG".to_string(),
    ])
    .expect("filter");

    let selected = selected_pages(&pageset_entries(), Some(&filter), None);
    assert_eq!(selected.len(), 2);
    assert!(selected
      .iter()
      .any(|entry| entry.url.ends_with("text-orientation")));
    assert!(selected
      .iter()
      .any(|entry| entry.url.contains("rust-lang.org")));
  }

  #[test]
  fn selected_pages_respects_leading_www() {
    // www.openstreetmap.org is in the pageset; normalization should handle the www prefix.
    let filter = PagesetFilter::from_inputs(&vec!["https://www.openstreetmap.org".to_string()])
      .expect("filter");

    let selected = selected_pages(&pageset_entries(), Some(&filter), None);
    assert!(selected
      .iter()
      .any(|entry| entry.url == "https://www.openstreetmap.org"));
  }

  #[test]
  fn selected_pages_deduplicates_filters() {
    // rust-lang.org is present in the pageset.
    let filter = PagesetFilter::from_inputs(&vec![
      "https://rust-lang.org".to_string(),
      "https://rust-lang.org/".to_string(),
    ])
    .expect("filter");

    let selected = selected_pages(&pageset_entries(), Some(&filter), None);
    assert_eq!(
      selected,
      vec![pageset_entries()
        .into_iter()
        .find(|entry| entry.url == "https://rust-lang.org")
        .unwrap()]
    );
  }

  #[test]
  fn selected_pages_none_returns_all() {
    let all = selected_pages(&pageset_entries(), None, None);
    assert_eq!(all.len(), PAGESET_URLS.len());
  }

  #[test]
  fn selected_pages_apply_shard_evenly() {
    let entries = pageset_entries();
    let shard0 = selected_pages(&entries, None, Some((0, 3)));
    let shard1 = selected_pages(&entries, None, Some((1, 3)));
    let shard2 = selected_pages(&entries, None, Some((2, 3)));

    assert!(!shard0.is_empty());
    assert!(!shard1.is_empty());
    assert!(!shard2.is_empty());

    let mut combined = Vec::new();
    combined.extend_from_slice(&shard0);
    combined.extend_from_slice(&shard1);
    combined.extend_from_slice(&shard2);

    let mut deduped = combined.clone();
    deduped.sort();
    deduped.dedup();

    let mut all = selected_pages(&entries, None, None);
    all.sort_by(|a, b| a.url.cmp(&b.url));

    assert_eq!(deduped, all);
  }

  #[test]
  fn pages_are_unique() {
    let mut seen = HashSet::new();
    let mut dupes = Vec::new();
    for &url in PAGESET_URLS {
      if !seen.insert(url) {
        dupes.push(url);
      }
    }
    assert!(
      dupes.is_empty(),
      "Duplicate entries in PAGESET_URLS: {:?}",
      dupes
    );
  }

  #[test]
  fn filter_accepts_full_urls_and_www() {
    let filter = PagesetFilter::from_inputs(&vec![
      "https://www.w3.org".to_string(),
      "w3.org".to_string(),
    ])
    .expect("filter");
    let selected = selected_pages(&pageset_entries(), Some(&filter), None);
    assert_eq!(selected.len(), 1);
    assert!(selected
      .iter()
      .any(|entry| entry.url == "https://www.w3.org"));
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
      Some(200),
    )
    .expect("write ok");

    let html = std::fs::read_to_string(&cache_path).expect("html read");
    assert_eq!(html, "hello");

    let meta_path = cache_path.with_extension("html.meta");
    let meta = std::fs::read_to_string(meta_path).expect("meta read");
    assert!(meta.contains("content-type: text/html; charset=utf-8"));
    assert!(meta.contains("url: https://example.com/page"));
    assert!(meta.contains("status: 200"));
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

    write_cached_html(&cache_path, b"hello", None, None, None).expect("write ok");

    let html = std::fs::read_to_string(&cache_path).expect("html read");
    assert_eq!(html, "hello");
    assert!(
      !meta_path.exists(),
      "meta should be removed when content type absent"
    );
  }

  #[test]
  fn write_cached_meta_records_final_redirect_target() {
    use std::io::Read;
    use std::io::Write;

    let Some(listener) = try_bind_localhost("write_cached_meta_records_final_redirect_target")
    else {
      return;
    };
    let addr = listener.local_addr().unwrap();

    let handle = std::thread::spawn(move || {
      let mut iter = listener.incoming();

      // Initial response redirects to /final.
      if let Some(stream) = iter.next() {
        let mut stream = stream.unwrap();
        let _ = stream.read(&mut [0u8; 1024]);
        let headers = format!(
          "HTTP/1.1 302 Found\r\nLocation: http://{}/final\r\nContent-Length: 0\r\n\r\n",
          addr
        );
        let _ = stream.write_all(headers.as_bytes());
      }

      // Final response serves the content.
      if let Some(stream) = iter.next() {
        let mut stream = stream.unwrap();
        let _ = stream.read(&mut [0u8; 1024]);
        let body = b"redirected body";
        let headers = format!(
          "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n",
          body.len()
        );
        let _ = stream.write_all(headers.as_bytes());
        let _ = stream.write_all(body);
      }
    });

    let url = format!("http://{}/start", addr);
    let res = fetch_page(
      &url,
      Duration::from_secs(5),
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
    )
    .expect("fetch succeeds");
    handle.join().unwrap();

    let dir = tempfile::tempdir().expect("temp dir");
    let cache_path = dir.path().join("page.html");
    write_cached_html(
      &cache_path,
      &res.bytes,
      res.content_type.as_deref(),
      res.final_url.as_deref(),
      res.status,
    )
    .expect("write cached");

    let meta = std::fs::read_to_string(cache_path.with_extension("html.meta")).expect("meta read");
    let final_url = format!("http://{}/final", addr);
    assert!(
      meta.contains(&format!("url: {}", final_url)),
      "meta should record final redirect target, got:\n{}",
      meta
    );
    assert!(meta.contains("status: 200"), "meta should record status");
    assert_eq!(res.final_url.as_deref(), Some(final_url.as_str()));
  }

  #[test]
  fn fetch_page_rejects_empty_body() {
    use std::io::Write;

    let Some(listener) = try_bind_localhost("fetch_page_rejects_empty_body") else {
      return;
    };
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
    assert!(
      result.is_err(),
      "empty bodies should be treated as failures"
    );
    handle.join().unwrap();
  }

  #[test]
  fn fetch_page_sets_accept_language_header() {
    use std::io::Read;
    use std::io::Write;

    let Some(listener) = try_bind_localhost("fetch_page_sets_accept_language_header") else {
      return;
    };
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
    let res = fetch_page(
      &url,
      Duration::from_secs(5),
      DEFAULT_USER_AGENT,
      "es-MX,es;q=0.8",
    )
    .expect("fetch succeeds");
    handle.join().unwrap();

    assert_eq!(res.bytes, b"ok");
    assert_eq!(res.final_url.as_deref(), Some(url.as_str()));
  }

  #[test]
  fn fetch_page_follows_meta_refresh() {
    use std::io::Read;
    use std::io::Write;

    let Some(listener) = try_bind_localhost("fetch_page_follows_meta_refresh") else {
      return;
    };
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
        assert!(
          req.starts_with("GET /next"),
          "unexpected redirect path: {req}"
        );

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
    let res = fetch_page(
      &url,
      Duration::from_secs(5),
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
    )
    .expect("fetch succeeds");
    handle.join().unwrap();

    let expected_final = format!("http://{}/next", addr);
    assert_eq!(res.bytes, b"refreshed");
    assert_eq!(res.content_type.as_deref(), Some("text/plain"));
    assert_eq!(res.final_url.as_deref(), Some(expected_final.as_str()));
  }

  #[test]
  fn fetch_page_follows_js_redirect() {
    use std::io::Read;
    use std::io::Write;

    let Some(listener) = try_bind_localhost("fetch_page_follows_js_redirect") else {
      return;
    };
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
        assert!(
          req.starts_with("GET /js"),
          "unexpected redirect path: {req}"
        );

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
    let res = fetch_page(
      &url,
      Duration::from_secs(5),
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
    )
    .expect("fetch succeeds");
    handle.join().unwrap();

    let expected_final = format!("http://{}/js", addr);
    assert_eq!(res.bytes, b"redirected");
    assert_eq!(res.content_type.as_deref(), Some("text/plain"));
    assert_eq!(res.final_url.as_deref(), Some(expected_final.as_str()));
  }

  #[test]
  fn fetch_page_keeps_original_when_js_redirect_fails() {
    use std::io::Read;
    use std::io::Write;

    let Some(listener) = try_bind_localhost("fetch_page_keeps_original_when_js_redirect_fails")
    else {
      return;
    };
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
    let res = fetch_page(
      &url,
      Duration::from_secs(5),
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
    )
    .expect("fetch succeeds");
    handle.join().unwrap();

    assert_eq!(
      res.bytes,
      b"<script>window.location.href='/missing'</script>"
    );
    assert_eq!(res.content_type.as_deref(), Some("text/html"));
    assert_eq!(res.final_url.as_deref(), Some(url.as_str()));
  }
}
