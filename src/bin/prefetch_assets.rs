//! Prefetch external CSS (and font subresources) into the disk-backed cache.
//!
//! This is a best-effort helper intended for the pageset workflow:
//! `fetch_pages` caches HTML, then this tool warms `fetches/assets/` so the
//! render step doesn't spend its 5s budget waiting on first-run network fetches.

mod common;

#[cfg(not(feature = "disk_cache"))]
fn main() {
  eprintln!(
    "prefetch_assets requires the `disk_cache` feature. Re-run with `--features disk_cache`."
  );
  std::process::exit(2);
}

#[cfg(feature = "disk_cache")]
mod disk_cache_main {
  use clap::{ArgAction, Parser};
  use fastrender::css::encoding::decode_css_bytes;
  use fastrender::css::loader::{
    absolutize_css_urls, extract_css_links, extract_embedded_css_urls, resolve_href,
  };
  use fastrender::css::parser::parse_stylesheet;
  use fastrender::css::types::CssImportLoader;
  use fastrender::css::types::StyleSheet;
  use fastrender::pageset::{cache_html_path, pageset_entries, PagesetEntry, PagesetFilter};
  use fastrender::resource::{
    CachingFetcherConfig, DiskCachingFetcher, ResourceFetcher, DEFAULT_ACCEPT_LANGUAGE,
    DEFAULT_USER_AGENT,
  };
  use fastrender::style::media::MediaContext;
  use fastrender::style::media::MediaQueryCache;
  use rayon::prelude::*;
  use rayon::ThreadPoolBuilder;
  use std::cell::RefCell;
  use std::collections::{BTreeSet, HashMap, HashSet};
  use std::path::PathBuf;
  use std::sync::Arc;

  use crate::common::args::{parse_shard, DiskCacheArgs, TimeoutArgs};
  use crate::common::render_pipeline::{
    build_http_fetcher, disk_cache_namespace, read_cached_document,
  };

  const DEFAULT_ASSET_DIR: &str = "fetches/assets";

  #[derive(Parser, Debug)]
  #[command(name = "prefetch_assets", version, about)]
  struct Args {
    /// Number of parallel pages to prefetch
    #[arg(long, short, default_value_t = num_cpus::get())]
    jobs: usize,

    #[command(flatten)]
    timeout: TimeoutArgs,

    #[command(flatten)]
    disk_cache: DiskCacheArgs,

    /// Prefetch font URLs referenced by fetched CSS (true/false)
    #[arg(
      long,
      default_value_t = true,
      action = ArgAction::Set,
      num_args = 0..=1,
      default_missing_value = "true"
    )]
    prefetch_fonts: bool,

    /// Override disk cache directory (defaults to fetches/assets)
    #[arg(long, default_value = DEFAULT_ASSET_DIR)]
    cache_dir: PathBuf,

    /// Prefetch only listed pages (comma-separated URLs or stems)
    #[arg(long, value_delimiter = ',')]
    pages: Option<Vec<String>>,

    /// Process only a deterministic shard of the page set (index/total, 0-based)
    #[arg(long, value_parser = parse_shard)]
    shard: Option<(usize, usize)>,
  }

  #[derive(Debug, Default, Clone)]
  struct PageSummary {
    stem: String,
    discovered_css: usize,
    fetched_css: usize,
    failed_css: usize,
    fetched_imports: usize,
    failed_imports: usize,
    fetched_fonts: usize,
    failed_fonts: usize,
    skipped: bool,
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
        Some(filter) => filter.matches_entry(entry),
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

  fn extract_font_urls(css: &str) -> Vec<String> {
    fn is_font_like(url: &str) -> bool {
      let lower = url.to_ascii_lowercase();
      let stripped = lower
        .split_once('#')
        .map(|(before, _)| before)
        .unwrap_or(&lower);
      let stripped = stripped
        .split_once('?')
        .map(|(before, _)| before)
        .unwrap_or(stripped);
      matches!(
        stripped,
        s if s.ends_with(".woff2")
          || s.ends_with(".woff")
          || s.ends_with(".ttf")
          || s.ends_with(".otf")
          || s.ends_with(".eot")
      )
    }

    let mut urls = Vec::new();
    let bytes = css.as_bytes();
    let mut i = 0usize;
    while i + 4 <= bytes.len() {
      if bytes[i].eq_ignore_ascii_case(&b'u')
        && bytes[i + 1].eq_ignore_ascii_case(&b'r')
        && bytes[i + 2].eq_ignore_ascii_case(&b'l')
        && bytes[i + 3] == b'('
      {
        let mut j = i + 4;
        while j < bytes.len() && (bytes[j] as char).is_whitespace() {
          j += 1;
        }
        if j >= bytes.len() {
          break;
        }
        let quote = bytes[j];
        let (start, end) = if quote == b'"' || quote == b'\'' {
          j += 1;
          let start = j;
          while j < bytes.len() {
            if bytes[j] == b'\\' {
              j += 2;
              continue;
            }
            if bytes[j] == quote {
              break;
            }
            j += 1;
          }
          (start, j)
        } else {
          let start = j;
          while j < bytes.len() && bytes[j] != b')' {
            j += 1;
          }
          (start, j)
        };

        if end > start && end <= css.len() {
          let candidate = css[start..end].trim();
          if !candidate.is_empty() && is_font_like(candidate) {
            urls.push(candidate.to_string());
          }
        }

        i = j;
      } else {
        i += 1;
      }
    }
    urls
  }

  struct PrefetchImportLoader<'a> {
    fetcher: &'a dyn ResourceFetcher,
    prefetch_fonts: bool,
    css_cache: RefCell<HashMap<String, String>>,
    seen_fonts: &'a RefCell<HashSet<String>>,
    summary: &'a RefCell<PageSummary>,
  }

  impl<'a> PrefetchImportLoader<'a> {
    fn new(
      fetcher: &'a dyn ResourceFetcher,
      prefetch_fonts: bool,
      seen_fonts: &'a RefCell<HashSet<String>>,
      summary: &'a RefCell<PageSummary>,
    ) -> Self {
      Self {
        fetcher,
        prefetch_fonts,
        css_cache: RefCell::new(HashMap::new()),
        seen_fonts,
        summary,
      }
    }
  }

  impl CssImportLoader for PrefetchImportLoader<'_> {
    fn load(&self, url: &str) -> fastrender::Result<String> {
      if let Some(cached) = self.css_cache.borrow().get(url).cloned() {
        return Ok(cached);
      }

      match self.fetcher.fetch(url) {
        Ok(res) => {
          self.summary.borrow_mut().fetched_imports += 1;
          let base = res.final_url.as_deref().unwrap_or(url);
          let decoded = decode_css_bytes(&res.bytes, res.content_type.as_deref());
          let rewritten = match absolutize_css_urls(&decoded, base) {
            Ok(css) => css,
            Err(_) => decoded,
          };

          if self.prefetch_fonts {
            let mut seen = self.seen_fonts.borrow_mut();
            let mut summary = self.summary.borrow_mut();
            prefetch_fonts_from_css(self.fetcher, base, &rewritten, &mut seen, &mut summary);
          }

          self
            .css_cache
            .borrow_mut()
            .insert(url.to_string(), rewritten.clone());

          Ok(rewritten)
        }
        Err(err) => {
          self.summary.borrow_mut().failed_imports += 1;
          Err(err)
        }
      }
    }
  }

  fn prefetch_fonts_from_css(
    fetcher: &dyn ResourceFetcher,
    css_base_url: &str,
    css_text: &str,
    seen: &mut HashSet<String>,
    summary: &mut PageSummary,
  ) {
    for raw in extract_font_urls(css_text) {
      let Some(resolved) = resolve_href(css_base_url, &raw) else {
        continue;
      };
      if resolved.starts_with("data:") {
        continue;
      }
      if !seen.insert(resolved.clone()) {
        continue;
      }
      match fetcher.fetch(&resolved) {
        Ok(_) => summary.fetched_fonts += 1,
        Err(_) => summary.failed_fonts += 1,
      }
    }
  }

  fn prefetch_page(
    entry: &PagesetEntry,
    fetcher: &dyn ResourceFetcher,
    prefetch_fonts: bool,
  ) -> PageSummary {
    let summary = RefCell::new(PageSummary {
      stem: entry.cache_stem.clone(),
      ..PageSummary::default()
    });

    let cache_path = cache_html_path(&entry.cache_stem);
    if !cache_path.exists() {
      summary.borrow_mut().skipped = true;
      return summary.into_inner();
    }

    let cached = match read_cached_document(&cache_path) {
      Ok(doc) => doc,
      Err(_) => {
        summary.borrow_mut().skipped = true;
        return summary.into_inner();
      }
    };
    let base_url = cached.document.base_url.clone();
    let html = cached.document.html;

    let media_ctx = MediaContext::screen(1200.0, 800.0).with_env_overrides();
    let mut media_query_cache = MediaQueryCache::default();

    let mut css_urls: BTreeSet<String> = BTreeSet::new();
    match extract_css_links(&html, &base_url, media_ctx.media_type) {
      Ok(urls) => css_urls.extend(urls),
      Err(_) => {}
    }
    match extract_embedded_css_urls(&html, &base_url) {
      Ok(urls) => css_urls.extend(urls),
      Err(_) => {}
    }

    summary.borrow_mut().discovered_css = css_urls.len();
    if css_urls.is_empty() {
      return summary.into_inner();
    }

    let seen_fonts: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
    let import_loader = PrefetchImportLoader::new(fetcher, prefetch_fonts, &seen_fonts, &summary);

    for css_url in css_urls {
      match fetcher.fetch(&css_url) {
        Ok(res) => {
          summary.borrow_mut().fetched_css += 1;
          let sheet_base = res.final_url.as_deref().unwrap_or(&css_url);
          let css_text = decode_css_bytes(&res.bytes, res.content_type.as_deref());
          let css_text = match absolutize_css_urls(&css_text, sheet_base) {
            Ok(css) => css,
            Err(_) => css_text,
          };
          if prefetch_fonts {
            let mut seen = seen_fonts.borrow_mut();
            let mut summary = summary.borrow_mut();
            prefetch_fonts_from_css(fetcher, sheet_base, &css_text, &mut seen, &mut summary);
          }

          let sheet: StyleSheet = match parse_stylesheet(&css_text) {
            Ok(sheet) => sheet,
            Err(_) => continue,
          };

          let _ = sheet.resolve_imports_with_cache(
            &import_loader,
            Some(sheet_base),
            &media_ctx,
            Some(&mut media_query_cache),
          );
        }
        Err(_) => summary.borrow_mut().failed_css += 1,
      }
    }

    summary.into_inner()
  }

  pub fn main() {
    let args = Args::parse();

    if args.jobs == 0 {
      eprintln!("jobs must be > 0");
      std::process::exit(2);
    }

    let timeout_secs = args.timeout.seconds(Some(30));
    let per_request_timeout_label = timeout_secs.unwrap_or(0);

    let page_filter = args
      .pages
      .as_ref()
      .and_then(|pages| PagesetFilter::from_inputs(pages));

    let entries = pageset_entries();
    let selected = selected_pages(&entries, page_filter.as_ref(), args.shard);
    if selected.is_empty() {
      if page_filter.is_some() {
        println!("No pages matched the provided filter");
      } else {
        println!("No pages to prefetch");
      }
      std::process::exit(1);
    }

    if let Some(filter) = &page_filter {
      let missing = filter.unmatched(&selected);
      if !missing.is_empty() {
        println!("Warning: unknown pages in filter: {}", missing.join(", "));
      }
    }

    let http = build_http_fetcher(DEFAULT_USER_AGENT, DEFAULT_ACCEPT_LANGUAGE, timeout_secs);
    let mut disk_config = args.disk_cache.to_config();
    disk_config.namespace = Some(disk_cache_namespace(
      DEFAULT_USER_AGENT,
      DEFAULT_ACCEPT_LANGUAGE,
    ));
    let fetcher: Arc<dyn ResourceFetcher> = Arc::new(DiskCachingFetcher::with_configs(
      http,
      args.cache_dir.clone(),
      CachingFetcherConfig {
        honor_http_cache_freshness: true,
        ..CachingFetcherConfig::default()
      },
      disk_config,
    ));

    println!(
      "Prefetching assets for {} page(s) ({} parallel, {}s timeout, fonts={})...",
      selected.len(),
      args.jobs,
      per_request_timeout_label,
      args.prefetch_fonts
    );
    if let Some((index, total)) = args.shard {
      println!("Shard: {}/{}", index, total);
    }
    println!("Cache dir: {}", args.cache_dir.display());
    let max_age = if args.disk_cache.max_age_secs == 0 {
      "none".to_string()
    } else {
      format!("{}s", args.disk_cache.max_age_secs)
    };
    println!(
      "Disk cache: max_bytes={} max_age={}",
      args.disk_cache.max_bytes, max_age
    );
    println!();

    let pool = ThreadPoolBuilder::new()
      .num_threads(args.jobs)
      .build()
      .expect("create thread pool");

    let mut results: Vec<PageSummary> = pool.install(|| {
      selected
        .par_iter()
        .map(|entry| prefetch_page(entry, fetcher.as_ref(), args.prefetch_fonts))
        .collect()
    });

    results.sort_by(|a, b| a.stem.cmp(&b.stem));

    let mut total_discovered = 0usize;
    let mut total_fetched = 0usize;
    let mut total_failed = 0usize;
    let mut total_skipped = 0usize;
    let mut total_imports_fetched = 0usize;
    let mut total_imports_failed = 0usize;
    let mut total_fonts_fetched = 0usize;
    let mut total_fonts_failed = 0usize;

    for r in &results {
      if r.skipped {
        total_skipped += 1;
        println!("• {} (no cached HTML, skipped)", r.stem);
        continue;
      }
      total_discovered += r.discovered_css;
      total_fetched += r.fetched_css;
      total_failed += r.failed_css;
      total_imports_fetched += r.fetched_imports;
      total_imports_failed += r.failed_imports;
      total_fonts_fetched += r.fetched_fonts;
      total_fonts_failed += r.failed_fonts;

      println!(
        "• {} css={} fetched={} failed={} imports_fetched={} imports_failed={} fonts_fetched={} fonts_failed={}",
        r.stem,
        r.discovered_css,
        r.fetched_css,
        r.failed_css,
        r.fetched_imports,
        r.failed_imports,
        r.fetched_fonts,
        r.failed_fonts
      );
    }

    println!();
    println!(
      "Done: css_discovered={} css_fetched={} css_failed={} pages_skipped={} imports_fetched={} imports_failed={} fonts_fetched={} fonts_failed={}",
      total_discovered,
      total_fetched,
      total_failed,
      total_skipped,
      total_imports_fetched,
      total_imports_failed,
      total_fonts_fetched,
      total_fonts_failed
    );

    // Best-effort tool: do not fail the process on fetch errors.
  }
}

#[cfg(feature = "disk_cache")]
fn main() {
  disk_cache_main::main();
}
