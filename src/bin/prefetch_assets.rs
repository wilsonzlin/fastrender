//! Prefetch external CSS (and optional HTML subresources) into the disk-backed cache.
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
    absolutize_css_urls, extract_css_links, extract_embedded_css_urls,
    link_rel_is_stylesheet_candidate, resolve_href, resolve_href_with_base,
  };
  use fastrender::css::parser::{extract_scoped_css_sources, parse_stylesheet, StylesheetSource};
  use fastrender::css::types::CssImportLoader;
  use fastrender::css::types::{FontFaceSource, StyleSheet};
  use fastrender::debug::runtime;
  use fastrender::dom::parse_html;
  use fastrender::html::asset_discovery::discover_html_asset_urls;
  use fastrender::pageset::{cache_html_path, pageset_entries, PagesetEntry, PagesetFilter};
  use fastrender::resource::{
    CachingFetcherConfig, DiskCachingFetcher, ResourceFetcher, DEFAULT_ACCEPT_LANGUAGE,
    DEFAULT_USER_AGENT,
  };
  use fastrender::style::media::{MediaContext, MediaQuery, MediaQueryCache};
  use rayon::prelude::*;
  use rayon::ThreadPoolBuilder;
  use std::cell::RefCell;
  use std::collections::{HashMap, HashSet};
  use std::path::{Path, PathBuf};
  use std::sync::Arc;
  use std::time::Duration;

  use crate::common::args::{parse_shard, DiskCacheArgs, TimeoutArgs, ViewportArgs};
  use crate::common::disk_cache_stats::scan_disk_cache_dir;
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

    /// Override the User-Agent header
    #[arg(long, default_value = DEFAULT_USER_AGENT)]
    user_agent: String,

    /// Override the Accept-Language header
    #[arg(long, default_value = DEFAULT_ACCEPT_LANGUAGE)]
    accept_language: String,

    #[command(flatten)]
    viewport: ViewportArgs,

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

    /// Prefetch image-like URLs referenced directly from HTML (true/false)
    #[arg(
      long,
      default_value_t = false,
      action = ArgAction::Set,
      num_args = 0..=1,
      default_missing_value = "true"
    )]
    prefetch_images: bool,

    /// Prefetch iframe/object/embed documents referenced directly from HTML (true/false)
    ///
    /// This can explode on pages that embed many third-party iframes, so it defaults to off.
    #[arg(
      long,
      alias = "prefetch-documents",
      default_value_t = false,
      action = ArgAction::Set,
      num_args = 0..=1,
      default_missing_value = "true"
    )]
    prefetch_iframes: bool,

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
    discovered_images: usize,
    fetched_images: usize,
    failed_images: usize,
    discovered_documents: usize,
    fetched_documents: usize,
    failed_documents: usize,
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

  struct PrefetchImportLoader<'a> {
    fetcher: &'a dyn ResourceFetcher,
    css_cache: RefCell<HashMap<String, String>>,
    summary: &'a RefCell<PageSummary>,
  }

  impl<'a> PrefetchImportLoader<'a> {
    fn new(fetcher: &'a dyn ResourceFetcher, summary: &'a RefCell<PageSummary>) -> Self {
      Self {
        fetcher,
        css_cache: RefCell::new(HashMap::new()),
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

  #[derive(Debug, Clone)]
  enum StylesheetTask {
    Inline(String),
    External(String),
  }

  fn stylesheet_type_is_css(type_attr: Option<&str>) -> bool {
    match type_attr {
      None => true,
      Some(value) => {
        let mime = value.split(';').next().map(str::trim).unwrap_or("");
        mime.is_empty() || mime.eq_ignore_ascii_case("text/css")
      }
    }
  }

  fn media_attr_allows(
    media_attr: Option<&str>,
    media_ctx: &MediaContext,
    cache: &mut MediaQueryCache,
  ) -> bool {
    match media_attr {
      None => true,
      Some(media) => {
        let trimmed = media.trim();
        if trimmed.is_empty() {
          return true;
        }

        match MediaQuery::parse_list(trimmed) {
          Ok(list) => media_ctx.evaluate_list_with_cache(&list, Some(cache)),
          Err(_) => false,
        }
      }
    }
  }

  fn discover_dom_stylesheet_tasks(
    html: &str,
    base_url: &str,
    media_ctx: &MediaContext,
    media_query_cache: &mut MediaQueryCache,
  ) -> Option<Vec<StylesheetTask>> {
    let dom = parse_html(html).ok()?;
    let scoped_sources = extract_scoped_css_sources(&dom);
    let toggles = runtime::runtime_toggles();
    let fetch_link_css = toggles.truthy_with_default("FASTR_FETCH_LINK_CSS", true);
    let preload_stylesheets_enabled =
      toggles.truthy_with_default("FASTR_FETCH_PRELOAD_STYLESHEETS", true);
    let modulepreload_stylesheets_enabled =
      toggles.truthy_with_default("FASTR_FETCH_MODULEPRELOAD_STYLESHEETS", false);
    let alternate_stylesheets_enabled =
      toggles.truthy_with_default("FASTR_FETCH_ALTERNATE_STYLESHEETS", true);

    let mut tasks = Vec::new();
    let mut seen_external: HashSet<String> = HashSet::new();

    let mut consider_source = |source: &StylesheetSource| match source {
      StylesheetSource::Inline(inline) => {
        if inline.disabled || !stylesheet_type_is_css(inline.type_attr.as_deref()) {
          return;
        }
        if !media_attr_allows(inline.media.as_deref(), media_ctx, media_query_cache) {
          return;
        }
        if inline.css.trim().is_empty() {
          return;
        }
        tasks.push(StylesheetTask::Inline(inline.css.clone()));
      }
      StylesheetSource::External(link) => {
        if !fetch_link_css {
          return;
        }
        if link.disabled
          || !link_rel_is_stylesheet_candidate(
            &link.rel,
            link.as_attr.as_deref(),
            preload_stylesheets_enabled,
            modulepreload_stylesheets_enabled,
            alternate_stylesheets_enabled,
          )
          || !stylesheet_type_is_css(link.type_attr.as_deref())
        {
          return;
        }
        if !media_attr_allows(link.media.as_deref(), media_ctx, media_query_cache) {
          return;
        }
        if link.href.trim().is_empty() {
          return;
        }

        let Some(stylesheet_url) = resolve_href_with_base(Some(base_url), &link.href) else {
          return;
        };
        if seen_external.insert(stylesheet_url.clone()) {
          tasks.push(StylesheetTask::External(stylesheet_url));
        }
      }
    };

    for source in scoped_sources.document.iter() {
      consider_source(source);
    }

    let mut shadow_hosts: Vec<usize> = scoped_sources.shadows.keys().copied().collect();
    shadow_hosts.sort_unstable();
    for host in shadow_hosts {
      if let Some(sources) = scoped_sources.shadows.get(&host) {
        for source in sources {
          consider_source(source);
        }
      }
    }

    Some(tasks)
  }

  fn prefetch_fonts_from_stylesheet(
    fetcher: &dyn ResourceFetcher,
    css_base_url: &str,
    sheet: &StyleSheet,
    media_ctx: &MediaContext,
    media_query_cache: &mut MediaQueryCache,
    seen_fonts: &mut HashSet<String>,
    summary: &mut PageSummary,
  ) {
    for face in sheet.collect_font_face_rules_with_cache(media_ctx, Some(media_query_cache)) {
      for source in face.sources {
        let FontFaceSource::Url(url_source) = source else {
          continue;
        };
        let Some(resolved) = resolve_href(css_base_url, &url_source.url) else {
          continue;
        };
        if resolved.starts_with("data:") {
          continue;
        }
        if !seen_fonts.insert(resolved.clone()) {
          continue;
        }
        match fetcher.fetch(&resolved) {
          Ok(_) => summary.fetched_fonts += 1,
          Err(_) => summary.failed_fonts += 1,
        }
      }
    }
  }

  fn prefetch_page(
    entry: &PagesetEntry,
    fetcher: &dyn ResourceFetcher,
    media_ctx: &MediaContext,
    prefetch_fonts: bool,
    prefetch_images: bool,
    prefetch_iframes: bool,
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

    let mut media_query_cache = MediaQueryCache::default();

    let mut tasks: Vec<StylesheetTask> =
      match discover_dom_stylesheet_tasks(&html, &base_url, media_ctx, &mut media_query_cache) {
        Some(tasks) => {
          if tasks.is_empty() {
            // Pages that load their primary stylesheet dynamically (without emitting `<link rel="stylesheet">`
            // or `<style>`) are still best-effort handled by scanning the raw HTML for `.css`-looking
            // substrings.
            let mut out = Vec::new();
            let mut seen = HashSet::new();
            if let Ok(urls) = extract_embedded_css_urls(&html, &base_url) {
              for url in urls {
                if seen.insert(url.clone()) {
                  out.push(StylesheetTask::External(url));
                }
              }
            }
            out
          } else {
            tasks
          }
        }
        None => {
          // DOM parse failed; fall back to the string-based extraction used by older versions.
          let mut out = Vec::new();
          let mut seen = HashSet::new();
          if let Ok(urls) = extract_css_links(&html, &base_url, media_ctx.media_type) {
            for url in urls {
              if seen.insert(url.clone()) {
                out.push(StylesheetTask::External(url));
              }
            }
          }
          if let Ok(urls) = extract_embedded_css_urls(&html, &base_url) {
            for url in urls {
              if seen.insert(url.clone()) {
                out.push(StylesheetTask::External(url));
              }
            }
          }
          out
        }
      };

    summary.borrow_mut().discovered_css = tasks.len();
    if tasks.is_empty() && !(prefetch_images || prefetch_iframes) {
      return summary.into_inner();
    }

    // Process external stylesheets in sorted order for more reproducible network/cache behavior.
    tasks.sort_by(|a, b| match (a, b) {
      (StylesheetTask::Inline(_), StylesheetTask::External(_)) => std::cmp::Ordering::Less,
      (StylesheetTask::External(_), StylesheetTask::Inline(_)) => std::cmp::Ordering::Greater,
      (StylesheetTask::Inline(_), StylesheetTask::Inline(_)) => std::cmp::Ordering::Equal,
      (StylesheetTask::External(a), StylesheetTask::External(b)) => a.cmp(b),
    });

    let import_loader = PrefetchImportLoader::new(fetcher, &summary);
    let mut seen_fonts: HashSet<String> = HashSet::new();

    for task in tasks {
      match task {
        StylesheetTask::Inline(css) => {
          let sheet: StyleSheet = match parse_stylesheet(&css) {
            Ok(sheet) => sheet,
            Err(_) => continue,
          };

          let resolved = match sheet.resolve_imports_with_cache(
            &import_loader,
            Some(&base_url),
            media_ctx,
            Some(&mut media_query_cache),
          ) {
            Ok(sheet) => sheet,
            Err(_) => sheet,
          };

          if prefetch_fonts {
            let mut summary = summary.borrow_mut();
            prefetch_fonts_from_stylesheet(
              fetcher,
              &base_url,
              &resolved,
              media_ctx,
              &mut media_query_cache,
              &mut seen_fonts,
              &mut summary,
            );
          }
        }
        StylesheetTask::External(css_url) => match fetcher.fetch(&css_url) {
          Ok(res) => {
            summary.borrow_mut().fetched_css += 1;
            let sheet_base = res.final_url.as_deref().unwrap_or(&css_url);
            let css_text = decode_css_bytes(&res.bytes, res.content_type.as_deref());
            let css_text = match absolutize_css_urls(&css_text, sheet_base) {
              Ok(css) => css,
              Err(_) => css_text,
            };

            let sheet: StyleSheet = match parse_stylesheet(&css_text) {
              Ok(sheet) => sheet,
              Err(_) => continue,
            };

            let resolved = match sheet.resolve_imports_with_cache(
              &import_loader,
              Some(sheet_base),
              media_ctx,
              Some(&mut media_query_cache),
            ) {
              Ok(sheet) => sheet,
              Err(_) => sheet,
            };

            if prefetch_fonts {
              let mut summary = summary.borrow_mut();
              prefetch_fonts_from_stylesheet(
                fetcher,
                sheet_base,
                &resolved,
                media_ctx,
                &mut media_query_cache,
                &mut seen_fonts,
                &mut summary,
              );
            }
          }
          Err(_) => summary.borrow_mut().failed_css += 1,
        },
      }
    }

    if prefetch_images || prefetch_iframes {
      let html_assets = discover_html_asset_urls(&html, &base_url);
      let mut image_urls = html_assets.images;
      let mut document_urls = html_assets.documents;

      if prefetch_images {
        summary.borrow_mut().discovered_images = image_urls.len();
        image_urls.sort();
        for url in image_urls {
          if url.starts_with("data:") {
            continue;
          }
          match fetcher.fetch(&url) {
            Ok(_) => summary.borrow_mut().fetched_images += 1,
            Err(_) => summary.borrow_mut().failed_images += 1,
          }
        }
      }

      if prefetch_iframes {
        summary.borrow_mut().discovered_documents = document_urls.len();
        document_urls.sort();
        for url in document_urls {
          if url.starts_with("data:") {
            continue;
          }
          match fetcher.fetch(&url) {
            Ok(_) => summary.borrow_mut().fetched_documents += 1,
            Err(_) => summary.borrow_mut().failed_documents += 1,
          }
        }
      }
    }

    summary.into_inner()
  }

  fn log_disk_cache_stats(
    phase: &str,
    cache_dir: &Path,
    lock_stale_after: Duration,
    max_bytes: u64,
    lock_stale_secs: u64,
  ) {
    let stats = match scan_disk_cache_dir(cache_dir, lock_stale_after) {
      Ok(stats) => stats,
      Err(err) => {
        println!("Disk cache stats ({phase}): unavailable ({err})");
        return;
      }
    };

    println!(
      "Disk cache stats ({phase}): bin_count={} meta_count={} alias_count={} bin_bytes={} locks={} stale_locks={} tmp={} journal={}",
      stats.bin_count,
      stats.meta_count,
      stats.alias_count,
      stats.bin_bytes,
      stats.lock_count,
      stats.stale_lock_count,
      stats.tmp_count,
      stats.journal_bytes
    );
    println!("{}", stats.usage_summary(max_bytes));

    if max_bytes != 0 && stats.bin_bytes > max_bytes {
      println!(
        "Warning: disk cache usage exceeds max_bytes (bin_bytes={} > max_bytes={}). Consider increasing --disk-cache-max-bytes or setting FASTR_DISK_CACHE_MAX_BYTES=0 to disable eviction.",
        stats.bin_bytes, max_bytes
      );
    }
    if stats.stale_lock_count > 0 {
      println!(
        "Warning: disk cache contains {} stale .lock file(s). Consider tuning FASTR_DISK_CACHE_LOCK_STALE_SECS (currently {}).",
        stats.stale_lock_count, lock_stale_secs
      );
    }
    println!();
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

    let media_ctx = MediaContext::screen(
      args.viewport.viewport.0 as f32,
      args.viewport.viewport.1 as f32,
    )
    .with_device_pixel_ratio(args.viewport.dpr)
    .with_env_overrides();
    let http = build_http_fetcher(&args.user_agent, &args.accept_language, timeout_secs);
    let mut disk_config = args.disk_cache.to_config();
    let lock_stale_after = disk_config.lock_stale_after;
    disk_config.namespace = Some(disk_cache_namespace(
      &args.user_agent,
      &args.accept_language,
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
    if args.prefetch_images || args.prefetch_iframes {
      println!(
        "HTML assets: images={} iframes={}",
        args.prefetch_images, args.prefetch_iframes
      );
    }
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
    log_disk_cache_stats(
      "start",
      &args.cache_dir,
      lock_stale_after,
      args.disk_cache.max_bytes,
      args.disk_cache.lock_stale_secs,
    );
    println!();

    let pool = ThreadPoolBuilder::new()
      .num_threads(args.jobs)
      .build()
      .expect("create thread pool");

    let mut results: Vec<PageSummary> = pool.install(|| {
      selected
        .par_iter()
        .map(|entry| {
          prefetch_page(
            entry,
            fetcher.as_ref(),
            &media_ctx,
            args.prefetch_fonts,
            args.prefetch_images,
            args.prefetch_iframes,
          )
        })
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
    let mut total_images_discovered = 0usize;
    let mut total_images_fetched = 0usize;
    let mut total_images_failed = 0usize;
    let mut total_documents_discovered = 0usize;
    let mut total_documents_fetched = 0usize;
    let mut total_documents_failed = 0usize;

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
      total_images_discovered += r.discovered_images;
      total_images_fetched += r.fetched_images;
      total_images_failed += r.failed_images;
      total_documents_discovered += r.discovered_documents;
      total_documents_fetched += r.fetched_documents;
      total_documents_failed += r.failed_documents;

      if args.prefetch_images || args.prefetch_iframes {
        println!(
          "• {} css={} fetched={} failed={} imports_fetched={} imports_failed={} fonts_fetched={} fonts_failed={} images={} img_fetched={} img_failed={} docs={} docs_fetched={} docs_failed={}",
          r.stem,
          r.discovered_css,
          r.fetched_css,
          r.failed_css,
          r.fetched_imports,
          r.failed_imports,
          r.fetched_fonts,
          r.failed_fonts,
          r.discovered_images,
          r.fetched_images,
          r.failed_images,
          r.discovered_documents,
          r.fetched_documents,
          r.failed_documents,
        );
      } else {
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
    }

    println!();
    if args.prefetch_images || args.prefetch_iframes {
      println!(
        "Done: css_discovered={} css_fetched={} css_failed={} pages_skipped={} imports_fetched={} imports_failed={} fonts_fetched={} fonts_failed={} images_discovered={} images_fetched={} images_failed={} docs_discovered={} docs_fetched={} docs_failed={}",
        total_discovered,
        total_fetched,
        total_failed,
        total_skipped,
        total_imports_fetched,
        total_imports_failed,
        total_fonts_fetched,
        total_fonts_failed,
        total_images_discovered,
        total_images_fetched,
        total_images_failed,
        total_documents_discovered,
        total_documents_fetched,
        total_documents_failed,
      );
    } else {
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
    }
    println!();
    log_disk_cache_stats(
      "end",
      &args.cache_dir,
      lock_stale_after,
      args.disk_cache.max_bytes,
      args.disk_cache.lock_stale_secs,
    );

    // Best-effort tool: do not fail the process on fetch errors.
  }
}

#[cfg(feature = "disk_cache")]
fn main() {
  disk_cache_main::main();
}
